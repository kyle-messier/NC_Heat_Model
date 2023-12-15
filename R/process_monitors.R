#' Format NOAA observations
#'
#' @param filename character path to noaa observations file
#' @returns a list with two datatable, one for the whole observations and
#' the other with the location metadata only
#' @importFrom magrittr "%>%"
#' @export
format_noaa_aws <- function(filename) {
  na_flag <- NULL
  obs <- data.table::fread(filename)
  # -- add source feature
  map_attrib_network <- function(x) {
    code <- stringr::str_sub(x, -1, -1)
    network <- ifelse(code == "7",
                      "COOP",
                      ifelse(code == "R",
                             "CRN",
                             ifelse(code == "U",
                                    "RAWS",
                                    ifelse(code == "W",
                                           "WBAN",
                                           NA))))
    return(network)
  }
  obs$network_code <- lapply(obs[, c("TMIN_ATTRIBUTES")],
                             FUN = function(x) stringr::str_sub(x, -1, -1))
  obs$network <- lapply(obs[, c("TMIN_ATTRIBUTES")],
                        FUN = function(x) map_attrib_network(x))
  obs <- obs[, c("STATION", "NAME", "network", "LATITUDE", "LONGITUDE",
                 "ELEVATION", "DATE", "TMIN", "TMAX"), ] %>%
    dplyr::rename("id" = "STATION",
                  "name" = "NAME",
                  "lat" = "LATITUDE",
                  "lon" = "LONGITUDE",
                  "elev" = "ELEVATION",
                  "time" = "DATE",
                  "tn" = "TMIN",
                  "tx" = "TMAX")
  obs <- obs[which(!(is.na(obs$tx) | is.na(obs$tn))), ]
  # convert in degree celcius
  obs$tn <- (obs$tn - 32) * 5 / 9
  obs$tx <- (obs$tx - 32) * 5 / 9
  obs$time <- as.Date(obs$time, format = "%Y-%m-%d")
  # flag stations with >5% missing data in tn or tx
  obs_ts_tn <- maditr::dcast(
    obs[, c("time", "id", "tn")],
    time ~ id,
    value.var = "tn"
  ) %>%
    xts::as.xts()
  nb_na_tn <- lapply(obs_ts_tn, FUN = function(x) sum(is.na(x)))
  nb_na_tn <- as.data.frame(do.call(rbind, nb_na_tn))
  nb_na_tn <-  cbind(id = rownames(nb_na_tn), nb_na_tn)
  rownames(nb_na_tn) <- seq_len(nrow(nb_na_tn))
  names(nb_na_tn)[names(nb_na_tn) == "V1"] <- "nb_na_tn"
  obs_ts_tx <- maditr::dcast(
    obs[, c("time", "id", "tx")],
    time ~ id,
    value.var = "tx"
  ) %>%
    xts::as.xts()
  nb_na_tx <- lapply(obs_ts_tx, FUN = function(x) sum(is.na(x)))
  nb_na_tx <- as.data.frame(do.call(rbind, nb_na_tx))
  nb_na_tx <- cbind(id = rownames(nb_na_tx), nb_na_tx)
  rownames(nb_na_tx) <- seq_len(nrow(nb_na_tx))
  names(nb_na_tx)[names(nb_na_tx) == "V1"] <- "nb_na_tx"
  locs <- unique(obs[, c("id", "name", "network", "lat", "lon", "elev")])
  locs <- list(locs, nb_na_tx, nb_na_tn) %>%
    purrr::reduce(dplyr::full_join, by = "id") %>%
    data.table::data.table()
  obs <- list(obs, nb_na_tx, nb_na_tn) %>%
    purrr::reduce(dplyr::full_join, by = "id") %>%
    data.table::data.table()
  obs <- obs[, na_flag := (nb_na_tn > 5 | nb_na_tx > 5), ]
  locs <- locs[, na_flag := (nb_na_tn > 5 | nb_na_tx > 5), ]
  return(list(obs = obs, locs = locs))
}

#' Format econet observations
#'
#' @param path character path to econet observations file
#' @returns a list with two datatable, one for the whole observations and
#' the other with the location metadata only
#' @export
format_econet_aws <- function(path) {
  files <- list.files(path = path, pattern = "M*.xlsx")
  obs <- do.call("rbind", lapply(files, FUN = function(file) {
    openxlsx::read.xlsx(paste0(path, file))
  }))
  colnames(obs) <- c("time", "tx", "tn", "id", "network", "name",
                     "county_default", "lat", "lon", "elev", "support")
  obs[obs == "QCF"] <- NA
  obs$tx <- as.numeric(obs$tx)
  obs$tn <- as.numeric(obs$tn)
  obs$time <- as.Date(obs$time, format = "%Y-%m-%d")
  obs <- data.table::data.table(obs)
  obs$tn <- (obs$tn - 32) * 5 / 9
  obs$tx <- (obs$tx - 32) * 5 / 9
  locs <- unique(obs[, c("id", "name", "network", "lat", "lon", "elev")]) %>%
    data.table::data.table()
  return(list(obs = obs, locs = locs))
}

#' Add all covariates to observation SpatVector
#'
#' @param obs_spvect a SpatVector of AWS observations
#' @param covar_files list of character path to covariates files
#' @returns a SpatVector with all covariates added
#' @export
add_cov <- function(obs_spvect, covar_files) {
  output <- add_imp(covar_files$imp, obs_spvect) %>%
    add_tcc(tcc_path = covar_files$tcc) %>%
    add_terrain(dem_path = covar_files$dem) %>%
    add_canopy_h(canopy_h_path = covar_files$canopy_h) %>%
    add_build_fp(build_fp_path = covar_files$build_fp) %>%
    add_build_h(build_h_path = covar_files$build_h) %>%
    add_nlcd_ratio(nlcd_path = covar_files$nlcd) %>%
    add_county(county_path = covar_files$county) %>%
    add_era5_vect(era5_path = covar_files$era5)
  return(output)
}
