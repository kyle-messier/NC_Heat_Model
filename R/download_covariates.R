
#' Function to download covariates for a given area and time period
#' @param covar_list: list of covariates to download. Items must be in
#' c("elev", "gridmet", "daymet", "nlcd", "lcz", "pop", "build_fp", "build_h",
#' "imp", "tcc", "fch", "era5")
#' @param area: area of interest (a sf, sfc, SpatRaster or SpatVector object)
#' @param ts: start time
#' @param te: end time
#' @import amadeus
#' @importFrom brassens format_area
#' @importFrom lubridate year
download_covariates <- function(covar_list, area, ts, te, storage_dir) {
  stopifnot(all(covar_list %in% c(
    "elev",
    "gridmet",
    "daymet",
    "nlcd",
    "lcz",
    "pop",
    "build_fp",
    "build_h",
    "imp",
    "tcc",
    "fch",
    "era5"
  )))
  
  # download elevation
  if ("elev" %in% covar_list) {
    amadeus::download_gmted(
      statistic = "Median Statistic",
      resolution = "7.5 arc-seconds",
      directory_to_save = storage_dir,
      acknowledgement = TRUE,
      download = TRUE
    )
  }
  
  # download gridmet
  if ("gridmet" %in% covar_list) {
    amadeus::download_gridmet(
      variables = c("tmmx", "tmmn", "pr", "rmin", "rmax", "srad"),
      year = c(lubridate::year(ts), lubridate::year(te)),
      directory_to_save = storage_dir,
      acknowledgement = TRUE,
      download = TRUE
    )
  }
  
  # download daymet
  # TODO add daymet download through mercury code
  
  # download population
  if ("pop" %in% covar_list) {
    # find pop years to download with ts and te
    pop_year <- function(ts, te) {
      yn_data <- lubridate::year(ts)
      yx_data <- lubridate::year(te)
      y_pop <- seq(2000, 2020, by = 5)
      y_pop[which.min(abs(y_pop - yn_data)):which.min(abs(y_pop - yx_data))]
    }
    for (y in pop_year(ts, te))
    amadeus::download_sedac_population(
      data_resolution = "30 second",
      data_format = c("GeoTIFF"),
      year = y,
      directory_to_save = storage_dir,
      acknowledgement = TRUE,
      download = TRUE
    )
  }
  
  nlcd_year <- function(ts, te) {
    yn_data <- lubridate::year(ts)
    yx_data <- lubridate::year(te)
    y_nlcd <- c(2001, 2004, 2006, 2008, 2011, 2013, 2016, 2019, 2021)
    y_nlcd[which.min(abs(y_nlcd - yn_data)):which.min(abs(y_nlcd - yx_data))]
  }
  # download NLCD data
  if ("nlcd" %in% covar_list) {
    # find NLCD years to download with ts and te
    for (y in nlcd_year(ts, te)) {
      amadeus::download_nlcd(
        collection = "Coterminous United States",
        year = y,
        directory_to_save = storage_dir,
        acknowledgement = TRUE,
        download = TRUE
      )
    }
  }
  
  # download LCZ (local climate zone)
  if ("lcz" %in% covar_list) {
    amadeus::download_lcz(
      directory_to_save = storage_dir,
      acknowledgement = TRUE,
      download = TRUE
    )
  }
  
  # download building footprint
  # TODO see if there is a way to automatically download building footprint
  
  # download building height
  # TODO see if there is a way to automatically download building height
  # (maybe the data is not relevant because too old)
  
  # download imperviousness
  if ("imp" %in% covar_list) {
    for (y in nlcd_year(ts, te)) {
      amadeus::download_imperviousness(
        collection = "Coterminous United States",
        year = 2021,
        directory_to_save = storage_dir,
        acknowledgement = TRUE,
        download = TRUE,
        unzip = TRUE,
        remove_zip = TRUE
      )
    }
  }
  
  # download tree canopy cover
  if ("tcc" %in% covar_list) {
    tcc_year <- function(ts, te) {
      yn_data <- lubridate::year(ts)
      yx_data <- lubridate::year(te)
      y_tcc <- seq(2011, 2021, by = 1)
      y_tcc[which.min(abs(y_tcc - yn_data)):which.min(abs(y_tcc - yx_data))]
    }
    for (y in tcc_year(ts, te)) {
      amadeus::download_tree_canopy_cover(
        collection = "Coterminous United States",
        year = y,
        directory_to_save = storage_dir,
        acknowledgement = TRUE,
        download = TRUE,
        unzip = TRUE,
        remove_zip = TRUE
      )
    }
  }
  
  # download forest canopy height
  if ("fch" %in% covar_list) {
    amadeus::download_forest_canopy_height(
      collection = "NAM",
      directory_to_save = storage_dir,
      acknowledgement = TRUE,
      download = TRUE
    )
  }
  
  # download ERA5
  if ("era5" %in% covar_list) {
  }
  
}


#' Function to download the building footprint .zip file
#' @param storage_path: path to save the .zip file
#' @import downloader
download_build_fp <- function(storage_path) {
  downloader::download(
    url = paste0("https://www.sciencebase.gov/catalog/item/requestDownload/",
      "5e432002e4b0edb47be84652?filePath=",
      "__disk__db%2F49%2F36%2Fdb4936093a8d8fb4caecb7377a989f0a6c885af8"),
    destfile = paste0(storage_path, "build_fp.zip"),
    mode = "wb"
  )
  
}