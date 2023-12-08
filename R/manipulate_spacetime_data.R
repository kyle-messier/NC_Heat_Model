library(data.table) # -- for large flat datasets

#' Create a new type of spacetime object (stdtobj)
#'
#' @param stdt a data.table with lat, lon, time columns
#' @param crs_stdt a character with the crs of lat-lon
#' @returns a list with stdt and crs_stdt as a new class
#' @export
create_stdtobj <- function(stdt, crs_stdt) {
  if (class(stdt)[1] != "data.table") {
    stop("stdt is not a data.table.")
  }
  if (!(all(c("lon", "lat", "time") %in% colnames(stdt)))) {
    stop("one of lon, lat, time columns missing or mispelled.")
  }
  if (class(crs_stdt) != "character") {
    stop("crs_stdt is not a character.")
  }
  stdtobj <- list("stdt" = stdt, "crs_stdt" = crs_stdt)
  class(stdtobj) <- c("list", "stdtobj")
  return(stdtobj)
}

#' Boolean to know if an object correspond to a stdtobj
#'
#' @param obj an object
#' @returns a boolean to know if obj is from newly created class "stdtobj"
#' @export
is_stdtobj <- function(obj) {
  if (!(identical(class(obj), c("list", "stdtobj")))) {
    return(FALSE)
  } else if (!(identical(names(obj), c("stdt", "crs_stdt")))) {
    return(FALSE)
  } else if (!(identical(class(obj$stdt)[1], "data.table"))) {
    return(FALSE)
  } else if (!(all(c("lon", "lat", "time") %in% colnames(obj$stdt)))) {
    return(FALSE)
  } else if (!(identical(class(obj$crs_stdt)[1], "character"))) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

#' Create a new type of spacetime object (starrayobj)
#'
#' @param starray a 4D array with dimensions lon, lat, time, variable
#' @param crs_starray a character with the crs of lat-lon
#' @returns a list with starray and crs_starray as a new class
#' @export
create_starrayobj <- function(starray, crs_starray) {
  if (class(starray) != "array") {
    stop("starray is not an array.")
  }
  if (!(identical(c("lon", "lat", "time", "variable"), names(dim(starray))))) {
    stop("array dimensions are not lon, lat, time, variable.")
  }
  if (class(crs_starray) != "character") {
    stop("crs_starray is not a character.")
  }
  starray <- list("starray" = starray, "crs_starray" = crs_starray)
  class(starray) <- c("list", "starrayobj")
  return(starray)
}

#' Boolean to know if an object correspond to a starrayobj
#'
#' @param obj an object
#' @returns a boolean to know if obj is from newly created class "starrayobj"
#' @export
is_starrayobj <- function(obj) {
  if (!(identical(class(obj), c("list", "starrayobj")))) {
    return(FALSE)
  } else if (!(identical(names(obj), c("starray", "crs_starray")))) {
    return(FALSE)
  } else if (!(identical(class(obj$starray), "array"))) {
    return(FALSE)
  } else if (!(identical(
    c("lon", "lat", "time", "variable"),
    names(dim(obj$starray))
  ))) {
    return(FALSE)
  } else if (!(identical(class(obj$crs_starray)[1], "character"))) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

#' Convert stdtobj to starrayobj
#'
#' @param stdtobj A stdt object: data.table object with columns
#' "lat", "lon", "time" + a set of covariates and associated crs
#' @returns a starrayobj object: 4D array with 4 dimensions corresponding to
#' lat, lon, time, variable and associated crs
#' To plot the timeserie: plot(starray[i,j,,l])
#' To map one variable at one time: image(starray[,,k,l])
#' @export
convert_stdt_starray <- function(stdtobj) {
  if (!is_stdtobj(stdtobj)) {
    stop("stdtobj is not a stdt object.")
  }
  stdt <- stdtobj$stdt
  stdt_complete <- tidyr::expand_grid(
    lon = unique(stdt$lon),
    lat = unique(stdt$lat),
    time = unique(stdt$time)
  )
  stdt_complete <- merge(stdt,
    stdt_complete,
    by = c("lon", "lat", "time"),
    all.y = TRUE
  )
  # reshape stdt from wide to long format
  stdt_long <- maditr::melt(stdt_complete, id.vars = c("lon", "lat", "time"))
  # sort columns in this order: variable -> time -> lat -> lon
  data.table::setorderv(stdt_long, c("variable", "time", "lat", "lon"))
  dimnames <- lapply(stdt_long[, 1:4], unique)
  starray <- array(stdt_long$value,
    dim = lengths(dimnames),
    dimnames = dimnames
  )
  starrayobj <- create_starrayobj(starray, stdtobj$crs_stdt)
  return(starrayobj)
}


#' Convert starrayobj to stdtobj
#'
#' @param starrayobj A starrayobj
#' @returns a stdtobj
#' @export
convert_starray_stdt <- function(starrayobj) {
  if (!is_starrayobj(starrayobj)) {
    stop("starrayobj is not a starray object.")
  }
  starray <- starrayobj$starray
  long_starray <- reshape2::melt(starray)
  wide_starray <- reshape(long_starray,
    idvar = c("lon", "lat", "time"),
    timevar = "variable",
    direction = "wide"
  )
  colnames(wide_starray) <- stringr::str_replace(
    colnames(wide_starray),
    "value.",
    ""
  )
  stdt <- data.table::as.data.table(wide_starray)
  stdtobj <- create_stdtobj(stdt, starrayobj$crs_starray)
  return(stdtobj)
}


#' Convert any kind of st object (sftime, SpatVector or SpatRasterDataset)
#' to stdtobj
#'
#' @param stobj sftime, SpatVector or SpatRasterDataset following some standard
#' in columns naming
#' @returns a stdtobj
#' @export
convert_stobj_to_stdt <- function(stobj) {
  format <- class(stobj)[[1]]
  if (format == "sf" || format == "sftime") {
    if (any(!(c("geometry", "time") %in% colnames(stobj)))) {
      stop("stobj does not contain geometry and time columns")
    }
    crs_dt <- as.character(sf::st_crs(stobj))[1]
    stobj$lon <- sf::st_coordinates(stobj)[, 1]
    stobj$lat <- sf::st_coordinates(stobj)[, 2]
    stdt <- data.table::as.data.table(stobj)
    stdt <- stdt[, geometry := NULL]
  } else if (format == "SpatVector") {
    if (!("time") %in% names(stobj)) {
      stop("stobj does not contain time column")
    }
    crs_dt <- crs(stobj)
    stdf <- as.data.frame(stobj, geom = "XY")
    names(stdf)[names(stdf) == "x"] <- "lon"
    names(stdf)[names(stdf) == "y"] <- "lat"
    stdt <- data.table::as.data.table(stdf)
  } else if (format == "SpatRasterDataset") {
    crs_dt <- crs(stobj)
    stdf <- as.data.frame(stobj[1], xy = TRUE)
    colnames(stdf)[1] <- "lon"
    colnames(stdf)[2] <- "lat"
    # -- tranform from wide to long format
    stdf <- stdf %>% tidyr::pivot_longer(
      cols = 3:ncol(stdf),
      names_to = "time",
      values_to = names(stobj)[1]
    )
    for (var in names(stobj)[2:length(names(stobj))]) {
      # test that the ts is identical to the ts of the 1st variable
      if (!(identical(names(stobj[var]), names(stobj[1])))) {
        stop("Error in SpatRastDataset: timeserie is different for at least
             2 variables - or not ordered for one of these.")
      }
      df_var <- as.data.frame(stobj[var], xy = TRUE)
      # -- tranform from wide to long format
      df_var <- df_var %>% tidyr::pivot_longer(
        cols = 3:ncol(df_var),
        names_to = "time",
        values_to = var
      )
      stdf[, var] <- df_var[, var]
    }
    stdt <- data.table::as.data.table(stdf)
  } else {
    stop("stobj class not accepted")
  }
  stdtobj <- create_stdtobj(stdt, crs_dt)
  return(stdtobj)
}


#' Convert a stdtobj to SpatVector
#'
#' @param stdtobj A stdtobj
#' @returns a SpatVector
#' @export
convert_stdt_spatvect <- function(stdtobj) {
  if (!is_stdtobj(stdtobj)) {
    stop("stdtobj is not an stdt object")
  }
  vect_obj <- vect(stdtobj$stdt,
    geom = c("lon", "lat"),
    crs = stdtobj$crs_stdt,
    keepgeom = FALSE
  )
  return(vect_obj)
}


#' Convert a stdtobj to sftime
#'
#' @param stdtobj A stdtobj
#' @returns a sftime object
#' @export
convert_stdt_sftime <- function(stdtobj) {
  if (!is_stdtobj(stdtobj)) {
    stop("stdtobj is not an stdt object")
  }
  stdt <- stdtobj$stdt
  stdt$time <- as.Date(stdt$time)
  sftime_obj <- sftime::st_as_sftime(stdt,
    coords = c("lon", "lat"),
    time_column_name = "time",
    crs = stdtobj$crs_stdt
  )
  return(sftime_obj)
}


#' Convert a stdtobj to SpatRasterDataset
#'
#' @param stdtobj A stdtobj
#' @returns a SpatRasterDataset with each raster corresponding to one variable
#' (layers are the time serie)
#' @export
convert_stdt_spatrastdataset <- function(stdtobj) {
  if (!is_stdtobj(stdtobj)) {
    stop("stdtobj is not an stdt object")
  }
  df <- as.data.frame(stdtobj$stdt)
  col <- colnames(df)
  variables <- col[!(col %in% c("lon", "lat", "time"))]
  rast_list <- list()
  for (var in variables) {
    newdf <- reshape(df[, c("lon", "lat", "time", var)],
      idvar = c("lon", "lat"),
      timevar = "time",
      direction = "wide"
    )
    colnames(newdf) <- stringr::str_replace(
      colnames(newdf),
      paste0(var, "."),
      ""
    )
    var_rast <- terra::as_spatraster(newdf,
      xycols = c(1, 2),
      crs = stdtobj$crs_stdt
    )
    rast_list[[var]] <- var_rast
  }
  rastdt_obj <- terra::sds(rast_list)
  return(rastdt_obj)
}

#' Create a sf object from a data.table
#'
#' @param datatable A data.table object with columns "lat", "lon"
#' @param crs A character containing the original crs
#' @returns an sf object
create_sf <- function(datatable, crs) {
  if (!("data.table" %in% class(datatable))) {
    stop("datatable is not a data.table")
  }
  if (class(crs) != "character") {
    stop("crs is not a character")
  }
  if (!("lon" %in% colnames(datatable))) {
    stop("datatable does not contain lon column")
  }
  if (!("lat" %in% colnames(datatable))) {
    stop("datatable does not contain lat column")
  }
  data_sf <- sf::st_as_sf(datatable,
    coords = c("lon", "lat"),
    remove = FALSE,
    crs = crs
  )
  return(data_sf)
}


#' Create a sftime object from a data.table
#'
#' @param datatable A data.table object with columns "lat", "lon", "date"
#' @param crs A character containing the original crs
#' @returns an sftime object
create_sftime <- function(datatable, crs) {
  if (!("data.table" %in% class(datatable))) {
    stop("datatable is not a data.table")
  }
  if (class(crs) != "character") {
    stop("crs is not a character")
  }
  if (!("lon" %in% colnames(datatable))) {
    stop("datatable does not contain lon column")
  }
  if (!("lat" %in% colnames(datatable))) {
    stop("datatable does not contain lat column")
  }
  if (!("date" %in% colnames(datatable))) {
    stop("datatable does not contain date column")
  }
  datatable$date <- as.Date(datatable$date)
  data_sft <- sf::st_as_sftime(datatable,
    coords = c("lon", "lat"),
    remove = FALSE,
    crs = crs,
    time_column_name = "date"
  )
  return(data_sft = data_sft)
}


#' Project coordinates in a datatable from crs_ori to crs_dest
#'
#' @param datatable A data.table object with columns "lat", "lon"
#' @param crs_ori A character containing the original crs of spatial data
#' @param crs_dest A character containing the destination crs of spatial data
#' @returns same datatable object with "lon", "lat",
#' "lon_ori", "lat_ori" columns
project_dt <- function(datatable, crs_ori, crs_dest) {
  if (class(crs_ori) != "character" || class(crs_dest) != "character") {
    stop("crs are not characters")
  }
  if (!("data.table" %in% class(datatable))) {
    stop("datatable is not a data.table")
  }
  if (!("lat" %in% colnames(datatable))) {
    stop("datatable does not contain lat column")
  }
  if (!("lon" %in% colnames(datatable))) {
    stop("datatable does not contain lon column")
  }
  loc <- unique(datatable[, c("lon", "lat")])
  loc_sf <- create_sf(loc, crs_ori)
  loc_sf <- sf::st_transform(loc_sf, crs_dest)
  colnames(loc_sf)[colnames(loc_sf) == "lon"] <- "lon_ori"
  colnames(loc_sf)[colnames(loc_sf) == "lat"] <- "lat_ori"
  loc_sf <- loc_sf %>%
    dplyr::mutate(
      lon = unlist(purr::map(loc_sf$geometry, 1)),
      lat = unlist(purr::map(loc_sf$geometry, 2))
    )
  loc_proj <- data.table::as.data.table(loc_sf)
  loc_proj <- loc_proj[, geometry := NULL]
  # renaming is only valid within the function
  colnames(datatable)[colnames(datatable) == "lon"] <- "lon_ori"
  colnames(datatable)[colnames(datatable) == "lat"] <- "lat_ori"
  datatable_proj <- merge(
    datatable,
    loc_proj,
    by = c("lon_ori", "lat_ori")
  )
  return(datatable_proj)
}
