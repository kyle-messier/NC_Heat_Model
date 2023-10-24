library(terra)
library(ncdf4)
# -- for timeseries
library(lubridate)
library(zoo)
library(xts)

#' Convert nc file with ERA5 data to a datatable
#'
#' @param era5nc a ncdf4 object (default: data from 2022-05-31 to 2022-09-30) 
#' @returns a datatable with columns geom, date, lon and lat in EPSG:4326
#' @export
convert_era5nc_to_dt <- function(
    era5nc = nc_open("../input/era5_hourly_reanalysis_20220531_20220930.nc")) {
  lon <- ncvar_get(era5nc, "longitude")
  lat <- ncvar_get(era5nc, "latitude")
  time <- ncvar_get(era5nc, "time")
  t2m <- ncvar_get(era5nc, "t2m") - 273.15

  na.value <- ncatt_get(era5nc, "t2m", "_FillValue")$value
  t2m[t2m == na.value] <- NA
  time <- as.POSIXct(time * 3600, origin = "1900-01-01", tz = "UTC")

  dt <- t2m %>%
    reshape2::melt() %>%
    setDT()
  dt <- dt[, .(time = time[Var3], 
               lon = lon[Var1], 
               lat = lat[Var2], 
               t2m = value)]

  pixels <- unique(dt[, c("lon", "lat")])
  pixels$geom <- index(pixels)
  dt <- merge(dt, pixels, by = c("lon", "lat"))

  return(dt)
}


#' Compute standard date for TN with WMO definition
#'
#' @param x a Date object
#' @returns a character
dateTNwmo <- function(x) {
  return(ifelse(hour(x) >= 18, substr(x + days(1), 0, 10), substr(x, 0, 10)))
}


#' Compute standard date for TN with 7am to 7am (localtime) definition
#'
#' @param x a Date object
#' @returns a character
date7am <- function(x.EDT) {
  return(ifelse(hour(x.EDT) >= 7, substr(x.EDT, 0, 10),
    substr(x.EDT - days(1), 0, 10)
  ))
}

#' Compute standard date for TN with 12am to 12am definition
#'
#' @param x a Date object
#' @returns a character
date12am <- function(x.EDT) {
  return(substr(x.EDT, 0, 10))
}

#' Compute daily minimum temperature with different definitions of day
#'
#' @param dt a data.table object
#' @returns a data.table object with columns TNwmo, TN7am, TN12am
#' @export
compute_tn <- function(dt) {
  # EDT = Eastern Daylight Time (consider clock-changing)
  # EST = Eastern Standard Time
  dt <- dt[, ":="(time.EDT = as.POSIXct(with_tz(time, 
                                                tzone = "America/New_York"),
    format = "%Y-%m-%d %H:%M:%s"
  ))]

  dt <- dt[, ":="(dateTNwmo = as.character(dateTNwmo(time)),
    date7am = as.character(date7am(time.EDT)),
    date12am = as.character(date12am(time.EDT)))]

  dt_TNwmo <- dt[, .(TNwmo = min(t2m)),
    keyby = c("geom", "dateTNwmo", "lon", "lat")
  ]
  dt_TN7am <- dt[, .(TN7am = min(t2m)),
    keyby = c("geom", "date7am", "lon", "lat")
  ]
  dt_TN12am <- dt[, .(TN12am = min(t2m)),
    keyby = c("geom", "date12am", "lon", "lat")
  ]

  # merge the 3 data.tables
  dt_TN <- merge(dt_TNwmo, dt_TN7am,
    by.x = c("geom", "dateTNwmo", "lon", "lat"),
    by.y = c("geom", "date7am", "lon", "lat")
  )
  dt_TN <- merge(dt_TN, dt_TN12am,
    by.x = c("geom", "dateTNwmo", "lon", "lat"),
    by.y = c("geom", "date12am", "lon", "lat")
  )
  colnames(dt_TN)[colnames(dt_TN) == "dateTNwmo"] <- "date"
  dt_TN$lon <- as.numeric(dt_TN$lon)
  dt_TN$lat <- as.numeric(dt_TN$lat)
  dt_TN$date <- ymd(dt_TN$date)

  # remove first and last date because computation might be erroneous
  dt_TN <- dt_TN[!(date %in% as.Date(range(dt$time)))]
  return(dt_TN)
}

#' Compute standard date for TX with WMO definition
#'
#' @param x a Date object
#' @returns a character
dateTXwmo <- function(x) {
  return(ifelse(hour(x) < 6, substr(x - days(1), 0, 10), substr(x, 0, 10)))
}

#' Compute daily maximum temperature with different definitions of day
#'
#' @param dt a data.table object
#' @returns a data.table object with columns TXwmo, TX7am, TX12am
#' @export
compute_tx <- function(dt) {
  # EDT = Eastern Daylight Time (consider clock-changing)
  # EST = Eastern Standard Time
  dt <- dt[, ":="(time.EDT = as.POSIXct(with_tz(time, 
                                                tzone = "America/New_York"),
    format = "%Y-%m-%d %H:%M:%s"
  ))]

  dt <- dt[, ":="(dateTXwmo = as.character(dateTXwmo(time)),
    date7am = as.character(date7am(time.EDT)),
    date12am = as.character(date12am(time.EDT)))]

  dt_TXwmo <- dt[, .(TXwmo = max(t2m)),
    keyby = c("geom", "dateTXwmo", "lon", "lat")
  ]
  dt_TX7am <- dt[, .(TX7am = max(t2m)),
    keyby = c("geom", "date7am", "lon", "lat")
  ]
  dt_TX12am <- dt[, .(TX12am = max(t2m)),
    keyby = c("geom", "date12am", "lon", "lat")
  ]

  # merge the 3 data.tables
  dt_TX <- merge(dt_TXwmo, dt_TX7am,
    by.x = c("geom", "dateTXwmo", "lon", "lat"),
    by.y = c("geom", "date7am", "lon", "lat")
  )
  dt_TX <- merge(dt_TX, dt_TX12am,
    by.x = c("geom", "dateTXwmo", "lon", "lat"),
    by.y = c("geom", "date12am", "lon", "lat")
  )
  colnames(dt_TX)[colnames(dt_TX) == "dateTXwmo"] <- "date"
  dt_TX$lon <- as.numeric(dt_TX$lon)
  dt_TX$lat <- as.numeric(dt_TX$lat)
  dt_TX$date <- ymd(dt_TX$date)

  # remove first and last date because computation might be erroneous
  dt_TX <- dt_TX[!(date %in% as.Date(range(dt$time)))]
  return(dt_TX)
}

