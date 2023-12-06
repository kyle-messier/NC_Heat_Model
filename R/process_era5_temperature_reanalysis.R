
era5_file_nc <- "../input/era5_hourly_reanalysis_20220531_20220930.nc"

#' Convert nc file with ERA5 data to a datatable
#'
#' @param era5_file a character path to era5 file
#' with longitude, latitude, time and t2m covariates
#' @returns a datatable with columns geom, date, lon and lat in EPSG:4326
#' @export
convert_era5nc_to_dt <- function(era5_file = era5_file_nc) {
  era5 <- ncdf4::nc_open(era5_file)
  lon <- ncdf4::ncvar_get(era5, "longitude")
  lat <- ncdf4::ncvar_get(era5, "latitude")
  time <- ncdf4::ncvar_get(era5, "time")
  t2m <- ncdf4::ncvar_get(era5, "t2m") - 273.15
  na_value <- ncdf4::ncatt_get(era5, "t2m", "_FillValue")$value
  t2m[t2m == na_value] <- NA
  time <- as.POSIXct(time * 3600, origin = "1900-01-01", tz = "UTC")
  dt <- t2m %>%
    reshape2::melt() %>%
    data.table::setDT()
  dt <- dt[, .(time = time["Var3"],
               lon = lon["Var1"],
               lat = lat["Var2"],
               t2m = "value")]
  pixels <- unique(dt[, c("lon", "lat")])
  pixels$geom <- index(pixels)
  dt <- merge(dt, pixels, by = c("lon", "lat"))
  return(dt)
}

#' Compute standard date for tn with WMO definition
#'
#' @param x_edt a Date object
#' @returns a character
assign_date_tn_wmo <- function(x_edt) {
  return(ifelse(hour(x_edt) >= 18,
                substr(x_edt + days(1), 0, 10),
                substr(x_edt, 0, 10)))
}


#' Compute standard date for tn with 7am to 7am (localtime) definition
#'
#' @param x_edt a Date object
#' @returns a character
assign_date_7am <- function(x_edt) {
  return(ifelse(hour(x_edt) >= 7, substr(x_edt, 0, 10),
    substr(x_edt - days(1), 0, 10)
  ))
}

#' Compute standard date for tn with 12am to 12am definition
#'
#' @param x_edt a Date object
#' @returns a character
assign_date_12am <- function(x_edt) {
  return(substr(x_edt, 0, 10))
}

#' Compute daily minimum temperature with different definitions of day
#'
#' @param dt a data.table object
#' @returns a data.table object with columns tnwmo, tn7am, tn12am
#' @export
compute_tn <- function(dt) {
  # EDT = Eastern Daylight Time (consider clock-changing)
  # EST = Eastern Standard Time
  dt <- dt[, ":="(time_edt = as.POSIXct(
                                        with_tz(time,
                                                tzone = "America/New_York"),
                                        format = "%Y-%m-%d %H:%M:%s"))]

  dt <- dt[, ":="(datetnwmo = as.character(assign_date_tn_wmo("time")),
                  date7am = as.character(assign_date_7am("time_edt")),
                  date12am = as.character(assign_date_12am("time_edt")))]

  dt_tnwmo <- dt[, .(tnwmo = min("t2m")),
    keyby = c("geom", "datetnwmo", "lon", "lat")
  ]
  dt_tn7am <- dt[, .(tn7am = min("t2m")),
    keyby = c("geom", "date7am", "lon", "lat")
  ]
  dt_tn12am <- dt[, .(tn12am = min("t2m")),
    keyby = c("geom", "date12am", "lon", "lat")
  ]

  # merge the 3 data.tables
  dt_tn <- merge(dt_tnwmo, dt_tn7am,
    by.x = c("geom", "datetnwmo", "lon", "lat"),
    by.y = c("geom", "date7am", "lon", "lat")
  )
  dt_tn <- merge(dt_tn, dt_tn12am,
    by.x = c("geom", "datetnwmo", "lon", "lat"),
    by.y = c("geom", "date12am", "lon", "lat")
  )
  colnames(dt_tn)[colnames(dt_tn) == "datetnwmo"] <- "date"
  dt_tn$lon <- as.numeric(dt_tn$lon)
  dt_tn$lat <- as.numeric(dt_tn$lat)
  dt_tn$date <- ymd(dt_tn$date)
  # remove first and last date because computation might be erroneous
  dt_tn <- dt_tn[!(date %in% as.Date(range(dt$time)))]
  return(dt_tn)
}

#' Compute standard date for tx with WMO definition
#'
#' @param x a Date object
#' @returns a character
datetxwmo <- function(x) {
  return(ifelse(hour(x) < 6, substr(x - days(1), 0, 10), substr(x, 0, 10)))
}

#' Compute daily maximum temperature with different definitions of day
#'
#' @param dt a data.table object
#' @returns a data.table object with columns txwmo, tx7am, tx12am
#' @export
compute_tx <- function(dt) {
  # EDT = Eastern Daylight Time (consider clock-changing)
  # EST = Eastern Standard Time
  dt <- dt[, ":="(time_edt = as.POSIXct(with_tz(time,
                                                tzone = "America/New_York"),
                                        format = "%Y-%m-%d %H:%M:%s"))]
  dt <- dt[, ":="(datetxwmo = as.character(datetxwmo(time)),
                  date7am = as.character(assign_date_7am("time_edt")),
                  date12am = as.character(assign_date_12am("time_edt")))]
  dt_txwmo <- dt[, .(txwmo = max("t2m")),
    keyby = c("geom", "datetxwmo", "lon", "lat")
  ]
  dt_tx7am <- dt[, .(tx7am = max("t2m")),
    keyby = c("geom", "date7am", "lon", "lat")
  ]
  dt_tx12am <- dt[, .(tx12am = max("t2m")),
    keyby = c("geom", "date12am", "lon", "lat")
  ]
  # merge the 3 data.tables
  dt_tx <- merge(dt_txwmo, dt_tx7am,
    by.x = c("geom", "datetxwmo", "lon", "lat"),
    by.y = c("geom", "date7am", "lon", "lat")
  )
  dt_tx <- merge(dt_tx, dt_tx12am,
    by.x = c("geom", "datetxwmo", "lon", "lat"),
    by.y = c("geom", "date12am", "lon", "lat")
  )
  colnames(dt_tx)[colnames(dt_tx) == "datetxwmo"] <- "date"
  dt_tx$lon <- as.numeric(dt_tx$lon)
  dt_tx$lat <- as.numeric(dt_tx$lat)
  dt_tx$date <- ymd(dt_tx$date)
  # remove first and last date because computation might be erroneous
  dt_tx <- dt_tx[!(date %in% as.Date(range(dt$time)))]
  return(dt_tx)
}
