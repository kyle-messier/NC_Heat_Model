
#' Function to download covariates for a given area and time period
#' @param area: area of interest (a sf, sfc, SpatRaster or SpatVector object)
#' @param ts: start time
#' @param te: end time
#' @import amadeus
#' @importFrom brassens format_area
#' @importFrom lubridate year
download_covariates <- function(area, ts, te, storage_dir) {
  # download elevation
  amadeus::download_gmted(
    statistic = "Median Statistic",
    resolution = "7.5 arc-seconds",
    directory_to_save = storage_dir,
    acknowledgement = TRUE,
    download = TRUE
  )
  
  # download gridmet? 
  # download daymet? 
  amadeus::download_sedac_population(
    data_resolution = "hello",
    data_format = c("GeoTIFF"),
    year = lubridate::year(ts):lubridate::year(te),
    directory_to_save = storage_dir,
    acknowledgement = TRUE,
    download = TRUE
  )
  
  # find NLCD years to download with ts and te
  nlcd_year <- function(ts, te) {
    yn_data <- lubridate::year(ts)
    yx_data <- lubridate::year(te)
    y_nlcd <- c(2001, 2004, 2006, 2008, 2011, 2013, 2016, 2019, 2021)
    y_nlcd[which.min(abs(y_nlcd - yn_data)):which.min(abs(y_nlcd - yx_data))]
  }
  
  # download nlcd
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