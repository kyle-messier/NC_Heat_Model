#' Give a list of covariate files names
#'
#' @param covar_folder character path to covariates files
#' @returns a list of covariate file paths
#' @export
list_covar_nc <- function(covar_folder) {
  covar_files <- list(
    imp = paste0(covar_folder, "NC_imperviousness_2019.tif"),
    dem = paste0(covar_folder, "NC-DEM-agg.tif"),
    tcc = paste0(covar_folder, "NC_tree-canopy-cover_2021.tif"),
    canopy_h = paste0(covar_folder, "NC_forest_height_2019_crs-wgs84.tif"),
    build_h = paste0(covar_folder,
                     "NC_building-height-by-block/",
                     "NC_building-heights-by-block.shp"),
    build_fp = paste0(covar_folder,
                      "NC_building-footprints/NorthCarolina_sum.tif"),
    nlcd = paste0(covar_folder,
                  "nlcd_2021_land_cover_l48_20230630/",
                  "nlcd_2021_land_cover_l48_20230630.img"),
    county = paste0(covar_folder,
                    "NC_county_boundary/",
                    "North_Carolina_State_and_County_Boundary_Polygons.shp"),
    era5 = paste0(covar_folder,
                  "era5_daily_reanalysis_2022-05-02_2022-09-29.csv")
  )
  if (!all(sapply(covar_files, file.exists))) {
    warning("Some of the files do not exist.")
  }
  return(covar_files)
}


#' Give a list of monitor files names
#'
#' @param folder character path to monitors folder
#' @returns a list of monitors file paths
#' @export
list_monitors_nc <- function(folder) {
  files <- list(
    noaa = paste0(folder,
                  "NC-AWS-NOAA-dailysummary-20220601-20220831.csv"),
    econet = paste0(folder,
                    "ECONET-stations/",
                    "NC-econet-dailysummary-20220601-20220831.csv"),
    if (!all(sapply(files, file.exists))) {
      warning("Some of the files do not exist.")
    }
  )
  return(files)
}



#' Building height metadata
#'
#' @returns a data.table with all building height metadata
#' @export
build_h_table <- function() {
  table <- data.table::as.data.table(
    list(
      "h_number" = c(
        1, 2, 3,
        4, 5, 6
      ),
      "h_name" = c(
        "Very High",
        "High",
        "Medium-High",
        "Medium",
        "Low-Medium",
        "Low"
      )
    )
  )
  return(table)
}

#' National Land Cover Dataset metadata (NLCD)
#'
#' @returns a data.table with all national land cover dataset metadata
#' @export
nlcd_table <- function() {
  table <- data.table::as.data.table(
    list(value = c(0, 11, 21, 22, 23, 24, 31, 41, 42, 43, 52,
                   71, 81, 82, 90, 95),
         class = c("Unc", "WTR", "OSD", "LID", "MID", "HID",
                   "BRN", "DFO", "EFO", "MFO", "SHB",
                   "GRS", "PAS", "CRP", "WDW", "EHW"),
         names = c("Unclassified",
                   "Open Water",
                   "Developed, Open Space",
                   "Developed, Low Intensity",
                   "Developed, Medium Intensity",
                   "Developed, High Intensity",
                   "Barren Land",
                   "Deciduous Forest",
                   "Evergreen Forest",
                   "Mixed Forest",
                   "Shrub/Scrub",
                   "Herbaceous",
                   "Hay/Pasture",
                   "Cultivated Crops",
                   "Woody Wetlands",
                   "Emergent Herbaceous Wetlands"),
         col = c("white", "#476ba1", "#decaca", "#d99482",
                 "#ee0000", "#ab0000", "#b3aea3", "#68ab63",
                 "#1c6330", "#b5ca8f", "#ccba7d",  "#e3e3c2",
                 "#dcd93d", "#ab7028", "#bad9eb", "#70a3ba"))
  )
  return(table)
}
