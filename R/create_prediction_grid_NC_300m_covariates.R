.libPaths("/ddn/gs1/home/marquesel/R/x86_64-redhat-linux-gnu-library/4.3/")

# LOAD TOOLS
pkgs <- c("terra", 
          "sf",
          "dplyr",
          "exactextractr", 
          "tidyterra")
sapply(pkgs, library, character.only = TRUE)

path_base <- "/ddn/gs1/group/set/Projects/NC_Heat_Model/"
source(paste0(path_base, "/R/add_covariates.R"))
source(paste0(path_base, "/R/manipulate_spacetime_data.R"))


# CREATE NORTH CAROLINA GRID 

nc <- vect(paste0(path_base, 
                  "/input/NC_county_boundary/",
                  "North_Carolina_State_and_County_Boundary_Polygons.shp"))

  
imp <- rast(paste0(path_base, "/input/NC_imperviousness_2019.tif"))

# to apply this script to NC
nc_proj <- terra::project(nc, crs(imp))
imp <- terra::mask(imp, nc_proj)

# to quickly test this script on a single county:
#durham <- nc[nc$County == "Durham"]
#durham_proj <- terra::project(durham, crs(imp))
#imp <- terra::mask(imp, durham_proj)

# aggregation at 300m
pred_rast <- aggregate(imp, fact = 10, fun = "mean")
names(pred_rast) <- "imp"

# vector format will also be useful for covariate extraction
pred_vect <- terra::as.points(pred_rast)


# ADD SPATIAL COVARIATES

# Tree canopy height
canopy_h_path = paste0(path_base, "input/NC_forest_height_2019_crs-wgs84.tif")
pred_vect <- add_canopy_h(canopy_h_path, sp_vect = pred_vect)

# Digital elevation model 
dem_path = paste0(path_base, "/input/NC-DEM-agg.tif")
pred_vect <- add_dem(dem_path, sp_vect = pred_vect)

# Tree canopy cover 
tcc_path = paste0(path_base, "/input/NC_tree-canopy-cover_2021.tif")
pred_vect <- add_tcc(tcc_path, sp_vect = pred_vect)

# Building footprint
build_fp_path = paste0(path_base, 
                       "/input/NC_building-footprints/NorthCarolina_sum.tif")
pred_vect <- add_build_fp(sp_vect = pred_vect)

# Building height
build_h_path = paste0(path_base, 
                      "/input/NC_building-height-by-block/",
                      "NC_building-heights-by-block.shp"
)
pred_vect <- add_build_h(build_h_path, sp_vect = pred_vect)

# Land cover ratio
lc <- rast(paste0(path_base, "/input/NC_nlcd_crs-wgs84.tif"))
pred_vect <- add_nlcd_class_ratio(pred_vect, nlcd = lc)

# turn into a raster
pred_rast <- terra::rasterize(pred_vect, 
                              pred_rast, 
                              field = names(pred_vect))

# Terrain covariates
pred_rast <- add_terrain(pred_rast)

# Create SpatRasterDataset with all spatial covariates
pred_rastds <- list()
for (i in names(pred_rast)) {
  pred_rastds[[i]] <- pred_rast[[i]]
}
pred_rastds <- terra::sds(pred_rastds)

# ADD ERA5 COVARIATES 

era5 <- fread(paste0(path_base, 
                     "input/era5_daily_reanalysis_2022-05-02_2022-09-29.csv"))
era5 <- era5 %>% rename(time = date)

# convert era5 to stdt and then to SpatRasterDataset
era5_stdt <- create_stdtobj(era5, "EPSG:4326")
era5_rastds <- convert_stdt_spatrastdataset(era5_stdt)

# empty prediction SpatVector
new_pred_vect <- vect(geom(pred_vect)[, c("x", "y")], 
                      type = "points", 
                      crs = crs(pred_vect))

# extract each daily covariate based on era5 and convert to raster
pred_rastds_era5 <- list()
for (i in 2:7){
  pred_rastds_era5[[i-1]] <- terra::project(new_pred_vect, 
                                            crs(era5_rastds[[i]])) %>%
    terra::extract(x = era5_rastds[[i]], y = ., bind = T) %>%
    terra::project(., crs(new_pred_vect)) %>%
    terra::rasterize(., pred_rast, field = names(.))
}

# turn into a SpatRasterDataset
pred_rastds_era5 <- terra::sds(pred_rastds_era5)
names(pred_rastds_era5) <- names(era5_rastds[[2:7]])


# MERGE SPATIAL AND ERA5 COVARIATES
#pred_rastds <- c(pred_rastds_era5, pred_rastds)


# SAVE OUTPUT PER DAY AS A .TIF
dates <- names(pred_rastds_era5[[1]])
era5_covs <- names(pred_rastds_era5)
list_rast_dates <- list()
for (date in dates) {
  rast_date <- pred_rast
  for (era5_cov in era5_covs) {
    rast_date[[era5_cov]] <- pred_rastds_era5[era5_cov][date]
  } 
  list_rast_dates[[date]] <- rast_date
  writeRaster(rast_date, filename = paste0(path_base, 
                                           "input/pred_grid_", 
                                           date, 
                                           ".tif"))
}


