era5-reanalysis-processing
================
2023-07-24

Librariesƒ

``` r
# -- for spatial data
#library(raster)   # -- old
#library(rgdal)    # -- deprecated in 10/2023
#library(rgeos)    # -- deprecated in 10/2023
#library(maptools) # -- deprecated in 10/2023
options("sp_evolution_status" = 2) # use sf instead of rgdal and rgeos in sp
library(sp)
library(terra)
library(sf)
library(ncdf4)

# -- for timeseries
library(lubridate)
library(zoo)
library(xts)

# -- 
library(ggplot2)
library(ggspatial)
library(tidyterra)
library(maditr)
library(tidyverse)
library(viridis)
```

#### NC shapefile

``` r
nc.borders <- vect("../input/NC_county_boundary/North_Carolina_State_and_County_Boundary_Polygons.shp")
```

Create a function to map rasters with NC borders

``` r
my.map.raster <- function(raster, date, title){
    plot <- ggplot() +
  geom_spatraster(data = raster) +
  geom_sf(data = st_as_sf(nc.borders), aes(geometry=geometry), 
               colour = "white", linewidth=.3, fill = NA) +
  scale_fill_whitebox_c(
    palette = "muted",
    labels = scales::label_number(suffix = "º"),
    n.breaks = 12,
    guide = guide_legend(reverse = TRUE)
  ) +
  labs(
    fill = "",
    title = title,
    subtitle = date
  ) +
  annotation_scale(
    location="bl", pad_x = unit(1, "cm"), 
        pad_y = unit(1, "cm"), 
        height = unit(0.30, "cm"), 
        text_cex = 1
    ) +
    annotation_north_arrow(
      location = "br", 
      which_north = "true", 
        pad_x = unit(0.2, "cm"), 
        pad_y = unit(0.2, "cm")
    ) +
    theme(
            axis.text = element_text(size=12, family="serif"),
            plot.caption = element_text(size=10, family="serif"),
            legend.text = element_text(size=12, family="serif"),
            legend.title = element_text(size=12, family="serif"),
            panel.background = element_rect(fill = "white"),
            panel.grid.major=element_line(colour="grey")
        )
    
  return(plot)
}


my.map.raster.points <- function(raster, points, points.fill, date, title){
  fill <- enquo(points.fill)
    plot <- ggplot() +
  geom_spatraster(data = raster) +
    geom_sf(data = points, 
             aes(geometry=geometry, fill=!!fill), color='black',
             size=2, shape=21)+
    geom_sf(data = st_as_sf(nc.borders), aes(geometry=geometry), 
               colour = "white", linewidth=.3, fill = NA) +
  scale_fill_whitebox_c(
    palette = "muted",
    labels = scales::label_number(suffix = "º"),
    n.breaks = 12,
    guide = guide_legend(reverse = TRUE)
  ) +
  labs(
    fill = "",
    title = title,
    subtitle = date
  ) +
  annotation_scale(
    location="bl", pad_x = unit(1, "cm"), 
        pad_y = unit(1, "cm"), 
        height = unit(0.30, "cm"), 
        text_cex = 1
    ) +
    annotation_north_arrow(
      location = "br", 
      which_north = "true", 
        pad_x = unit(0.2, "cm"), 
        pad_y = unit(0.2, "cm")
    ) +
    theme(
            axis.text = element_text(size=12, family="serif"),
            plot.caption = element_text(size=10, family="serif"),
            legend.text = element_text(size=12, family="serif"),
            legend.title = element_text(size=12, family="serif"),
            panel.background = element_rect(fill = "white"),
            panel.grid.major=element_line(colour="grey")
        )
    
  return(plot)
}
```

#### Temperature ERA5 reanalysis

Open netcdf data with all JJA hourly ERA5 reanalysis

``` r
t2m.rea <- nc_open("../input/era5_hourly_reanalysis_20220601_20220831.nc")
print(t2m.rea)
```

    ## File ../input/era5_hourly_reanalysis_20220601_20220831.nc (NC_FORMAT_64BIT):
    ## 
    ##      1 variables (excluding dimension variables):
    ##         short t2m[longitude,latitude,time]   
    ##             scale_factor: 0.000480174647219721
    ##             add_offset: 297.344486475176
    ##             _FillValue: -32767
    ##             missing_value: -32767
    ##             units: K
    ##             long_name: 2 metre temperature
    ## 
    ##      3 dimensions:
    ##         longitude  Size:41 
    ##             units: degrees_east
    ##             long_name: longitude
    ##         latitude  Size:15 
    ##             units: degrees_north
    ##             long_name: latitude
    ##         time  Size:2208 
    ##             units: hours since 1900-01-01 00:00:00.0
    ##             long_name: time
    ##             calendar: gregorian
    ## 
    ##     2 global attributes:
    ##         Conventions: CF-1.6
    ##         history: 2023-07-24 20:09:56 GMT by grib_to_netcdf-2.25.1: /opt/ecmwf/mars-client/bin/grib_to_netcdf.bin -S param -o /cache/data1/adaptor.mars.internal-1690229391.5737147-17528-3-cb79dd1f-4d40-4f85-b17b-6992eadff943.nc /cache/tmp/cb79dd1f-4d40-4f85-b17b-6992eadff943-adaptor.mars.internal-1690229343.5283673-17528-4-tmp.grib

Extract data in vectors and 3d-matrix

``` r
lon <- ncvar_get(t2m.rea, "longitude")
lat <- ncvar_get(t2m.rea, "latitude")
time <- ncvar_get(t2m.rea, "time")
t2m <- ncvar_get(t2m.rea, "t2m") - 273.15
na.value <- ncatt_get(t2m.rea, "t2m", "_FillValue")$value
t2m[t2m==na.value] <- NA
cat(paste('time units:', ncatt_get(t2m.rea, "time", "units")$value))
```

    ## time units: hours since 1900-01-01 00:00:00.0

``` r
time <- as.POSIXct(time*3600, origin="1900-01-01", tz="UTC")
```

Map T2M for one hour

``` r
# one time
ts <- as.POSIXct("2022-08-02 18:00:00 UTC", tz='UTC')
samp <- t2m[,,which(time==ts)]
# need to add 0.25/2 to 
samp.rast <- rast(t(samp), extent=c(-85.125,-74.875,33.375,37.125), crs='+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')

my.map.raster(raster=samp.rast, date=paste(ts, 'UTC'), title="T2m North Carolina")
```

![](era5-reanalysis-processing_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

Code to create a dataframe from the netcdf

``` r
# -- /!\ latitude vector is decreasing
lon.lat.time <- as.matrix(expand.grid(lon, rev(lat), time))
t2m.vect <- as.vector(t2m[,ncol(t2m):1,])
t2m.df <- data.frame(cbind(lon.lat.time, t2m.vect))
colnames(t2m.df) <- c("lon", "lat", "time", 't2m')
t2m.df$t2m <- as.numeric(t2m)
t2m.df$time <- as.POSIXct(t2m.df$time, tz="UTC")
pixels <- unique(t2m.df[,c('lon', 'lat')])
pixels$geom <- index(pixels)
t2m.df <- merge(t2m.df, pixels, by=c('lon','lat'))
```

Compute of each day

``` r
ts <- as.POSIXct("2022-06-01 18:00:00 UTC", tz='UTC')
te <- as.POSIXct("2022-08-31 18:00:00 UTC", tz='UTC')
t2m.ts <- t2m.df[which(between(t2m.df$time, ts, te)), ]

dateTN <- function(x){
  return(ifelse(hour(x)>=18, substr(x+days(1),0,10), substr(x,0,10)))
}

t2m.ts$dateTN <- lapply(t2m.ts$time, dateTN)
t2m.ts$dateTN <- as.character(t2m.ts$dateTN)
summary(t2m.ts[,c('t2m','dateTN', 'geom')])
```

    ##       t2m            dateTN               geom    
    ##  Min.   : 8.461   Length:1343775     Min.   :  1  
    ##  1st Qu.:23.050   Class :character   1st Qu.:154  
    ##  Median :25.458   Mode  :character   Median :308  
    ##  Mean   :25.436                      Mean   :308  
    ##  3rd Qu.:27.748                      3rd Qu.:462  
    ##  Max.   :39.928                      Max.   :615

``` r
# transform data.frame to data.table to faster computations
t2m.ts <- setDT(t2m.ts) 
t2m.TN <- t2m.ts[, min(t2m), keyby=c('geom', 'dateTN', 'lon', 'lat')]
colnames(t2m.TN) <- c("geom", "dateTN", "lon", "lat", "t2m")
summary(t2m.TN$t2m)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   8.461  20.229  22.346  21.992  24.066  32.573

``` r
t2m.TN$lon <- as.numeric(t2m.TN$lon)
t2m.TN$lat <- as.numeric(t2m.TN$lat)

beepr::beep(1)
```

Compute TX of each day

``` r
ts <- as.POSIXct("2022-06-01 06:00:00 UTC", tz='UTC')
te <- as.POSIXct("2022-08-31 06:00:00 UTC", tz='UTC')
t2m.ts <- t2m.df[which(between(t2m.df$time, ts, te)), ]

dateTX <- function(x){
  return(ifelse(hour(x)<6, substr(x-days(1),0,10), substr(x,0,10)))
}

t2m.ts$dateTX <- lapply(t2m.ts$time, dateTX)
t2m.ts$dateTX <- as.character(t2m.ts$dateTX)
summary(t2m.ts[,c('t2m','dateTX', 'geom')])
```

    ##       t2m            dateTX               geom    
    ##  Min.   : 8.461   Length:1343775     Min.   :  1  
    ##  1st Qu.:23.044   Class :character   1st Qu.:154  
    ##  Median :25.452   Mode  :character   Median :308  
    ##  Mean   :25.436                      Mean   :308  
    ##  3rd Qu.:27.749                      3rd Qu.:462  
    ##  Max.   :39.928                      Max.   :615

``` r
# transform data.frame to data.table to faster computations
t2m.ts <- setDT(t2m.ts) 
t2m.TX <- t2m.ts[, max(t2m), keyby=c('geom', 'dateTX', 'lon', 'lat')]
colnames(t2m.TX) <- c("geom", "dateTX", "lon", "lat", "t2m")
summary(t2m.TX$t2m)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   16.40   27.32   29.46   29.61   32.05   39.93

``` r
t2m.TX$lon <- as.numeric(t2m.TX$lon)
t2m.TX$lat <- as.numeric(t2m.TX$lat)

beepr::beep(1)
```

### Check coherence with NOAA observations

Open NOAA obs

``` r
aws <- data.table::fread("../input/NC-AWS-NOAA-dailysummary-20220601-20220831.csv") 
length(unique(aws$STATION))
```

    ## [1] 1131

``` r
aws <- aws[which(!(is.na(aws$TMAX)|is.na(aws$TMIN))),] 
aws$DATE <- as.Date(aws$DATE, format = "%Y-%m-%d")
length(unique(aws$STATION))
```

    ## [1] 182

Daily max temperatures

``` r
aws.ts.tx <- maditr::dcast(aws[, c('DATE', 'STATION', 'TMAX')], DATE ~ STATION) %>%
  as.xts() 
```

Daily min temperatures

``` r
aws.ts.tn <- maditr::dcast(aws[, c('DATE', 'STATION', 'TMIN')], DATE ~ STATION) %>%
  as.xts() 
```

Daily mean temperatures

``` r
aws.ts.tm <- maditr::dcast(aws[, c('DATE', 'STATION', 'TAVG')], DATE ~ STATION) %>%
  as.xts() 
```

Timeseries plots

``` r
plot((aws.ts.tn-32)*5/9)
```

![](era5-reanalysis-processing_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

``` r
plot((aws.ts.tx-32)*5/9)
```

![](era5-reanalysis-processing_files/figure-gfm/unnamed-chunk-14-2.png)<!-- -->

``` r
plot((aws.ts.tm-32)*5/9)
```

![](era5-reanalysis-processing_files/figure-gfm/unnamed-chunk-14-3.png)<!-- -->

``` r
boxplot(t((aws.ts.tn-32)*5/9))
```

![](era5-reanalysis-processing_files/figure-gfm/unnamed-chunk-14-4.png)<!-- -->

``` r
boxplot(t((aws.ts.tx-32)*5/9))
```

![](era5-reanalysis-processing_files/figure-gfm/unnamed-chunk-14-5.png)<!-- -->

``` r
boxplot(t((aws.ts.tm-32)*5/9))
```

![](era5-reanalysis-processing_files/figure-gfm/unnamed-chunk-14-6.png)<!-- -->

NOAA aws with less than 5% missing data

``` r
nb.na.tn <- lapply(aws.ts.tn, FUN=function(x) sum(is.na(x)))  
nb.na.tn <- as.data.frame(do.call(rbind, nb.na.tn))
nb.na.tn <- cbind(STATION = rownames(nb.na.tn), nb.na.tn)
rownames(nb.na.tn) <- 1:nrow(nb.na.tn)
names(nb.na.tn)[names(nb.na.tn) == 'V1'] <- 'nb.na.tn'

nb.na.tx <- lapply(aws.ts.tx, FUN=function(x) sum(is.na(x)))  
nb.na.tx <- as.data.frame(do.call(rbind, nb.na.tx))
nb.na.tx <- cbind(STATION = rownames(nb.na.tx), nb.na.tx)
rownames(nb.na.tx) <- 1:nrow(nb.na.tx)
names(nb.na.tx)[names(nb.na.tx) == 'V1'] <- 'nb.na.tx'

nb.na.tm <- lapply(aws.ts.tm, FUN=function(x) sum(is.na(x)))  
nb.na.tm <- as.data.frame(do.call(rbind, nb.na.tm))
nb.na.tm <- cbind(STATION = rownames(nb.na.tm), nb.na.tm)
rownames(nb.na.tm) <- 1:nrow(nb.na.tm)
names(nb.na.tm)[names(nb.na.tm) == 'V1'] <- 'nb.na.tm'

stations <- unique(aws[,c('STATION', 'NAME', 'LATITUDE', 'LONGITUDE', 'ELEVATION')])
stations <- list(stations, nb.na.tx, nb.na.tn, nb.na.tm) %>% reduce(full_join, by='STATION')
aws <- list(aws, nb.na.tx, nb.na.tn, nb.na.tm) %>% reduce(full_join, by='STATION')

# -- turn into a sf object to reproject CRS
stations <- st_as_sf(stations, coords=c('LONGITUDE', 'LATITUDE'))
st_crs(stations) <- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'
stations <- st_transform(stations, crs(nc.borders))
stations <- data.frame(stations)
```

Map minimum temperatures on 2022-07-07 (one of the hottest day)

``` r
# -- turn into a sf object to reproject CRS
aws <- st_as_sf(aws, coords=c('LONGITUDE', 'LATITUDE'))
st_crs(aws) <- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'
aws <- st_transform(aws, crs(nc.borders))
aws <- data.frame(aws)
```

Function to extract a date in era5 TX and TN data.table

``` r
era5.date <- function(date, era5){
  era5.date <- era5[which(era5$date==date), ]
  era5.date <- vect(era5.date, geom=c('lon', 'lat'), crs="+proj=longlat +datum=WGS84")
  empty.rast <- rast(nrows=15, ncol=41, extent=c(-85.125,-74.875,33.375,37.125), crs='+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')
  era5.date <- rasterize(x=era5.date, y=empty.rast, field='t2m')
  era5.date <- flip(era5.date, direction="vertical")
  era5.date <- project(era5.date, crs(nc.borders))
  return(era5.date)
}
```

Map several date examples

``` r
date <- as.Date('2022-06-06')
my.map.raster.points(era5.date(date, t2m.TN), 
                     points=aws[which(aws$DATE==date),], 
                     points.fill = (TMIN-32)*5/9, 
                     date=date, title="TN North Carolina")
```

![](era5-reanalysis-processing_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

``` r
my.map.raster.points(era5.date(date, t2m.TX), 
                     points=aws[which(aws$DATE==date),], 
                     points.fill = (TMAX-32)*5/9, 
                     date=date, title="TX North Carolina")
```

![](era5-reanalysis-processing_files/figure-gfm/unnamed-chunk-18-2.png)<!-- -->

``` r
date <- as.Date('2022-06-14')
my.map.raster.points(era5.date(date, t2m.TN), 
                     points=aws[which(aws$DATE==date),], 
                     points.fill = (TMIN-32)*5/9, 
                     date=date, title="TN North Carolina (very high TN)")
```

![](era5-reanalysis-processing_files/figure-gfm/unnamed-chunk-18-3.png)<!-- -->

``` r
my.map.raster.points(era5.date(date, t2m.TX), 
                     points=aws[which(aws$DATE==date),], 
                     points.fill = (TMAX-32)*5/9, 
                     date=date, title="TX North Carolina")
```

![](era5-reanalysis-processing_files/figure-gfm/unnamed-chunk-18-4.png)<!-- -->

``` r
date <- as.Date('2022-06-20')
my.map.raster.points(era5.date(date, t2m.TN), 
                     points=aws[which(aws$DATE==date),], 
                     points.fill = (TMIN-32)*5/9, 
                     date=date, title="TN North Carolina")
```

![](era5-reanalysis-processing_files/figure-gfm/unnamed-chunk-18-5.png)<!-- -->

``` r
my.map.raster.points(era5.date(date, t2m.TX), 
                     points=aws[which(aws$DATE==date),], 
                     points.fill = (TMAX-32)*5/9, 
                     date=date, title="TX North Carolina")
```

![](era5-reanalysis-processing_files/figure-gfm/unnamed-chunk-18-6.png)<!-- -->

``` r
date <- as.Date('2022-07-26')
my.map.raster.points(era5.date(date, t2m.TN), 
                     points=aws[which(aws$DATE==date),], 
                     points.fill = (TMIN-32)*5/9, 
                     date=date, title="TN North Carolina (very high TN) ")
```

![](era5-reanalysis-processing_files/figure-gfm/unnamed-chunk-18-7.png)<!-- -->

``` r
my.map.raster.points(era5.date(date, t2m.TX), 
                     points=aws[which(aws$DATE==date),], 
                     points.fill = (TMAX-32)*5/9, 
                     date=date, title="TX North Carolina")
```

![](era5-reanalysis-processing_files/figure-gfm/unnamed-chunk-18-8.png)<!-- -->

``` r
date <- as.Date('2022-07-30')
my.map.raster.points(era5.date(date, t2m.TN), 
                     points=aws[which(aws$DATE==date),], 
                     points.fill = (TMIN-32)*5/9, 
                     date=date, title="TN North Carolina (very high TN)")
```

![](era5-reanalysis-processing_files/figure-gfm/unnamed-chunk-18-9.png)<!-- -->

``` r
my.map.raster.points(era5.date(date, t2m.TX), 
                     points=aws[which(aws$DATE==date),], 
                     points.fill = (TMAX-32)*5/9, 
                     date=date, title="TX North Carolina")
```

![](era5-reanalysis-processing_files/figure-gfm/unnamed-chunk-18-10.png)<!-- -->

``` r
date <- as.Date('2022-08-15')
my.map.raster.points(era5.date(date, t2m.TN), 
                     points=aws[which(aws$DATE==date),], 
                     points.fill = (TMIN-32)*5/9, 
                     date=date, title="TN North Carolina (low TN)")
```

![](era5-reanalysis-processing_files/figure-gfm/unnamed-chunk-18-11.png)<!-- -->

``` r
my.map.raster.points(era5.date(date, t2m.TX), 
                     points=aws[which(aws$DATE==date),], 
                     points.fill = (TMAX-32)*5/9, 
                     date=date, title="TX North Carolina")
```

![](era5-reanalysis-processing_files/figure-gfm/unnamed-chunk-18-12.png)<!-- -->

Session info

``` r
sessionInfo()
```

    ## R version 4.3.1 (2023-06-16)
    ## Platform: aarch64-apple-darwin20 (64-bit)
    ## Running under: macOS Ventura 13.4.1
    ## 
    ## Matrix products: default
    ## BLAS:   /Library/Frameworks/R.framework/Versions/4.3-arm64/Resources/lib/libRblas.0.dylib 
    ## LAPACK: /Library/Frameworks/R.framework/Versions/4.3-arm64/Resources/lib/libRlapack.dylib;  LAPACK version 3.11.0
    ## 
    ## locale:
    ## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
    ## 
    ## time zone: America/New_York
    ## tzcode source: internal
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ##  [1] viridis_0.6.3     viridisLite_0.4.2 forcats_1.0.0     stringr_1.5.0    
    ##  [5] dplyr_1.1.2       purrr_1.0.1       readr_2.1.4       tidyr_1.3.0      
    ##  [9] tibble_3.2.1      tidyverse_2.0.0   maditr_0.8.3      tidyterra_0.4.0  
    ## [13] ggspatial_1.1.8   ggplot2_3.4.2     xts_0.13.1        zoo_1.8-12       
    ## [17] lubridate_1.9.2   ncdf4_1.21        sf_1.0-14         terra_1.7-39     
    ## [21] sp_2.0-0         
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] utf8_1.2.3         generics_0.1.3     class_7.3-22       KernSmooth_2.23-21
    ##  [5] stringi_1.7.12     lattice_0.21-8     hms_1.1.3          digest_0.6.33     
    ##  [9] magrittr_2.0.3     evaluate_0.21      grid_4.3.1         timechange_0.2.0  
    ## [13] beepr_1.3          fastmap_1.1.1      e1071_1.7-13       DBI_1.1.3         
    ## [17] audio_0.1-10       gridExtra_2.3      fansi_1.0.4        scales_1.2.1      
    ## [21] codetools_0.2-19   cli_3.6.1          rlang_1.1.1        units_0.8-2       
    ## [25] munsell_0.5.0      withr_2.5.0        yaml_2.3.7         tools_4.3.1       
    ## [29] tzdb_0.4.0         colorspace_2.1-0   vctrs_0.6.3        R6_2.5.1          
    ## [33] proxy_0.4-27       lifecycle_1.0.3    classInt_0.4-9     pkgconfig_2.0.3   
    ## [37] pillar_1.9.0       gtable_0.3.3       data.table_1.14.8  glue_1.6.2        
    ## [41] Rcpp_1.0.11        highr_0.10         xfun_0.39          tidyselect_1.2.0  
    ## [45] rstudioapi_0.15.0  knitr_1.43         farver_2.1.1       htmltools_0.5.5   
    ## [49] labeling_0.4.2     rmarkdown_2.23     compiler_4.3.1
