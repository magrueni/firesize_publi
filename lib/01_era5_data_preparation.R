###################################################################
### M. Gruenig & C. Senf 29.11.2022 ###############################
###################################################################

### ------------------------------------------------------------------------
### In this first script the ERA5-Land data is processed to extract summer VPD for each year
### The raw climate data can be downloaded manually from https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-land-monthly-means?tab=overview
### ------------------------------------------------------------------------


### libraries ----------------------------------------------------------------
if (!require("raster")) install.packages("raster")
if (!require("terra")) install.packages("terra")
if (!require("sf")) install.packages("sf")
if (!require("rgdal")) install.packages("rgdal")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("lubridate")) install.packages("lubridate")
if (!require("patchwork")) install.packages("patchwork")
if (!require("ncdf4")) install.packages("ncdf4")
if (!require("reticulate")) install.packages("reticulate")



### Studyregion -------------------------------------------------------------

studyregion <- read_sf("./data/climate/climategrid_epsg3035.gpkg")

studyregion_latlng <- st_transform(studyregion, "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

cntrs <- read_sf("./data/countries/europe.shp")

# Load ERA5 soil moisture data --------------------------------------------
# Data was downloaded as netCDF from https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-land-monthly-means?tab=overview
# Please download dewpoint temperature and 2m temperature as monthly data for all years between 1986 - 2020 from monthly averaged reanalysis
# This can also be downloaded with the following cds api

#install the CDS API
conda_install("r-reticulate","cdsapi", pip=TRUE)

# if not working - go to conda and insert pip install cdsapi
# then create file ".cdsapirc" in "home" with the following information:
# url: 
# key: 

#import python CDS-API 
cdsapi <- import('cdsapi')

# define what we need
vars <- c("2m_dewpoint_temperature", "2m_temperature")
years <- c(1980:2020)
months <- c("01","02","03","04","05","06","07","08","09","10","11","12")

# create the query
query <- r_to_py(list(
  variable= vars,
  product_type= "reanalysis",
  year= years,
  month= months,
  time= 00:00,
  format= "netcdf",
  area = "80/-15/20/50" # North, West, South, East North 80°, West -15°, South 20°, East 50°
))

#query to get the ncdf
server$retrieve('reanalysis-era5-land-monthly-means',
                query,
                "era5_preds.nc")


### data processing ---------------------------------------------------------------

# look at the data
nc <- nc_open("./data/climate/era_preds.nc")
vars <- c("d2m", "t2m") # d2m = dewpoint temp; t2m = mean temp
years <- c(1986:2020)

# for both variables we extract thte data and save as raster layers
for(k in 1:length(vars)){
  
  print(vars[k])
  dat_ras <- brick("./data/climate/era_preds.nc", var = vars[k])
  dat_ras <- stack(dat_ras)
  
  dat_ras <- raster::crop(dat_ras, studyregion_latlng)
  dat_ras <- raster::mask(dat_ras, studyregion_latlng)
  
  for(y in years){
    
    print(y)
    stack_year <- brick(raster::subset(dat_ras, grep(y, grep(as.character(y), names(dat_ras), value = T), value = T)))
    
    mean_layer <- calc(stack_year, mean)
    if(vars[k] %in% c("d2m", "t2m")){
      mean_layer <- mean_layer - 273.15
      assign(paste0("months_", y, vars[k]), stack_year)
    }
    
  }
  
}


# vpd calculation formula from August-Roche-Magnus formula (Alduchov & Eskridge, 1996)
vpd_calc <- function(t, td, c1 = 0.611, c2 = 17.67, c3 = 243.5) {
  c1 * exp((c2 * (t - 273.15)) / (t - 273.15 + c3)) - c1 * exp((c2 * (td - 273.15)) / (td - 273.15 + c3))
}


# calculate summer VPD for each year 
for(y in years){
  
  print(y)
  
  t <- get(paste0("months_", y, "t2m"))
  td <- get(paste0("months_", y, "d2m"))
  
  stack_vpd <- stack()
  
  # june, july, august 
  for(i in 6:8){
    
    tx <- t[[i]]
    tdx <- td[[i]]
    
    vpdx <- vpd_calc(tx, tdx)
    
    stack_vpd <- stack(stack_vpd, vpdx)
    
  }
  
  mean_vpd <- calc(stack_vpd, mean)
  
  raster::writeRaster(mean_vpd, paste0("./data/climate/vpd/summer_vpd_", y, ".tif"), overwrite = T)
  
}
