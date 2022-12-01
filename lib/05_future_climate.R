###################################################################
### M. Gruenig & C. Senf 29.11.2022 ###############################
###################################################################

### ---------------------------------------------------------------
### In this script we download and bias correct CMIP6 data from CDS ----------------
### ---------------------------------------------------------------


### download ------------------------------------------------------
if (!require("ecmwfr")) install.packages("ecmwfr")
if (!require("keyring")) install.packages("keyring")

# API
API <- ""
UID <- ""

# set user key
wf_set_key(user = "",  service = 'cds')

# scenarios
scen <- c('ssp5_8_5', 'ssp2_4_5', "historical")
# define gcms we want to download
gcms <- c('ec_earth3_veg_lr', 'mpi_esm1_2_lr', 'cnrm_cm6_1_hr', 'cnrm_esm2_1', "hadgem3_gc31_ll") #
# rcms that are available for the three gcms

years.past <- c('1986', '1987', '1988',
                '1989', '1990', '1991',
                '1992', '1993', '1994',
                '1995', '1996', '1997',
                '1998', '1999', '2000',
                '2001', '2002', '2003',
                '2004', '2005', '2006',
                '2007', '2008', '2009',
                '2010', '2011', '2012',
                '2013', '2014')

years.fut <- c('2015', '2016', '2017',
               '2018', '2019', '2020',
               '2021', '2022', '2023',
               '2024', '2025', '2026',
               '2027', '2028', '2029',
               '2030', '2031', '2032',
               '2033', '2034', '2035',
               '2036', '2037', '2038',
               '2039', '2040', '2041',
               '2042', '2043', '2044',
               '2045', '2046', '2047',
               '2048', '2049', '2050',
               '2051', '2052', '2053',
               '2054', '2055', '2056',
               '2057', '2058', '2059',
               '2060', '2061', '2062',
               '2063', '2064', '2065',
               '2066', '2067', '2068',
               '2069', '2070', '2071',
               '2072', '2073', '2074',
               '2075', '2076', '2077',
               '2078', '2079', '2080',
               '2081', '2082', '2083',
               '2084', '2085', '2086',
               '2087', '2088', '2089',
               '2090', '2091', '2092',
               '2093', '2094', '2095',
               '2096', '2097', '2098',
               '2099')

all.months <- c('01', '02', '03',
                '04', '05', '06',
                '07', '08', '09',
                '10', '11', '12')

variables <- c('relative_humidity', 'daily_maximum_near_surface_air_temperature', 'daily_minimum_near_surface_air_temperature')


for(v in variables){
  
  var <- ifelse(v == "daily_maximum_near_surface_air_temperature", "tas_max",
                ifelse(v == "daily_maximum_near_surface_air_temperature", "tas_min", "hurs"))
  level <- ifelse(v == "relative_humidity", "1000", "")
  
  for(s in scen){
    
    for(gcm in gcms){
      
      cat(gcm, "\n")
      
      if(s == "historical"){
        years <- years.past
      }else{
        years <- years.fut
      }
      
      file_nme <- paste(s, "_", gcm, "_", v, ".zip", sep="")
      if(file.exists(paste("", file_nme, sep=""))){
        next
        cat("already downloaded")}
      # define request
      request <- list(
        dataset_short_name = 'projections-cmip6',
        temporal_resolution = 'monthly',
        experiment = paste(s),
        variable = paste(v),
        level = level,
        model = paste(gcm),
        
        year = years,
        month = all.months,
        #area = c(80, -20, 30, 60)
        
        format = "zip",
        target = file_nme
      )
      
      
      # Start downloading the data, the path of the file
      # will be returned as a variable (ncfile)
      ncfile <- wf_request(user = UID,
                           request = request,   
                           transfer = TRUE,  
                           path = paste("", sep=""),
                           verbose = TRUE)
    }
  }
  
}



### ---------------------------------------------------------------
### Bias correction  ----------------
### ---------------------------------------------------------------


### Packages ----------------------------------------------------------------

if (!require("tidyverse")) install.packages("tidyverse")
if (!require("raster")) install.packages("raster")
if (!require("lubridate")) install.packages("lubridate")
if (!require("sf")) install.packages("sf")
if (!require("patchwork")) install.packages("patchwork")
if (!require("terra")) install.packages("terra")
if (!require("ncdf4")) install.packages("ncdf4")
if (!require("weathermetrics")) install.packages("weathermetrics")
if (!require("raster")) install.packages("raster")


# Studyregion -------------------------------------------------------------

studyregion <- read_sf("./data/climate/climategrid_epsg3035_complexes.gpkg")

studyregion_latlng <- st_transform(studyregion, "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

cntrs <- read_sf("./data/countries/europe.shp")

proj_wgs84 <- "+proj=longlat +datum=WGS84 +no_defs"
proj_nc <- "+proj=ob_tran +o_proj=longlat +o_lon_p=-162 +o_lat_p=39.25 +lon_0=180"
proj_leae <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs" 


### historical data ----------
ncs <- list.files("./data/climate/cmip6", pattern = ".nc")
historical_fils <- ncs[grepl("historical", ncs)]


gcms <- c("CNRM-CM6-1-HR", "FIO-ESM-2-0", "CMCC-ESM2", "MPI-ESM1-2-LR", "EC-Earth3-Veg-LR") 
rcp <- "historical"

for(g in gcms){
  
  tasmax_file <- historical_fils[grepl(g, historical_fils) & grepl("tasmax_", historical_fils)]
  tasmax_rast <- rast(paste0("./data/climate/cmip6/", tasmax_file))
  
  tasmin_file <- historical_fils[grepl(g, historical_fils) & grepl("tasmin_", historical_fils)]
  tasmin_rast <- rast(paste0("./data/climate/cmip6/", tasmin_file))
  
  hur_file <- historical_fils[grepl(g, historical_fils) & grepl("hur_", historical_fils)]
  hur_rast <- rast(paste0("./data/climate/cmip6/", hur_file))
  
  
  tas_stack <- stack()
  hurs_stack <- stack()
  
  
  for(y in 0:28){
    
    
    year_stack_tas <- stack()
    year_stack_hurs <- stack()
    
    for(i in 6:8){
      
      index <- i + y*12
      print(index)
      
      tasmax_r <- raster(tasmax_rast[[index]])
      tasmin_r <- raster(tasmin_rast[[index]])
      
      tas_r <- (tasmax_r + tasmin_r)/2
      year_stack_tas <- stack(year_stack_tas, tas_r)
      
      hurs_r <- raster(hur_rast[[index]])
      year_stack_hurs <- stack(year_stack_hurs, hurs_r)
    }
    
    year_tas <- calc(year_stack_tas, mean)
    tas_stack <- stack(tas_stack, year_tas)
    
    year_hurs <- calc(year_stack_hurs, mean)
    hurs_stack <- stack(hurs_stack, year_hurs)
  }
  
  
  ### calc VPD -----------------------------------------------------------
  vpd_calc <- function(t, td, c1 = 0.611, c2 = 17.67, c3 = 243.5) {
    c1 * exp((c2 * (t)) / (t + c3)) - c1 * exp((c2 * (td)) / (td + c3))
  }
  
  
  df <- as.data.frame(cbind(expand.grid(c(3000000, 4000000, 5000000, 6000000), c(2000000, 3000000,4000000,5000000))))
  df_extract <- NULL
  # second method with meteometrics pacakge
  for(y in 1:29){
    
    year <- 1985 + y
    
    tas_year <- tas_stack[[y]] - 273.15
    hurs_year <- hurs_stack[[y]]
    
    td <- humidity.to.dewpoint(rh = hurs_year, t = tas_year, temperature.metric = "celsius")
    vpd_method2 <- vpd_calc(tas_year, td)
    crs(vpd_method2) <- proj_wgs84
    vpd <- terra::project(rast(vpd_method2), proj_leae)
    
    #df_extract <- cbind(df_extract, extract(vpd, df)[,2])
    
    plot(vpd, main = paste0(g, " ", year))
    terra::writeRaster(vpd, paste0("./data/climate/cmip6/vpd_summer_",year,"_", g, "_historical_cmip6.tif"), overwrite = T)
    
    
    
  }
}



### calculate bias with ERA5 data
era5_vpd <- stack()
for(k in 1986:2014){
  
  era5_vpd <- stack(era5_vpd, raster(paste0("./data/climate/vpd/summer_vpd_", k, ".tif")))
  
  
}

era5_mean <- rast(calc(era5_vpd, mean))
plot(era5_mean)

# calc mean for gmcs
gcms <- c("CNRM-CM6-1-HR", "FIO-ESM-2-0", "CMCC-ESM2", "MPI-ESM1-2-LR", "EC-Earth3-Veg-LR") 
gcms_short <- c("CNRM_CM6", "FIO_ESM","CMCC_ESM", "MPI_ESM", "CMCC_ESM")
for(g in 1:length(gcms)){
  
  gcm <- gcms[g]
  gcm_short <- gcms_short[g]
  print(gcm_short)
  
  gcm_vpd <- stack()
  for(k in 1986:2014){
    
    gcm_vpd <- stack(gcm_vpd, raster(paste0("./data/climate/cmip6/vpd_summer_", k,"_", gcm, "_historical_cmip6.tif")))
    
  }
  
  gcm_mean <- rast(calc(gcm_vpd, mean))
  gcm_mean_proj <- terra::project(gcm_mean, era5_mean)
  gcm_mean_proj <- terra::crop(gcm_mean_proj, era5_mean)
  gcm_mean_proj <- terra::mask(gcm_mean_proj, era5_mean)
  plot(gcm_mean_proj, main = gcm_short)
  
  bias <- era5_mean - gcm_mean_proj
  assign(paste0(gcm_short, "_bias"), bias)
  
  terra::writeRaster(bias, paste0("./data/climate/cmip6/vpd_summer_", gcm, "_bias.tif"), overwrite = T)
  
  tmpFiles(remove = T)
  removeTmpFiles(h=0.5)
}

### calc vpd of future ---------------
### calc VPD -----------------------------------------------------------
vpd_calc <- function(t, td, c1 = 0.611, c2 = 17.67, c3 = 243.5) {
  c1 * exp((c2 * (t)) / (t + c3)) - c1 * exp((c2 * (td)) / (td + c3))
}

df <- as.data.frame(cbind(expand.grid(c(3000000, 4000000, 5000000, 6000000), c(2000000, 3000000,4000000,5000000))))
df_extract <- NULL

ncs <- list.files("./data/climate/cmip6", pattern = ".nc")
scenarios <- c("ssp245")#"ssp585", 
for(s in scenarios){
  
  scenario_files <- ncs[grepl(s, ncs)]
  
  gcms <- c("CNRM-CM6-1-HR", "FIO-ESM-2-0", "CMCC-ESM2", "MPI-ESM1-2-LR", "EC-Earth3-Veg-LR") 
  gcms_short <- c("CNRM_CM6", "FIO_ESM","CMCC_ESM", "MPI_ESM", "CMCC_ESM")
  
  for(g in 1:length(gcms)){
    
    gcm <- gcms[g]
    gcm_short <- gcms_short[g]
    
    
    tasmax_file <- scenario_files[grepl(gcm, scenario_files) & grepl("tasmax_", scenario_files)]
    tasmax_rast <- rast(paste0("./data/climate/cmip6/", tasmax_file))
    
    tasmin_file <- scenario_files[grepl(gcm, scenario_files) & grepl("tasmin_", scenario_files)]
    tasmin_rast <- rast(paste0("./data/climate/cmip6/", tasmin_file))
    
    hur_file <- scenario_files[grepl(gcm, scenario_files) & grepl("hur_", scenario_files)]
    hur_rast <- rast(paste0("./data/climate/cmip6/", hur_file))
    
    
    tas_stack <- stack()
    hurs_stack <- stack()
    
    bias <- rast(paste0("./data/climate/cmip6/vpd_summer_", gcm, "_bias.tif"))
    
    
    for(y in 0:84){
      
      year <- 2015 + y
      if(file.exists(paste0("./data/climate/cmip6/vpd_summer_",year,"_", gcm, "_", s, "_cmip6_biascor.tif"))){next}
      
      year_stack_tas <- stack()
      year_stack_hurs <- stack()
      
      for(i in 6:8){
        
        index <- i + y*12
        print(index)
        
        tasmax_r <- raster(tasmax_rast[[index]])
        tasmin_r <- raster(tasmin_rast[[index]])
        
        tas_r <- (tasmax_r + tasmin_r)/2
        year_stack_tas <- stack(year_stack_tas, tas_r)
        
        hurs_r <- raster(hur_rast[[index]])
        year_stack_hurs <- stack(year_stack_hurs, hurs_r)
      }
      
      tas_year <- calc(year_stack_tas, mean)
      tas_year <- tas_year - 273.15
      
      hurs_year <- calc(year_stack_hurs, mean)
      hurs_year[hurs_year > 100] <- 100
      
      td <- humidity.to.dewpoint(rh = hurs_year, t = tas_year, temperature.metric = "celsius")
      vpd_method2 <- rast(vpd_calc(tas_year, td))
      crs(vpd_method2) <- proj_wgs84
      
      vpd_method2 <- terra::project(vpd_method2, bias)
      vpd_method2 <- terra::crop(vpd_method2, bias)
      vpd_method2 <- terra::mask(vpd_method2, bias)
      
      # do bias correction
      vpd_biascor <- vpd_method2 + bias
      vpd_biascor <- terra::project(vpd_biascor, proj_leae)
      
      #plot(vpd)
      terra::writeRaster(vpd_biascor, paste0("./data/climate/cmip6/vpd_summer_",year,"_", gcm, "_", s, "_cmip6_biascor.tif"), overwrite = T)
      
      
    } # close years
    
    tmpFiles(remove = T)
    removeTmpFiles(h = 1)
    
  } # close gcms
  
} # close scenario




### historical -----
gcms <- c("CNRM-CM6-1-HR", "FIO-ESM-2-0", "CMCC-ESM2", "MPI-ESM1-2-LR", "EC-Earth3-Veg-LR") 
for(g in gcms){
  
  bias <- rast(paste0("./data/climate/cmip6/vpd_summer_", gcm, "_bias.tif"))
  
  for(year in 1986:2014){
    
    print(year)
    
    vpd <- rast(paste0("./data/climate/cmip6/vpd_summer_", year,"_", g, "_historical_cmip6.tif"))
    vpd <- terra::project(vpd, bias)
    vpd <- terra::crop(vpd, bias)
    vpd <- terra::mask(vpd, bias)
    
    vpd_biascor <- vpd + bias   
    vpd_biascor <- terra::project(vpd_biascor, proj_leae)
    
    terra::writeRaster(vpd_biascor, paste0("./data/climate/cmip6/vpd_summer_", year,"_", g, "_historical_cmip6_biascor.tif"), overwrite = T)
    
    
    
  }
}



