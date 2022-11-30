###################################################################
### M. Gruenig & C. Senf 29.11.2022 ###############################
###################################################################

### ------------------------------------------------------------------------
### Script to extract future VPD data for fire complexes -----
### ------------------------------------------------------------------------


#librs
library(terra)
library(sf)
library(raster)
library(rgdal)
library(tidyverse)
library(sf)
library(exactextractr)
library(layer)
library(stars)
library(maptools)

setwd("")

#General temp directory 
write("TMP = './firesize/tmp/'", file = file.path('~.Renviron')) 

# Raster package
rasterOptions(tmpdir = "./firesize/tmp/")
terraOptions(memfrac=0.5, tempdir = "./firesize/tmp/")
tmpFiles(remove=TRUE, current=T, orphan=T)
removeTmpFiles()

# leae CRS
proj_leae <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs" 

# countries shp
countries <- list.files("data/countries", pattern="*.shp", full.names = T)
countries <- countries[-c(12,13)] # remove europe shp

#download.file("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/cultural/ne_10m_admin_0_countries.zip", "countries.zip")
# Then unzip
#unzip("countries.zip")

# Read in the shapefile
world <- readShapeSpatial("ne_10m_admin_0_countries.shp")
# biomes shp
ecoregions <- read_sf("data/ecoregions/terrestrial_ecoregions_olson.shp") %>%
  st_transform(., st_crs(proj_leae))


### loop over all countries to extract the data --------------------------------------------
r <- 0

#gcms <- c("CNRM-CM6-1", "EC-Earth3-Veg-LR")
gcms <- c("FIO-ESM-2-0", "CMCC-ESM2", "MPI-ESM1-2-LR", "EC-Earth3-Veg-LR", "CNRM-CM6-1")

for(y in 1986:2020){
  
  print(y)
  
  r <- r + 1
  # get year before and after
  if(y == 1986){year_prev <- y}else{year_prev <- y - 1}
  if(y == 2020){year_aftr <- y}else{year_aftr <- y + 1}
  
  if(y < 2015){scen <- "historical"}else{scen <- "fut"}
  
  # load the data
  if(scen == "fut"){
    
    gcm_stack <- rast()
    
    for(g in gcms){
      gcm_stack <-  c(gcm_stack, rast(paste0("./summer_vpd/vpd_summer_",y, "_", g, "_ssp585_cmip6_biascor.tif")))
    }

    for(g in gcms){
      gcm_stack <-  c(gcm_stack, rast(paste0("./summer_vpd/vpd_summer_",y, "_", g, "_ssp245_cmip6_biascor.tif")))
    }

    summer_vpd <- terra::app(gcm_stack, mean)
    
  }else{
    
    gcm_stack <- rast()
    
    for(g in gcms){
      gcm_stack <-  c(gcm_stack, rast(paste0("./summer_vpd/vpd_summer_",y, "_", g, "_historical_cmip6_biascor.tif")))
    }
    
    summer_vpd <- terra::app(gcm_stack, mean)
    
  }
  
  # load data of previous year
  if(y < 2016){scen <- "historical"}else{scen <- "fut"}

  if(scen == "fut"){
    
    gcm_stack <- rast()
    
    for(g in gcms){
      gcm_stack <-  c(gcm_stack, rast(paste0("./summer_vpd/vpd_summer_", year_prev, "_", g, "_ssp585_cmip6_biascor.tif")))
    }
    
    for(g in gcms){
      gcm_stack <-  c(gcm_stack, rast(paste0("./summer_vpd/vpd_summer_", year_prev, "_", g, "_ssp245_cmip6_biascor.tif")))
    }
    
    prev_summer_vpd <- terra::app(gcm_stack, mean)
    
  }else{
    
    gcm_stack <- rast()
    
    for(g in gcms){
      gcm_stack <-  c(gcm_stack, rast(paste0("./summer_vpd/vpd_summer_", year_prev, "_", g, "_historical_cmip6_biascor.tif")))
    }
    
    prev_summer_vpd <- terra::app(gcm_stack, mean)
    
  }

  # get data from the following year
  if(y < 2014){scen <- "historical"}else{scen <- "fut"}
  
  if(scen == "fut"){
    
    gcm_stack <- rast()
    
    for(g in gcms){
      gcm_stack <-  c(gcm_stack, rast(paste0("./summer_vpd/vpd_summer_", year_aftr, "_", g, "_ssp585_cmip6_biascor.tif")))
    }
    
    for(g in gcms){
      gcm_stack <-  c(gcm_stack, rast(paste0("./summer_vpd/vpd_summer_", year_aftr, "_", g, "_ssp245_cmip6_biascor.tif")))
    }
    
    aftr_summer_vpd <- terra::app(gcm_stack, mean)
    
  }else{
    
    gcm_stack <- rast()
    
    for(g in gcms){
      gcm_stack <-  c(gcm_stack, rast(paste0("./summer_vpd/vpd_summer_", year_aftr, "_", g, "_historical_cmip6_biascor.tif")))
    }
    
    aftr_summer_vpd <- terra::app(gcm_stack, mean)
    
  }
  
 
  # calculate rolling max for each grid
  rolling_max_vpd_summer <- max(summer_vpd, prev_summer_vpd, aftr_summer_vpd)
  plot(rolling_max_vpd_summer, main = paste0(y))
  
  if(y == 1986){rast_df <- as.data.frame(rolling_max_vpd_summer, xy = T)}else{
    df <- as.data.frame(rolling_max_vpd_summer, xy = T)
    rast_df <- rast_df %>% left_join(df, by = c("x", "y"))
    
  }
  
  colnames(rast_df)[r + 2] <- y
  
  
}

# convert dataframe to sf object
df_sf <- st_as_sf(x = rast_df,                         
                  coords = c("x", "y"),
                  crs = proj_leae)

# st intersect with countries and biomes
df_biome <- st_intersection(df_sf, ecoregions)

# Read in the shapefile
world <- read_sf("ne_10m_admin_0_countries.shp")
world <- st_transform(world, proj_leae)
world <- world[,c("SOVEREIGNT", "geometry")]

df_countries <- st_intersection(df_sf, world)
df_countries_reduced <- df_countries[, c("SOVEREIGNT", "geometry")]


df_full <- as.data.frame(df_biome) %>% left_join(as.data.frame(df_countries[, c("SOVEREIGNT", "geometry")]), by = "geometry")

#df_countries <- st_intersection(df_sf[, c("geometry")], all_countries)
df_full <- df_full %>% mutate(biome = factor(BIOME, 
                                             labels = c("Temperate Broadleaf",
                                                        "Temperate Coniferous",
                                                        "Boreal Forests",
                                                        "Temperate Grasslands",
                                                        "Tundra",
                                                        "Mediterranean")),
                              country = tolower(SOVEREIGNT),
                              country = gsub("republic of ", "", country),
                              country = gsub(" and ", "", country),
                              country = gsub(" ", "", country),
                              country = gsub("northmacedonia", "macedonia", country))

df_full <- cbind(gridID = 1:nrow(df_full), df_full %>% select(X1986:X2020, biome, country))
#df_full <- df_full %>% st_drop_geometry()

write.csv(df_full, paste0("./data/hist_data_allgrids.csv"), row.names = F)


### now do the same for the future data 2020  - 2100 ----------------------------------------

# loop over all countries to extract the data 



gcms <- c("FIO-ESM-2-0", "CMCC-ESM2", "MPI-ESM1-2-LR", "EC-Earth3-Veg-LR", "CNRM-CM6-1")
ssps <- c("ssp245", "ssp585")#

for(c in ssps){
  
  r <- 0
  for(y in 2020:2099){
    
    print(y)
    
    r <- r + 1
    
    # get year before and after
    if(y == 2020){year_prev <- y}else{year_prev <- y - 1}
    if(y == 2099){year_aftr <- y}else{year_aftr <- y + 1}

    # load the data
    gcm_stack <- rast()
    
    for(g in gcms){
      gcm_stack <-  c(gcm_stack, rast(paste0("./summer_vpd/vpd_summer_",y, "_", g, "_", c, "_cmip6_biascor.tif")))
    }

    summer_vpd <- terra::app(gcm_stack, mean)

    # load data of previous year
    gcm_stack <- rast()
    
    for(g in gcms){
      gcm_stack <-  c(gcm_stack, rast(paste0("./summer_vpd/vpd_summer_",year_prev, "_", g, "_", c, "_cmip6_biascor.tif")))
    }
    
    prev_summer_vpd <- terra::app(gcm_stack, mean)
    
   # load data of following year
    gcm_stack <- rast()
    
    for(g in gcms){
      gcm_stack <-  c(gcm_stack, rast(paste0("./summer_vpd/vpd_summer_",year_aftr, "_", g, "_", c, "_cmip6_biascor.tif")))
    }
    
    aftr_summer_vpd <- terra::app(gcm_stack, mean)
    
    # calculate rolling max for each grid
    rolling_max_vpd_summer <- max(summer_vpd, prev_summer_vpd, aftr_summer_vpd)
    
    if(y == 2020){rast_df <- as.data.frame(rolling_max_vpd_summer, xy = T)}else{
      df <- as.data.frame(rolling_max_vpd_summer, xy = T)
      rast_df <- rast_df %>% left_join(df, by = c("x", "y"))
      
    }
    
    colnames(rast_df)[r + 2] <- y
    
    
    
  }
  
  # convert dataframe to sf object
  df_sf <- st_as_sf(x = rast_df,                         
                    coords = c("x", "y"),
                    crs = proj_leae)
  
  # st intersect with countries and biomes
  df_biome <- st_intersection(df_sf, ecoregions)
  
  # Read in the shapefile
  world <- read_sf("ne_10m_admin_0_countries.shp")
  world <- st_transform(world, proj_leae)
  world <- world[,c("SOVEREIGNT", "geometry")]
  
  df_countries <- st_intersection(df_sf, world)
  df_countries_reduced <- df_countries[, c("SOVEREIGNT", "geometry")]
  
  
  df_full <- as.data.frame(df_biome) %>% left_join(as.data.frame(df_countries[, c("SOVEREIGNT", "geometry")]), by = "geometry")
  
  #df_countries <- st_intersection(df_sf[, c("geometry")], all_countries)
  df_full <- df_full %>% mutate(biome = factor(BIOME, 
                                               labels = c("Temperate Broadleaf",
                                                          "Temperate Coniferous",
                                                          "Boreal Forests",
                                                          "Temperate Grasslands",
                                                          "Tundra",
                                                          "Mediterranean")),
                                country = tolower(SOVEREIGNT),
                                country = gsub("republic of ", "", country),
                                country = gsub(" and ", "", country),
                                country = gsub(" ", "", country),
                                country = gsub("northmacedonia", "macedonia", country))
  
  df_full <- cbind(gridID = 1:nrow(df_full), df_full %>% select(X2020:X2099, biome, country))
  
  write.csv(df_full, paste0("./data/fut_data_allgrids_", c, ".csv"), sep = ",", row.names = F)

}

#### check visually -----
df_hist <- read.csv("./data/hist_data_allgrids.csv", sep = ",") 
df_hist <- df_hist %>% select(gridID, X1986:X2020, biome) %>% 
  gather(key = "year", value = "value", -gridID, -biome) %>% 
  mutate(year = gsub("X", "", year)) %>% group_by(biome, year) %>% 
  mutate(value =  as.numeric(value)) %>% summarise(value = mean(value))


df_fut <- read.csv("./data/fut_data_allgrids_ssp585.csv", sep = ",") 
df_fut <- df_fut %>% select(gridID, X2021:X2098, biome) %>% 
  gather(key = "year", value = "value", -gridID, -biome) %>% 
  mutate(year = gsub("X", "", year)) %>% group_by(biome, year) %>% summarise(value = mean(value))

df_full <- rbind(df_hist, df_fut)

# add ERA5 data
complexes_df <- read.csv(paste0("./data/pres_extract_complexes_final.csv"))
dim(complexes_df)
dim(na.omit(complexes_df))
biomes <- c(12, 5, 8, 6, 11, 4)
names_biomes <- c("Mediterranean", "Temperate Coniferous", "Temperate Grasslands", "Boreal Forests", "Tundra", "Temperate Broadleaf")
biomes_df <- data.frame(cbind(as.numeric(biomes), names_biomes))
complexes_df <- cbind(complexes_df, Biome = biomes_df$names_biomes[match(complexes_df$biom, biomes_df$V1)]) 

dat_era <- complexes_df %>% select(year, Biome, vpd_summer) %>% group_by(Biome, year) %>% summarise(value = mean(vpd_summer))

ggplot() +
  geom_line(dat_era, mapping = aes(x = as.integer(year), y = value), col = "blue") +
  geom_line(df_hist, mapping = aes(x = as.integer(year), y = value), col = "red") +
  geom_line(df_fut,  mapping = aes(x = as.integer(year), y = value)) +
  facet_wrap(~biome)

