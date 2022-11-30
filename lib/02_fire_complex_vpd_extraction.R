###################################################################
### M. Gruenig & C. Senf 29.11.2022 ###############################
###################################################################

### ------------------------------------------------------------------------
### In this script we extract the ERA5 VPD data for the fire complexes -----
### The complexes are obtained from the European disturbance maps (download from https://doi.org/10.5281/zenodo.7080016)
### But the raw complexes are saved in the data/complexes folder in this repo
### The result from this step is already saved in the folder ./data/results/
### ------------------------------------------------------------------------


### libraries ---------------------------------------------------------
if (!require("raster")) install.packages("raster")
if (!require("terra")) install.packages("terra")
if (!require("sf")) install.packages("sf")
if (!require("rgdal")) install.packages("rgdal")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("exactextractr")) install.packages("exactextractr")
if (!require("layer")) install.packages("layer")
if (!require("stars")) install.packages("stars")
if (!require("dplyr")) install.packages("dplyr")


### settings --------------------------------------------------

#General temp directory 
write("TMP = './tmp/'", file = file.path('~.Renviron')) 

# Raster package
rasterOptions(tmpdir = "./tmp/")
terraOptions(memfrac=0.5, tempdir = "./tmp/")
tmpFiles(remove=TRUE, current=T, orphan=T)
removeTmpFiles()

# leae CRS
crs_leae <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs" 


### load fire complexes -----------------------------------------------

cntrs <- list.files("./data/complexes", ".gpkg$") %>% 
  str_sub(., 14, nchar(.) - 5)
cntrs <- cntrs[grepl("*eps150", cntrs)] %>% str_sub(., 0, nchar(.) - 7)


k <- 0
out <- vector("list", length(cntrs))

cntrs.fullnam <- list.files("./data/complexes", ".gpkg", full.names = TRUE)
cntrs.fullnam <- cntrs.fullnam[grepl("*eps150", cntrs.fullnam)]

for (i in cntrs.fullnam) {
  
  k <- k + 1
  out[[k]] <- read_sf(i) %>%
    mutate(area = st_area(.)) %>%
    st_drop_geometry() %>%
    mutate(area = as.integer(area))
}

nfire <- out %>% map(nrow) %>% unlist()

a <- out[which(nfire > 0)] %>%
  set_names(cntrs[which(nfire > 0)]) %>%
  bind_rows(.id = "country")

# read the grid
grid <- read_sf("./data/climate/climategrid_epsg3035_complexes.gpkg")
grid_raster <- st_rasterize(grid)
grid_r <- rast(grid_raster)


### load ecoregions -------------------------------------------------------

# load the olson ecoregions 
# this dataset can be downloaded at https://www.arcgis.com/home/item.html?id=be0f9e21de7a4a61856dad78d1c79eae
crs_grid <- st_crs(grid)
ecoregions <- st_read("./data/ecoregions/terrestrial_ecoregions_olson.shp")
ecoregions <- st_transform(ecoregions, crs_grid)
ecoregions <- vect(ecoregions)

# make raster from shapefile for later use
ecoreg_rast <- rasterize(ecoregions, grid_r, field = "BIOME")


### loop over all countries to extract the data --------------------------------------------
df.new <- NULL
for(c in unique(a$country)){
  
  print(c)
  # separate dataframe
  a.country <- a[a$country == c,]

  # load complexes
  dat_complexes <- st_read(paste0("./data/complexes/fire_complex_", c, "_eps150.gpkg"), quiet = T) 

  # check if there are any non-polygon geometries (there is one in Greece) and sort it out
  dat_nas <- dat_complexes %>% filter(!grepl("POLYGON", st_geometry_type(dat_complexes)))
  if(nrow(dat_nas) > 0){
    dat_complexes <- dat_complexes[!(dat_complexes$clust == dat_nas$clust & dat_complexes$year == dat_nas$year), ]
    a.country <- a.country[!(a.country$clust == dat_nas$clust & a.country$year == dat_nas$year),]
  }
  
  # extract biome information
  df_preds <- as.data.frame(cbind(a.country, biom = exact_extract(ecoreg_rast, dat_complexes, fun = "mode", progress = F)))
  
  # check for NA values in the Biome column and use larger buffer because some points are outside the biome border
  nas_biom <- df_preds[is.na(df_preds$biom),]
  if(nrow(nas_biom) > 0){
    dat_complexes_nas <- dat_complexes %>% semi_join(., nas_biom, by = c("clust", "year"))
    
    #create a buffer around the points 2*r2 = 78300 which is the median patch size
    sp_buffer_biomes <- st_buffer(dat_complexes_nas, dist = 10000) 
    
    # extract the data
    df_preds_biom <- as.data.frame(cbind(nas_biom[,c(1:4)], 
                                         biom = exact_extract(ecoreg_rast, sp_buffer_biomes, fun = "mode", progress = F)))
    # put into df_preds
    df_preds[is.na(df_preds$biom), "biom"] <- df_preds_biom$biom
    
    # again iwth larger area
    nas_biom <- df_preds[is.na(df_preds$biom),]
    if(nrow(nas_biom) > 0){
      dat_complexes_nas <- dat_complexes %>% semi_join(., nas_biom, by = c("clust", "year"))
      
      #create a buffer around the points 2*r2 = 78300 which is the median patch size
      sp_buffer_biomes <- st_buffer(dat_complexes_nas, dist = 20000) 
      
      # extract the data
      df_preds_biom <- as.data.frame(cbind(nas_biom[,c(1:4)], 
                                           biom = exact_extract(ecoreg_rast, sp_buffer_biomes, fun = "mode", progress = F)))
      # put into df_preds
      df_preds[is.na(df_preds$biom), "biom"] <- df_preds_biom$biom
    }
  }
  
  # loop over the years to extract the vpd  -----------------------------------------------------------------
  years <- 1986:2020
  
  presabs_extract <- NULL
  for(y in years){
    #print(y)
    
    year_prev <- y - 1
    year_aftr <- y + 1
    
    if(y == 1986){year_prev <- y}; if(y == 2020){year_aftr <- y}
    
    # isolate the presabs data for this year
    pres_year <- a.country[a.country$year == y,]
    if(nrow(pres_year) == 0){next}
    
    # load the data
    summer_vpd <- rast(paste0("./data/climate/vpd/summer_vpd_", y, ".tif"))
    summer_vpd <- terra::project(summer_vpd, crs_leae)
    
    # load data of previous year
    prev_summer_vpd <- rast(paste0("./data/climate/vpd/summer_vpd_", year_prev, ".tif"))
    prev_summer_vpd <- terra::project(prev_summer_vpd, crs_leae)
    
    # load data of following year
    aftr_summer_vpd <- rast(paste0("./data/climate/vpd/summer_vpd_", year_aftr, ".tif"))
    aftr_summer_vpd <- terra::project(aftr_summer_vpd, crs_leae)
    
    # calculate rolling max for each grid
    rolling_max_vpd_summer <- max(summer_vpd, prev_summer_vpd, aftr_summer_vpd)
    
    
      # isolate the complexes of the year    
    dat_complexes_year <- dat_complexes %>% filter(., year == y)
    #dat_complexes_year_vect <- vect(dat_complexes_year)
    
    # extract the data
    df_preds_year <- as.data.frame(cbind(pres_year,
                                         vpd_summer = exact_extract(summer_vpd, dat_complexes_year, fun = "mean", progress = F),
                                         vpd_summer_roll = exact_extract(rolling_max_vpd_summer, dat_complexes_year, fun = "mean", progress = F)))
    
    #colnames(df_preds_year)[c(19,20)] <- c("vpd_summer", "vpd_summer_roll")
    
    # use larger buffer because some points are outside the biome border
    nas <- df_preds_year[is.na(df_preds_year$vpd_summer) | is.nan(df_preds_year$vpd_summer),]
    
    if(nrow(nas) > 0){
      #print(paste0("NAs ", y))
      #print(paste0("NA detected ", y))
      dat_complexes_year_nas <- dat_complexes_year[dat_complexes_year$clust %in% nas$clust,]
      #create a buffer around the points 2*r2 = 78300 which is the median patch size
      sp_buffer_biomes <- vect(st_buffer(dat_complexes_year_nas, dist = 5000))
      # extract the data
      df_preds_year_nas <- as.data.frame(cbind(nas[,1:4],
                                               vpd_summer = terra::extract(summer_vpd, sp_buffer_biomes, exact = TRUE, fun = "mean", na.rm = T)[,2],
                                               vpd_summer_roll = terra::extract(rolling_max_vpd_summer, sp_buffer_biomes, exact = TRUE, fun = "mean", na.rm = T)[,2]))
      
      #df_preds_year_nas[,-c(5,7)]
      colnames(df_preds_year_nas)[c(5,6)] <- c("vpd_summer", "vpd_summer_roll")
      # combine 
      df_preds_year[is.na(df_preds_year$vpd_summer), c("vpd_summer", "vpd_summer_roll")] <- df_preds_year_nas[, c("vpd_summer", "vpd_summer_roll")]
      
 
      
      plot(sp_buffer_biomes, add = T, col = "blue")
      
    }
    
    
    # use larger buffer because some points are outside the biome border
    nas <- df_preds_year[is.na(df_preds_year$vpd_summer) | is.nan(df_preds_year$vpd_summer),]
    
    if(nrow(nas) > 0){
      
      dat_complexes_year_nas <- dat_complexes_year[dat_complexes_year$clust %in% nas$clust,]
      #create a buffer around the points 2*r2 = 78300 which is the median patch size
      sp_buffer_biomes <- vect(st_buffer(dat_complexes_year_nas, dist = 10000))
      # extract the data
      df_preds_year_nas <- as.data.frame(cbind(nas[,1:4],
                                               vpd_summer = terra::extract(summer_vpd, sp_buffer_biomes, fun = "mean", na.rm = T)[,2],
                                               vpd_summer_roll = terra::extract(rolling_max_vpd_summer, sp_buffer_biomes, fun = "mean", na.rm = T)[,2]))
      
      #df_preds_year_nas[,-c(5,7)]
      colnames(df_preds_year_nas)[c(5,6)] <- c("vpd_summer", "vpd_summer_roll")
      # combine 
      df_preds_year[is.na(df_preds_year$vpd_summer), c("vpd_summer", "vpd_summer_roll")] <- df_preds_year_nas[, c("vpd_summer", "vpd_summer_roll")]
      
      plot(sp_buffer_biomes, add = T, col = "blue")
      
    }
    

    # use larger buffer because some points are outside the biome border
    nas <- df_preds_year[is.na(df_preds_year$vpd_summer) | is.nan(df_preds_year$vpd_summer),]
    
    if(nrow(nas) > 0){
     
      dat_complexes_year_nas <- dat_complexes_year[dat_complexes_year$clust %in% nas$clust,]
      #create a buffer around the points 2*r2 = 78300 which is the median patch size
      sp_buffer_biomes <- vect(st_buffer(dat_complexes_year_nas, dist = 20000))
      # extract the data
      df_preds_year_nas <- as.data.frame(cbind(nas[,1:4],
                                               vpd_summer = terra::extract(summer_vpd, sp_buffer_biomes, exact = TRUE, fun = "mean", na.rm = T)[,2],
                                               vpd_summer_roll = terra::extract(rolling_max_vpd_summer, sp_buffer_biomes, exact = TRUE, fun = "mean", na.rm = T)[,2]))
      
      #df_preds_year_nas[,-c(5,7)]
      colnames(df_preds_year_nas)[c(5,6)] <- c("vpd_summer", "vpd_summer_roll")
      # combine 
      df_preds_year[is.na(df_preds_year$vpd_summer), c("vpd_summer", "vpd_summer_roll")] <- df_preds_year_nas[, c("vpd_summer", "vpd_summer_roll")]
      
      
      
      plot(sp_buffer_biomes, add = T, col = "blue")
      
    }
    
    # use larger buffer because some points are outside the biome border
    nas <- df_preds_year[is.na(df_preds_year$vpd_summer) | is.nan(df_preds_year$vpd_summer),]
    presabs_extract <- rbind(presabs_extract, df_preds_year)
    
  }
  
  df_all <- left_join(df_preds, presabs_extract[, c("country", "clust", "year", "vpd_summer", "vpd_summer_roll")], by = c("country", "clust", "year"))
  
  
  if(nrow(df_all) != nrow(na.omit(df_all))){print(paste0("PROBLEM", c, " ", nrow(df_all) - nrow(na.omit(df_all))))}
  
  
  df.new <- rbind(df.new, df_all)
  
  
}


write.csv(df.new, paste0("./data/pres_extract_complexes_final.csv"))
gc()
tmpFiles(remove=TRUE, current=T, orphan=T)

