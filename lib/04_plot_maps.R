###################################################################
### M. Gruenig & C. Senf 29.11.2022 ###############################
###################################################################

### ------------------------------------------------------------------------
### script to plot maximum fire size and severity for each country -----
### ------------------------------------------------------------------------

### libraries -------------------------------
library(dplyr)
library(ggplot2)
library(sf)
library(MetBrewer)
library(tidyverse)
library(ggpubr)
library(gridExtra)
library(rnaturalearth)

### settings -------------------------------

setwd("")

#General temp directory 
write("TMP = './firesize/tmp/'", file = file.path('.Renviron')) 

### load data --------------------------------------------------------------------------------------------

# load df with extracted env variables
complexes_df <- read.csv(paste0("./firesize/data/pres_extract_complexes_final_10082022.csv"))

# add biomes names
biomes <- c(12, 5, 8, 6, 11, 4)
names_biomes <- c("Mediterranean", "Temperate Coniferous", "Temperate Grasslands", "Boreal Forests", "Tundra", "Temperate Broadleaf")
biomes_df <- data.frame(cbind(as.numeric(biomes), names_biomes))
complexes_df <- cbind(complexes_df, Biome = biomes_df$names_biomes[match(complexes_df$biom, biomes_df$V1)]) 

# filter infinite nbr values
out <- complexes_df %>% 
  filter(is.finite(complex_severity_nbr)) %>% drop_na()

# Overview plots
plotdat <- out %>%
  bind_rows() %>%
  group_by(year, biome = Biome, country) %>%
  summarize(complex_size_m2_max = max(complex_size_m2),
            complex_severity_nbr_max = max(complex_severity_nbr)) %>%
  ungroup() %>%
  select(year, biome, country, complex_size_m2_max, complex_severity_nbr_max) 

plotdat %>% group_by(biome, year) %>% summarise(max_size = max(complex_size_m2_max),
                                                max_seve = max(complex_severity_nbr_max)) %>% 
  group_by(biome) %>% summarise(max_size_mean = mean(max_size) * 0.0001,
                                max_size_max = max(max_size) * 0.0001,
                                median_seve = median(max_seve))


plot_data <- plotdat %>% group_by(country, year) %>% summarise(max_size = max(complex_size_m2_max),
                                                               max_seve = max(complex_severity_nbr_max)) %>% 
  group_by(country) %>% summarise(mean_max_fire_size = mean(max_size) * 0.0001,
                                  mean_max_fire_seve = median(max_seve))



### mapping -----------------------------------------------
box <-  c(xmin = -12, ymin = 35, xmax = 35, ymax = 72)

world <- ne_countries(scale = "medium", returnclass = "sf") %>% 
  st_make_valid() %>% 
  st_transform(., "+proj=longlat +datum=WGS84 +no_defs") %>% st_crop(
    x = ., y = st_as_sfc(st_bbox(c(xmin= -12, ymin = 32, xmax = 42, ymax = 72), crs = "+proj=longlat +datum=WGS84 +no_defs")))


world_df <- world %>% mutate(country = tolower(sovereignt),
                             country = gsub("republic of ", "", country),
                             country = gsub("czech republic", "czechia", country),
                             country = gsub(" and ", "", country),
                             country = gsub(" ", "", country),
                             country = gsub("northmacedonia", "macedonia", country))

world_df <- st_transform(world_df, "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
plot(world_df)

# countries with no data get zeros
belgium <- plot_data %>% filter(country == "netherlands")
belgium[, c(2:3)] <- cbind(1,0) # size to 1 because we take log10 afterwards
belgium[, 1] <- "belgium"
luxembourg <- belgium
luxembourg[, 1] <- "luxembourg"
andorra <- belgium
andorra[, 1] <- "andorra"
kosovo <- plot_data %>% filter(country == "serbia") # kosovo mapped with values from serbia
kosovo[, 1] <- "kosovo"
plot_data <- rbind(plot_data, belgium, kosovo, luxembourg, andorra)

# bring together
plot_data_map <- right_join(world_df, plot_data, by = "country", all.x = TRUE) %>% select(country, mean_max_fire_size, mean_max_fire_seve, geometry)

#organise the base map
proj_leae <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"

countries <- read_sf("data/countries/europe.shp")
countries <- st_transform(countries, proj_leae)

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
world <- st_transform(world, "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
world <- st_crop(world, st_bbox(countries) + c(-0.05, -0.05, 0.01, 0.01) * as.double(st_bbox(countries)))

# make plot
p <- ggplot() +
  geom_sf(data = world, color = "black", fill = "lightgray", size = 0.1) +
  geom_sf(data = plot_data_map, aes(fill = log10(mean_max_fire_size)), color = "black", size = 0.1) +
  scale_fill_met_c(name = "OKeeffe2", direction = 1) +
  theme_linedraw() +
  theme(legend.key.height = unit(0.2, "in"),
        legend.key.width = unit(0.05, "in"),
        legend.position = c(0.93,0.8),
        legend.direction = "vertical",
        legend.title = element_text(size = 6),
        legend.text = element_text(size = 6),
        legend.background = element_rect(fill = "transparent"),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        plot.margin = unit(c(0.1,0.1,0.1,0.1), "in")) +
  guides(fill = guide_colorbar(title = expression(log[10](ha)), 
                               title.position = "top",
                               title.align = 0.5)) +
  coord_sf(expand = FALSE)


# severity plot
p2 <- ggplot() +
  geom_sf(data = world, color = "black", fill = "lightgray", size = 0.1) +
  geom_sf(data = plot_data_map, aes(fill = (mean_max_fire_seve)), color = "black", size = 0.1) +
  scale_fill_met_c(name = "OKeeffe2", direction = 1) +
  theme_linedraw() +
  theme(legend.key.height = unit(0.2, "in"),
        legend.key.width = unit(0.05, "in"),
        legend.position = c(0.93,0.8),
        legend.direction = "vertical",
        legend.title = element_text(size = 6),
        legend.text = element_text(size = 6),
        legend.background = element_rect(fill = "transparent"),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        plot.margin = unit(c(0.1,0.1,0.1,0.1), "in")) +
  guides(fill = guide_colorbar(title = "dNBR", 
                               title.position = "top",
                               title.align = 0.5)) +
  coord_sf(expand = FALSE)


# bring plots together
p_fin <- grid.arrange(p, p2, ncol = 2)
p_fin

# save
ggsave("./firesize/figures/figure1_maps_final.png", p_fin, width = 7.5, height = 3.5, unit = "in", dpi = 600)


# check max fire size for biomes
out %>%
  bind_rows() %>%
  group_by(year, biome = Biome) %>%
  summarize(complex_size_m2_max = max(complex_size_m2),
            complex_severity_nbr_max = max(complex_severity_nbr)) %>%
  ungroup() %>%
  select(year, biome, complex_size_m2_max, complex_severity_nbr_max) %>%
  group_by(biome) %>% 
  summarize(avg_max_size = mean(complex_size_m2_max) * 0.0001,
            max_max_size = max(complex_size_m2_max) * 0.0001,
            avg_max_seve = mean(complex_severity_nbr_max))

out %>%
  bind_rows() %>%
  group_by(year) %>%
  summarize(complex_size_m2_max = max(complex_size_m2),
            complex_severity_nbr_max = max(complex_severity_nbr)) %>%
  ungroup() %>%
  select(year, complex_size_m2_max, complex_severity_nbr_max) %>% 
  summarize(avg_max_size = mean(complex_size_m2_max) * 0.0001,
            max_max_size = max(complex_size_m2_max) * 0.0001,
            avg_max_seve = mean(complex_severity_nbr_max))


plotdat <- out %>%
  bind_rows() %>%
  group_by(year, country) %>%
  summarize(complex_size_m2_max = max(complex_size_m2),
            complex_severity_nbr_max = max(complex_severity_nbr)) %>%
  ungroup() %>%
  select(year, country, complex_size_m2_max, complex_severity_nbr_max) %>%
  group_by(country) %>% 
  summarize(avg_max_size = mean(complex_size_m2_max) * 0.0001,
            max_max_size = max(complex_size_m2_max) * 0.0001,
            avg_max_seve = mean(complex_severity_nbr_max))

plotdat %>% filter(country == "portugal")

max(plotdat$avg_max_seve)
min(plotdat$avg_max_seve)
