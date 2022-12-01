###################################################################
### M. Gruenig & C. Senf 29.11.2022 ###############################
###################################################################

### ------------------------------------------------------------------------
### script for plots:
### Figure 1
### Figure 2
### Figure 3
### Figure S1
### ------------------------------------------------------------------------

# some plots were done in the analysis scripts.
# the maps were done in the plot_maps.R script.


### libraries ----------------
if (!require("terra")) install.packages("terra")
if (!require("brms")) install.packages("brms")
if (!require("raster")) install.packages("raster")
if (!require("rgdal")) install.packages("rgdal")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("sf")) install.packages("sf")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")
if (!require("MetBrewer")) install.packages("MetBrewer")
if (!require("ggpubr")) install.packages("ggpubr")
if (!require("gridExtra")) install.packages("gridExtra")
if (!require("rnaturalearth")) install.packages("rnaturalearth")


### settings -------------------------------

#General temp directory 
write("TMP = './data/tmp/'", file = file.path('~.Renviron')) 

# Raster package
rasterOptions(tmpdir = "./data/tmp/")
terraOptions(memfrac=0.5, tempdir = "./data/tmp/")
tmpFiles(remove=TRUE, current=T, orphan=T)
removeTmpFiles()

### Figure 1 Maps --------------------------------------------------------------------------------------------

# load df with extracted env variables
complexes_df <- read.csv(paste0("./data/results/pres_extract_complexes_final.csv"))

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
  dplyr::select(year, biome, country, complex_size_m2_max, complex_severity_nbr_max) 

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

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>% 
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
plot_data_map <- right_join(world_df, plot_data, by = "country", all.x = TRUE) %>% dplyr::select(country, mean_max_fire_size, mean_max_fire_seve, geometry)

#organise the base map
proj_leae <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"

countries <- read_sf("./data/countries/europe.shp")
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
ggsave("./data/figures/figure1_maps.png", p_fin, width = 7.5, height = 3.5, unit = "in", dpi = 600)


# check max fire size for biomes
out %>%
  bind_rows() %>%
  group_by(year, biome = Biome) %>%
  summarize(complex_size_m2_max = max(complex_size_m2),
            complex_severity_nbr_max = max(complex_severity_nbr)) %>%
  ungroup() %>%
  dplyr::select(year, biome, complex_size_m2_max, complex_severity_nbr_max) %>%
  group_by(biome) %>% 
  summarize(avg_max_size = mean(complex_size_m2_max) * 0.0001,
            max_max_size = max(complex_size_m2_max) * 0.0001,
            avg_max_seve = mean(complex_severity_nbr_max))
# and overall
out %>%
  bind_rows() %>%
  group_by(year) %>%
  summarize(complex_size_m2_max = max(complex_size_m2),
            complex_severity_nbr_max = max(complex_severity_nbr)) %>%
  ungroup() %>%
  dplyr::select(year, complex_size_m2_max, complex_severity_nbr_max) %>% 
  summarize(avg_max_size = mean(complex_size_m2_max) * 0.0001,
            max_max_size = max(complex_size_m2_max) * 0.0001,
            avg_max_seve = mean(complex_severity_nbr_max))





### Figure 2 --------------------------------------------------------------------------------------------

# load df with extracted env variables
complexes_df <- read.csv(paste0("./data/results/pres_extract_complexes_final.csv"))

# add biomes names
biomes <- c(12, 5, 8, 6, 11, 4)
names_biomes <- c("Mediterranean", "Temperate Coniferous", "Temperate Grasslands", "Boreal Forests", "Tundra", "Temperate Broadleaf")
biomes_df <- data.frame(cbind(as.numeric(biomes), names_biomes))
complexes_df <- cbind(complexes_df, Biome = biomes_df$names_biomes[match(complexes_df$biom, biomes_df$V1)]) 

# filter infinite nbr values
out <- complexes_df %>% 
  filter(is.finite(complex_severity_nbr)) %>% drop_na()

plotdat <- out %>%
  bind_rows() %>%
  group_by(year, biome = Biome) %>%
  summarize(n_fires = n(),
            burnt_area = sum(complex_size_m2),
            complex_size_m2_max = max(complex_size_m2),
            complex_severity_nbr_max = max(complex_severity_nbr)) %>%
  ungroup() %>%
  dplyr::select(year, biome, n_fires, burnt_area, complex_size_m2_max, complex_severity_nbr_max) %>%
  gather(key = key, value = value, -year, -biome)

plotdat2 <- plotdat %>%
  filter(key == "burnt_area") %>%
  group_by(year) %>%
  mutate(total = sum(value)) %>%
  ungroup() %>%
  mutate(share = value / total)

plotdat2 %>% filter(year == 2020)

# how much dows each biome contribute in total
plotdat2 %>% group_by(biome) %>% summarise(total_biome = sum(value)) %>%  mutate(total_all = sum(total_biome)) %>%
  ungroup() %>%
  mutate(share = total_biome / total_all)

stacked_bar <- ggplot(plotdat2 %>% mutate(biome = fct_relevel(biome, rev(c("Mediterranean", "Temperate Broadleaf", "Temperate Coniferous", "Temperate Grasslands", "Boreal Forests", "Tundra")))) %>% 
                      mutate(biome = factor(biome, 
                                            labels = rev(c("Mediterranean", "Temperate broadleaf", "Temperate coniferous", "Temperate grasslands", "Boreal forests", "Tundra")))),
                                            aes(x = year, y = share, fill = biome)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = rev(c("#66CCEE", "#228833", "#CCBB44", "#EE6677", "#4477AA", "#AA3377"))) +
  theme_classic() +
  labs(x = "Year", y = "Propotion of total area burned") + 
  theme_classic() +
  theme(legend.position = "right",
        strip.background = element_blank(),
        strip.text = element_text(size=8), 
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        legend.title = element_blank())

stacked_bar

ggsave(
  paste0("burnt_area_proportion.png"),
  path = "./data/figures/",
  stacked_bar,
  width = 7, 
  height = 3.5,
  device = "png"
)


### Pareto Figure ---------------------------------------------------------

# this is part of figure 3

dat_largest_fire <- out %>%
  bind_rows() %>% 
  group_by(biome = Biome) %>%
  mutate(q_size = 1/n()) %>%
  arrange(complex_size_m2) %>%
  mutate(q_size = cumsum(q_size)) %>%
  ungroup()

q90_biome <- dat_largest_fire %>%
  group_by(Biome) %>%
  summarize(c_ba = sum(complex_size_m2[q_size >= 0.9]) / sum(complex_size_m2))

q99_biome <- dat_largest_fire %>%
  group_by(Biome) %>%
  summarize(c_ba = sum(complex_size_m2[q_size >= 0.99]) / sum(complex_size_m2))

dat_largest_fire <- dat_largest_fire %>%
  group_by(biome) %>%
  arrange(q_size) %>% 
  mutate(ba = cumsum(complex_size_m2),
         ba_rel = ba / max(ba)) %>%
  ungroup()

pareto <- ggplot(dat_largest_fire, aes(x = (q_size), y = (ba_rel), col = biome)) +
  geom_line() +
  theme_classic() +
  scale_color_manual(values = c("#4477AA", "#66CCEE", "#228833", "#CCBB44", "#EE6677", "#AA3377")) +
  labs(x = "Fire size quantiles", y = "Burnt area quantiles", col = NULL) +
  theme(legend.position = c(0, 1),
        legend.justification = c(0, 1),
        legend.key.height = unit(0.2, "cm"),
        legend.key.width = unit(0.2, "cm"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        legend.text = element_text(size = 14)) +
  theme(legend.position = "")

pareto

# save the plot
ggsave(
  paste0("pareto_figure.png"),
  path = "./data/figures/",
  pareto,
  width = 2, 
  height = 2,
  device = "png"
)


# Numbers to the figure
dat_largest_fire_eu <- out %>%
  mutate(q_size = 1/n()) %>%
  arrange(complex_size_m2) %>%
  mutate(q_size = cumsum(q_size)) %>%
  ungroup()

q90_eu <- dat_largest_fire %>%
  summarize(c_ba = sum(complex_size_m2[q_size >= 0.9]) / sum(complex_size_m2))

q99_eu <- dat_largest_fire %>%
  summarize(c_ba = sum(complex_size_m2[q_size >= 0.99]) / sum(complex_size_m2))


fin_df <- cbind(q90_biome, q99 = q99_biome[,2]) 
fin_df[,1] <- as.character(fin_df[,1])
fin_df <- rbind(fin_df, c(as.character("Europe"), as.numeric(q90_eu), as.numeric(q99_eu)))
colnames(fin_df) <- c("biome", "q90", "q99")
fin_df$q90 <- as.numeric(fin_df$q90)
fin_df$q99 <- as.numeric(fin_df$q99)

fin_df <- fin_df %>%
  mutate(biome = fct_relevel(biome, c("Europe", "Mediterranean", "Temperate Broadleaf", "Temperate Coniferous", "Temperate Grasslands", "Boreal Forests", "Tundra"))) %>%
  mutate(biome = factor(biome, 
                          labels =c( "Europe", "Mediterranean", "Temperate broadleaf", "Temperate coniferous", "Temperate grasslands", "Boreal forests", "Tundra")))




### biomes plot -------------------------------------------------------------------------

# also part of figure 3

# Raster package
rasterOptions(tmpdir = "./data/tmp/")
terraOptions(memfrac=0.5, tempdir = "./data/tmp/")
tmpFiles(remove=TRUE, current=T, orphan=T)
removeTmpFiles()

# read the grid
grid <- read_sf("./data/climate/climategrid_epsg3035_complexes.gpkg")
grid_raster <- st_rasterize(grid)
grid_r <- rast(grid_raster)

# load ecoregions 
crs_grid <- st_crs(grid)
ecoregions <- st_read("./data/ecoregions/terrestrial_ecoregions_olson.shp")
ecoregions <- st_transform(ecoregions, crs_grid)
biomes <- ecoregions[ecoregions$BIOME %in% c(4,5,6,8,11,12), "BIOME"] 
biomes <- st_crop(biomes, ext(grid_r))

biomes_gouped <- biomes %>% group_by(BIOME) %>% summarize(geometry = st_union(geometry))

# plot the ecoregions
plot(biomes_gouped, col = c("#4477AA", "#66CCEE", "#228833", "#CCBB44", "#EE6677", "#AA3377"))


# load countries
proj_leae <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs" 

eu_shp <- read_sf("./data/countries/europe_lowres.shp")
eu_shp <- st_transform(eu_shp, crs_grid)

# mask biomes
eu_vect <- vect(eu_shp)
biomes_vect <- vect(biomes_gouped)

biomes_masked <- terra::intersect(biomes_vect,eu_vect)

# plot and save
plot(biomes_masked, col = c("#228833", "#CCBB44", "#4477AA", "#EE6677", "#AA3377", "#66CCEE"), axes = F, lwd = 0.5)
dev.print(tiff, "./data/figures/biomes_map.tif", width = 3.5, height = 3.5, units = "in", res = 300)



### Figure S1 trends ---------------------------------------------------------------------------------------------------

# load df with extracted env variables
complexes_df <- read.csv(paste0("./data/results/pres_extract_complexes_final.csv"))

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
  group_by(year, biome = Biome) %>%
  summarize(n_fires = log10(n()),
            burnt_area = log10(sum(complex_size_m2)),
            complex_size_m2_max = log10(max(complex_size_m2)),
            complex_severity_nbr_max = max(complex_severity_nbr)) %>%
  ungroup() %>%
  dplyr::select(year, biome, n_fires, burnt_area, complex_size_m2_max, complex_severity_nbr_max) %>%
  gather(key = key, value = value, -year, -biome)



x <- plotdat %>% filter(., key == "complex_severity_nbr_max")
median(x$value)

plotdat_all <- out %>%
  bind_rows() %>%
  group_by(year) %>%
  summarize(n_fires = log10(n()),
            burnt_area = log10(sum(complex_size_m2)),
            complex_size_m2_max = log10(max(complex_size_m2)),
            complex_severity_nbr_max = max(complex_severity_nbr)) %>%
  ungroup() %>% 
  dplyr::select(year, n_fires, burnt_area, complex_size_m2_max, complex_severity_nbr_max) %>% 
  mutate(biome = "Europe") %>% 
  gather(key = key, value = value, -year, -biome)

x <- plotdat_all %>% filter(., key == "complex_severity_nbr_max")
median(x$value)

plotdat_full <- bind_rows(plotdat, plotdat_all)

plot1 <- plotdat_full %>% filter(biome == "Europe") %>% 
  mutate(key = fct_relevel(key, c("burnt_area", "n_fires", "complex_size_m2_max", "complex_severity_nbr_max"))) %>%
  mutate(biome = factor(biome, 
                        labels =c(""))) %>% 
  split(.$key) %>%
  map2(.x = .,
       .y = list("Tot. area burned [log10(ha)]", "Number of fires [log10]", "Max. fire size [log10(ha)]", "Max. burn severity [dNBR]"), 
       ~ggplot(., aes(x = year, y = value, col = biome)) +
         geom_line(size = 1.25) +
         facet_wrap(~biome, ncol = 4) +
         scale_color_manual(values = c("darkblue", "#66CCEE", "#228833", "#CCBB44", "#4477AA")) +
         theme_classic() +
         theme(legend.position = "none",
               strip.background = element_blank(),
               strip.text = element_text(size=6), 
               axis.text.x = element_text(size = 6),
               axis.text.y = element_text(size = 6),
               axis.title.y = element_text(size = 10),
               axis.title.x = element_text(size = 10)) +
         labs(x = "Year", y = .y))

plot1


ggsave(
  filename = "metric_plots_europe.png", path = "./data/figures/",
  plot = marrangeGrob(plot1, nrow=1, ncol=4, top = NULL), 
  width = 7.5, height = 2, device = "png"
)


### Supplementary plots --------------------------------------------------------------
# Figure S7 & S8

# load df with extracted env variables
complexes_df <- read.csv(paste0("./data/results/pres_extract_complexes_final.csv"))

# add biomes names
biomes <- c(12, 5, 8, 6, 11, 4)
names_biomes <- c("Mediterranean", "Temperate Coniferous", "Temperate Grasslands", "Boreal Forests", "Tundra", "Temperate Broadleaf")
biomes_df <- data.frame(cbind(as.numeric(biomes), names_biomes))
complexes_df <- cbind(complexes_df, Biome = biomes_df$names_biomes[match(complexes_df$biom, biomes_df$V1)]) 

# filter infinite nbr values
out <- complexes_df %>% 
  filter(is.finite(complex_severity_nbr)) %>% drop_na()

dat_all <- out %>%
  bind_rows() %>%
  group_by(year, biome = Biome, country) %>%
  summarize(vpd_summer_mean_rollmax = mean(vpd_summer_roll),
            vpd_summer_mean = mean(vpd_summer),
            vpd_summer_max_rollmax = max(vpd_summer_roll),
            vpd_summer_max = max(vpd_summer),
            n_fires = n(),
            burnt_area = sum(complex_size_m2),
            complex_size_m2_mean = mean(complex_size_m2),
            complex_size_m2_max = max(complex_size_m2),
            complex_severity_nbr_mean = mean(complex_severity_nbr),
            complex_severity_nbr_max = max(complex_severity_nbr)) %>%
  ungroup()

dat_model_size <- dat_all %>%
  dplyr::select(complex_size_m2_max, vpd_summer_mean_rollmax, biome, country) %>%
  drop_na()


# load model
mod_size <- readRDS("./data/models/final_size.rds")

# look at the effects
posteriors_size <- as.matrix(mod_size)

vpd_effect_size <- posteriors_size[, "b_log10vpd_summer_mean_rollmax"] %>%
  tibble(vpd_effect_size = .)

# plot random slopes for biomes
random_slope_country_size <- posteriors_size[, grep(glob2rx("r_biome:country[*log10vpd_summer_mean_rollmax*"), colnames(posteriors_size))] %>%
  as_tibble() %>%
  mutate(draw = 1:n()) %>%
  gather(key = country, value = value, -draw) %>%
  mutate(country = gsub("r_biome:country\\[", "", country),
         country = gsub(",log10vpd_summer_mean_rollmax\\]", "", country),
         country = gsub("\\.", " ", country),
         country = gsub("_", " ", country))

# plot the random slopes
p <- ggplot(random_slope_country_size, aes(x = value, y = country, fill = country)) +
  geom_violin() +
  theme_classic() +
  scale_y_discrete(limits=rev) +
  theme(
    axis.title.x = element_text(size = 8),
    axis.text.x = element_text(size = 6),
    axis.text.y = element_text(size = 6),
    axis.title.y = element_text(size = 6),
    legend.title = element_text(size = 6),
    legend.text=element_text(size=18),
    legend.position = c(1.15, 0.9)) + # position moved out of the picture
  geom_vline(xintercept = 0, col = "red", linetype="dashed", size = 1) +
  labs(fill = "", y = "", x = "VPD effect") +
  theme(legend.position = "none",
        legend.background=element_rect(colour = "transparent", fill = "transparent"))

p

# save the plot
ggsave(
  paste0("country_variation_model_effects_size.png"),
  path = "./data/figures/",
  p,
  width = 7, 
  height = 7,
  device = "png"
)


# same for severity 
mod_seve <- readRDS("./data/models/final_seve.rds")

# look at the effects
posteriors_size <- as.matrix(mod_seve)

vpd_effect_size <- posteriors_size[, "b_log10vpd_summer_mean_rollmax"] %>%
  tibble(vpd_effect_size = .)

# plot random slopes for biomes
random_slope_country_size <- posteriors_size[, grep(glob2rx("r_biome:country[*log10vpd_summer_mean_rollmax*"), colnames(posteriors_size))] %>%
  as_tibble() %>%
  mutate(draw = 1:n()) %>%
  gather(key = country, value = value, -draw) %>%
  mutate(country = gsub("r_biome:country\\[", "", country),
         country = gsub(",log10vpd_summer_mean_rollmax\\]", "", country),
         country = gsub("\\.", " ", country),
         country = gsub("_", " ", country))

# plot the random slopes
p <- ggplot(random_slope_country_size, aes(x = value, y = country, fill = country)) +
  geom_violin() +
  theme_classic() +
  scale_y_discrete(limits=rev) +
  theme(
    axis.title.x = element_text(size = 8),
    axis.text.x = element_text(size = 6),
    axis.text.y = element_text(size = 6),
    axis.title.y = element_text(size = 6),
    legend.title = element_text(size = 6),
    legend.text=element_text(size=18),
    legend.position = c(1.15, 0.9)) + # position moved out of the picture
  geom_vline(xintercept = 0, col = "red", linetype="dashed", size = 1) +
  labs(fill = "", y = "", x = "VPD effect") +
  theme(legend.position = "none",
        legend.background=element_rect(colour = "transparent", fill = "transparent"))


# save the plot
ggsave(
  paste0("country_variation_model_effects_seve.png"),
  path = "./data/figures/",
  p,
  width = 7, 
  height = 7,
  device = "png"
)

