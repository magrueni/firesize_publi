###################################################################
### M. Gruenig 29.11.2022           ###############################
###################################################################

### ------------------------------------------------------------------------
### Script for future climate analysis                                 -----
### ------------------------------------------------------------------------


### libraries ----------------
#libraries
if (!require("terra")) install.packages("terra")
if (!require("brms")) install.packages("brms")
if (!require("raster")) install.packages("raster")
if (!require("MetBrewer")) install.packages("MetBrewer")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("sf")) install.packages("sf")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")


### prepare data --------------------------------

# set seed for reproducability
set.seed(14)

# load the models
mod_severity <- readRDS("./data/models/final_seve.rds")
mod_size <- readRDS("./data/models/final_size.rds")

# load the data
dat_hist <- read_csv("./data/results/hist_data_allgrids.csv")
dat_hist <- dat_hist %>% drop_na() %>% 
  dplyr::select(gridID, X1986:X2020, biome, country) %>%
  gather(key = "year", value = "vpd_summer_mean_rollmax", -gridID, -biome, -country) %>%
  mutate(year = as.integer(gsub("X", "", year)))

dat_hist_biome_prediction_biome <- dat_hist %>%
  group_by(biome, year, country) %>%
  summarize(vpd_summer_mean_rollmax = mean(vpd_summer_mean_rollmax)) %>% 
  ungroup()

dat_fut_ssp245 <- read_csv(file = "./data/results/fut_data_allgrids_ssp245.csv")
dat_fut_ssp245 <- dat_fut_ssp245 %>%
  dplyr::select(gridID, X2020:X2099, biome, country) %>%
  gather(key = "year", value = "vpd_summer_mean_rollmax", -gridID, -biome, -country) %>%
  mutate(year = as.integer(gsub("X", "", year)))

dat_fut_ssp245_biome_prediction_biome <- dat_fut_ssp245 %>%
  group_by(biome, year, country) %>%
  summarize(vpd_summer_mean_rollmax = mean(vpd_summer_mean_rollmax)) %>%
  ungroup()


dat_fut_ssp585 <- read_csv(file = "./data/results/fut_data_allgrids_ssp585.csv")
dat_fut_ssp585 <- dat_fut_ssp585 %>%
  dplyr::select(gridID, X2020:X2099, biome, country) %>%
  gather(key = "year", value = "vpd_summer_mean_rollmax", -gridID, -biome, -country) %>%
  mutate(year = as.integer(gsub("X", "", year)))

dat_fut_ssp585_biome_prediction_biome <- dat_fut_ssp585 %>%
  group_by(biome, year, country) %>%
  summarize(vpd_summer_mean_rollmax = mean(vpd_summer_mean_rollmax)) %>%
  ungroup()


### predict models ------------------------------------------------------------------

# Size
prediction_size_hist <- posterior_predict(mod_size, newdata = dat_hist_biome_prediction_biome, re.form = ~ (1 + log10(vpd_summer_mean_rollmax) | biome/country), allow_new_levels = TRUE)
prediction_size_fut_ssp245 <- posterior_predict(mod_size, newdata = dat_fut_ssp245_biome_prediction_biome, re.form = ~ (1 + log10(vpd_summer_mean_rollmax) | biome/country), allow_new_levels = TRUE)
prediction_size_fut_ssp585 <- posterior_predict(mod_size, newdata = dat_fut_ssp585_biome_prediction_biome, re.form = ~ (1 + log10(vpd_summer_mean_rollmax) | biome/country), allow_new_levels = TRUE)

prediction_size_hist_summary <- 10^prediction_size_hist %>%
  t() %>%
  as_tibble() %>%
  mutate(biome = dat_hist_biome_prediction_biome$biome,
         year = dat_hist_biome_prediction_biome$year) %>%
  gather(key = "draw", value = "size", -biome, -year) %>%
  group_by(biome, draw, year) %>%
  summarize(size = max(size * 0.0001)) %>%
  group_by(year, biome) %>%
  summarize(size_predicted_median = median(size),
            size_predicted_lower = quantile(size, 0.025, na.rm = T),
            size_predicted_upper = quantile(size, 0.975, na.rm = T)) %>% 
  mutate(biome = fct_relevel(biome, c("Mediterranean", "Temperate Broadleaf", "Boreal Forests", "Temperate Grasslands", "Temperate Coniferous", "Tundra"))) %>% 
  mutate(biome = factor(biome, 
                        labels =c("Mediterranean", "Temperate broadleaf", "Boreal forests", "Temperate grasslands", "Temperate coniferous", "Tundra")))

prediction_size_fut_ssp245_summary <- 10^prediction_size_fut_ssp245 %>%
  t() %>%
  as_tibble() %>%
  mutate(biome = dat_fut_ssp245_biome_prediction_biome$biome,
         year = dat_fut_ssp245_biome_prediction_biome$year) %>%
  gather(key = "draw", value = "size", -biome, -year) %>%
  group_by(biome, draw, year) %>%
  summarize(size = max(size * 0.0001)) %>%
  group_by(year, biome) %>%
  summarize(size_predicted_median = median(size),
            size_predicted_lower = quantile(size, 0.025, na.rm = T),
            size_predicted_upper = quantile(size, 0.975, na.rm = T)) %>% 
  mutate(biome = fct_relevel(biome, c("Mediterranean", "Temperate Broadleaf", "Boreal Forests", "Temperate Grasslands", "Temperate Coniferous", "Tundra"))) %>% 
  mutate(biome = factor(biome, 
                        labels =c("Mediterranean", "Temperate broadleaf", "Boreal forests", "Temperate grasslands", "Temperate coniferous", "Tundra")))

prediction_size_fut_ssp585_summary <- 10^prediction_size_fut_ssp585 %>%
  t() %>%
  as_tibble() %>%
  mutate(biome = dat_fut_ssp585_biome_prediction_biome$biome,
         year = dat_fut_ssp585_biome_prediction_biome$year) %>%
  gather(key = "draw", value = "size", -biome, -year) %>%
  group_by(biome, draw, year) %>%
  summarize(size = max(size * 0.0001)) %>%
  group_by(year, biome) %>%
  summarize(size_predicted_median = median(size),
            size_predicted_lower = quantile(size, 0.025),
            size_predicted_upper = quantile(size, 0.975)) %>% 
  mutate(biome = fct_relevel(biome, c("Mediterranean", "Temperate Broadleaf", "Boreal Forests", "Temperate Grasslands", "Temperate Coniferous", "Tundra"))) %>% 
  mutate(biome = factor(biome, 
                        labels =c("Mediterranean", "Temperate broadleaf", "Boreal forests", "Temperate grasslands", "Temperate coniferous", "Tundra")))

# plotting
p_size <- ggplot() +
  geom_ribbon(data = prediction_size_hist_summary, aes(x = year, ymin = size_predicted_lower, ymax = size_predicted_upper), alpha = 0.1) +
  geom_line(data = prediction_size_hist_summary, aes(x = year, y = size_predicted_median)) +
  geom_ribbon(data = prediction_size_fut_ssp245_summary, aes(x = year, ymin = size_predicted_lower, ymax = size_predicted_upper), alpha = 0.25, fill = "#004488") +
  geom_line(data = prediction_size_fut_ssp245_summary, aes(x = year, y = size_predicted_median), col = "#004488") +
  geom_ribbon(data = prediction_size_fut_ssp585_summary, aes(x = year, ymin = size_predicted_lower, ymax = size_predicted_upper), alpha = 0.25, fill = "#BB5566") +
  geom_line(data = prediction_size_fut_ssp585_summary, aes(x = year, y = size_predicted_median), col = "#BB5566") +
  facet_wrap(~biome, ncol = 6) +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 7),
        axis.text = element_text(size = 7),
        axis.title = element_text(size = 8),
        panel.background = element_rect(color = "black", fill = NA, size = 1.2)) +
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  labs(x = "Year", y = "Max. fire size (ha)")

# extract the numbers
max_fut <- prediction_size_fut_ssp585_summary %>% filter(year == 2099)
min_fut <- prediction_size_fut_ssp585_summary %>% filter(year == 2020)

diff <- cbind(min = min_fut[,3], max = max_fut[,3])
colnames(diff) <- c("min", "max")
diff %>% mutate(diff_prct = max/min * 100)



# Severity
prediction_severity_hist <- posterior_predict(mod_severity, newdata = dat_hist_biome_prediction_biome, re.form = ~ (1 + log10(vpd_summer_mean_rollmax) | biome/country), allow_new_levels = TRUE)
prediction_severity_fut_ssp245 <- posterior_predict(mod_severity, newdata = dat_fut_ssp245_biome_prediction_biome, re.form = ~ (1 + log10(vpd_summer_mean_rollmax) | biome/country), allow_new_levels = TRUE)
prediction_severity_fut_ssp585 <- posterior_predict(mod_severity, newdata = dat_fut_ssp585_biome_prediction_biome, re.form = ~ (1 + log10(vpd_summer_mean_rollmax) | biome/country), allow_new_levels = TRUE)

prediction_severity_hist_summary <- 10^prediction_severity_hist %>%
  t() %>%
  as_tibble() %>%
  mutate(biome = dat_hist_biome_prediction_biome$biome,
         year = dat_hist_biome_prediction_biome$year) %>%
  gather(key = "draw", value = "severity", -biome, -year) %>%
  group_by(biome, draw, year) %>%
  summarize(severity = max(severity)) %>%
  group_by(year, biome) %>%
  summarize(severity_predicted_median = median(severity),
            severity_predicted_lower = quantile(severity, 0.025),
            severity_predicted_upper = quantile(severity, 0.975)) %>% 
  mutate(biome = fct_relevel(biome, c("Mediterranean", "Temperate Broadleaf", "Boreal Forests", "Temperate Grasslands", "Temperate Coniferous", "Tundra"))) %>% 
  mutate(biome = factor(biome, 
                        labels =c("Mediterranean", "Temperate broadleaf", "Boreal forests", "Temperate grasslands", "Temperate coniferous", "Tundra")))

prediction_severity_fut_ssp245_summary <- 10^prediction_severity_fut_ssp245 %>%
  t() %>%
  as_tibble() %>%
  mutate(biome = dat_fut_ssp245_biome_prediction_biome$biome,
         year = dat_fut_ssp245_biome_prediction_biome$year) %>%
  gather(key = "draw", value = "severity", -biome, -year) %>%
  group_by(biome, draw, year) %>%
  summarize(severity = max(severity)) %>%
  group_by(year, biome) %>%
  summarize(severity_predicted_median = median(severity),
            severity_predicted_lower = quantile(severity, 0.025),
            severity_predicted_upper = quantile(severity, 0.975)) %>% 
  mutate(biome = fct_relevel(biome, c("Mediterranean", "Temperate Broadleaf", "Boreal Forests", "Temperate Grasslands", "Temperate Coniferous", "Tundra"))) %>% 
  mutate(biome = factor(biome, 
                        labels =c("Mediterranean", "Temperate broadleaf", "Boreal forests", "Temperate grasslands", "Temperate coniferous", "Tundra")))

prediction_severity_fut_ssp585_summary <- 10^prediction_severity_fut_ssp585 %>%
  t() %>%
  as_tibble() %>%
  mutate(biome = dat_fut_ssp585_biome_prediction_biome$biome,
         year = dat_fut_ssp585_biome_prediction_biome$year) %>%
  gather(key = "draw", value = "severity", -biome, -year) %>%
  group_by(biome, draw, year) %>%
  summarize(severity = max(severity)) %>%
  group_by(year, biome) %>%
  summarize(severity_predicted_median = median(severity),
            severity_predicted_lower = quantile(severity, 0.025),
            severity_predicted_upper = quantile(severity, 0.975)) %>% 
  mutate(biome = fct_relevel(biome, c("Mediterranean", "Temperate Broadleaf", "Boreal Forests", "Temperate Grasslands", "Temperate Coniferous", "Tundra"))) %>% 
  mutate(biome = factor(biome, 
                        labels =c("Mediterranean", "Temperate broadleaf", "Boreal forests", "Temperate grasslands", "Temperate coniferous", "Tundra")))

p_severity <- ggplot() +
  geom_ribbon(data = prediction_severity_hist_summary, aes(x = year, ymin = severity_predicted_lower, ymax = severity_predicted_upper), alpha = 0.1) +
  geom_line(data = prediction_severity_hist_summary, aes(x = year, y = severity_predicted_median)) +
  geom_ribbon(data = prediction_severity_fut_ssp245_summary, aes(x = year, ymin = severity_predicted_lower, ymax = severity_predicted_upper), alpha = 0.25, fill = "#004488") +
  geom_line(data = prediction_severity_fut_ssp245_summary, aes(x = year, y = severity_predicted_median), col = "#004488") +
  geom_ribbon(data = prediction_severity_fut_ssp585_summary, aes(x = year, ymin = severity_predicted_lower, ymax = severity_predicted_upper), alpha = 0.25, fill = "#BB5566") +
  geom_line(data = prediction_severity_fut_ssp585_summary, aes(x = year, y = severity_predicted_median), col = "#BB5566") +
  facet_wrap(~biome, ncol = 6) +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 7),
        axis.text = element_text(size = 7),
        axis.title = element_text(size = 8),
        panel.background = element_rect(color = "black", fill = NA, size = 1.2)) +
  labs(x = "Year", y = "Max. fire severity (dNBR)")

library(patchwork)
p <- p_size + theme(axis.text.x = element_blank(), 
                    axis.title.x = element_blank(),
                    axis.ticks.length.x = unit(0, "cm")) + 
  p_severity + 
  theme(strip.text = element_blank()) +
  plot_layout(ncol = 1)

p

ggsave("./data/figures/future_simulations.png", p, width = 7.5, height = 3)


## extract some numbers
prediction_severity_fut_ssp585_summary %>% filter(year %in% c(2020, 2099))

max_fut <- prediction_severity_fut_ssp585_summary %>% filter(year == 2099)
min_fut <- prediction_severity_fut_ssp585_summary %>% filter(year == 2020)

diff <- cbind(min = min_fut[,3], max = max_fut[,3])
colnames(diff) <- c("min", "max")
diff %>% mutate(diff_prct = max/min * 100,
                diff = max - min)
  
                                                
### Probability of extremely large fires ------------------------------------------------

# add continental scale predictions
dat_hist_biome_prediction_continent <- dat_hist %>%
  group_by(year) %>%
  summarize(vpd_summer_mean_rollmax = mean(vpd_summer_mean_rollmax)) %>%
  ungroup() %>% 
  mutate(biome = paste("Europe"),
         country = NA)

dat_hist_biome_prediction_biome <- rbind(dat_hist_biome_prediction_biome, dat_hist_biome_prediction_continent)


dat_fut_ssp245_biome_prediction_continent <- dat_fut_ssp245 %>%
  group_by(year) %>%
  summarize(vpd_summer_mean_rollmax = mean(vpd_summer_mean_rollmax)) %>%
  ungroup() %>% 
  mutate(biome = paste("Europe"),
         country = NA)

dat_fut_ssp245_biome_prediction_biome <- rbind(dat_fut_ssp245_biome_prediction_biome, dat_fut_ssp245_biome_prediction_continent)


dat_fut_ssp585_biome_prediction_continent <- dat_fut_ssp585 %>%
  group_by(year) %>%
  summarize(vpd_summer_mean_rollmax = mean(vpd_summer_mean_rollmax)) %>%
  ungroup() %>% 
  mutate(biome = paste("Europe"),
         country = NA)

dat_fut_ssp585_biome_prediction_biome <- rbind(dat_fut_ssp585_biome_prediction_biome, dat_fut_ssp585_biome_prediction_continent)

# Size
prediction_size_hist <- posterior_predict(mod_size, newdata = dat_hist_biome_prediction_biome, re.form = ~ (1 + log10(vpd_summer_mean_rollmax) | biome/country), allow_new_levels = TRUE)
prediction_size_fut_ssp245 <- posterior_predict(mod_size, newdata = dat_fut_ssp245_biome_prediction_biome, re.form = ~ (1 + log10(vpd_summer_mean_rollmax) | biome/country), allow_new_levels = TRUE)
prediction_size_fut_ssp585 <- posterior_predict(mod_size, newdata = dat_fut_ssp585_biome_prediction_biome, re.form = ~ (1 + log10(vpd_summer_mean_rollmax) | biome/country), allow_new_levels = TRUE)


extreme_hist <- 10^prediction_size_hist %>%
  t() %>%
  as_tibble() %>%
  mutate(biome = dat_hist_biome_prediction_biome$biome,
         year = dat_hist_biome_prediction_biome$year) %>%
  gather(key = "draw", value = "size", -biome, -year) %>%
  group_by(biome, year) %>%
  summarize(p_extreme = mean(size * 0.0001 > 2500)) %>%
  ungroup() %>%
  mutate(period = "past") %>% 
  mutate(biome = factor(biome, 
                        labels =c("Boreal forests", "Europe", "Mediterranean", "Temperate broadleaf", "Temperate coniferous", "Temperate grasslands", "Tundra")))

extreme_fut_ssp245 <- 10^prediction_size_fut_ssp245 %>%
  t() %>%
  as_tibble() %>%
  mutate(biome = dat_fut_ssp245_biome_prediction_biome$biome,
         year = dat_fut_ssp245_biome_prediction_biome$year) %>%
  gather(key = "draw", value = "size", -biome, -year) %>%
  group_by(biome, year) %>%
  summarize(p_extreme = mean(size * 0.0001 > 2500)) %>%
  ungroup() %>%
  mutate(period = "past") %>% 
  mutate(biome = factor(biome, 
                        labels =c("Boreal forests", "Europe", "Mediterranean", "Temperate broadleaf", "Temperate coniferous", "Temperate grasslands", "Tundra")))

extreme_fut_ssp585 <- 10^prediction_size_fut_ssp585 %>%
  t() %>%
  as_tibble() %>%
  mutate(biome = dat_fut_ssp585_biome_prediction_biome$biome,
         year = dat_fut_ssp585_biome_prediction_biome$year) %>%
  gather(key = "draw", value = "size", -biome, -year) %>%
  group_by(biome, year) %>%
  summarize(p_extreme = mean(size * 0.0001 > 2500)) %>%
  ungroup() %>%
  mutate(period = "past") %>% 
  mutate(biome = factor(biome, 
                        labels =c("Boreal forests", "Europe", "Mediterranean", "Temperate broadleaf", "Temperate coniferous", "Temperate grasslands", "Tundra")))


legend_ord <- levels(with(extreme_hist, fact_reorder(biomes, p_extreme)))

p <- ggplot() +
  geom_line(data = extreme_hist, aes(x = year, y = p_extreme, col = biome), size = 0.8) +
  geom_line(data = extreme_fut_ssp585, aes(x = year, y = p_extreme, col = biome), alpha = 0.4, size = 0.8) +
  theme_classic() +
  theme(legend.position = c(0, 1),
        legend.justification = c(0, 1),
        legend.background = element_blank(),
        legend.key.height = unit(0.3, "cm"),
        legend.key.width = unit(0.3, "cm"),
        legend.text = element_text(size = 7),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 10),
        panel.background = element_rect(color = "black", fill = NA, size = 1.2)) +
  labs(x = "Year", y = "Probability of extremely large fires (> 2500 ha)", col = NULL) +
  scale_color_manual(breaks = c("Europe",  "Boreal forests", "Mediterranean", "Temperate broadleaf", "Temperate coniferous", "Temperate grasslands", "Tundra"), 
                     values = c("black", "#4477AA", "#66CCEE", "#228833", "#CCBB44", "#EE6677", "#AA3377"))

p

#ggsave("./data/figures/future_simulations_extremely_large_fires_ssp585.png", p, width = 3.5, height = 3.5)


p <- ggplot() +
  geom_line(data = extreme_hist, aes(x = year, y = p_extreme, col = biome), size = 0.8) +
  geom_line(data = extreme_fut_ssp245, aes(x = year, y = p_extreme, col = biome), alpha = 0.4, size = 0.8) +
  theme_classic() +
  theme(legend.position = c(0, 1),
        legend.justification = c(0, 1),
        legend.background = element_blank(),
        legend.key.height = unit(0.3, "cm"),
        legend.key.width = unit(0.3, "cm"),
        legend.text = element_text(size = 7),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 10),
        panel.background = element_rect(color = "black", fill = NA, size = 1.2)) +
  labs(x = "Year", y = "Probability of extremely large fires (> 2500 ha)", col = NULL) +
  scale_color_manual(breaks = c("Europe",  "Boreal forests", "Mediterranean", "Temperate broadleaf", "Temperate coniferous", "Temperate grasslands", "Tundra"), 
                     values = c("black", "#4477AA", "#66CCEE", "#228833", "#CCBB44", "#EE6677", "#AA3377"))

p

#ggsave("./data/figures/future_simulations_extremely_large_fires_SSP245.png", p, width = 3.5, height = 3.5)


