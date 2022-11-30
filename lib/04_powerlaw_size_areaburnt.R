###################################################################
### M. Gruenig & C. Senf 29.11.2022 ###############################
###################################################################

### ------------------------------------------------------------------------
###  investigate the correlation between burnt area and maximum fire size -----
### ------------------------------------------------------------------------


### libraries ----------------
if (!require("raster")) install.packages("raster")
if (!require("terra")) install.packages("terra")
if (!require("sf")) install.packages("sf")
if (!require("rgdal")) install.packages("rgdal")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("exactextractr")) install.packages("exactextractr")
if (!require("layer")) install.packages("layer")
if (!require("stars")) install.packages("stars")
if (!require("dplyr")) install.packages("dplyr")
if (!require("brms")) install.packages("brms")
if (!require("bayesplot")) install.packages("bayesplot")
if (!require("loo")) install.packages("loo")
if (!require("mcmcplots")) install.packages("mcmcplots")
if (!require("ggmcmc")) install.packages("ggmcmc")


### settings -------------------------------

#General temp directory 
write("TMP = './tmp/'", file = file.path('~.Renviron')) 

# Raster package
rasterOptions(tmpdir = "./tmp/")
terraOptions(memfrac=0.5, tempdir = "./tmp/")
tmpFiles(remove=TRUE, current=T, orphan=T)
removeTmpFiles()

### load data --------------------------------------------------------------------------------------------

# load df with extracted env variables
complexes_df <- read.csv(paste0("./data/results/pres_extract_complexes_final.csv"))

# add biomes names
biomes <- c(12, 5, 8, 6, 11, 4)
names_biomes <- c("Mediterranean", "Temperate Coniferous", "Temperate Grasslands", "Boreal Forests", "Tundra", "Temperate Broadleaf")
biomes_df <- data.frame(cbind(as.numeric(biomes), names_biomes))
complexes_df <- cbind(complexes_df, Biome = biomes_df$names_biomes[match(complexes_df$biom, biomes_df$V1)]) 

# filter infinite nbr values
out <- complexes_df %>% 
  filter(is.finite(complex_severity_nbr))

dat_hist <- out %>%
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
  ungroup() %>% drop_na()



# cor between max fire size and burnt area
ggplot(dat_hist, aes(x = log10(complex_size_m2_max), y = log10(burnt_area), color = biome)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_grid(~biome, scales = "free")

# cor between frequency and burnt area
ggplot(dat_hist, aes(x = log10(n_fires), y = log10(burnt_area), color = biome)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_grid(~biome, scales = "free")

# cor between severity and burnt area
ggplot(dat_hist, aes(x = log10(complex_severity_nbr_max), y = log10(burnt_area), color = biome)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_grid(~biome, scales = "free")


### investigate the correlation between burnt area and maximum fire size ---------------------------------
# fit model
# watch out: run time. can be loaded directly
fit_size_ba <- brm(log10(burnt_area) ~ log10(complex_size_m2_max) + (1 + log10(complex_size_m2_max) | biome/country), data = dat_hist, control = list(adapt_delta = 0.99))

saveRDS(fit_size_ba, "./data/models/max_size_burnt_area_final.rds")
#fit_size_ba <- readRDS("./data/models/max_size_burnt_area_final.rds")

# check performance
summary(fit_size_ba)
plot(fit_size_ba)
pp_check(fit_size_ba)
r2_plr <- performance::r2_bayes(fit_size_ba)
r2_plr

ppt_size <- posterior_predict(fit_size_ba)
ppc_dens_overlay(y = log10(dat_hist$burnt_area), yrep = ppt_size[1:30,])

# check the model effects
posteriors <- as.matrix(fit_size_ba)

global_k <- posteriors[, "b_log10complex_size_m2_max"] %>%
  tibble(vpd_effect_size = .)

summary(global_k$vpd_effect_size)


biome_k <- as.data.frame(posteriors[, grep(glob2rx("r_biome[*log10complex_size_m2_max]"), colnames(posteriors))]) %>%
  map(., ~ .+ posteriors[ , colnames(posteriors) == "b_log10complex_size_m2_max"]) %>% 
  as_tibble() %>%
  mutate(draw = 1:n()) %>%
  gather(key = biome, value = value, -draw) %>%
  mutate(biome = gsub("r_biome\\[", "", biome),
         biome = gsub(",log10complex_size_m2_max\\]", "", biome),
         biome = gsub("\\.", " ", biome))

biome_k %>% group_by(biome) %>% summarise(median_effect = median(value),
                                                             q05_effect = quantile(value, 0.05),
                                                             q95_effect = quantile(value, 0.95))

area_size_model_effects <- ggplot(biome_k  %>% 
                                    mutate(biome = factor(biome, 
                                                          labels = c("Boreal forests",
                                                                     "Mediterranean",
                                                                     "Temperate broadleaf",
                                                                     "Temperate coniferous",
                                                                     "Temperate grasslands",
                                                                     "Tundra"))) %>% 
                                    mutate(biome = fct_relevel(biome, rev(c("Tundra",
                                                                            "Boreal forests",
                                                                            "Temperate coniferous",
                                                                            "Temperate grasslands",
                                                                            "Temperate broadleaf",
                                                                            "Mediterranean"))))) +
  geom_violin(aes(x = biome, y = value, fill = biome), lwd = 0.3) + 
  coord_flip() +
  scale_fill_manual(values = c("#66CCEE", "#228833", "#EE6677", "#CCBB44", "#4477AA", "#AA3377")) + 
  theme_classic() +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_text(size=8), 
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        strip.text.x = element_blank()) +
  labs(y = "Global effect + biome effects", x = "") +
  geom_boxplot(aes(x = biome, y = value, fill = biome), width=0.1, lwd = 0.2, outlier.size = 0.4)

area_size_model_effects

ggsave(
  paste0("area_size_model_effects.png"),
  path = "./data/figures/",
  area_size_model_effects,
  width = 5, 
  height = 3.5,
  device = "png"
)


newdat_biome <- dat_hist %>%
  split(.$biome) %>%
  map(., ~ expand_grid(complex_size_m2_max = seq(min(.$complex_size_m2_max, na.rm = TRUE), 
                                                 max(.$complex_size_m2_max, na.rm = TRUE), length.out = 100),
                       biome = unique(.$biome))) %>%
  bind_rows()

pred_biome <- posterior_linpred(fit_size_ba, newdata = newdat_biome, re.form = ~ (1 + log10(complex_size_m2_max) | biome))
pred_biome <- 10^pred_biome

newdat_biome$pred_median <- apply(pred_biome, 2, median)
newdat_biome$CI_95_lower <- apply(pred_biome, 2, quantile, 0.025)
newdat_biome$CI_95_upper <- apply(pred_biome, 2, quantile, 0.975)

newdat <- expand_grid(complex_size_m2_max = seq(min(dat_hist$complex_size_m2_max, na.rm = TRUE), 
                                                max(dat_hist$complex_size_m2_max, na.rm = TRUE), 
                                                    length.out = 100))

pred_global <- posterior_linpred(fit_size_ba, newdata = newdat, re.form = NA)
pred_global <- 10^pred_global

newdat$pred_median <- apply(pred_global, 2, median)
newdat$CI_95_lower <- apply(pred_global, 2, quantile, 0.025)
newdat$CI_95_upper <- apply(pred_global, 2, quantile, 0.975)

# the plot
p_plr <- dat_hist %>%
  ggplot(., aes(x = (complex_size_m2_max * 0.001), y = (burnt_area * 0.001))) +
  geom_point(aes(col = biome, shape = biome), alpha = 0.3) +
  #facet_wrap(~biome) +
  geom_line(data = newdat_biome, aes(x = (complex_size_m2_max * 0.001), y = (pred_median * 0.001), col = biome), size = 1) +
  geom_line(data = newdat, aes(x = (complex_size_m2_max * 0.001), y = (pred_median * 0.001)), size = 1.1) +
  theme_classic() +
  #ylim(0, 5e+03) +
  scale_color_manual(values = c("#4477AA", "#66CCEE", "#228833", "#CCBB44", "#EE6677", "#AA3377")) +
  labs(x = "Maximum fire size (ha)", y = "Total area burned (ha)", col = NULL, shape = NULL) +
  theme() +
   annotate("text", x = 0.5, y = 10e+6, 
            label = paste0("beta[max.firesize]==", round(mean(global_k$vpd_effect_size), 2), "~(", round(quantile(global_k$vpd_effect_size, 0.025), 2), "-", round(quantile(global_k$vpd_effect_size, 0.975), 2), ")"),
            parse = TRUE, size = 2.5, hjust = 0) +
   annotate("text", x = 0.5, y = 3e+6, 
            label = paste0("R[marg]^2==", round(r2_plr$R2_Bayes_marginal, 2), "~(", round(0.839,2), "-", round(0.888,2), ")"),
            parse = TRUE, size = 2.5, hjust = 0) +
   annotate("text", x = 0.5, y = 1e+6, 
            label = paste0("R[cond]^2==", round(r2_plr$R2_Bayes, 2), "~(", round(0.949,2), "-", round(0.953,2), ")"), 
            parse = TRUE, size = 2.5, hjust = 0) +
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_text("", size=8), 
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        plot.title = element_text("")) +
    geom_abline(intercept = 0, slope = 1, col = "grey", linetype = "dashed")

p_plr

# save the plot
ggsave(
  paste0("powerlaw_figure.png"),
  path = "./data/figures/",
  p_plr,
  width = 3.5, 
  height = 3.5,
  device = "png"
)


