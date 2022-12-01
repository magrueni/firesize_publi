###################################################################
### M. Gruenig & C. Senf 29.11.2022 ###############################
###################################################################

### ------------------------------------------------------------------------
### Script for data analysis and modelling
### For this step we need the fire complexes with extracted VPDs
### 
### ------------------------------------------------------------------------


### libraries ---------------------------------
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
edit_r_environ()

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


plotdat_full <- bind_rows(plotdat, plotdat_all)

### plot the data and look at trends ---------------------------------------------
plot1 <- plotdat_full %>% 
  mutate(biome = fct_relevel(biome, c("Europe", "Mediterranean", "Temperate Broadleaf", "Temperate Coniferous", "Temperate Grasslands", "Boreal Forests", "Tundra"))) %>%
  mutate(biome = factor(biome, 
                        labels =c("Europe", "Mediterranean", "Temperate broadleaf", "Temperate coniferous", "Temperate grasslands", "Boreal forests", "Tundra"))) %>% 
  
  split(.$key) %>%
  map2(.x = .,
       .y = list("Tot. area burned [log10(ha)]", "Max. fire severity [dNBR]", "Max. fire size [log10(ha)]", "Number of fires (log10)"), 
       ~ ggplot(., aes(x = year, y = value, col = biome)) +
           geom_line() +
           facet_wrap(~biome, ncol = 7) +
           scale_color_manual(values = c("black", "#66CCEE", "#228833", "#CCBB44", "#EE6677", "#4477AA", "#AA3377")) +
           theme_classic() +
           theme(legend.position = "none",
                 strip.background = element_blank(),
                 strip.text = element_text(size=6), 
                 axis.text.x = element_text(size = 6),
                 axis.text.y = element_text(size = 6),
                 axis.title.y = element_text(size = 10),
                 axis.title.x = element_text(size = 10)) +
           labs(x = "Year", y = .y) +
           geom_smooth(method = "lm", se = FALSE, linetype = "dotted"))


ggsave(
  filename = "plots.png", path = "./firesize/figures/",
  plot = marrangeGrob(plot1, nrow=4, ncol=1, top = NULL), 
  width = 7, height = 7, device = "png"
)



### data preparation for modelling -----------------------------------------------------------------

# Look at the data grouped by biome/country
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
            complex_size_m2_q95 = quantile(complex_size_m2, 0.95),
            complex_severity_nbr_mean = mean(complex_severity_nbr),
            complex_severity_nbr_q95 = quantile(complex_severity_nbr, 0.95),
            complex_severity_nbr_q90 = quantile(complex_severity_nbr, 0.90),
            complex_severity_nbr_max = max(complex_severity_nbr)) %>%
  ungroup()


### size modelling -----------------------------------------------------------------------------

# specify data
dat_model_size <- dat_all %>%
  dplyr::select(complex_size_m2_max, vpd_summer_mean_rollmax, biome, country) %>%
  drop_na()

# models
# watch out: long runtime. We provide the calibrated models in the data/models folder
options(buildtools.check = function(action) TRUE)
mod0_size <- brm(complex_size_m2_max ~ (vpd_summer_mean_rollmax) + (1 + (vpd_summer_mean_rollmax) | biome/country), data = dat_model_size, family = "gaussian", cores = 4, chains = 4, save_pars = save_pars(all = TRUE), control = list(adapt_delta = 0.95))
mod1_size <- brm(log10(complex_size_m2_max) ~ (vpd_summer_mean_rollmax) + (1 + (vpd_summer_mean_rollmax) | biome/country), data = dat_model_size, family = "gaussian", cores = 4, chains = 4, save_pars = save_pars(all = TRUE), control = list(adapt_delta = 0.95))
mod2_size <- brm(log10(complex_size_m2_max) ~ log10(vpd_summer_mean_rollmax) + (1 + log10(vpd_summer_mean_rollmax) | biome/country), data = dat_model_size, family = "gaussian", cores = 4, chains = 4, save_pars = save_pars(all = TRUE), control = list(adapt_delta = 0.95))

# assess which model is better
loo_compare(loo(mod0_size), loo(mod1_size), loo(mod2_size))
bayes_factor(mod2_size, mod1_size, log = T)
bayes_factor(mod2_size, mod0_size, log = T)

# continue with better model
mod_size <- mod2_size
summary(mod_size)

saveRDS(mod_size, "./data/models/final_size.rds")
#mod_size <- readRDS("./data/models/final_size.rds")


# make postorior predictions and look at model
ppt_size <- posterior_predict(mod_size)
ppc_dens_overlay(y = log10(dat_model_size$complex_size_m2_max), yrep = ppt_size[1:100,])
dev.print(tiff, "./data/figures/ppc_densplot_size.tif", width = 3.5, height = 3.5, units = "in", res = 300)

# look at the effects
posteriors_size <- as.matrix(mod_size)

vpd_effect_size <- posteriors_size[, "b_log10vpd_summer_mean_rollmax"] %>%
  tibble(vpd_effect_size = .)

# plot random slopes for biomes
random_slope_biome_size <- posteriors_size[, grep(glob2rx("r_biome[*log10vpd_summer_mean_rollmax*"), colnames(posteriors_size))] %>%
  as_tibble() %>%
  mutate(draw = 1:n()) %>%
  gather(key = biome, value = value, -draw) %>%
  mutate(biome = gsub("r_biome\\[", "", biome),
         biome = gsub(",log10vpd_summer_mean_rollmax\\]", "", biome),
         biome = gsub("\\.", " ", biome))

# plot the random slopes
ggplot(random_slope_biome_size, aes(x = value, y = biome, fill = biome)) +
  geom_violin() +
  scale_fill_manual(values = c("#4477AA", "#66CCEE", "#228833", "#CCBB44", "#EE6677", "#AA3377")) + 
  theme_classic() +
  scale_y_discrete(limits=rev) +
  theme(plot.title = element_text(size=26),
        plot.subtitle = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10, angle = 45),
        axis.title.y = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text=element_text(size=18),
        legend.position = c(1.15, 0.9)) + # position moved out of the picture
  geom_vline(xintercept = 0, col = "red", linetype="dashed", size = 1) +
  labs(fill = "", y = "", x = "VPD effect") +
  theme(legend.position = "none",
        legend.background=element_rect(colour = "transparent", fill = "transparent"))


# plot random slopes plus the global vpd effect 
p_vpdsu_tempav <- as.data.frame(posteriors_size[, grep(glob2rx("r_biome[*log10vpd_summer_mean_rollmax*"), colnames(posteriors_size))]) %>% 
  map(., ~ .+ posteriors_size[ , colnames(posteriors_size) == "b_log10vpd_summer_mean_rollmax"]) %>% 
  as.tibble() %>%
  mutate(draw = 1:n()) %>%
  gather(key = biome, value = value, -draw) %>%
  mutate(biome = gsub("r_biome\\[", "", biome),
         biome = gsub(",log10vpd_summer_mean_rollmax\\]", "", biome),
         biome = gsub("\\.", " ", biome))

model_effects_size <- p_vpdsu_tempav %>% group_by(biome) %>% summarize(mean_effect = mean(value),
                                                                       lower = quantile(value, 0.025),
                                                                       upper = quantile(value, 0.975))

# plot the random slopes
vpd_effect_biomes <- ggplot(p_vpdsu_tempav %>% 
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
  geom_violin(aes(x = value, y = biome,fill = biome), lwd = 0.3) + 
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
  labs(fill = "", y = "", x = "VPD effect") +
  geom_vline(xintercept = round(mean(vpd_effect_size$vpd_effect_size), 2), col = "red", linetype="dashed", size = 1) +
  geom_boxplot(aes(x = value, y = biome, fill = biome), width=0.1, lwd = 0.2, outlier.size = 0.4)

vpd_effect_biomes


# save the plot
ggsave(
  paste0("vpd_effect_biomes_plot.png"),
  path = "./data/figures/",
  vpd_effect_biomes,
  width = 5, 
  height = 3.5,
  device = "png"
)


# assess the performance of the final model
r2_size <- performance::r2_bayes(mod_size)
r2_size


# draw posterior predictions with linepred over the full span of VPD in the dataset for the biomes
newdat_biome <- dat_model_size %>%
  split(.$biome) %>%
  map(., ~ expand_grid(vpd_summer_mean_rollmax = seq(min(.$vpd_summer_mean_rollmax, na.rm = TRUE), 
                                                    max(.$vpd_summer_mean_rollmax, na.rm = TRUE), length.out = 100),
                       biome = unique(.$biome))) %>%
  bind_rows()

# predictions
pred_biome <- posterior_linpred(mod_size, newdata = newdat_biome, re.form = ~ (1 + log10(vpd_summer_mean_rollmax) | biome))
pred_biome <- 10^pred_biome

# statistics of predictions
newdat_biome$pred_median <- apply(pred_biome, 2, median)
newdat_biome$CI_95_lower <- apply(pred_biome, 2, quantile, 0.025)
newdat_biome$CI_95_upper <- apply(pred_biome, 2, quantile, 0.975)

# predictions also including biome and country random slopes
newdat <- expand_grid(vpd_summer_mean_rollmax = seq(min(dat_model_size$vpd_summer_mean_rollmax, na.rm = TRUE), 
                                                   max(dat_model_size$vpd_summer_mean_rollmax, na.rm = TRUE), 
                                                   length.out = 100))

pred_global <- posterior_linpred(mod_size, newdata = newdat, re.form = NA)
pred_global <- 10^pred_global

newdat$pred_median <- apply(pred_global, 2, median)
newdat$CI_95_lower <- apply(pred_global, 2, quantile, 0.025)
newdat$CI_95_upper <- apply(pred_global, 2, quantile, 0.975)

# make the plot for all biomes including stats on the graph
p_size_lin <- dat_model_size %>%
  ggplot(., aes(x = (vpd_summer_mean_rollmax), y = (complex_size_m2_max) * 0.0001)) +
  geom_point(aes(col = biome, shape = biome), alpha = 0.3) +
  #facet_wrap(~biome) +
  geom_line(data = newdat_biome, aes(x = (vpd_summer_mean_rollmax), y = (pred_median) * 0.0001, col = biome), size = 0.6) +
  geom_line(data = newdat, aes(x = (vpd_summer_mean_rollmax), y = (pred_median) * 0.0001), size = 0.7) +
  theme_classic() +
  #ylim(0, 5e+03) +
  scale_color_manual(values = c("#4477AA", "#66CCEE", "#228833", "#CCBB44", "#EE6677", "#AA3377")) +
  labs(x = "Mean summer VPD (kPa)", y = "Maximum fire size (ha)", col = NULL, shape = NULL) +
  theme() +
  annotate("text", x = 0.2, y = 10e+5, 
           label = paste0("beta[VPD]==", round(mean(vpd_effect_size$vpd_effect_size), 2), "~(", round(quantile(vpd_effect_size$vpd_effect_size, 0.025), 2), "-", round(quantile(vpd_effect_size$vpd_effect_size, 0.975), 2), ")"),
           parse = TRUE, size = 2.8, hjust = 0) +
  annotate("text", x = 0.2, y = 3e+5, 
           label = paste0("R[marg]^2==", round(r2_size$R2_Bayes_marginal, 2), "~(", round(0.034,2), "-", round(0.376,2), ")"),
           parse = TRUE, size = 2.8, hjust = 0) +
  annotate("text", x = 0.2, y = 1e+5, 
           label = paste0("R[cond]^2==", round(r2_size$R2_Bayes, 2), "~(", round(0.495,2), "-", round(0.553,2), ")"),
           parse = TRUE, size = 2.8, hjust = 0) +
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                     labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  theme(legend.position = "none",
        legend.background=element_rect(colour = "transparent", fill = "transparent"),
        strip.background = element_blank(),
        strip.text = element_text(size=8), 
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        strip.text.x = element_blank())


p_size_lin

# save the plot
ggsave(
  paste0("fire_size_model.png"),
  path = "./data/figures/",
  p_size_lin,
  width = 3.5, 
  height = 3.5,
  device = "png"
)


### severity -----------------------------------------------------------------------------------------------------------------------------

# select data
dat_model_seve <- dat_all %>%
  dplyr::select(complex_severity_nbr_max, vpd_summer_mean_rollmax, biome, country) %>%
  drop_na() 

# change the negative values to close to 0
dat_model_seve[dat_model_seve$complex_severity_nbr_max <= 0, "complex_severity_nbr_max"] <- 0.00001

# build models: linear, exponential, power law
# watch out: long runtime. We provide the calibrated models in the data/models folder
options(buildtools.check = function(action) TRUE)
mod0_seve <- brm((complex_severity_nbr_max) ~ (vpd_summer_mean_rollmax) + (1 + (vpd_summer_mean_rollmax) | biome/country), data = dat_model_seve, family = "gaussian", cores = 4, chains = 4, save_pars = save_pars(all = TRUE), control = list(adapt_delta = 0.98))
mod1_seve <- brm(log10(complex_severity_nbr_max) ~ (vpd_summer_mean_rollmax) + (1 + (vpd_summer_mean_rollmax) | biome/country), data = dat_model_seve, family = "gaussian", cores = 4, chains = 4, save_pars = save_pars(all = TRUE), control = list(adapt_delta = 0.98))
mod2_seve <- brm(log10(complex_severity_nbr_max) ~ log10(vpd_summer_mean_rollmax) + (1 + log10(vpd_summer_mean_rollmax) | biome/country), data = dat_model_seve, family = "gaussian", cores = 4, chains = 4, save_pars = save_pars(all = TRUE), control = list(adapt_delta = 0.98))

# compare models
loo_compare(loo(mod0_seve), loo(mod1_seve), loo(mod2_seve))

# check if one model is better than the other
bayes_factor(mod2_seve, mod1_seve, log = TRUE)
bayes_factor(mod2_seve, mod0_seve, log = TRUE)

# continue with the best model
mod_seve <- mod2_seve

saveRDS(mod_seve, "./data/models/final_seve.rds")
#mod_seve <- readRDS("./data/models/final_seve.rds")

## check summary
summary(mod_seve)
r2_size <- performance::r2_bayes(mod_seve)

# make posterior preds to check performance visually
ppt_size <- posterior_predict(mod_seve)
ppc_dens_overlay(y = log10(dat_model_seve$complex_severity_nbr_max), yrep = ppt_size[1:100,])

dev.print(tiff, "./data/figures/ppc_densplot_seve.tif", width = 3.5, height = 3.5, units = "in", res = 300)

# look at the effects
posteriors_size <- as.matrix(mod_seve)

vpd_effect_size <- posteriors_size[, "b_log10vpd_summer_mean_rollmax"] %>%
  tibble(vpd_effect_size = .)

random_slope_biome_seve <- posteriors_size[, grep(glob2rx("r_biome[*log10vpd_summer_mean_rollmax*"), colnames(posteriors_size))] %>%
  as_tibble() %>%
  mutate(draw = 1:n()) %>%
  gather(key = biome, value = value, -draw) %>%
  mutate(biome = gsub("r_biome\\[", "", biome),
         biome = gsub(",log10vpd_summer_mean_rollmax\\]", "", biome),
         biome = gsub("\\.", " ", biome))

# plot the random slopes
ggplot(random_slope_biome_seve, aes(x = value, y = biome, fill = biome)) +
  geom_violin() +
  scale_fill_manual(values = c("#4477AA", "#66CCEE", "#228833", "#CCBB44", "#EE6677", "#AA3377")) + 
  theme_classic() +
  scale_y_discrete(limits=rev) +
  theme(plot.title = element_text(size=26),
        plot.subtitle = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 10, hjust = 1),
        axis.text.y = element_text(size = 10, angle = 45),
        axis.title.y = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text=element_text(size=18),
        legend.position = c(1.15, 0.9)) + # position moved out of the picture
  geom_vline(xintercept = 0, col = "red", linetype="dashed", size = 1) +
  labs(fill = "", y = "", x = "VPD effect") +
  theme(legend.position = "none",
        legend.background=element_rect(colour = "transparent", fill = "transparent"))


# plot random slopes plus the global vpd effect 
# change colors and make nice
p_vpdsu_tempav <- as.data.frame(posteriors_size[, grep(glob2rx("r_biome[*log10vpd_summer_mean_rollmax*"), colnames(posteriors_size))]) %>% 
  map(., ~ .+ posteriors_size[ , colnames(posteriors_size) == "b_log10vpd_summer_mean_rollmax"]) %>% 
  as.tibble() %>%
  mutate(draw = 1:n()) %>%
  gather(key = biome, value = value, -draw) %>%
  mutate(biome = gsub("r_biome\\[", "", biome),
         biome = gsub(",log10vpd_summer_mean_rollmax\\]", "", biome),
         biome = gsub("\\.", " ", biome))

model_effects_seve <- p_vpdsu_tempav %>% group_by(biome) %>% summarize(mean_effect = mean(value),
                                                 lower = quantile(value, 0.025),
                                                 upper = quantile(value, 0.975))

# plot the random slopes
vpd_effect_biomes_seve <- ggplot(p_vpdsu_tempav %>% 
                                   mutate(biome = factor(biome, 
                                                         labels = c("Boreal forests",
                                                                    "Mediterranean",
                                                                    "Temperate broadleaf",
                                                                    "Temperate coniferous",
                                                                    "Temperate grasslands",
                                                                    "Tundra"))) %>% 
                                   mutate(biome = fct_relevel(biome, rev(c("Tundra", "Boreal forests", "Temperate coniferous", "Temperate grasslands", "Temperate broadleaf", "Mediterranean"))))) +
  geom_violin(aes(x = value, y = biome,fill = biome), lwd = 0.3) + 
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
  labs(fill = "", y = "", x = "VPD effect") +
  geom_vline(xintercept = round(mean(vpd_effect_size$vpd_effect_size), 2), col = "red", linetype="dashed", size = 1) +
  geom_boxplot(aes(x = value, y = biome, fill = biome), width=0.1, lwd = 0.2, outlier.size = 0.4)

vpd_effect_biomes_seve

# save the plot
ggsave(
  paste0("vpd_effect_biomes_severity_plot.png"),
  path = "./data/figures/",
  vpd_effect_biomes_seve,
  width = 5, 
  height = 3.5,
  device = "png"
)

# need to add the intercept to get the full effects

# get R2
r2_seve <- performance::r2_bayes(mod_seve)
r2_seve

# create newdat for posterior linpreds including only biome random effects
newdat_biome <- dat_model_seve %>% 
  split(.$biome) %>%
  map(., ~ expand_grid(vpd_summer_mean_rollmax = seq(min(.$vpd_summer_mean_rollmax, na.rm = TRUE), 
                                                    max(.$vpd_summer_mean_rollmax, na.rm = TRUE), length.out = 100),
                       biome = unique(.$biome))) %>%
  bind_rows()

pred_biome <- posterior_linpred(mod_seve, newdata = newdat_biome, re.form = ~ (1 + log10(vpd_summer_mean_rollmax) | biome))
pred_biome <- 10^pred_biome

newdat_biome$pred_median <- apply(pred_biome, 2, median)
newdat_biome$CI_95_lower <- apply(pred_biome, 2, quantile, 0.025)
newdat_biome$CI_95_upper <- apply(pred_biome, 2, quantile, 0.975)


# create newdat for posterior linpreds including no random effects
newdat <- expand_grid(vpd_summer_mean_rollmax = seq(min(dat_model_seve$vpd_summer_mean_rollmax, na.rm = TRUE), 
                                                   max(dat_model_seve$vpd_summer_mean_rollmax, na.rm = TRUE), 
                                                   length.out = 100))

pred_global <- posterior_linpred(mod_seve, newdata = newdat, re.form = NA)
pred_global <- 10^pred_global

newdat$pred_median <- apply(pred_global, 2, median)
newdat$CI_95_lower <- apply(pred_global, 2, quantile, 0.025)
newdat$CI_95_upper <- apply(pred_global, 2, quantile, 0.975)


# make the plot for all biomes including stats on the graph
p_seve_lin <- dat_model_seve %>%
  ggplot(., aes(x = (vpd_summer_mean_rollmax), y = (complex_severity_nbr_max))) +
  geom_point(aes(col = biome, shape = biome), alpha = 0.3) +
  #facet_wrap(~biome) +
  geom_line(data = newdat_biome, aes(x = (vpd_summer_mean_rollmax), y = (pred_median), col = biome), size = 0.6) +
  geom_line(data = newdat, aes(x = (vpd_summer_mean_rollmax), y = (pred_median)), size = 0.7) +
  theme_classic() +
  #ylim(0, 4) + # one value is excluded
  #ylim(0, 5e+03) +
  scale_color_manual(values = c("#4477AA", "#66CCEE", "#228833", "#CCBB44", "#EE6677", "#AA3377"),
                     labels =  c("Boreal Forests" = "Boreal forests",
                                 "Mediterranean" = "Mediterranean",
                                 "Temperate Broadleaf"  = "Temperate broadleaf",
                                 "Temperate Coniferous" = "Temperate coniferous",
                                 "Temperate Grasslands" = "Temperate grasslands",
                                 "Tundra" = "Tundra")) + 
  scale_shape_manual(values = c(19,17,15,3,7,8),
                     labels =  c("Boreal Forests" = "Boreal forests",
                                 "Mediterranean" = "Mediterranean",
                                 "Temperate Broadleaf"  = "Temperate broadleaf",
                                 "Temperate Coniferous" = "Temperate coniferous",
                                 "Temperate Grasslands" = "Temperate grasslands",
                                 "Tundra" = "Tundra")) + 
  
  labs(x = "Mean summer VPD (kPa)", y = "Maximum burn severity [dNBR]", col = NULL, shape = NULL) +
  theme() +
  annotate("text", x = 0.2, y = 6.9, 
           label = paste0("beta[VPD]==", round(mean(vpd_effect_size$vpd_effect_size), 2), "~(", round(quantile(vpd_effect_size$vpd_effect_size, 0.025), 2), "-", round(quantile(vpd_effect_size$vpd_effect_size, 0.975), 2), ")"),
           parse = TRUE, size = 2.8, hjust = 0) +
  annotate("text", x = 0.2, y = 5, 
           label = paste0("R[marg]^2==", round(r2_size$R2_Bayes_marginal, 2), "~(", round(0.013,2), "-", round(0.236,2), ")"),
           parse = TRUE, size = 2.8, hjust = 0) +
  annotate("text", x = 0.2, y = 3.7, 
           label = paste0("R[cond]^2==", round(r2_size$R2_Bayes, 2), "~(", round(0.525,2), "-", round(0.580,2), ")"),
           parse = TRUE, size = 2.8, hjust = 0) +
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  theme(legend.position = "bottom",
        legend.background=element_rect(colour = "transparent", fill = "transparent"),
        strip.background = element_blank(),
        strip.text = element_text(size=8), 
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        strip.text.x = element_blank())

p_seve_lin


# save the plot
ggsave(
  paste0("fire_severity_model.png"),
  path = "./firesize/figures/",
  p_seve_lin,
  width = 3.5, 
  height = 3.5,
  device = "png"
)




### model effect table -------------------------------------------------------------------

model_effects <- cbind(model_effects_size, model_effects_seve[,2:4])
colnames(model_effects) <- c("Biome", "size_effect", "lower_CI_size", "upper_CI_size", "mean_severity_effect", "lower_CI_severity", "upper_CI_severity")
europe_effect <- c("Europe", as.numeric(fixef(mod_size)[2,1]), as.numeric(fixef(mod_size)[2,3]), as.numeric(fixef(mod_size)[2,4]), as.numeric(fixef(mod_seve)[2,1]), as.numeric(fixef(mod_seve)[2,3]), as.numeric(fixef(mod_seve)[2,4]))

model_effect <- rbind(europe_effect, model_effects)
model_effect[,2:7] <- lapply(model_effect[,2:7], function(x) as.numeric(as.character(x)))
model_effect[, 1] <- c("Europe", "Boreal forests", "Mediterranean", "Temperate broadleaf", "Temperate coniferous", "Temperate grasslands", "Tundra")

write_xlsx(model_effect, "./data/results/table1_final.xlsx")



