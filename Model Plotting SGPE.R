# ====================================================================================================================
#                                           BAYESIAN MODEL PLOTTING - SGPE
# ====================================================================================================================

rm(list = ls())

library(dplyr); library(readr); library(ggplot2); library(brms); library(scales); library(tidyr); library(modelr);
library(tidybayes); library(ggdist); library(lubridate); library(forcats); library(stringr); library(patchwork)

# ============================
# Loading datasets and models
# ============================

SGPE_hatching <- read_csv("SGPE_hatching_bayesian_data.csv")
SGPE_fledging <- read_csv("SGPE_fledging_bayesian_data.csv")
SGPE_breeding <- read_csv("SGPE_breeding_bayesian_data.csv")
SGPE_hatching_no_2023 <- read_csv("SGPE_hatching_bayesian_data_no_2023.csv")

SGPE_hatching_model <- readRDS("SGPE_hatching_model.rds")
SGPE_fledging_model <- readRDS("SGPE_fledging_model.rds")
SGPE_breeding_model <- readRDS("SGPE_breeding_model.rds")
SGPE_hatching_model_no2023 <- readRDS("SGPE_hatching_model_no_2023.rds")

SGPE_final <- read_csv("SGPE_final.csv")
SGPE_window_climate <- read_csv("SGPE_window_climate.csv")
SGPE_AIC_nestlevel <- read_csv("SGPE_AIC_nestlevel.csv")

hatching_scaling <- read_csv("SGPE_hatching_scaling.csv", show_col_types = FALSE)
fledging_scaling <- read_csv("SGPE_fledging_scaling.csv", show_col_types = FALSE)
hatching_scaling_no_2023 <- read_csv("SGPE_hatching_scaling_no_2023.csv", show_col_types = FALSE)

# ==================================================================================================================
#                                                  Hatching success
# ==================================================================================================================

# Aggregating observations into years for plotting 
SGPE_hatching_annual <- SGPE_hatching %>%
  group_by(Year) %>%
  summarise(hatching_success = mean(outcome),
            hatched_nests = sum(outcome == 1),
            total_nests = n(),
            hatching_precip_raw = first(hatching_precip_raw),
            hatching_temp_raw = first(hatching_temp_raw),
            hatching_wind_raw = first(hatching_wind_raw),
            .groups = "drop") %>%
  mutate(Year = as.numeric(as.character(Year)))

# ==============================
# Hatching success by year plot 
# ==============================

# Annual hatching success of Southern Giant Petrels - proportion of nests that hatched successfully
SGPE_hatch_year <- ggplot(SGPE_hatching_annual, aes(x = Year, y = hatching_success)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  geom_text(aes(label = total_nests, vjust = ifelse(Year %in% c(2011, 2013, 2015, 2020, 2023), -1, 2)), size = 3) + 
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  scale_x_continuous(breaks = seq(min(SGPE_hatching_annual$Year), max(SGPE_hatching_annual$Year),by = 1)) +
  theme_minimal() +
  labs(x = "Year",
       y = "Hatching success")  +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# =============================================================
# Hatching success vs precipitation predicted probability plot 
# =============================================================

# Loading the mean and SD directly from modelling script to ensure predictors are standardised in figure in the 
# exact same way as the model predictions - plot must show raw values in mm
hatching_precip_mean <- hatching_scaling$hatching_precip_raw_mean
hatching_precip_sd   <- hatching_scaling$hatching_precip_raw_sd

# Plot to show the relationship between full-incubation precipitation and SGPE hatching success. Temperature and 
# wind speed are held at its mean value and year is excluded.
hatch_precip_plot <- SGPE_hatching_annual %>%
  data_grid(hatching_precip_raw = seq_range(hatching_precip_raw, n = 101)) %>%
  mutate(hatching_precip_z = (hatching_precip_raw - hatching_precip_mean) / hatching_precip_sd,
         hatching_temp_z = 0,
         hatching_wind_z = 0,
         Year = NA) %>%
  add_epred_draws(SGPE_hatching_model, re_formula = NA) %>%
  ggplot(aes(x = hatching_precip_raw)) +
  stat_lineribbon(aes(y = .epred), .width = c(.99, .95, .8, .5), colour = "black") +
  geom_jitter(data = SGPE_hatching, aes(x = hatching_precip_raw, y = outcome),
              height = 0.03, width = 0.7, colour = "grey40", size = 0.05, inherit.aes = FALSE) +
  geom_point(data = SGPE_hatching_annual,
             aes(x = hatching_precip_raw, y = hatching_success, size = total_nests),
             colour = "black", inherit.aes = FALSE) +
  scale_size_continuous(range = c(2, 7), name = "Annual sample size",
                        limits = range(SGPE_hatching_annual$total_nests),
                        breaks = c(140, 170, 200)) +
  scale_fill_brewer(palette = "Blues") +
  scale_y_continuous(labels = percent_format(accuracy = 1), breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
  scale_x_continuous(breaks = c(6, 8, 10, 12, 14, 16, 18,20,22), expand = c(0,0)) +
  coord_cartesian(ylim = c(0, 1), xlim = c(6.5,21.5)) +
  labs(x = "Mean precipitation during full incubation (mm)",
       y = "Hatching success",
       fill = "Credible interval") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8))

print(hatch_precip_plot)

# =====================================================================
# Hatching success vs precipitation predicted probability plot NO 2023
# =====================================================================

# Making new aggregated annual values from the no-2023 data 
SGPE_hatching_annual_no_2023 <- SGPE_hatching_no_2023 %>%
  group_by(Year) %>%
  summarise(hatching_success = mean(outcome),
            total_nests = n(),
            hatching_precip_raw = first(hatching_precip_raw),
            hatching_temp_raw = first(hatching_temp_raw),
            hatching_wind_raw = first(hatching_wind_raw),
            .groups = "drop") %>%
  mutate(Year = as.numeric(as.character(Year)))

# Loading the mean and SD directly from modelling script to ensure predictors are standardised in figure in the 
# exact same way as the model predictions - plot must show raw values in mm
hatching_precip_mean_no_2023 <- hatching_scaling_no_2023$hatching_precip_raw_mean
hatching_precip_sd_no_2023   <- hatching_scaling_no_2023$hatching_precip_raw_sd

# Plot to show the relationship between full-incubation precipitation and SGPE hatching success excluding 2023. 
# Temperature and wind speed are held at its mean value and year is excluded.
hatch_precip_plot_no_2023 <- SGPE_hatching_annual_no_2023 %>%
  data_grid(hatching_precip_raw = seq_range(hatching_precip_raw, n = 101)) %>%
  mutate(hatching_precip_z = (hatching_precip_raw - hatching_precip_mean_no_2023)/ hatching_precip_sd_no_2023,
         hatching_temp_z = 0,
         hatching_wind_z = 0,
         Year = NA) %>%
  add_epred_draws(SGPE_hatching_model_no2023, re_formula = NA) %>%
  ggplot(aes(x = hatching_precip_raw)) +
  stat_lineribbon(aes(y = .epred), .width = c(.99, .95, .8, .5), colour = "black") +
  geom_jitter(data = SGPE_hatching_no_2023, aes(x = hatching_precip_raw, y = outcome),
              height = 0.03, width = 0.5, colour = "gray40", size = 0.05, inherit.aes = FALSE) +
  geom_point(data = SGPE_hatching_annual_no_2023,
             aes(x = hatching_precip_raw, y = hatching_success, size = total_nests),
             colour = "black", inherit.aes = FALSE) +
  scale_size_continuous(range = c(2, 7), name = "Annual sample size",
                        limits = range(SGPE_hatching_annual$total_nests),
                        breaks = c(140, 170, 200)) +
  scale_fill_brewer(palette = "Blues") +
  scale_y_continuous(labels = percent_format(accuracy = 1), breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
  scale_x_continuous(breaks = c(7, 8, 9, 10, 11, 12, 13, 14), expand = c(0, 0)) +
  coord_cartesian(ylim = c(0, 1), xlim = c(6.5, 14.5)) +
  labs(x = "Mean precipitation during full incubation -2023 (mm)",
       y = "Hatching success",
       fill = "Credible interval") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8))

print(hatch_precip_plot_no_2023)

# ===============================
# Precipitation comparison panel
# ===============================

right_no_y <- hatch_precip_plot_no_2023 +
  labs(y = NULL) +
  theme(axis.text.y  = element_blank(),
        axis.ticks.y = element_blank())

hatch_precip_combined <- (hatch_precip_plot | right_no_y) +
  plot_annotation(tag_levels = "a") +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

print(hatch_precip_combined)

hatch_precip_combo <- (hatch_precip_plot/ hatch_precip_plot_no_2023) + 
  plot_annotation(tag_levels = "a") + 
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

print(hatch_precip_combo)

ggsave(filename = "figures/SGPE_hatch_combo_panel.png", 
       plot = hatch_precip_combo, width = 10, height = 9.5, dpi = 300)


# ===========================================================
# Hatching success vs temperature predicted probability plot 
# ===========================================================

# Loading the mean and SD directly from modelling script to ensure predictors are standardised in figure in the 
# exact same way as the model predictions - plot must show raw values in °C
hatching_temp_mean <- hatching_scaling$hatching_temp_raw_mean
hatching_temp_sd   <- hatching_scaling$hatching_temp_raw_sd

# Plot to show the relationship between full-incubation temperature and SGPE hatching success. Precipitation and 
# wind speed are held at its mean value and year is excluded.
hatch_temp_plot <- SGPE_hatching_annual %>%
  data_grid(hatching_temp_raw = seq_range(hatching_temp_raw, n = 101)) %>%
  mutate(hatching_temp_z = (hatching_temp_raw - hatching_temp_mean) / hatching_temp_sd,
         hatching_precip_z = 0,
         hatching_wind_z = 0,
         Year = NA) %>%
  add_epred_draws(SGPE_hatching_model, re_formula = NA) %>%
  ggplot(aes(x = hatching_temp_raw)) +
  stat_lineribbon(aes(y = .epred), .width = c(.99, .95, .8, .5), colour = "black") +
  geom_jitter(data = SGPE_hatching, aes(x = hatching_temp_raw, y = outcome),
              height = 0.03, width = 0.25, colour = "gray40", size = 0.05, inherit.aes = FALSE) +
  geom_point(data = SGPE_hatching_annual,
             aes(x = hatching_temp_raw, y = hatching_success, size = total_nests),
             colour = "black", inherit.aes = FALSE) +
  scale_size_continuous(range = c(2, 7), name = "Annual sample size",
                        limits = range(SGPE_hatching_annual$total_nests),
                        breaks = c(140, 170, 200)) +
  scale_fill_brewer(palette = "Blues") +
  scale_y_continuous(labels = percent_format(accuracy = 1), breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
  scale_x_continuous(expand = c(0, 0), breaks = c(10,11,12,13,14)) +
  coord_cartesian(ylim = c(0, 1), xlim = c(10.4, 13.7)) +
  labs(x = "Mean temperature during full incubation (°C)",
       y = "Hatching success",
       fill = "Credible interval") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8))

print(hatch_temp_plot)

# ==========================================================
# Hatching success vs wind speed predicted probability plot 
# ==========================================================

# Loading the mean and SD directly from modelling script to ensure predictors are standardised in figure in the 
# exact same way as the model predictions - plot must show raw values in m/s
hatching_wind_mean <- hatching_scaling$hatching_wind_raw_mean
hatching_wind_sd   <- hatching_scaling$hatching_wind_raw_sd

# Plot to show the relationship between late-incubation wind speed and SGPE hatching success. Precipitation and 
# temperature are held at its mean value and year is excluded.
hatch_wind_plot <- SGPE_hatching_annual %>%
  data_grid(hatching_wind_raw = seq_range(hatching_wind_raw, n = 101)) %>%
  mutate(hatching_wind_z = (hatching_wind_raw - hatching_wind_mean) / hatching_wind_sd,
         hatching_precip_z = 0,
         hatching_temp_z = 0,
         Year = NA) %>%
  add_epred_draws(SGPE_hatching_model, re_formula = NA) %>%
  ggplot(aes(x = hatching_wind_raw)) +
  stat_lineribbon(aes(y = .epred), .width = c(.99, .95, .8, .5), colour = "black") +
  geom_jitter(data = SGPE_hatching, aes(x = hatching_wind_raw, y = outcome),
              height = 0.03, width = 0.05, colour = "gray40", size = 0.05, inherit.aes = FALSE) +
  geom_point(data = SGPE_hatching_annual,
             aes(x = hatching_wind_raw, y = hatching_success, size = total_nests),
             colour = "black", inherit.aes = FALSE) +
  scale_size_continuous(range = c(2, 7), name = "Annual sample size",
                        limits = range(SGPE_hatching_annual$total_nests),
                        breaks = c(140, 170, 200)) +
  scale_fill_brewer(palette = "Blues") +
  scale_y_continuous(labels = percent_format(accuracy = 1), breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
  scale_x_continuous(expand = c(0, 0), breaks = c(5.25, 5.5, 5.75, 6.0, 6.25, 6.5, 6.75)) +
  coord_cartesian(ylim = c(0, 1), xlim = c(5.1,6.95)) +
  labs(x = "Mean wind speed during late incubation (m/s)",
       y = "Hatching success",
       fill = "Credible interval") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8))

print(hatch_wind_plot)

# ==================================
# Making a panel for all four plots
# ==================================

hatch_final <- (hatch_precip_plot | hatch_precip_plot_no_2023) / (hatch_temp_plot | hatch_wind_plot) +
  plot_annotation(tag_levels = "a", 
                  theme = theme(plot.tag = element_text(face = "bold", size = 14))) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

print(hatch_final)

# =============================================
# Panel for hatching temp and wind speed plots
# =============================================

hatch_temp_wind_combo <- (hatch_temp_plot/ hatch_wind_plot) + 
  plot_annotation(tag_levels = "a") + 
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

print(hatch_temp_wind_combo)

ggsave(filename = "figures/SGPE_hatch_temp_wind_combo.png", 
       plot = hatch_temp_wind_combo, width = 10, height = 9.5, dpi = 300)

# =========================
# Exporting hatching plots
# =========================

ggsave(filename = "figures/SGPE_yearly_hatching_success.png", 
       plot = SGPE_hatch_year, width = 10, height = 5, dpi = 300)

ggsave(filename = "figures/SGPE_hatching_precipitation_plot.png", 
       plot = hatch_precip_plot, width = 10, height = 5, dpi = 300)

ggsave(filename = "figures/SGPE_hatching_precipitation_no_2023_plot.png", 
       plot = hatch_precip_plot_no_2023, width = 10, height = 5, dpi = 300)

ggsave(filename = "figures/SGPE_hatching_precipitation_comparison.png", 
       plot = hatch_precip_combined, width = 10, height = 5, dpi = 300)

ggsave(filename = "figures/SGPE_hatching_temperature_plot.png", 
       plot = hatch_temp_plot, width = 10, height = 5, dpi = 300)

ggsave(filename = "figures/SGPE_hatching_wind_speed_plot.png", 
       plot = hatch_wind_plot, width = 10, height = 5, dpi = 300)

ggsave(filename = "figures/SGPE_hatching_final_panel.png", 
       plot = hatch_final, width = 14, height = 8, dpi = 300)


# ==================================================================================================================
#                                                  Fledging success
# ==================================================================================================================

# Aggregating observations into years for plotting 
SGPE_fledging_annual <- SGPE_fledging %>%
  group_by(Year) %>%
  summarise(fledging_success = mean(outcome),
            fledged_nests = sum(outcome == 1),
            total_hatched = n(),
            fledging_temp_raw = first(fledging_temp_raw),
            fledging_wind_raw = first(fledging_wind_raw),
            .groups = "drop") %>%
  mutate(Year = as.numeric(as.character(Year)))

# ==============================
# Fledging success by year plot 
# ==============================

# Annual fledging success of Southern Giant Petrels - proportion of hatched chicks that fledged successfully
SGPE_fledg_year <- ggplot(SGPE_fledging_annual, aes(x = Year, y = fledging_success)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  geom_text(aes(label = total_hatched, vjust = ifelse(Year %in% c(2013, 2017, 2018, 2023), -1, 2)), size = 3)+ 
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  scale_x_continuous(breaks = seq(min(SGPE_fledging_annual$Year), max(SGPE_fledging_annual$Year),by = 1)) +
  theme_minimal() +
  labs(x = "Year",
       y = "Fledging success")  +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ===========================================================
# Fledging success vs temperature predicted probability plot 
# ===========================================================

# Loading the mean and SD directly from modelling script to ensure predictors are standardised in figure in the 
# exact same way as the model predictions - plot must show raw values in °C
fledging_temp_mean <- fledging_scaling$fledging_temp_raw_mean
fledging_temp_sd   <- fledging_scaling$fledging_temp_raw_sd

# Plot to show the relationship between early chick-rearing temperature and SGPE fledging success. Wind speed is 
# held at its mean value and year is excluded.
fledg_temp_plot <- SGPE_fledging_annual %>%
  data_grid(fledging_temp_raw = seq_range(fledging_temp_raw, n = 101)) %>%
  mutate(fledging_temp_z = (fledging_temp_raw - fledging_temp_mean) / fledging_temp_sd,
         fledging_wind_z = 0,
         Year = NA) %>%
  add_epred_draws(SGPE_fledging_model, re_formula = NA) %>%
  ggplot(aes(x = fledging_temp_raw)) + 
  stat_lineribbon(aes(y = .epred), .width = c(.99, .95, .8, .5), colour = "black") +
  geom_jitter(data = SGPE_fledging, aes(x = fledging_temp_raw, y = outcome),
              height = 0.03, width = 0.06, colour = "grey40", size = 0.3, inherit.aes = FALSE) +
  geom_point(data = SGPE_fledging_annual,
             aes(x = fledging_temp_raw, y = fledging_success, size = total_hatched),
             colour = "black", inherit.aes = FALSE) +
  scale_size_continuous(range = c(2, 7), name = "Annual sample size",
                        limits = range(SGPE_fledging_annual$total_hatched),
                        breaks = c(45, 90, 140)) +
  scale_fill_brewer(palette = "Blues") +
  scale_y_continuous(labels = percent_format(accuracy = 1), breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
  scale_x_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = "Mean temperature during early chick-rearing (°C)",
       y = "Fledging success",
       fill = "Credible interval") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8))

print(fledg_temp_plot)

# ==========================================================
# Fledging success vs wind speed predicted probability plot 
# ==========================================================

# Loading the mean and SD directly from modelling script to ensure predictors are standardised in figure in the 
# exact same way as the model predictions - plot must show raw values in m/s
fledging_wind_mean <- fledging_scaling$fledging_wind_raw_mean
fledging_wind_sd   <- fledging_scaling$fledging_wind_raw_sd

# Plot to show the relationship between late chick-rearing wind speed and SGPE fledging success. Temperature is 
# held at its mean value and year is excluded.
fledg_wind_plot <- SGPE_fledging_annual %>%
  data_grid(fledging_wind_raw = seq_range(fledging_wind_raw, n = 101)) %>%
  mutate(fledging_wind_z = (fledging_wind_raw - fledging_wind_mean) / fledging_wind_sd,
         fledging_temp_z = 0,
         Year = NA) %>%
  add_epred_draws(SGPE_fledging_model, re_formula = NA) %>%
  ggplot(aes(x = fledging_wind_raw)) +
  stat_lineribbon(aes(y = .epred), .width = c(.99, .95, .8, .5), colour = "black") +
  geom_jitter(data = SGPE_fledging, aes(x = fledging_wind_raw, y = outcome),
              height = 0.03, width = 0.05, colour = "gray40", size = 0.3, inherit.aes = FALSE) +
  geom_point(data = SGPE_fledging_annual,
             aes(x = fledging_wind_raw, y = fledging_success, size = total_hatched),
             colour = "black", inherit.aes = FALSE) +
  scale_size_continuous(range = c(2, 7), name = "Annual sample size",
                        limits = range(SGPE_fledging_annual$total_hatched),
                        breaks = c(45, 90, 140)) +
  scale_fill_brewer(palette = "Blues") +
  scale_y_continuous(labels = percent_format(accuracy = 1), breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
  scale_x_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = "Mean wind speed during late chick-rearing (m/s)",
       y = "Fledging success",
       fill = "Credible interval") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8))

print(fledg_wind_plot)


# ===========================================
# Making a panel for both plots side-by-side
# ===========================================

fledg_combined <- (fledg_temp_plot / fledg_wind_plot) +
  plot_annotation(tag_levels = "a") +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

print(fledg_combined)

# ================
# Exporting plots
# ================

ggsave(filename = "figures/SGPE_yearly_fledging_success.png", 
       plot = SGPE_fledg_year, width = 10, height = 5, dpi = 300)

ggsave(filename = "figures/SGPE_fledging_temperature_plot.png", 
       plot = fledg_temp_plot, width = 10, height = 5, dpi = 300)

ggsave(filename = "figures/SGPE_fledging_wind_plot.png", 
       plot = fledg_wind_plot, width = 10, height = 5, dpi = 300)

ggsave(filename = "figures/SGPE_fledging_combined_panel2.png", 
       plot = fledg_combined, width = 10, height = 11, dpi = 300)


# ==================================================================================================================
#                                                  Breeding success
# ==================================================================================================================

# Aggregating observations into years for plotting 
SGPE_breeding_annual <- SGPE_breeding %>%
  group_by(Year) %>%
  summarise(breeding_success = mean(outcome),
            fledged_nests = sum(outcome == 1),
            total_nests = n(),
            .groups = "drop") %>%
  mutate(Year = as.numeric(as.character(Year)))

# ==============================
# Breeding success by year plot 
# ==============================

# Annual breeding success of Southern Giant Petrels - proportion of nests that fledged successfully
SGPE_breed_year <- ggplot(SGPE_breeding_annual, aes(x = Year, y = breeding_success)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  geom_text(aes(label = total_nests, vjust = ifelse(Year %in% c(2011,2013,2015,2017,2018,2020,2023), -1, 2)), size = 3)+ 
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  scale_x_continuous(breaks = seq(min(SGPE_breeding_annual$Year), max(SGPE_breeding_annual$Year),by = 1)) +
  labs(x = "Year",
       y = "Breeding success")  +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(SGPE_breed_year)


# =================================================================================================================
#                                                 Other/General plots
# =================================================================================================================

# ====================================================
# Number of total, hatched and fledged nests per year 
# ====================================================

SGPE_nest_counts <- SGPE_final %>%
  group_by(Year) %>%
  summarise(total_nests = n_distinct(Nest_label),
            hatched_nests = sum(hatched == "Yes", na.rm = TRUE),
            fledged_nests = sum(fledged == "Yes", na.rm = TRUE),
            .groups = "drop") %>%
  mutate(Year = as.numeric(as.character(Year))) %>%
  pivot_longer(cols = c(total_nests, hatched_nests, fledged_nests),
               names_to = "Outcome",
               values_to = "Nests") %>%
  mutate(Outcome = recode(Outcome,
                          total_nests = "Total nests",
                          hatched_nests = "Hatched",
                          fledged_nests = "Fledged"))

# Annual numbers of total, hatched and fledged Southern Giant Petrel nests
SGPE_counts_plot <- ggplot(SGPE_nest_counts, aes(x = Year, y = Nests, group = Outcome, linetype = Outcome)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq(min(SGPE_nest_counts$Year), max(SGPE_nest_counts$Year), by = 1)) +
  scale_y_continuous(limits = c(0, 220), breaks = seq(0, 220, by = 20)) +
  labs(x = "Year",
       y = "Annual sample size",
       linetype = "Outcome") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "right")

print(SGPE_counts_plot)


# ================================
# AIC Candidate Windows Grid Plot
# ================================

# Function to convert dates to days - July 1st = day 1 of the breeding season
season_day <- function(date) {
  date <- as.Date(date)
  start_of_season <- if_else(month(date) >= 7, 
                             make_date(year(date), 7, 1),
                             make_date(year(date) - 1, 7, 1))
  as.numeric(date - start_of_season) + 1}

# Formatting a table for plotting the AIC candidate windows 
AIC_window_plot_data <- SGPE_window_climate %>%
  filter(window != "baseline") %>%
  mutate(start_day = season_day(start),
         end_day = season_day(end)) %>%
  group_by(response, window) %>%
  summarise(plot_start = min(start_day, na.rm = TRUE),
            plot_end = max(end_day, na.rm = TRUE),
            mean_start = mean(start_day, na.rm = TRUE),
            mean_end = mean(end_day, na.rm = TRUE),
            .groups = "drop") %>%
  left_join(SGPE_AIC_nestlevel %>%
              filter(window != "baseline") %>%
              select(response, climate_driver, window, AIC, delta_AIC, AIC_minus_baseline),
            by = c("response", "window")) %>%
  group_by(response, climate_driver) %>%
  mutate(best_window = AIC == min(AIC, na.rm = TRUE),
         baseline_selected = all(AIC_minus_baseline > 0),
         retained_window = best_window & AIC_minus_baseline < 0) %>%
  ungroup() %>%
  mutate(response = recode(response,
                           hatching = "Hatching success",
                           fledging = "Fledging success",
                           breeding = "Breeding success"),
         response = factor(response,levels = c("Hatching success", "Fledging success", "Breeding success")),
         climate_driver = recode(climate_driver,
                                 mean_precip = "Precipitation",
                                 mean_temp = "Temperature",
                                 mean_wind_speed = "Wind speed"),
         climate_driver = factor(climate_driver,levels = c("Precipitation", "Temperature", "Wind speed")),
         window_label = str_replace_all(window, "_", " "),
         window_label = str_to_sentence(window_label),
         window_label = factor(window_label, 
                               levels = c("Full incubation", "Late incubation", "Early incubation",
                                          "Full chick rearing", "Late chick rearing", "Early chick rearing",
                                          "Full breeding", "Chick rearing", "Incubation")))

# The AIC candidate windows plot with full date range and mean date range per breeding stage. Pale grey bars show 
# the full range of candidate-window timings across all years. Black/blue bars show mean window timing. Blue 
# indicates the AIC-retained mean window.
SGPE_AIC_window_plot <- ggplot(AIC_window_plot_data, aes(y = window_label, yend = window_label)) +
  # Full observed range across years
  geom_segment(aes(x = plot_start,
                   xend = plot_end,
                   linetype = "Full window range"),
               linewidth = 4,
               colour = "gray75") +
  # Mean window range
  geom_segment(aes(x = mean_start,
                   xend = mean_end,
                   colour = retained_window,
                   linetype = "Mean window range"),
               linewidth = 4) +
  scale_colour_manual(values = c("FALSE" = "grey20", "TRUE" = "dodgerblue3"),
                      labels = c("Mean window not retained", "Mean window retained"),
                      name = "AIC selection") +
  scale_linetype_manual(values = c("Full window range" = "solid",
                                   "Mean window range" = "solid"),
                        name = "Window timing") +
  scale_x_continuous(breaks = c(32, 63, 93, 124, 154, 185, 216),
                     labels = c("Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb")) +
  coord_cartesian(xlim = c(40, 225)) +
  facet_grid(response ~ climate_driver,
             scales = "free_y",
             space = "free_y") +
  labs(x = "Breeding season timeline",
       y = "Candidate window") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        strip.text = element_text(face = "bold"))

SGPE_AIC_window_plot

# ================
# Exporting plots
# ================


ggsave(filename = "figures/SGPE_yearly_breeding_success.png", 
       plot = SGPE_breed_year, width = 10, height = 5, dpi = 300)

ggsave(filename = "figures/SGPE_nest_counts_yearly.png", 
       plot = SGPE_counts_plot, width = 10, height = 5, dpi = 300)

ggsave(filename = "figures/SGPE_AIC_windows.png", 
       plot = SGPE_AIC_window_plot, width = 10, height = 5, dpi = 300)




