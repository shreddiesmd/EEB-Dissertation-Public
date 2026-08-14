# ====================================================================================================================
#                                           BAYESIAN MODEL PLOTTING - AYNA
# ====================================================================================================================

rm(list = ls())

library(dplyr); library(readr); library(ggplot2); library(brms); library(scales); library(tidyr); library(modelr);
library(tidybayes); library(ggdist); library(lubridate); library(forcats); library(stringr); library(patchwork)

# ============================
# Loading datasets and models
# ============================

AYNA_hatching <- read_csv("AYNA_hatching_bayesian_data.csv")
AYNA_fledging <- read_csv("AYNA_fledging_bayesian_data.csv")
AYNA_breeding <- read_csv("AYNA_breeding_bayesian_data.csv")

AYNA_AIC_nestlevel <- read_csv("AYNA_AIC_nestlevel.csv")
AYNA_window_climate <- read_csv("AYNA_window_climate.csv")
AYNA_final <- read_csv("AYNA_final.csv")

AYNA_hatching_model <- readRDS("AYNA_hatching_model.rds")
AYNA_fledging_model <- readRDS("AYNA_fledging_model.rds")
AYNA_breeding_model <- readRDS("AYNA_breeding_model.rds")

hatching_scaling <- read_csv("AYNA_hatching_scaling.csv", show_col_types = FALSE)
fledging_scaling <- read_csv("AYNA_fledging_scaling.csv", show_col_types = FALSE)
breeding_scaling <- read_csv("AYNA_breeding_scaling.csv", show_col_types = FALSE)

# ==================================================================================================================
#                                                  Hatching success
# ==================================================================================================================

# Aggregating observations into years for plotting 
AYNA_hatching_annual <- AYNA_hatching %>%
  group_by(Year) %>%
  summarise(hatching_success = mean(outcome),
            hatched_nests = sum(outcome == 1),
            total_nests = n(),
            hatching_temp_raw = first(hatching_temp_raw),
            .groups = "drop") %>%
  mutate(Year = as.numeric(as.character(Year)))

# ==============================
# Hatching success by year plot 
# ==============================

# Annual hatching success of Atlantic Yellow-nosed Albatrosses - proportion of nests that hatched successfully
AYNA_hatch_year <- ggplot(AYNA_hatching_annual, aes(x = Year, y = hatching_success)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  geom_text(aes(label = total_nests, vjust = ifelse(Year %in% c(2003, 2009, 2012, 2015, 2018, 2020), -1, 2)), size = 3) + 
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  scale_x_continuous(breaks = seq(min(AYNA_hatching_annual$Year), max(AYNA_hatching_annual$Year),by = 1)) +
  theme_minimal() +
  labs(x = "Year",
       y = "Hatching success")  +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(AYNA_hatch_year)


# ===========================================================
# Hatching success vs temperature predicted probability plot 
# ===========================================================

# Loading the mean and SD directly from modelling script to ensure predictors are standardised in figure in the 
# exact same way as the model predictions - plot must show raw values in °C
hatching_temp_mean <- hatching_scaling$hatching_temp_raw_mean
hatching_temp_sd   <- hatching_scaling$hatching_temp_raw_sd

# Plot to show the relationship between early-incubation temperature and AYNA hatching success. Year effects are excluded.
hatch_temp_plot <- AYNA_hatching_annual %>%
  data_grid(hatching_temp_raw = seq_range(hatching_temp_raw, n = 101)) %>%
  mutate(hatching_temp_z = (hatching_temp_raw - hatching_temp_mean) / hatching_temp_sd,
         Year = NA) %>%
  add_epred_draws(AYNA_hatching_model, re_formula = NA) %>%
  ggplot(aes(x = hatching_temp_raw)) +
  stat_lineribbon(aes(y = .epred), .width = c(.99, .95, .8, .5), colour = "black") +
  geom_jitter(data = AYNA_hatching, aes(x = hatching_temp_raw, y = outcome),
              height = 0.03, width = 0.06, colour = "gray40", size = 0.3, inherit.aes = FALSE) +
  geom_point(data = AYNA_hatching_annual,
             aes(x = hatching_temp_raw, y = hatching_success, size = total_nests),
             colour = "black", inherit.aes = FALSE) +
  scale_size_continuous(range = c(2, 7), name = "Annual sample size",
                        limits = range(AYNA_hatching_annual$total_nests),
                        breaks = c(30, 55, 80)) + 
  scale_fill_brewer(palette = "Oranges") +
  scale_y_continuous(labels = percent_format(accuracy = 1), breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
  scale_x_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = "Mean temperature during early incubation (°C)",
       y = "Hatching success",
       fill = "Credible interval") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8))

print(hatch_temp_plot)

# ==================================================================================================================
#                                                  Fledging success
# ==================================================================================================================

# Aggregating observations into years for plotting 
AYNA_fledging_annual <- AYNA_fledging %>%
  group_by(Year) %>%
  summarise(fledging_success = mean(outcome),
            fledged_nests = sum(outcome == 1),
            total_hatched = n(),
            fledging_temp_raw = first(fledging_temp_raw),
            .groups = "drop") %>%
  mutate(Year = as.numeric(as.character(Year)))

# ==============================
# Fledging success by year plot 
# ==============================

# Annual fledging success of Atlantic Yellow-nosed Albatrosses - proportion of hatched chicks that fledged successfully
AYNA_fledg_year <- ggplot(AYNA_fledging_annual, aes(x = Year, y = fledging_success)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  geom_text(aes(label = total_hatched, vjust = ifelse(Year %in% c(2003,2009,2011,2013,2015,2018,2019,2021), -1, 2)), size = 3)+ 
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  scale_x_continuous(breaks = seq(min(AYNA_fledging_annual$Year), max(AYNA_fledging_annual$Year),by = 1)) +
  theme_minimal() +
  labs(x = "Year",
       y = "Fledging success")  +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(AYNA_fledg_year)

# ===========================================================
# Fledging success vs temperature predicted probability plot 
# ===========================================================

# Loading the mean and SD directly from modelling script to ensure predictors are standardised in figure in the 
# exact same way as the model predictions - plot must show raw values in °C
fledging_temp_mean <- fledging_scaling$fledging_temp_raw_mean
fledging_temp_sd   <- fledging_scaling$fledging_temp_raw_sd

# Plot to show the relationship between late chick-rearing temperature and AYNA fledging success. Year effects are excluded.
fledg_temp_plot <- AYNA_fledging_annual %>%
  data_grid(fledging_temp_raw = seq_range(fledging_temp_raw, n = 101)) %>%
  mutate(fledging_temp_z = (fledging_temp_raw - fledging_temp_mean) / fledging_temp_sd,
         Year = NA) %>%
  add_epred_draws(AYNA_fledging_model, re_formula = NA) %>%
  ggplot(aes(x = fledging_temp_raw)) +
  stat_lineribbon(aes(y = .epred), .width = c(.99, .95, .8, .5), colour = "black") +
  geom_jitter(data = AYNA_fledging, aes(x = fledging_temp_raw, y = outcome),
              height = 0.03, width = 0.06, colour = "gray40", size = 0.3, inherit.aes = FALSE) +
  geom_point(data = AYNA_fledging_annual,
             aes(x = fledging_temp_raw, y = fledging_success, size = total_hatched),
             colour = "black",inherit.aes = FALSE) +
  scale_size_continuous(range = c(2, 7), name = "Annual sample size",
                        limits = range(AYNA_fledging_annual$total_hatched),
                        breaks = c(15, 40, 65)) +   
  scale_fill_brewer(palette = "Oranges") +
  scale_y_continuous(labels = percent_format(accuracy = 1), breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  scale_x_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = "Mean temperature during late chick-rearing (°C)",
       y = "Fledging success",
       fill = "Credible interval") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8))

print(fledg_temp_plot)


# PANEL
breed_fledg_panel <- (hatch_temp_plot | fledg_temp_plot)

ggsave(filename = "figures/AYNA_breed_and_fledg_panel.png", 
       plot = breed_fledg_panel, width = 10, height = 5, dpi = 300)

# ==================================================================================================================
#                                                  Breeding success
# ==================================================================================================================

# Aggregating observations into years for plotting 
AYNA_breeding_annual <- AYNA_breeding %>%
  group_by(Year) %>%
  summarise(breeding_success = mean(outcome),
            fledged_nests = sum(outcome == 1),
            total_nests = n(),
            breeding_temp_raw = first(breeding_temp_raw),
            .groups = "drop") %>%
  mutate(Year = as.numeric(as.character(Year)))

# ==============================
# Breeding success by year plot -> don't use this plot, shown by other probability plots 
# ==============================

# Annual breeding success of Atlantic Yellow-nosed Albatrosses - proportion of nests that fledged successfully
AYNA_breed_year <- ggplot(AYNA_breeding_annual, aes(x = Year, y = breeding_success)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  geom_text(aes(label = total_nests, vjust = ifelse(Year %in% c(2003,2009,2012,2015,2018,2021), -1, 2)), size = 3)+ 
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  scale_x_continuous(breaks = seq(min(AYNA_breeding_annual$Year), max(AYNA_breeding_annual$Year),by = 1)) +
  theme_minimal() +
  labs(x = "Year",
       y = "Breeding success")  +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(AYNA_breed_year)

# ===========================================================
# Breeding success vs temperature predicted probability plot 
# ===========================================================

# Loading the mean and SD directly from modelling script to ensure predictors are standardised in figure in the 
# exact same way as the model predictions - plot must show raw values in °C
breeding_temp_mean <- breeding_scaling$breeding_temp_raw_mean
breeding_temp_sd   <- breeding_scaling$breeding_temp_raw_sd

# Plot to show the relationship between chick-rearing temperature and AYNA breeding success. Year is excluded.
breed_temp_plot <- AYNA_breeding_annual %>%
  data_grid(breeding_temp_raw = seq_range(breeding_temp_raw, n = 101)) %>%
  mutate(breeding_temp_z = (breeding_temp_raw - breeding_temp_mean) / breeding_temp_sd,
         Year = NA) %>%
  add_epred_draws(AYNA_breeding_model, re_formula = NA) %>%
  ggplot(aes(x = breeding_temp_raw)) +
  stat_lineribbon(aes(y = .epred), .width = c(.99, .95, .8, .5), colour = "black") +
  geom_jitter(data = AYNA_breeding, aes(x = breeding_temp_raw, y = outcome),
              height = 0.03, width = 0.06, colour = "gray40", size = 0.3, inherit.aes = FALSE) +
  geom_point(data = AYNA_breeding_annual,
             aes(x = breeding_temp_raw, y = breeding_success, size = total_nests),
             colour = "black", inherit.aes = FALSE) +
  scale_size_continuous(range = c(2, 7), name = "Annual sample size",
                        limits = range(AYNA_breeding_annual$total_nests),
                        breaks = c(30, 55, 80)) +   
  scale_fill_brewer(palette = "Oranges") +
  scale_y_continuous(labels = percent_format(accuracy = 1), breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  scale_x_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = "Mean temperature during chick-rearing (°C)",
       y = "Breeding success",
       fill = "Credible interval") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8))

print(breed_temp_plot)


# =================================================================================================================
#                                                 Other/General plots
# =================================================================================================================

# ====================================================
# Number of total, hatched and fledged nests per year 
# ====================================================

AYNA_nest_counts <- AYNA_final %>%
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

# Annual numbers of total, hatched and fledged Atlantic Yellow-nosed Albatross nests
AYNA_counts_plot <- ggplot(AYNA_nest_counts, aes(x = Year, y = Nests, group = Outcome, linetype = Outcome)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq(min(AYNA_nest_counts$Year), max(AYNA_nest_counts$Year), by = 1)) +
  scale_y_continuous(limits = c(0, 120), breaks = seq(0, 120, by = 20)) +
  labs(x = "Year",
       y = "Number of nests",
       linetype = "Outcome") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "right")

print(AYNA_counts_plot)

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
AIC_window_plot_data <- AYNA_window_climate %>%
  filter(window != "baseline") %>%
  mutate(start_day = season_day(start),
         end_day = season_day(end)) %>%
  group_by(response, window) %>%
  summarise(plot_start = min(start_day, na.rm = TRUE),
            plot_end = max(end_day, na.rm = TRUE),
            mean_start = mean(start_day, na.rm = TRUE),
            mean_end = mean(end_day, na.rm = TRUE),
            .groups = "drop") %>%
  left_join(AYNA_AIC_nestlevel %>%
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
AYNA_AIC_window_plot <- ggplot(AIC_window_plot_data, aes(y = window_label, yend = window_label)) +
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
  scale_colour_manual(values = c("FALSE" = "grey20", "TRUE" = "orangered2"),
                      labels = c("Mean window not retained", "Mean window retained"),
                      name = "AIC selection") +
  scale_linetype_manual(values = c("Full window range" = "solid",
                                   "Mean window range" = "solid"),
                        name = "Window timing") +
  scale_x_continuous(breaks = c(63, 93, 124, 154, 185, 216, 244, 275, 305),
                     labels = c("Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May")) +
  coord_cartesian(xlim = c(55, 315)) +
  facet_grid(response ~ climate_driver,
             scales = "free_y",
             space = "free_y") +
  labs(x = "Breeding season timeline",
       y = "Candidate window") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        strip.text = element_text(face = "bold"))

AYNA_AIC_window_plot


# ================
# Exporting plots 
# ================

ggsave(filename = "figures/AYNA_yearly_hatching_success.png", 
       plot = AYNA_hatch_year, width = 10, height = 5, dpi = 300)

ggsave(filename = "figures/AYNA_hatching_temperature_plot.png",
       plot = hatch_temp_plot, width = 10, height = 5, dpi = 300)

ggsave(filename = "figures/AYNA_yearly_fledging_success.png", 
       plot = AYNA_fledg_year, width = 10, height = 5, dpi = 300)

ggsave(filename = "figures/AYNA_fledging_temperature_plot.png", 
       plot = fledg_temp_plot, width = 10, height = 5, dpi = 300)

ggsave(filename = "figures/AYNA_yearly_breeding_success.png", 
       plot = AYNA_breed_year, width = 10, height = 5, dpi = 300)

ggsave(filename = "figures/AYNA_breeding_temperature_plot.png", 
       plot = breed_temp_plot, width = 10, height = 5, dpi = 300)

ggsave(filename = "figures/AYNA_nest_counts_yearly.png", 
       plot = AYNA_counts_plot, width = 10, height = 5, dpi = 300)

ggsave(filename = "figures/AYNA_AIC_windows.png", 
       plot = AYNA_AIC_window_plot, width = 10, height = 5, dpi = 300)

