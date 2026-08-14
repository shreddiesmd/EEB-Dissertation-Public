rm(list = ls())

library(dplyr); library(readr); library(ggplot2); library(brms); library(scales); library(tidyr); library(modelr);
library(tidybayes); library(ggdist); library(lubridate); library(forcats); library(stringr); library(patchwork)

# ============================
# Loading datasets and models
# ============================

SGPE_window_climate <- read_csv("SGPE_window_climate.csv")
SGPE_AIC_nestlevel <- read_csv("SGPE_AIC_nestlevel.csv")

AYNA_AIC_nestlevel <- read_csv("AYNA_AIC_nestlevel.csv")
AYNA_window_climate <- read_csv("AYNA_window_climate.csv")

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
SGPE_AIC_window_plot_data <- SGPE_window_climate %>%
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
SGPE_AIC_window_plot <- ggplot(SGPE_AIC_window_plot_data, aes(y = window_label, yend = window_label)) +
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
AYNA_AIC_window_plot_data <- AYNA_window_climate %>%
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
AYNA_AIC_window_plot <- ggplot(AYNA_AIC_window_plot_data, aes(y = window_label, yend = window_label)) +
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


# PANEL SIDE-BY-SIDE
AIC_panel <- (SGPE_AIC_window_plot | AYNA_AIC_window_plot) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom",
        axis.text   = element_text(size = 20),
        axis.title  = element_text(size = 23),
        strip.text  = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 19),
        legend.title = element_text(size = 20))

print(AIC_panel)

ggsave(filename = "figures/AIC_panel.png",
       plot = AIC_panel, width = 12, height = 5, dpi = 300)

