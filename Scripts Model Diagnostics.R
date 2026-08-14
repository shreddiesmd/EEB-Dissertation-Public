# ======================================================================================================================
#                                           ALL MODEL DIAGNOSTICS SGPE & AYNA
# ======================================================================================================================

rm(list = ls())

library(dplyr); library(readr); library(ggplot2); library(brms); library(rstan); library(bayesplot); 
library(ggplot2); library(patchwork)

# ============================
# Loading datasets and models
# ============================

SGPE_hatching_model <- readRDS("SGPE_hatching_model.rds")
SGPE_fledging_model <- readRDS("SGPE_fledging_model.rds")
SGPE_breeding_model <- readRDS("SGPE_breeding_model.rds")
SGPE_hatching_model_no_2023 <- readRDS("SGPE_hatching_model_no_2023.rds")

AYNA_hatching_model <- readRDS("AYNA_hatching_model.rds")
AYNA_fledging_model <- readRDS("AYNA_fledging_model.rds")
AYNA_breeding_model <- readRDS("AYNA_breeding_model.rds")

# ============================================================================
# CONVERGENCE DIAGNOSTICS -> Rhat, Bulk_ESS, Tail_ESS, divergences, treedepth
# ============================================================================a

# SGPE
summary(SGPE_hatching_model)                        
check_hmc_diagnostics(SGPE_hatching_model$fit)

summary(SGPE_hatching_model_no_2023)                        
check_hmc_diagnostics(SGPE_hatching_model_no_2023$fit)

summary(SGPE_fledging_model)                        
check_hmc_diagnostics(SGPE_fledging_model$fit)

summary(SGPE_breeding_model)                        
check_hmc_diagnostics(SGPE_breeding_model$fit)


# AYNA
summary(AYNA_hatching_model)                        
check_hmc_diagnostics(AYNA_hatching_model$fit)

summary(AYNA_fledging_model)                        
check_hmc_diagnostics(AYNA_fledging_model$fit)

summary(AYNA_breeding_model)                        
check_hmc_diagnostics(AYNA_breeding_model$fit)


# ==============================
# MCMC VISUAL ASSESSMENT CHECKS -> SGPE
# ==============================

bayesplot_theme_set(theme_minimal())
color_scheme_set("blue")

# ===============
# Hatching model 
# ===============

# Initial plot
plot(SGPE_hatching_model)

# Add some spice and make it pretty 
SGPE_hatch_pars <- c("b_Intercept", "b_hatching_precip_z", "b_hatching_temp_z", "b_hatching_wind_z", "sd_Year__Intercept")

SGPE_hatch_labels <- as_labeller(c(b_Intercept = "Intercept",
                                   b_hatching_precip_z = "Precipitation (z)",
                                   b_hatching_temp_z = "Temperature (z)",
                                   b_hatching_wind_z = "Wind Speed (z)",
                                   sd_Year__Intercept = "Year SD"))

# Density plots
SGPE_hatch_dens <- mcmc_dens_overlay(SGPE_hatching_model, pars = SGPE_hatch_pars,
                                     facet_args = list(labeller = SGPE_hatch_labels, nrow = 1)) +
  labs(x = "Parameter value", y = "Density")

# Trace plots
SGPE_hatch_trace <- mcmc_trace(SGPE_hatching_model, pars = SGPE_hatch_pars,
                               facet_args = list(labeller = SGPE_hatch_labels, nrow = 1)) +
  labs(x = "Iteration", y = "Parameter value", colour = "Chain")

# Plots panel
SGPE_hatch <- SGPE_hatch_dens / SGPE_hatch_trace

print(SGPE_hatch)

ggsave("Model diagnostics/SGPE_hatching_mcmc.png", SGPE_hatch, width = 14, height = 6, dpi = 300)

# =========================
# Hatching model - no 2023
# =========================

# Initial plot
plot(SGPE_hatching_model_no_2023)

# Add some spice and make it pretty 
SGPE_hatch_pars_no_2023 <- c("b_Intercept", "b_hatching_precip_z", "b_hatching_temp_z", "b_hatching_wind_z", "sd_Year__Intercept")

SGPE_hatch_labels_no_2023 <- as_labeller(c(b_Intercept = "Intercept",
                                           b_hatching_precip_z = "Precipitation (z)",
                                           b_hatching_temp_z = "Temperature (z)",
                                           b_hatching_wind_z = "Wind Speed (z)",
                                           sd_Year__Intercept = "Year SD"))

# Density plots
SGPE_hatch_dens_no_2023 <- mcmc_dens_overlay(SGPE_hatching_model_no_2023, pars = SGPE_hatch_pars_no_2023,
                                             facet_args = list(labeller = SGPE_hatch_labels_no_2023, nrow = 1)) +
  labs(x = "Parameter value", y = "Density")

# Trace plots
SGPE_hatch_trace_no_2023 <- mcmc_trace(SGPE_hatching_model_no_2023, pars = SGPE_hatch_pars_no_2023,
                                       facet_args = list(labeller = SGPE_hatch_labels_no_2023, nrow = 1)) +
  labs(x = "Iteration", y = "Parameter value", colour = "Chain")

# Plots panel
SGPE_hatch_no_2023 <- SGPE_hatch_dens_no_2023 / SGPE_hatch_trace_no_2023

print(SGPE_hatch_no_2023)

ggsave("Model diagnostics/SGPE_hatching_no_2023_mcmc.png", SGPE_hatch_no_2023, width = 14, height = 6, dpi = 300)

# ===============
# Fledging model 
# ===============

# Initial plot
plot(SGPE_fledging_model)

# Add some spice and make it pretty 
SGPE_fledg_pars <- c("b_Intercept", "b_fledging_temp_z", "b_fledging_wind_z", "sd_Year__Intercept")

SGPE_fledg_labels <- as_labeller(c(b_Intercept = "Intercept",
                                   b_fledging_temp_z = "Temperature (z)",
                                   b_fledging_wind_z = "Wind Speed (z)",
                                   sd_Year__Intercept = "Year SD"))

# Density plots
SGPE_fledg_dens <- mcmc_dens_overlay(SGPE_fledging_model, pars = SGPE_fledg_pars,
                                     facet_args = list(labeller = SGPE_fledg_labels, nrow = 1)) +
  labs(x = "Parameter value", y = "Density")

# Trace plots
SGPE_fledg_trace <- mcmc_trace(SGPE_fledging_model, pars = SGPE_fledg_pars,
                               facet_args = list(labeller = SGPE_fledg_labels, nrow = 1)) +
  labs(x = "Iteration", y = "Parameter value", colour = "Chain")

# Plots panel
SGPE_fledg <- SGPE_fledg_dens / SGPE_fledg_trace

print(SGPE_fledg)

ggsave("Model diagnostics/SGPE_fledging_mcmc.png", SGPE_fledg, width = 14, height = 6, dpi = 300)

# ===============
# Breeding model 
# ===============

# Initial plot
plot(SGPE_breeding_model)

# Add some spice and make it pretty 
SGPE_breed_pars <- c("b_Intercept", "sd_Year__Intercept")

SGPE_breed_labels <- as_labeller(c(b_Intercept = "Intercept",
                                   sd_Year__Intercept = "Year SD"))

# Density plots
SGPE_breed_dens <- mcmc_dens_overlay(SGPE_breeding_model, pars = SGPE_breed_pars,
                                     facet_args = list(labeller = SGPE_breed_labels)) +
  labs(x = "Parameter value", y = "Density")

# Trace plots
SGPE_breed_trace <- mcmc_trace(SGPE_breeding_model, pars = SGPE_breed_pars,
                               facet_args = list(labeller = SGPE_breed_labels)) +
  labs(x = "Iteration", y = "Parameter value", colour = "Chain")

# Plots panel
SGPE_breed <- SGPE_breed_dens / SGPE_breed_trace

print(SGPE_breed)

ggsave("Model diagnostics/SGPE_breeding_mcmc.png", SGPE_breed, width = 14, height = 8, dpi = 300)


# ==============================
# MCMC VISUAL ASSESSMENT CHECKS -> AYNA
# ==============================

# ===============
# Hatching model 
# ===============

# Initial plot
plot(AYNA_hatching_model)

# Add some spice and make it pretty 
AYNA_hatch_pars <- c("b_Intercept", "b_hatching_temp_z", "sd_Year__Intercept")

AYNA_hatch_labels <- as_labeller(c(b_Intercept  = "Intercept",
                                   b_hatching_temp_z = "Temperature (z)",
                                  sd_Year__Intercept = "Year SD"))

# Density plots
AYNA_hatch_dens <- mcmc_dens_overlay(AYNA_hatching_model, pars = AYNA_hatch_pars,
                          facet_args = list(labeller = AYNA_hatch_labels)) +
  labs(x = "Parameter value", y = "Density")

# Trace plots
AYNA_hatch_trace <- mcmc_trace(AYNA_hatching_model, pars = AYNA_hatch_pars,
                    facet_args = list(labeller = AYNA_hatch_labels)) +
  labs(x = "Iteration", y = "Parameter value", colour = "Chain")

# Plots panel
AYNA_hatch <- AYNA_hatch_dens / AYNA_hatch_trace

print(AYNA_hatch)

ggsave("Model diagnostics/AYNA_hatching_mcmc.png", AYNA_hatch, width = 14, height = 6, dpi = 300)

# ===============
# Fledging model 
# ===============

# Initial plot
plot(AYNA_fledging_model)

# Add some spice and make it pretty 
AYNA_fledg_pars <- c("b_Intercept", "b_fledging_temp_z", "sd_Year__Intercept")

AYNA_fledg_labels <- as_labeller(c(b_Intercept = "Intercept",
                                   b_fledging_temp_z = "Temperature (z)",
                                   sd_Year__Intercept = "Year SD"))

# Density plots
AYNA_fledg_dens <- mcmc_dens_overlay(AYNA_fledging_model, pars = AYNA_fledg_pars,
                                facet_args = list(labeller = AYNA_fledg_labels)) +
  labs(x = "Parameter value", y = "Density")

# Trace plots
AYNA_fledg_trace <- mcmc_trace(AYNA_fledging_model, pars = AYNA_fledg_pars,
                          facet_args = list(labeller = AYNA_fledg_labels)) +
  labs(x = "Iteration", y = "Parameter value", colour = "Chain")

# Plots panel
AYNA_fledg <- AYNA_fledg_dens / AYNA_fledg_trace

print(AYNA_fledg)

ggsave("Model diagnostics/AYNA_fledging_mcmc.png", AYNA_fledg, width = 14, height = 6, dpi = 300)

# ===============
# Breeding model 
# ===============

# Initial plot
plot(AYNA_breeding_model)

# Add some spice and make it pretty 
AYNA_breed_pars <- c("b_Intercept", "b_breeding_temp_z", "sd_Year__Intercept")

AYNA_breed_labels <- as_labeller(c(b_Intercept = "Intercept",
                                   b_breeding_temp_z = "Temperature (z)",
                                  sd_Year__Intercept = "Year SD"))

# Density plots
AYNA_breed_dens <- mcmc_dens_overlay(AYNA_breeding_model, pars = AYNA_breed_pars,
                                facet_args = list(labeller = AYNA_breed_labels)) +
  labs(x = "Parameter value", y = "Density")

# Trace plots
AYNA_breed_trace <- mcmc_trace(AYNA_breeding_model, pars = AYNA_breed_pars,
                          facet_args = list(labeller = AYNA_breed_labels)) +
  labs(x = "Iteration", y = "Parameter value", colour = "Chain")

# Plots panel
AYNA_breed <- AYNA_breed_dens / AYNA_breed_trace

print(AYNA_breed)

ggsave("Model diagnostics/AYNA_breeding_mcmc.png", AYNA_breed, width = 14, height = 6, dpi = 300)



