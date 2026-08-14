# ===============================================================================================================
#                                            BAYESIAN MODELLING - SGPE 
# ===============================================================================================================

rm(list=ls())

# ===========================
# Adding packages to library
# ===========================

library(dplyr); library(readr); library(brms); library(lme4)

# ============================
# Uploading required datasets 
# ============================

SGPE_window_climate <- read_csv("SGPE_window_climate.csv")
SGPE_hatching_nests <- read_csv("SGPE_hatching_nests.csv")
SGPE_fledging_nests <- read_csv("SGPE_fledging_nests.csv")
SGPE_breeding_nests <- read_csv("SGPE_breeding_nests.csv")
SGPE_AIC_nestlevel <- read_csv("SGPE_AIC_nestlevel.csv")
   
# =============================================
# Extracting the climate predictors for models
# =============================================

# REMEMBER: The predictors are standardised so must be transformed back before interpretation or plotting!!

# Hatching predictors -> mean precip during full incubation, mean temp during full incubation & mean wind speed 
# during late incubation 

hatching_full_incubation <- SGPE_window_climate %>%
  filter(response == "hatching",
         window == "full_incubation") %>%
  transmute(Year,
            hatching_precip_raw = mean_precip,
            hatching_temp_raw = mean_temp)

hatching_late_incubation <- SGPE_window_climate %>%
  filter(response == "hatching",
         window == "late_incubation") %>%
  transmute(Year,
            hatching_wind_raw = mean_wind_speed)

hatching_climate_final <- hatching_full_incubation %>%
  left_join(hatching_late_incubation,
            by = "Year") %>%
  mutate(hatching_precip_z = as.numeric(scale(hatching_precip_raw)),
         hatching_temp_z = as.numeric(scale(hatching_temp_raw)),
         hatching_wind_z = as.numeric(scale(hatching_wind_raw)))

# Fledging predictors -> mean temp during early chick rearing and mean wind speed during late chick rearing
fledging_climate_final <- SGPE_window_climate %>%
  filter(response == "fledging",
         window == "early_chick_rearing") %>%
  transmute(Year,
            fledging_temp_raw = mean_temp,
            fledging_temp_z = as.numeric(scale(mean_temp))) %>%
  left_join(SGPE_window_climate %>%
      filter(response == "fledging",
             window == "late_chick_rearing") %>%
      transmute(Year,
                fledging_wind_raw = mean_wind_speed,
                fledging_wind_z = as.numeric(scale(mean_wind_speed))),
    by = "Year")

# Breeding success retained climate predictors -> none

# =============================================================
# Exporting the scaled predictors to use in my plotting script 
# =============================================================

hatching_scaling <- hatching_climate_final %>%
  summarise(across(c(hatching_precip_raw, hatching_temp_raw, hatching_wind_raw),
                   list(mean = mean, sd = sd)))

write_csv(hatching_scaling, "SGPE_hatching_scaling.csv")

fledging_scaling <- fledging_climate_final %>%
  summarise(across(c(fledging_temp_raw, fledging_wind_raw),
                   list(mean = mean, sd = sd)))

write_csv(fledging_scaling, "SGPE_fledging_scaling.csv")

# =================================================================
# Formatting the final datatsets for input into the Bayesian model
# =================================================================

# Hatching success 
SGPE_hatching <- SGPE_hatching_nests %>%
  left_join(hatching_climate_final, by = "Year") %>%
  mutate(Year = factor(Year))

# Fledging success 
SGPE_fledging <- SGPE_fledging_nests %>%
  left_join(fledging_climate_final, by = "Year") %>%
  mutate(Year = factor(Year))

# =====================================================================
# Checking the datasets are all complete and accurate before modelling
# =====================================================================

summary(SGPE_hatching)
summary(SGPE_fledging)

table(SGPE_hatching$Year)
table(SGPE_fledging$Year)

sum(is.na(SGPE_hatching$hatching_precip_z))
sum(is.na(SGPE_hatching$hatching_temp_z))
sum(is.na(SGPE_hatching$hatching_wind_z))
sum(is.na(SGPE_fledging$fledging_temp_z))
sum(is.na(SGPE_fledging$fledging_wind_z))

# ==================================================================================================================
#                                             Hatching success model 
# ==================================================================================================================

# Weakly informative priors. The predictors are standardised so these priors describe a change in log-odds of 
# success per 1 standard deviation change in climate predictor. 

priors_hatch <- c(set_prior("normal(0, 1.5)", class = "b"),
                  set_prior("normal(0, 2.5)", class = "Intercept"),
                  set_prior("exponential(1)", class = "sd"))

SGPE_hatching_model <- brms::brm(outcome ~ hatching_precip_z + hatching_temp_z + hatching_wind_z+ (1 | Year),
                                 data = SGPE_hatching,
                                 family = bernoulli(),
                                 prior = priors_hatch,
                                 chains = 3,
                                 cores = 3,
                                 iter = 3000,
                                 warmup = 1000, 
                                 seed = 123)

# =======================================
# Testing for robustness and sensibility 
# =======================================

# Model checks 
summary(SGPE_hatching_model)
plot(SGPE_hatching_model)
pp_check(SGPE_hatching_model, type = "bars")
prior_summary(SGPE_hatching_model)

# Testing the model with looser priors and more iterations 
priors_hatch_test <- c(set_prior("normal(0, 3)", class = "b"),
                  set_prior("normal(0, 10)", class = "Intercept"),
                  set_prior("exponential(1)", class = "sd"))

SGPE_hatching_model_test <- brms::brm(outcome ~ hatching_precip_z + hatching_temp_z + hatching_wind_z + (1 | Year),
                                 data = SGPE_hatching,
                                 family = bernoulli(),
                                 prior = priors_hatch_test,
                                 chains = 3,
                                 cores = 3,
                                 iter = 30000,
                                 warmup = 15000, 
                                 seed = 123)

summary(SGPE_hatching_model_test)
plot(SGPE_hatching_model_test)
pp_check(SGPE_hatching_model_test, type = "bars")
prior_summary(SGPE_hatching_model_test)

# Testing using a frequentist glmer model 
SGPE_hatching_glmer <- glmer(outcome ~ hatching_precip_z + hatching_temp_z + hatching_wind_z + (1 | Year),
                             data = SGPE_hatching,
                             family = binomial)

summary(SGPE_hatching_glmer)

# Comparing fixed effects
fixef(SGPE_hatching_glmer)
fixef(SGPE_hatching_model)

# CONCLUSIONS: 

# Running 30,000 iter and loosening priors produced very similar (within 0.02) parameter estimates compared with the
# original model - shows stability and results are not sensitive to altered priors, thus results are data driven
# Comparison to a non-Bayesian model produced very similar estimates in both direction and magnitude -> sense check
# I will stick with my original priors 

# ==================================================================================================================
#                                             Fledging success model 
# ==================================================================================================================

# Weakly informative priors. The predictors are standardised so these priors describe a change in log-odds of 
# success per 1 standard deviation change in climate predictor. 

priors_fledg <- c(set_prior("normal(0, 1.5)", class = "b"),
                  set_prior("normal(0, 2.5)", class = "Intercept"),
                  set_prior("exponential(1)", class = "sd"))

SGPE_fledging_model <- brms::brm(outcome ~ fledging_temp_z + fledging_wind_z + (1 | Year),
                                 data = SGPE_fledging,
                                 family = bernoulli(),
                                 prior = priors_fledg,
                                 chains = 3,
                                 cores = 3,
                                 iter = 3000,
                                 warmup = 1000, 
                                 seed = 123)

# =======================================
# Testing for robustness and sensibility 
# =======================================

# Model checks 
summary(SGPE_fledging_model)
plot(SGPE_fledging_model)
pp_check(SGPE_fledging_model, type = "bars")
prior_summary(SGPE_fledging_model)

# Testing the model with looser priors and more iterations 
priors_fledge_test <- c(set_prior("normal(0, 3)", class = "b"),
                       set_prior("normal(0, 10)", class = "Intercept"),
                       set_prior("exponential(1)", class = "sd"))

SGPE_fledging_model_test <- brms::brm(outcome ~ fledging_temp_z + fledging_wind_z + (1 | Year),
                                      data = SGPE_fledging,
                                      family = bernoulli(),
                                      prior = priors_fledge_test,
                                      chains = 3,
                                      cores = 3,
                                      iter = 30000,
                                      warmup = 15000, 
                                      seed = 123)

summary(SGPE_fledging_model_test)
plot(SGPE_fledging_model_test)
pp_check(SGPE_fledging_model_test, type = "bars")
prior_summary(SGPE_fledging_model_test)

# Testing using a frequentist glmer model 
SGPE_fledging_glmer <- glmer(outcome ~ fledging_temp_z + fledging_wind_z + (1 | Year),
                             data = SGPE_fledging,
                             family = binomial)

summary(SGPE_fledging_glmer)

# Comparing fixed effects
fixef(SGPE_fledging_glmer)
fixef(SGPE_fledging_model)

# CONCLUSIONS: 
# Running 30,000 iter and loosening priors also produced very similar (within 0.02) parameter estimates compared with
# the original model - shows stability and results are not sensitive to altered priors, thus results are data driven
# Comparison to non-Bayesian model produced very similar (within 0.02) estimates in both direction and magnitude 
# I will stick with my original priors 


# ==================================================================================================================
#                                             Breeding success model 
# ==================================================================================================================

# No climate windows retained for breeding success during sliding window analysis. A null model is used instead which
# provides baseline SGPE breeding success with year as a random effect, and no climate predictors.

# Priors chosen using same logic as hatching and fledging success, to be weakly informative
priors_breed <- c(set_prior("normal(0, 2.5)", class = "Intercept"),
                 set_prior("exponential(1)", class = "sd"))

SGPE_breeding <- SGPE_breeding_nests %>%
  mutate(Year = factor(Year))

SGPE_breeding_model <- brms::brm(outcome ~ 1 + (1 | Year),
                                 data = SGPE_breeding,
                                 family = bernoulli(),
                                 prior = priors_breed,
                                 chains = 3,
                                 cores = 3,
                                 iter = 3000,
                                 warmup = 1000, 
                                 seed = 123)

summary(SGPE_breeding_model)
plot(SGPE_breeding_model)
pp_check(SGPE_breeding_model, type = "bars")
prior_summary(SGPE_breeding_model)


# ==================================================================================================================
#                                             Exporting files for plotting 
# ==================================================================================================================

write.csv(SGPE_hatching, file = "SGPE_hatching_bayesian_data.csv", row.names = FALSE)
write.csv(SGPE_fledging, file = "SGPE_fledging_bayesian_data.csv", row.names = FALSE)
write.csv(SGPE_breeding, file = "SGPE_breeding_bayesian_data.csv", row.names = FALSE)

saveRDS(SGPE_hatching_model, file = "SGPE_hatching_model.rds")
saveRDS(SGPE_fledging_model, file = "SGPE_fledging_model.rds")
saveRDS(SGPE_breeding_model, file = "SGPE_breeding_model.rds")

saveRDS(SGPE_hatching_model_test, file = "SGPE_hatching_model_sensitivity.rds")
saveRDS(SGPE_fledging_model_test, file = "SGPE_fledging_model_sensitivity.rds")

capture.output(summary(SGPE_hatching_model), file = "Model Summaries/SGPE_hatching_model_summary.txt")
capture.output(summary(SGPE_fledging_model), file = "Model Summaries/SGPE_fledging_model_summary.txt")
capture.output(summary(SGPE_breeding_model), file = "Model Summaries/SGPE_breeding_model_summary.txt")

capture.output(summary(SGPE_hatching_model_test), file = "Model Summaries/SGPE_hatching_model_summary_test.txt")
capture.output(summary(SGPE_fledging_model_test), file = "Model Summaries/SGPE_fledging_model_summary_test.txt")



# SENSITIVITY ANALYSIS

# ===================================================================================================
# Test for hatching success v precipitation without data from 2023 - model plot sensitivity analysis
# ===================================================================================================
# Checked raw data and data window from candidate windows contained two +40mm precip days - could removal show better
# results...

# Removing 2023 nest observations and re-standardising predictors without 2023
SGPE_hatching_no_2023 <- SGPE_hatching %>%
  filter(Year != 2023) %>%
  mutate(Year = factor(Year),
         hatching_precip_z = as.numeric(scale(hatching_precip_raw)),
         hatching_temp_z = as.numeric(scale(hatching_temp_raw)),
         hatching_wind_z = as.numeric(scale(hatching_wind_raw)))

SGPE_hatching_model_no_2023 <- brms::brm(outcome ~ hatching_precip_z + hatching_temp_z + hatching_wind_z + (1|Year),
                                         data = SGPE_hatching_no_2023,
                                         family = bernoulli(),
                                         prior = priors_hatch,
                                         chains = 3,
                                         cores = 3,
                                         iter = 3000,
                                         warmup = 1000, 
                                         seed = 123)

summary(SGPE_hatching_model_no_2023)
plot(SGPE_hatching_model_no_2023)
pp_check(SGPE_hatching_model_no_2023, type = "bars")
prior_summary(SGPE_hatching_model_no_2023)

# Comparing fixed effects
fixef(SGPE_hatching_model)
fixef(SGPE_hatching_model_no_2023)


write.csv(SGPE_hatching_no_2023, file = "SGPE_hatching_bayesian_data_no_2023.csv", row.names = FALSE)
saveRDS(SGPE_hatching_model_no_2023, file = "SGPE_hatching_model_no_2023.rds")

capture.output(summary(SGPE_hatching_model_no_2023), file = "Model Summaries/SGPE_hatching_model_summary_no_2023.txt")

# =============================================================
# Exporting the scaled predictors to use in my plotting script 
# =============================================================

hatching_scaling_no_2023 <- SGPE_hatching_no_2023 %>%
  summarise(across(c(hatching_precip_raw, hatching_temp_raw, hatching_wind_raw),
                   list(mean = mean, sd = sd)))

write_csv(hatching_scaling_no_2023, "SGPE_hatching_scaling_no_2023.csv")
