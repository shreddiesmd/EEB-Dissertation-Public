# ===============================================================================================================
#                                            BAYESIAN MODELLING - AYNA
# ===============================================================================================================

rm(list=ls())

# ===========================
# Adding packages to library
# ===========================

library(dplyr); library(readr); library(brms); library(lme4)

# ============================
# Uploading required datasets 
# ============================

AYNA_window_climate <- read_csv("AYNA_window_climate.csv")
AYNA_hatching_nests <- read_csv("AYNA_hatching_nests.csv")
AYNA_fledging_nests <- read_csv("AYNA_fledging_nests.csv")
AYNA_breeding_nests <- read_csv("AYNA_breeding_nests.csv")
AYNA_AIC_nestlevel <- read_csv("AYNA_AIC_nestlevel.csv")

# =============================================
# Extracting the climate predictors for models
# =============================================

# REMEMBER: The predictors are standardised so must be transformed back before interpretation or plotting!!

# Hatching success predictor -> mean temperature during early incubation
hatching_climate_final <- AYNA_window_climate %>%
  filter(response == "hatching",
         window == "early_incubation") %>%
  transmute(Year, 
            hatching_temp_raw = mean_temp, 
            hatching_temp_z = as.numeric(scale(mean_temp))) 

# Fledging success predictor -> mean temperature during late chick rearing 
fledging_climate_final <- AYNA_window_climate %>%
  filter(response == "fledging",
         window == "late_chick_rearing") %>%
  transmute(Year, 
            fledging_temp_raw = mean_temp, 
            fledging_temp_z = as.numeric(scale(mean_temp)))

# Breeding success predictor -> mean temp during chick rearing 
breeding_climate_final <- AYNA_window_climate %>%
  filter(response == "breeding", 
         window == "chick_rearing") %>%
  transmute(Year,
            breeding_temp_raw = mean_temp,
            breeding_temp_z = as.numeric(scale(mean_temp)))

# =============================================================
# Exporting the scaled predictors to use in my plotting script 
# =============================================================

hatching_scaling <- hatching_climate_final %>%
  summarise(across(c(hatching_temp_raw),
                   list(mean = mean, sd = sd)))

write_csv(hatching_scaling, "AYNA_hatching_scaling.csv")

fledging_scaling <- fledging_climate_final %>%
  summarise(across(c(fledging_temp_raw),
                   list(mean = mean, sd = sd)))

write_csv(fledging_scaling, "AYNA_fledging_scaling.csv")

breeding_scaling <- breeding_climate_final %>%
  summarise(across(c(breeding_temp_raw),
                   list(mean = mean, sd = sd)))

write_csv(breeding_scaling, "AYNA_breeding_scaling.csv")

# =================================================================
# Formatting the final datatsets for input into the Bayesian model
# =================================================================

# Hatching success 
AYNA_hatching <- AYNA_hatching_nests %>%
  left_join(hatching_climate_final, by = "Year") %>%
  mutate(Year = factor(Year))

# Fledging success 
AYNA_fledging <- AYNA_fledging_nests %>%
  left_join(fledging_climate_final, by = "Year") %>%
  mutate(Year = factor(Year))

# Breeding success
AYNA_breeding <- AYNA_breeding_nests %>%
  left_join(breeding_climate_final, by = "Year") %>%
  mutate(Year = factor(Year))

# =====================================================================
# Checking the datasets are all complete and accurate before modelling
# =====================================================================

summary(AYNA_hatching)
summary(AYNA_fledging)
summary(AYNA_breeding)

table(AYNA_hatching$Year)
table(AYNA_fledging$Year)
table(AYNA_breeding$Year)

sum(is.na(AYNA_hatching$hatching_temp_z))
sum(is.na(AYNA_fledging$fledging_temp_z))
sum(is.na(AYNA_breeding$breeding_temp_z))

# ==================================================================================================================
#                                             Hatching success model 
# ==================================================================================================================

# Weakly informative priors. The predictors are standardised so these priors describe a change in log-odds of 
# success per 1 standard deviation change in climate predictor. 

priors_hatch <- c(set_prior("normal(0, 1.5)", class = "b"),
                  set_prior("normal(0, 2.5)", class = "Intercept"),
                  set_prior("exponential(1)", class = "sd"))

AYNA_hatching_model <- brms::brm(outcome ~ hatching_temp_z + (1 | Year),
                                 data = AYNA_hatching,
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
summary(AYNA_hatching_model)
plot(AYNA_hatching_model)
pp_check(AYNA_hatching_model, type = "bars")
prior_summary(AYNA_hatching_model)

# Testing the model with looser priors and more iterations 
priors_hatch_test <- c(set_prior("normal(0, 3)", class = "b"),
                       set_prior("normal(0, 10)", class = "Intercept"),
                       set_prior("exponential(1)", class = "sd"))

AYNA_hatching_model_test <- brms::brm(outcome ~ hatching_temp_z + (1 | Year),
                                      data = AYNA_hatching,
                                      family = bernoulli(),
                                      prior = priors_hatch_test,
                                      chains = 3,
                                      cores = 3,
                                      iter = 30000,
                                      warmup = 15000, 
                                      seed = 123)

summary(AYNA_hatching_model_test)
plot(AYNA_hatching_model_test)
pp_check(AYNA_hatching_model_test, type = "bars")
prior_summary(AYNA_hatching_model_test)

# Testing using a frequentist glmer model 
AYNA_hatching_glmer <- glmer(outcome ~ hatching_temp_z  + (1 | Year),
                             data = AYNA_hatching,
                             family = binomial)

summary(AYNA_hatching_glmer)

# Comparing fixed effects
fixef(AYNA_hatching_glmer)
fixef(AYNA_hatching_model)

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

AYNA_fledging_model <- brms:: brm(outcome ~ fledging_temp_z + (1| Year),
                                  data = AYNA_fledging, 
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
summary(AYNA_fledging_model)
plot(AYNA_fledging_model)
pp_check(AYNA_fledging_model, type = "bars")
prior_summary(AYNA_fledging_model)

# Testing the model with looser priors and more iterations 
priors_fledg_test <- c(set_prior("normal(0, 3)", class = "b"),
                       set_prior("normal(0, 10)", class = "Intercept"),
                       set_prior("exponential(1)", class = "sd"))

AYNA_fledging_model_test <- brms::brm(outcome ~ fledging_temp_z + (1 | Year),
                                      data = AYNA_fledging,
                                      family = bernoulli(),
                                      prior = priors_fledg_test,
                                      chains = 3,
                                      cores = 3,
                                      iter = 30000,
                                      warmup = 15000, 
                                      seed = 123)

summary(AYNA_fledging_model_test)
plot(AYNA_fledging_model_test)
pp_check(AYNA_fledging_model_test, type = "bars")
prior_summary(AYNA_fledging_model_test)

# Testing using a frequentist glmer model 
AYNA_fledging_glmer <- glmer(outcome ~ fledging_temp_z  + (1 | Year),
                             data = AYNA_fledging,
                             family = binomial)

summary(AYNA_fledging_glmer)

# Comparing fixed effects
fixef(AYNA_fledging_glmer)
fixef(AYNA_fledging_model)

# CONCLUSIONS: 

# Running 30,000 iter and loosening priors produced very similar (within 0.02) parameter estimates compared with the
# original model - shows stability and results are not sensitive to altered priors, thus results are data driven
# Comparison to a non-Bayesian model produced very similar estimates in both direction and magnitude -> sense check
# I will stick with my original priors 


# ==================================================================================================================
#                                             Breeding success model 
# ==================================================================================================================

# Weakly informative priors. The predictors are standardised so these priors describe a change in log-odds of 
# success per 1 standard deviation change in climate predictor. 

priors_breed <- c(set_prior("normal(0, 1.5)", class = "b"),
                  set_prior("normal(0, 2.5)", class = "Intercept"),
                  set_prior("exponential(1)", class = "sd"))

AYNA_breeding_model <- brms::brm(outcome ~ breeding_temp_z + (1|Year),
                                 data = AYNA_breeding, 
                                 family = bernoulli(),
                                 prior = priors_breed,
                                 chains = 3,
                                 cores = 3,
                                 iter = 3000, 
                                 warmup = 1000,
                                 seed = 123)

# =======================================
# Testing for robustness and sensibility 
# =======================================

# Model checks 
summary(AYNA_breeding_model)
plot(AYNA_breeding_model)
pp_check(AYNA_breeding_model, type = "bars")
prior_summary(AYNA_breeding_model)

# Testing the model with looser priors and more iterations 
priors_breed_test <- c(set_prior("normal(0, 3)", class = "b"),
                       set_prior("normal(0, 10)", class = "Intercept"),
                       set_prior("exponential(1)", class = "sd"))

AYNA_breeding_model_test <- brms::brm(outcome ~ breeding_temp_z + (1 | Year),
                                      data = AYNA_breeding,
                                      family = bernoulli(),
                                      prior = priors_breed_test,
                                      chains = 3,
                                      cores = 3,
                                      iter = 30000,
                                      warmup = 15000, 
                                      seed = 123)

summary(AYNA_breeding_model_test)
plot(AYNA_breeding_model_test)
pp_check(AYNA_breeding_model_test, type = "bars")
prior_summary(AYNA_breeding_model_test)

# Testing using a frequentist glmer model 
AYNA_breeding_glmer <- glmer(outcome ~ breeding_temp_z  + (1 | Year),
                             data = AYNA_breeding,
                             family = binomial)

summary(AYNA_breeding_glmer)

# Comparing fixed effects
fixef(AYNA_breeding_glmer)
fixef(AYNA_breeding_model)

# CONCLUSIONS: 

# Running 30,000 iter and loosening priors produced identical parameter estimates compared with the original model -
# shows stability and results are not sensitive to altered priors, thus results are data driven
# Comparison to a non-Bayesian model produced very similar estimates in both direction and magnitude -> sense check
# I will stick with my original priors 


# ==================================================================================================================
#                                                     Exporting files
# ==================================================================================================================

write.csv(AYNA_hatching, file = "AYNA_hatching_bayesian_data.csv", row.names = FALSE)
write.csv(AYNA_fledging, file = "AYNA_fledging_bayesian_data.csv", row.names = FALSE)
write.csv(AYNA_breeding, file = "AYNA_breeding_bayesian_data.csv", row.names = FALSE)

saveRDS(AYNA_hatching_model, file = "AYNA_hatching_model.rds")
saveRDS(AYNA_fledging_model, file = "AYNA_fledging_model.rds")
saveRDS(AYNA_breeding_model, file = "AYNA_breeding_model.rds")

saveRDS(AYNA_hatching_model_test, file = "AYNA_hatching_model_sensitivity.rds")
saveRDS(AYNA_fledging_model_test, file = "AYNA_fledging_model_sensitivity.rds")
saveRDS(AYNA_breeding_model_test, file = "AYNA_breeding_model_sensitivity.rds")

capture.output(summary(AYNA_hatching_model), file = "Model Summaries/AYNA_hatching_model_summary.txt")
capture.output(summary(AYNA_fledging_model), file = "Model Summaries/AYNA_fledging_model_summary.txt")
capture.output(summary(AYNA_breeding_model), file = "Model Summaries/AYNA_breeding_model_summary.txt")

capture.output(summary(AYNA_hatching_model_test), file = "Model Summaries/AYNA_hatching_model_summary_test.txt")
capture.output(summary(AYNA_fledging_model_test), file = "Model Summaries/AYNA_fledging_model_summary_test.txt")
capture.output(summary(AYNA_breeding_model_test), file = "Model Summaries/AYNA_breeding_model_summary_test.txt")



