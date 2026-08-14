# ===============================================================================================================
#        CLIMATE SLIDING WINDOWS - INDIVIDUAL NEST LEVEL ANALYSIS WITH RETAINED PREDICTORS AND SCALING
# ===============================================================================================================

rm(list=ls())

# ===========================
# Adding packages to library
# ===========================

library(dplyr); library(lubridate); library(tidyr); library(purrr); library(lme4)

# ============================
# Uploading required datasets 
# ============================

SGPE_breakdown <- read.csv("SGPE_breakdown.csv")
SGPE_final <- read.csv("SGPE_final.csv")
climate_final <- read.csv("climate_final.csv")

# ====================================
# Creating the candidate date windows
# ====================================

SGPE_breakdown <- SGPE_breakdown %>%
  mutate(across(c(incubation_start, incubation_end, chick_start, fledging_end), as.Date))

# Hatching success window defined by incubation stage, fledging success window defined by chick rearing stage, 
# breeding success window defined by entire breeding season
SGPE_windows <- SGPE_breakdown %>%
  mutate(across(c(hatch_start_window = incubation_start,
                  hatch_end_window = incubation_end,
                  fledg_start_window = chick_start,
                  fledg_end_window = chick_end,
                  breed_start_window = incubation_start,
                  breed_end_window = fledging_end),
                as.Date))

# I am using three biologically defined candidate windows per breeding stage to avoid overfitting of my model. 
# e.g. early, late & full incubation/ chick stage. Although models are fitted at the nest level, climate predictors 
# vary annually, so I will use relative windows that are aligned to stage-specific biology.

SGPE_candidate_dates <- SGPE_windows %>%
  mutate(
    # Creating midpoint of observed incubation and chick-rearing stages     
    incubation_mid = incubation_start + floor(as.numeric(incubation_end - incubation_start)/2),
    chick_mid = chick_start + floor(as.numeric(fledging_end - chick_start)/2))%>%
  transmute(Year,
            # Hatching success candidate windows
            hatch_early_start = as.Date(incubation_start),
            hatch_early_end = as.Date(incubation_mid),
            
            hatch_late_start = as.Date(incubation_mid + days(1)),
            hatch_late_end = as.Date(incubation_end),
            
            hatch_full_start = as.Date(incubation_start),
            hatch_full_end = as.Date(incubation_end),
            # Fledging success candidate windows 
            fledg_early_start = as.Date(chick_start),
            fledg_early_end = as.Date(chick_mid),
            
            fledg_late_start = as.Date(chick_mid + days(1)),
            fledg_late_end = as.Date(fledging_end),
            
            fledg_full_start = as.Date(chick_start),
            fledg_full_end = as.Date(fledging_end),
            # Breeding success candidate windows 
            breed_incu_start = as.Date(incubation_start),
            breed_incu_end = as.Date(incubation_end),
            
            breed_chick_start = as.Date(chick_start),
            breed_chick_end = as.Date(fledging_end),
            
            breed_full_start = as.Date(incubation_start),
            breed_full_end = as.Date(fledging_end))

# Converting table to long format with one row per breeding year for easier AIC calculations
SGPE_window_dates <- bind_rows(
  # Hatching success
  SGPE_candidate_dates %>%
    transmute(Year,
              response = "hatching",
              window = "early_incubation",
              start = hatch_early_start,
              end = hatch_early_end),
  SGPE_candidate_dates %>%
    transmute(Year,
              response = "hatching",
              window = "late_incubation",
              start = hatch_late_start,
              end = hatch_late_end),
  SGPE_candidate_dates %>%
    transmute(Year,
              response = "hatching",
              window = "full_incubation",
              start = hatch_full_start,
              end = hatch_full_end),
  # Fledging success
  SGPE_candidate_dates %>%
    transmute(Year,
              response = "fledging",
              window = "early_chick_rearing",
              start = fledg_early_start,
              end = fledg_early_end),
  SGPE_candidate_dates %>%
    transmute(Year,
              response = "fledging",
              window = "late_chick_rearing",
              start = fledg_late_start,
              end = fledg_late_end),
  SGPE_candidate_dates %>%
    transmute(Year,
              response = "fledging",  
              window = "full_chick_rearing",
              start = fledg_full_start,
              end = fledg_full_end),
  # Breeding success 
  SGPE_candidate_dates %>%
    transmute(Year,
              response = "breeding",
              window = "incubation",
              start = breed_incu_start,
              end = breed_incu_end),
  SGPE_candidate_dates %>%
    transmute(Year,
              response = "breeding",
              window = "chick_rearing",
              start = breed_chick_start,
              end = breed_chick_end),
  SGPE_candidate_dates %>%
    transmute(Year,
              response = "breeding",
              window = "full_breeding",
              start = breed_full_start,
              end = breed_full_end))

# Applying imported function for climate windows to the candidate windows per breeding success parameter and 
# creating table with final values

climate_final <- climate_final %>%
  mutate(date = as.Date(date)) %>%
  arrange(date)

source("climate_function.R")  

SGPE_window_climate <- SGPE_window_dates %>%
  mutate(climate_summary = map2(start, end, \(start_date, end_date){
    climate_window_summaries(climate = climate_final,
                             start_date = start_date,
                             end_date = end_date)})) %>%
  unnest_wider(climate_summary)

# ===========================================
# Creating tables for mixed-effect modelling 
# ===========================================

# For all three success responses: 1 = success, 0 = failure. 
# E.g. for hatching success: 1 = egg hatched, 0 = egg failed

# Hatching success 
SGPE_hatching_nests <- SGPE_final %>%
  transmute(Nest_label, 
            Year,
            outcome = if_else(hatched == "Yes", 1, 0))

# Fledging success
SGPE_fledging_nests <- SGPE_final %>%
  filter(hatched == "Yes") %>%
  transmute(Nest_label,
            Year,
            outcome = if_else(fledged == "Yes", 1, 0))

# Breeding success
SGPE_breeding_nests <- SGPE_final %>%
  transmute(Nest_label,
            Year,
            outcome = if_else(fledged == "Yes", 1, 0))

# ===========================================================
# Running the AIC candidate-windows for nest level analysis
# ===========================================================

# Importing model function to produce AIC values
source("model_function_predictor_retained.R")

# ===================
# Mean precipitation
# ===================

# Hatching success
precip_AIC_hatching <- all_candidate_windows_glmer_adjusted(climate_window = SGPE_window_climate,
                                                   nest_data = SGPE_hatching_nests,
                                                   response_name = "hatching",
                                                   climate_driver = "mean_precip")

# Fledging success
precip_AIC_fledging <- all_candidate_windows_glmer_adjusted(climate_window = SGPE_window_climate,
                                                   nest_data = SGPE_fledging_nests,
                                                   response_name = "fledging",
                                                   climate_driver = "mean_precip")

# Breeding success
precip_AIC_breeding <- all_candidate_windows_glmer_adjusted(climate_window = SGPE_window_climate, 
                                                   nest_data = SGPE_breeding_nests,
                                                   response_name = "breeding",
                                                   climate_driver = "mean_precip")

# Binding the results to one table
precip_AIC <- bind_rows(precip_AIC_hatching, precip_AIC_fledging, precip_AIC_breeding)

# Hatching success favours the full_incubation window and fledging success and breeding success favour the 
# baseline/null. The former will be included when modelling temperature.

# ============================================================
# Retained precipitation predictors for temperature modelling 
# ============================================================
# Scale adjusts the mean temperature to a z-score for standardised climate predictors 

# Hatching -> full-incubation precip
hatching_precip_retained <- SGPE_window_climate %>%
  filter(response == "hatching",
         window == "full_incubation") %>%
  transmute(Year, retained_precip = as.numeric(scale(mean_precip)))

# =================
# Mean temperature 
# =================

# Hatching success 
temp_AIC_hatching <- all_candidate_windows_glmer_adjusted(climate_window = SGPE_window_climate,
                                                 nest_data = SGPE_hatching_nests,
                                                 response_name = "hatching",
                                                 climate_driver = "mean_temp",
                                                 add_predictor_data = hatching_precip_retained,
                                                 add_predictor_names = "retained_precip")
# Fledging success
temp_AIC_fledging <- all_candidate_windows_glmer_adjusted(climate_window = SGPE_window_climate,
                                                 nest_data = SGPE_fledging_nests,
                                                 response_name = "fledging",
                                                 climate_driver = "mean_temp")
# Breeding success
temp_AIC_breeding <- all_candidate_windows_glmer_adjusted(climate_window = SGPE_window_climate, 
                                                 nest_data = SGPE_breeding_nests,
                                                 response_name = "breeding",
                                                 climate_driver = "mean_temp")
# Binding the results to one table 
temp_AIC <- bind_rows(temp_AIC_hatching, temp_AIC_fledging, temp_AIC_breeding)

# Hatching success favours the full_incubation window and fledging success favours early_chick_rearing. 
# Breeding success shows baseline with lowest AIC. The former two will be included in wind speed modelling below.

# =============================================
# Retained predictors for wind speed modelling 
# =============================================
# Scale adjusts the mean temperature to a z-score for standardised climate predictors 

# Hatching -> full-incubation temp
hatching_temp_retained <- SGPE_window_climate %>%
  filter(response == "hatching",
         window == "full_incubation") %>%
  transmute(Year, retained_temp = as.numeric(scale(mean_temp)))
# AND Hatching -> full incubation precipitation
hatching_retained_predictors <- hatching_precip_retained %>%
  left_join(hatching_temp_retained, by = "Year")

# Fledging -> early-chick-rearing temp
fledging_temp_retained <- SGPE_window_climate %>%
  filter(response == "fledging",
         window == "early_chick_rearing") %>%
  transmute(Year, retained_temp = as.numeric(scale(mean_temp)))

# Converting Year to a factor
SGPE_window_climate <- SGPE_window_climate %>%
  mutate(Year = factor(Year))

# ================
# Mean wind speed
# ================

# Hatching success 
wind_speed_AIC_hatching <- all_candidate_windows_glmer_adjusted(climate_window = SGPE_window_climate,
                                                       nest_data = SGPE_hatching_nests,
                                                       response_name = "hatching",
                                                       climate_driver = "mean_wind_speed",
                                                       add_predictor_data = hatching_retained_predictors,
                                                       add_predictor_names = c("retained_precip", "retained_temp"))
# Fledging success
wind_speed_AIC_fledging <- all_candidate_windows_glmer_adjusted(climate_window = SGPE_window_climate,
                                                       nest_data = SGPE_fledging_nests,
                                                       response_name = "fledging",
                                                       climate_driver = "mean_wind_speed",
                                                       add_predictor_data = fledging_temp_retained,
                                                       add_predictor_names  = "retained_temp")
# Breeding success
wind_speed_AIC_breeding <- all_candidate_windows_glmer_adjusted(climate_window = SGPE_window_climate, 
                                                       nest_data = SGPE_breeding_nests,
                                                       response_name = "breeding",
                                                       climate_driver = "mean_wind_speed")
# Binding the results to one table 
wind_speed_AIC <- bind_rows(wind_speed_AIC_hatching, wind_speed_AIC_fledging, wind_speed_AIC_breeding)

# Hatching favours late incubation window, fledging favours the late_chick_rearing window and breeding favours baseline.

# =================================================================
# Final table with all climate windows for each breeding parameter
# =================================================================

SGPE_AIC_nestlevel <- bind_rows(precip_AIC, temp_AIC, wind_speed_AIC)

# Exporting table with all window results 
write.csv(SGPE_AIC_nestlevel, file = "SGPE_AIC_nestlevel.csv", row.names = FALSE)

# Table showing just optimal window per breeding response and climate predictor 
SGPE_AIC_nestlevel_optimal <- SGPE_AIC_nestlevel %>%
  group_by(response, climate_driver) %>%
  slice_min(AIC) 

print(SGPE_AIC_nestlevel_optimal)

# ==============================================
# Exporting files for Bayesian modelling script
# ==============================================

write.csv(SGPE_window_climate, file = "SGPE_window_climate.csv", row.names = FALSE)
write.csv(SGPE_hatching_nests, file = "SGPE_hatching_nests.csv", row.names = FALSE)
write.csv(SGPE_fledging_nests, file = "SGPE_fledging_nests.csv", row.names = FALSE)
write.csv(SGPE_breeding_nests, file = "SGPE_breeding_nests.csv", row.names = FALSE)
write.csv(SGPE_AIC_nestlevel, file = "SGPE_AIC_nestlevel.csv", row.names = FALSE)

