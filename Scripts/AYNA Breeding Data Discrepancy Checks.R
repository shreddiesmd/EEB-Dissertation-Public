# ====================================================================================================
# CHECKING THE AYNA DATASET USING THE SAME LOGIC AS SGPE INCU/ALIVE -> CHIC/FAILED IN CLEANING SCRIPT
# ====================================================================================================

rm(list=ls())

library(dplyr)

AYNA_all_clean <- read.csv("AYNA_all_clean_discrep_test.csv")

# Checking the complete AYNA dataset with all years
AYNA_test <- AYNA_all_clean %>%
  mutate(Date = as.Date(Date),
         original_order = row_number()) %>%
  arrange(Nest_label, Date, original_order) %>%
  group_by(Nest_label) %>%
  mutate(pair_start = Stage == "INCU" & Status == "Alive" &
           lead(Stage, default = "") == "CHIC" & lead(Status, default = "") == "Failed",
         keep_row = pair_start | lag(pair_start, default = FALSE)) %>%
  filter(keep_row) %>%
  ungroup() %>%
  select(-pair_start, -keep_row, -original_order)

length(unique(AYNA_test$Nest_label)) 
# 6 nests that could be changed if AYNA breeding success is different to RSPB
# 1x 2020 , 2x 2022 , 3x 2023


# All the nests with this notation produce hatching/fledging success results closer to RSPB results when treated as
# failed hatching rather than failed fledging. Manually editing the nests below to update.

# All the nests where the above logic applied
AYNA_discrep_nests <- AYNA_test %>%
  distinct(Nest_label) %>%
  pull(Nest_label)

# Editing the nests in AYNA_all_clean
AYNA_all_clean <- AYNA_all_clean %>%
  mutate(Stage = if_else(Nest_label %in% AYNA_discrep_nests &
                           Stage == "CHIC" & Status == "Failed", "INCU", Stage))

# ======================
# Final cleaned dataset
# ======================

# Creating the final columns which identify if each nest achieved hatching success and/or fledging success
AYNA_all_clean <- AYNA_all_clean %>%
  group_by(Nest_label) %>%
  mutate(hatched = ifelse(any(Stage %in% c("CHIC", "FLED")), "Yes", "No"),
         fledged = ifelse(any(Stage == "FLED" | Status == "Fledged") |
                            (Stage == "CHIC" & Status == "Alive" & row_number() == n()), "Yes", "No")) %>%
  ungroup()

# Trimming the nest observations so only the final observation is shown. This makes the AYNA_final quite similar 
# to the AYNA_sum table but also includes the standardised columns: nest outcome, hatched and fledged column, that 
# I can use to model these breeding metrics. Also tidying the AYNA_final table by removing unnecessary columns 
AYNA_final <- AYNA_all_clean %>%
  group_by(Nest_label) %>%
  arrange(desc(Date))%>%
  slice_head(n = 1) %>%
  ungroup() %>%
  select(-stage_number, -total_stages, -Content, -Species)

# ===================================================================
# Final hatching and fledging calculation per year AYNA was observed 
# ===================================================================

# Nest counts per year 
AYNA_nest_count <- AYNA_final %>%
  group_by(Year) %>%
  summarise(nests = n_distinct(Nest_label))

# Breeding success = fledged/nests, fledging_success = fledged/hatched, hatching success = hatched/nests
AYNA_breeding_success <- AYNA_final %>%
  group_by(Year) %>%
  summarise(breeding_success = mean(fledged == "Yes"),
            fledging_success = sum(fledged == "Yes" & hatched == "Yes")/ sum(hatched == "Yes"),
            hatching_success = mean(hatched == "Yes")) %>%
  right_join(AYNA_nest_count, by = "Year")

print(AYNA_breeding_success)

# Exporting table
write.csv(AYNA_breeding_success, file = "AYNA_breeding_success_discrep_test.csv", row.names = FALSE)






# NOW RUNNING THE WINDOW ANALYSIS FROM CLIM SLIDING WINDOW WITH RETAINED PREDICTORS AYNA.R TO SEE IF CHNAGING THE 
# BREEDING NOTATIONS ALSO CHANGES THE CNADIDATE WINDOWS - THIS IS COPIED AND PASTED. 

# ===============================================================================================================
#         CLIMATE SLIDING WINDOWS - INDIVIDUAL NEST LEVEL ANALYSIS WITH RETAINED PREDICTORS AND SCALING
# ===============================================================================================================

# ===========================
# Adding packages to library
# ===========================

library(dplyr); library(lubridate); library(tidyr); library(purrr); library(lme4)

# ============================
# Uploading required datasets 
# ============================

AYNA_breakdown <- read.csv("AYNA_breakdown.csv")
climate_final <- read.csv("climate_final.csv")

# ====================================
# Creating the candidate date windows
# ====================================

AYNA_breakdown <- AYNA_breakdown %>%
  mutate(across(c(incubation_start, incubation_end, chick_start, fledging_end), as.Date))

# Hatching success window defined by incubation stage, fledging success window defined by chick rearing stage, 
# breeding success window defined by entire breeding season
AYNA_windows <- AYNA_breakdown %>%
  mutate(across(c(hatch_start_window = incubation_start,
                  hatch_end_window = incubation_end,
                  fledg_start_window = chick_start,
                  fledg_end_window = chick_end,
                  breed_start_window = incubation_start,
                  breed_end_window = fledging_end),
                as.Date))

# I am looking at 3 candidate windows per breeding stage, e.g. early, late & full incubation/ chick stage. I have 
# a sample size of 13 years - response variable is annual breeding success, thus n = 13. Therefore I don't want
# to overfit my model, instead I will use relative windows that are aligned to stage-specific biology

AYNA_candidate_dates <- AYNA_windows %>%
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

############################## ONLY USE THIS CODE IF I HAVE TIME - I NEED TO COMBINE WITH OTHER TABLE TOO ETC.
# Identifying laydate candidate windows from date spread of egg first observed
AYNA_first_obs <- AYNA_all_clean %>%
  group_by(Nest_label) %>%
  arrange(Date) %>%
  slice_head(n = 1) %>%
  filter(Stage !="CHIC")

AYNA_lay_window <- AYNA_first_obs %>%
  group_by(Year) %>%
  summarise(FirstLayDate = min(Date, na.rm = TRUE),
            LastLayDate = max(Date, na.rm = TRUE))

############################

# Converting table to long format with one row per breeding year for easier AIC calculations
AYNA_window_dates <- bind_rows(
  # Hatching success
  AYNA_candidate_dates %>%
    transmute(Year,
              response = "hatching",
              window = "early_incubation",
              start = hatch_early_start,
              end = hatch_early_end),
  AYNA_candidate_dates %>%
    transmute(Year,
              response = "hatching",
              window = "late_incubation",
              start = hatch_late_start,
              end = hatch_late_end),
  AYNA_candidate_dates %>%
    transmute(Year,
              response = "hatching",
              window = "full_incubation",
              start = hatch_full_start,
              end = hatch_full_end),
  # Fledging success
  AYNA_candidate_dates %>%
    transmute(Year,
              response = "fledging",
              window = "early_chick_rearing",
              start = fledg_early_start,
              end = fledg_early_end),
  AYNA_candidate_dates %>%
    transmute(Year,
              response = "fledging",
              window = "late_chick_rearing",
              start = fledg_late_start,
              end = fledg_late_end),
  AYNA_candidate_dates %>%
    transmute(Year,
              response = "fledging",  
              window = "full_chick_rearing",
              start = fledg_full_start,
              end = fledg_full_end),
  # Breeding success 
  AYNA_candidate_dates %>%
    transmute(Year,
              response = "breeding",
              window = "incubation",
              start = breed_incu_start,
              end = breed_incu_end),
  AYNA_candidate_dates %>%
    transmute(Year,
              response = "breeding",
              window = "chick_rearing",
              start = breed_chick_start,
              end = breed_chick_end),
  AYNA_candidate_dates %>%
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

AYNA_window_climate <- AYNA_window_dates %>%
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
AYNA_hatching_nests <- AYNA_final %>%
  transmute(Nest_label, 
            Year,
            outcome = if_else(hatched == "Yes", 1, 0))

# Fledging success
AYNA_fledging_nests <- AYNA_final %>%
  filter(hatched == "Yes") %>%
  transmute(Nest_label,
            Year,
            outcome = if_else(fledged == "Yes", 1, 0))

# Breeding success
AYNA_breeding_nests <- AYNA_final %>%
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
precip_AIC_hatching <- all_candidate_windows_glmer_adjusted(climate_window = AYNA_window_climate,
                                                            nest_data = AYNA_hatching_nests,
                                                            response_name = "hatching",
                                                            climate_driver = "mean_precip")

# Fledging success
precip_AIC_fledging <- all_candidate_windows_glmer_adjusted(climate_window = AYNA_window_climate,
                                                            nest_data = AYNA_fledging_nests,
                                                            response_name = "fledging",
                                                            climate_driver = "mean_precip")

# Breeding success
precip_AIC_breeding <- all_candidate_windows_glmer_adjusted(climate_window = AYNA_window_climate, 
                                                            nest_data = AYNA_breeding_nests,
                                                            response_name = "breeding",
                                                            climate_driver = "mean_precip")

# Binding the results to one table
precip_AIC <- bind_rows(precip_AIC_hatching, precip_AIC_fledging, precip_AIC_breeding)

# All precipitation windows favour the null so null/basleine will be included when modelling temperature

# =================
# Mean temperature 
# =================

# Hatching success 
temp_AIC_hatching <- all_candidate_windows_glmer_adjusted(climate_window = AYNA_window_climate,
                                                          nest_data = AYNA_hatching_nests,
                                                          response_name = "hatching",
                                                          climate_driver = "mean_temp")
# Fledging success
temp_AIC_fledging <- all_candidate_windows_glmer_adjusted(climate_window = AYNA_window_climate,
                                                          nest_data = AYNA_fledging_nests,
                                                          response_name = "fledging",
                                                          climate_driver = "mean_temp")
# Breeding success
temp_AIC_breeding <- all_candidate_windows_glmer_adjusted(climate_window = AYNA_window_climate, 
                                                          nest_data = AYNA_breeding_nests,
                                                          response_name = "breeding",
                                                          climate_driver = "mean_temp")
# Binding the results to one table 
temp_AIC <- bind_rows(temp_AIC_hatching, temp_AIC_fledging, temp_AIC_breeding)

# Hatching success favours the early_incubation window, fledging success favours late_chick_rearing. Breeding 
# success favours chick_rearing window. All three will be included in wind speed modelling below.

# =========================================================
# Retained temperature predictors for wind speed modelling 
# =========================================================
# Scale adjusts the mean temperature to a z-score for standardised climate predictors 

# Hatching -> early_incubation temp
hatching_temp_retained <- AYNA_window_climate %>%
  filter(response == "hatching",
         window == "early_incubation") %>%
  transmute(Year, retained_temp = as.numeric(scale(mean_temp)))

# Fledging -> early_chick_rearing temp
fledging_temp_retained <- AYNA_window_climate %>%
  filter(response == "fledging",
         window == "early_chick_rearing") %>%
  transmute(Year, retained_temp = as.numeric(scale(mean_temp)))

# Breeding -> chick_rearing temp
breeding_temp_retained <- AYNA_window_climate %>%
  filter(response == "breeding",
         window == "chick_rearing") %>%
  transmute(Year, retained_temp = as.numeric(scale(mean_temp)))

# Converting Year to a factor
AYNA_window_climate <- AYNA_window_climate %>%
  mutate(Year = factor(Year))

# ================
# Mean wind speed
# ================

# Hatching success 
wind_speed_AIC_hatching <- all_candidate_windows_glmer_adjusted(climate_window = AYNA_window_climate,
                                                                nest_data = AYNA_hatching_nests,
                                                                response_name = "hatching",
                                                                climate_driver = "mean_wind_speed",
                                                                add_predictor_data = hatching_temp_retained,
                                                                add_predictor_name = "retained_temp")
# Fledging success
wind_speed_AIC_fledging <- all_candidate_windows_glmer_adjusted(climate_window = AYNA_window_climate,
                                                                nest_data = AYNA_fledging_nests,
                                                                response_name = "fledging",
                                                                climate_driver = "mean_wind_speed",
                                                                add_predictor_data = fledging_temp_retained,
                                                                add_predictor_name = "retained_temp")
# Breeding success
wind_speed_AIC_breeding <- all_candidate_windows_glmer_adjusted(climate_window = AYNA_window_climate, 
                                                                nest_data = AYNA_breeding_nests,
                                                                response_name = "breeding",
                                                                climate_driver = "mean_wind_speed",
                                                                add_predictor_data = breeding_temp_retained,
                                                                add_predictor_name = "retained_temp")
# Binding the results to one table 
wind_speed_AIC <- bind_rows(wind_speed_AIC_hatching, wind_speed_AIC_fledging, wind_speed_AIC_breeding)

# =================================================================
# Final table with all climate windows for each breeding parameter
# =================================================================

AYNA_AIC_nestlevel <- bind_rows(precip_AIC, temp_AIC, wind_speed_AIC)

# Exporting table with all window results 
write.csv(AYNA_AIC_nestlevel, file = "AYNA_AIC_nestlevel.csv", row.names = FALSE)

# Table showing just optimal window per breeding response and climate predictor 
AYNA_AIC_nestlevel_optimal <- AYNA_AIC_nestlevel %>%
  group_by(response, climate_driver) %>%
  slice_min(AIC) 

print(AYNA_AIC_nestlevel_optimal)

# CONCLUSIONS - UPDATINMG THE NOTATIONS FOR THE 6 NESTS DOES NOT CHANGE THE CANDIDATE WINDOWS - WILL NOT IMPACT MODEL
# ALSO WHEN COMPARED TO RSPB NUMBERS, MY INITIAL CODING PRODUCES RESULTS FAR CLOSER TO THE RSPB -> THEREFORE, THIS 
# SCRIPT IS NO LONGER NEEDED BUT WILL KEEP ANYWAY IF NEEDED IN FUTURE




# NEXT CHECK 




# ====================
# Checking 2010 dates 
# ====================

rm(list=ls())

library(readxl); library(dplyr)

AYNA_all_clean <- read.csv("AYNA_all_clean_discrep_test.csv")
AYNA_sum <- read_excel("AYNA_nest_summaries.xlsx", col_types = "text")


# Converting excel numeric dates to dates in R format
AYNA_sum <- AYNA_sum %>%
  mutate(DateLastAlive = as.numeric(DateLastAlive)) %>%
  mutate(DateLastAlive = as.Date(DateLastAlive, origin = "1899-12-30")) %>%
  mutate(DateFound = as.numeric(DateFound)) %>%
  mutate(DateFound = as.Date(DateFound, origin = "1899-12-30")) %>%
  mutate(DateLastChecked = as.numeric(DateLastChecked)) %>%
  mutate(DateLastChecked = as.Date(DateLastChecked, origin = "1899-12-30"))

# Summary table colony column has lots of blank entries - merging colony details from AYNA_all to update the table
AYNA_sum <- AYNA_sum %>%
  left_join(AYNA_all_clean %>% select(Nest_label, Colony) %>% distinct(),
            by = "Nest_label") %>%
  mutate(Colony = coalesce(Colony.x, Colony.y)) %>%
  select(-Colony.x, -Colony.y)

# Isolating observations from Area 1 and 2010 (biggest discrpency year between my success estimates and RSPB's)
AYNA_sum <- AYNA_sum[AYNA_sum$Colony == "Area 1",]
AYNA_sum <- AYNA_sum[AYNA_sum$Year == "2010",]

# Isolating problem nests to identify the dates when last recorded to see if there is overlap with entire breeding
# season and if there is justification to think the team could no longer visit the nests or if the nests were 
# empty on next visit 
AYNA_incu_success_true <- AYNA_sum %>%
  filter(LastStage == "INCU",
         SUCCESS == TRUE,
        Completed == 0) %>%
  select(Year, Nest_label, Colony, DateLastAlive, DateLastChecked, LastStage, Completed, SUCCESS) %>%
  arrange(Year, DateLastChecked, Nest_label)

# CONCLUSION - Nests were last observed from 06/09/2010 - 18/01/2011 but breeding timelines shows monitoring took 
# place from 06/09/2010 - 06/05/2011. My interpretation: team could still visit nests and likely did but egg was 
# missing/ predated/ failed and nest was empty with no evidence of its fate. I could treat these as failed hatching
# attempts and keep them in my original analysis. 
# However, my estimates are so vastly different to the RSPB's that I will remove them from because I cannot be 
# certain of their outcome and it will impact analysis
