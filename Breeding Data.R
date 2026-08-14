# ================================================================================================================
#                                       BREEDING DATASETS CLEANING AND FORMATTING
# ================================================================================================================

rm(list=ls())

# ===========================
# Adding packages to library
# ===========================

library(readxl); library(dplyr); library(tidyr); library(stringr); library(lubridate); library(zoo)

# ================================================================================================================
#                                                SOUTHERN GIANT PETREL
# ================================================================================================================

# =================================
# Importing the breeding data sets
# =================================

# This dataset contains all observation (multiple per nest) taken during the breeding season
SGPE_all <- read_excel("SGPE_2011-2023_breeding _data.xlsx")
# This dataset contains a summary of the nests by showing the last stage each nest reached during the season 
SGPE_sum <- read_excel("SGPE_nest_summaries.xlsx")

# ============================================================
# # Standardising the notations for nest status between years
# ============================================================

# ============================================
# Discrepancy fixes for notations in SGPE_sum
# ============================================

# Nest SGPE_2018_LowHump_000 - recorded as LastStage = FAIL. All nests in 2018 LastStage recorded as stage it failed 
# e.g failed at INCU = recorded as INCU/FALSE, not FAIL/FALSE. Changed nest to LastStage = INCU, SUCCESS = FALSE 
# for consistency.
SGPE_sum <- SGPE_sum %>%
  mutate(LastStage = ifelse(LastStage == "FAIL", "INCU", LastStage))

# Checking for further outlying/ unique notations - none detected :)
table(SGPE_sum$LastStage, SGPE_sum$SUCCESS)
table(SGPE_sum$LastStage, SGPE_sum$Year)
table(SGPE_sum$LastStage, SGPE_sum$SUCCESS, SGPE_sum$Year)

# Checking proportion of nests that fledged per year to see if notation issue in summary data.
# From 2011-19, fledged nests are recorded as CHIC/Success = TRUE. This changed in 2020 and fledged was then noted
# as FLED/Success = TRUE. Checking the proportion of nests each year that fledged using the different notations 
# will show me which notations to use to calculate the fledging and hatching success. 

# Making new fledged column with just yes or no labels 
SGPE_sum$fledged <- ifelse((SGPE_sum$LastStage == "CHIC"|
                              SGPE_sum$LastStage == "FLED") & SGPE_sum$SUCCESS == "TRUE","YES","NO")

# Proportion of fledged nests per year
SGPE_fledged <- SGPE_sum %>%
  group_by(Year) %>%
  summarise(proportion_fledged = mean(fledged == "YES"))

# Testing the 2020 & 2023 years (problem years which have different/ multiple notations - do I combine notations
# with same biological result for one fledging success result?) 
SGPE_fledged_test <- SGPE_sum[SGPE_sum$Year %in% c(2020,2023),]

SGPE_fledged_test$fledged<- ifelse(SGPE_fledged_test$LastStage == "FLED" & SGPE_fledged_test$SUCCESS == "TRUE","YES","NO")

SGPE_fledged_test <- SGPE_fledged_test %>%
  group_by(Year) %>%
  summarise(proportion_fledged = mean(fledged == "YES"))
# This shows that years with nests notated as FLED/TRUE and CHIC/TRUE (2020 & 2023) have a similar proportion 
# of fledged nests to years just notated as CHIC/TRUE. Therefore, I will update the SGPE_all data set to reflect.

# ============================================
# Discrepancy fixes for notations in SGPE_all
# ============================================

# Creating a new table which has a nest outcome column to show how each stage and status reflects the biological 
# state of the nest when observed and recorded
SGPE_all_clean <- SGPE_all %>%
  group_by(Nest_label) %>%
  arrange(Date) %>%
  mutate(stage_number = row_number(),
         total_stages = n(),
         nest_outcome = case_when(
           Stage == "INCU" & Status == "Alive" ~ "Egg Laid",
           Stage == "CHIC" & Status == "Alive" & stage_number < total_stages ~ "Chick Hatched",
           Stage == "CHIC" & Status == "Alive" & stage_number == total_stages ~ "Chick Fledged",
           Stage == "FLED" & Status == "Fledged" ~ "Chick Fledged",
           Stage == "FAIL" & Status == "Failed" ~ "Nest Failed",
           Stage == "INCU" & Status == "Failed" ~ "Egg Failed",
           Stage == "CHIC" & Status == "Failed" ~ "Chick Failed",
           TRUE ~ "Other")) %>%
  ungroup()

# Checking the results
table(SGPE_all_clean$nest_outcome, SGPE_all_clean$Year)
# NOTE: 2011-2019 only record failed nests as nest failed, while 2020-2023 get more specific and also note the 
# stage the nest failed. Useful to know but not further required for calculating hatching and fledging success,
# other than a failure occurred 

# ======================================================================
# Checking discrepancy in 2022 breeding notations based on RSPB numbers
# ======================================================================

# Checking the SGPE 2022 dataset
SGPE_2022 <- SGPE_all_clean %>%
  filter(Year == 2022)

length(unique(SGPE_2022$Nest_label))

# I noticed SGPE_2022_LowHump_004 had an observation of INCU/Alive and then CHIC/Failed - isolating all nests with
# a similar notation to see identify a pattern -> all were recorded same day. My code reads CHIC/Failed as a 
# hatched chick that died in the nest, but these could mean hatched a dead chick. 
SGPE_2022_discrep <- SGPE_2022 %>%
  mutate(Date = as.Date(Date),
         original_order = row_number()) %>%
  arrange(Nest_label, Date, original_order) %>%
  group_by(Nest_label) %>%
  mutate(pair_start = Stage == "INCU" & Status == "Alive" &
           lead(Stage, default = "") == "CHIC" & 
           lead(Status, default = "") == "Failed",
         keep_row = pair_start | 
           lag(pair_start, default = FALSE)) %>%
  filter(keep_row) %>%
  ungroup() %>%
  select(-pair_start, -keep_row, -original_order)

length(unique(SGPE_2022_discrep$Nest_label))

# Checking the complete dataset with all years
SGPE_test <- SGPE_all_clean %>%
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

# Following the same process as 2022 data for 2020 & 2021
SGPE_2020 <- SGPE_all_clean %>%
  filter(Year == 2020)
SGPE_2021 <- SGPE_all_clean %>%
  filter(Year == 2021)

length(unique(SGPE_test$Nest_label))

# All the nests in 2020-2022 with this notation produce hatching/fledging success results closer to RSPB results
# when treated as failed hatching rather than failed fledging. Manually editing the nests below to update.

# All the 2020-2022 nests where the above logic applied
SGPE_discrep_nests <- SGPE_test %>%
  distinct(Nest_label) %>%
  pull(Nest_label)

# Editing the nests in SGPE_all_clean
SGPE_all_clean <- SGPE_all_clean %>%
  mutate(Stage = if_else(Nest_label %in% SGPE_discrep_nests &
                           Stage == "CHIC" & Status == "Failed", "INCU", Stage))

# ======================
# Final cleaned dataset
# ======================

# Creating the final columns which identify if each nest achieved hatching success and/or fledging success
SGPE_all_clean <- SGPE_all_clean %>%
  group_by(Nest_label) %>%
  mutate(hatched = ifelse(any(Stage %in% c("CHIC", "FLED")), "Yes", "No"),
         fledged = ifelse(any((Stage == "FLED") | (Status == "Fledged") | 
                                (Stage == "CHIC" & Status == "Alive" & row_number() == n())), "Yes", "No")) %>%
  ungroup()

table(SGPE_all_clean$fledged, SGPE_all_clean$Year)

# Trimming the nest observations so only the final observation is shown. This makes the SGPE_final quite similar 
# to the SGPE_sum table but also includes the standardised columns: nest outcome, hatched and fledged column that 
# I can use to model these breeding metrics. Also tidying the SGPE_final table by removing unnecessary columns 
SGPE_final <- SGPE_all_clean %>%
  group_by(Nest_label) %>%
  arrange(desc(Date)) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  select(-stage_number, -total_stages, -Content, -Species)

# Exporting table
write.csv(SGPE_final, file = "SGPE_final.csv", row.names = FALSE)

# ==============================================================
# Final breeding success calculation per year SGPE was observed 
# ==============================================================

# Nest counts per year
SGPE_nest_count <- SGPE_final %>%
  group_by(Year) %>%
  summarise(nests = n_distinct(Nest_label))

# Breeding success = fledged/nests, fledging_success = fledged/hatched, hatching success = hatched/nests
SGPE_breeding_success <- SGPE_final %>%
  group_by(Year) %>%
  summarise(breeding_success = mean(fledged == "Yes"),
            fledging_success = sum(fledged == "Yes" & hatched == "Yes")/ sum(hatched == "Yes"),
            hatching_success = mean(hatched == "Yes")) %>%
  right_join(SGPE_nest_count, by = "Year")

print(SGPE_breeding_success)

# Exporting table
write.csv(SGPE_breeding_success, file = "SGPE_breeding_success.csv", row.names = FALSE)


# ================================================================================================================
#                                         ATLANTIC YELLOW-NOSED ALBATROSS 
# ================================================================================================================

# =================================
# Importing the breeding data sets 
# =================================

# This dataset contains all observation (multiple per nest) taken during the breeding season
AYNA_all <- read_excel("AYNA_2003-2023_breeding_data.xlsx")
# This dataset contains a summary of the nests by showing the last stage each nest reached during the season 
AYNA_sum <- read_excel("AYNA_nest_summaries.xlsx", col_types = "text")
# Converting excel numeric dates to dates in R format
AYNA_sum <- AYNA_sum %>%
  mutate(DateLastAlive = as.numeric(DateLastAlive)) %>%
  mutate(DateLastAlive = as.Date(DateLastAlive, origin = "1899-12-30")) %>%
  mutate(DateFound = as.numeric(DateFound)) %>%
  mutate(DateFound = as.Date(DateFound, origin = "1899-12-30")) %>%
  mutate(DateLastChecked = as.numeric(DateLastChecked)) %>%
  mutate(DateLastChecked = as.Date(DateLastChecked, origin = "1899-12-30"))

# =========================================
# Isolating colony observations for Area 1 
# =========================================

# Isolating observations from Area 1 - main study colony, better to model one population 
AYNA_all <- AYNA_all[AYNA_all$Colony == "Area 1",]

# Summary table colony column has lots of blank entries - merging colony details from AYNA_all to update the table
AYNA_sum <- AYNA_sum %>%
  left_join(AYNA_all %>% select(Nest_label, Colony) %>% distinct(),
            by = "Nest_label") %>%
  mutate(Colony = coalesce(Colony.x, Colony.y)) %>%
  select(-Colony.x, -Colony.y)

# Isolating observations from Area 1 
AYNA_sum <- AYNA_sum[AYNA_sum$Colony == "Area 1",]
# Removing NA rows 
AYNA_sum <- AYNA_sum[!is.na(AYNA_sum$Colony), ]

# ============================================================
# # Standardising the notations for nest status between years
# ============================================================

# Both AYNA_sum and AYNA_all have incomplete datasets (NA values). There are missing stages and statuses within 
# individual nest observations and across years. Luckily I can fill in some of this missing info by comparing and 
# updating using each dataset against each other.

# =====================================================
# Standardising and updating the notations in AYNA_sum
# =====================================================

# There are missing values and NA values in the LastStage column. All NA results are linked to SUCCESS/False. 
# Need to update the missing values using data from the AYNA_all observations table
table(AYNA_sum$LastStage, AYNA_sum$SUCCESS, useNA = "ifany")

# Table to show all rows with NA values 
AYNA_na_sum <- AYNA_sum[is.na(AYNA_sum$LastStage),]

# Need to fill in all the failure results with their LastStage they failed at - get this from AYNA_all

# =====================================================
# Standardising and updating the notations in AYNA_all
# =====================================================

# Checking number of breeding stages recorded as well as NA values for removal in Stage column
table(AYNA_all$Status, useNA = "ifany")

# Isolating NA rows - both contain NA values and one is a duplicate result, so can both be removed from data
AYNA_all[is.na(AYNA_all$Status),]
AYNA_all <- AYNA_all[!(is.na(AYNA_all$Stage)&is.na(AYNA_all$Status)),]

# Checking number of breeding stages recorded as well as NA values for removal in Status column
table(AYNA_all$Stage, useNA = "ifany")

# 525 results! oh no... Code below will identify reasons for this and fix NA values if possible.

# Some observations that end in nest failure have recorded stage = NA or FAIL and status = Failed. Updating stage
# to reflect the previous observed nest stage before failure so result shows stage the nest was at during failure
AYNA_all <- AYNA_all %>%
  group_by(Nest_label) %>%
  arrange(Date, .by_group = TRUE) %>%
  mutate(Stage = case_when(Status == "Failed" & is.na(Stage) ~ lag(Stage),
                           Status == "Failed" & Stage == "FAIL" ~ lag(Stage),
                           TRUE ~ Stage)) %>%
  ungroup()

# Checking leftover NA and FAIL nests
table(AYNA_all$Stage, AYNA_all$Status, useNA = "ifany")
AYNA_all %>% filter(Status == "Failed") %>% count(Stage)

# Some FAIL/Failed observations remain - these are duplicate results. Final stage and status was recorded and then
# a final FAIL/Failed observation was also noted - can be removed because unnecessary with previous observation
AYNA_all <- AYNA_all %>%
  group_by(Nest_label) %>%
  mutate(real_fail_stage = any(Stage %in% c("INCU", "CHIC") & Status == "Failed"))%>%
  filter(!(Stage == "FAIL" & Status == "Failed" & real_fail_stage)) %>%
  select(-real_fail_stage)%>%
  ungroup()

# Checking leftover NA and FAIL nests again
table(AYNA_all$Stage, AYNA_all$Status, useNA = "ifany")
AYNA_all %>% filter(Status == "Failed") %>% count(Stage)

# Result shows one FAIL nest - AYNA_2017_Area_1_049. All observations were recorded failed - cannot determine 
# breeding success since no stage recorded so will be removed from data. Removal = no more FAIL notation :)
AYNA_all <- AYNA_all[AYNA_all$Nest_label != "AYNA_2017_Area_1_049", ]

# Some NA/Failed observations also remain. Some nests are partially incomplete with observations partially filled
# out. Code below will edit the NA stages by filling in with previous non-missing stage notation.
AYNA_all <- AYNA_all %>%
  group_by(Nest_label) %>%
  arrange(Date, .by_group = TRUE) %>%
  mutate(Stage = zoo::na.locf(Stage, na.rm = FALSE)) %>%
  ungroup()

# Checking leftover NA results + table to show all rows where chick Alive/Failed is recorded but also stage (NA)
table(AYNA_all$Stage, AYNA_all$Status, useNA = "ifany")
AYNA_na_all <- AYNA_all[is.na(AYNA_all$Stage),] 

# Results show all leftover NA rows have Alive nests - there are 10 problem nests - checking problem manually
length(unique(AYNA_na_all$Nest_label)) 

# MANUALLY UPDATING THE 10 PROBLEM NESTS  

# Manual edit: AYNA_2019_Area1_brokeneggshells01 & AYNA_2019_Area1_brokeneggshells02. Single observation recorded:
# Stage = NA, Status = Failed, egg broke in INCU stage (thanks nest label) so updating FAIL -> INCU/Failed
AYNA_all$Stage[AYNA_all$Nest_label %in% 
                 c("AYNA_2019_Area1_brokeneggshells01", "AYNA_2019_Area1_brokeneggshells02") 
               & is.na(AYNA_all$Stage)] <- "INCU" # 2 nests 

# Manual edit: AYNA_2010__NA_C22 had missing stage values labelled NA/Alive, then started labeling INCU/Alive
# Updating the NA stage notations to reflect the INCU stage
AYNA_all$Stage[AYNA_all$Nest_label == "AYNA_2010__NA_C22" & is.na(AYNA_all$Stage)] <- "INCU" # 1 nest

# Manual edit: AYNA_2016_Area_1_NA_38 has observations recorded as NA/Alive until final notation of NA/Failed.
# no way to tell stage nest was alive and failed in so will be removed from data
AYNA_all <- AYNA_all[AYNA_all$Nest_label != "AYNA_2016_Area_1_NA_38", ] # 1 nest

# The last 6 problem nests are from 2011 - these all are Alive but no stage recorded HOWEVER last observation from
# all is FLED/Fledged. Not a problem because fledging means hatching, fledging & breeding success can still be 
# calculated as fledging status is confirmed despite NA stage values.
# Breeding success = fledged/nests, fledging success = fledged/hatched, hatching success = hatched/nests

# =========================================================
# Filling in the missing NA values from the AYNA_sum table
# =========================================================

# Doing this to check the proportion of FLED, CHIC, INCU nests to see if notation system changes between years 
# similar to the SGPE 

# Taking the last stage observed for each nest in AYNA_all
last_stage <- AYNA_all %>%
  arrange(Nest_label, Date) %>%
  group_by(Nest_label) %>%
  summarise(last_stage = last(na.omit(Stage)), .groups = "drop")

# Joining to fill in the AYNA_sum table
AYNA_sum_clean <- AYNA_sum %>%
  left_join(last_stage, by = "Nest_label") %>%
  mutate(LastStage = coalesce(LastStage, last_stage)) %>%
  select(-last_stage)

# test code start -----------------------------------------------------------------------------

# =========================================
# Unresolved INCU/Alive nests discrepancy - added after checking RSPB breeding estimates wirh Antje
# =========================================

# In the raw AYNA_sum and AYNA_all datasets these nests have been considered as successful nests despite never 
# progressing past the incubation stage but are then also labelled as Completed = 0. My code identified the 
# stagnation at INCU/Alive as a failed hatching attempt. RSPB estimates probably removed these nests from calculations
# altogether as their outcome was uncertain. This code removes these nests from breeding estimates. 

# Identifying problem nests
AYNA_unresolved <- AYNA_sum_clean %>%
  filter(LastStage == "INCU", SUCCESS == TRUE, Completed == 0) %>%
  select(Year, Nest_label, Colony, DateFound, DateLastAlive, DateLastChecked, LastStage, Completed, SUCCESS) %>%
  arrange(Year, DateLastChecked, Nest_label)

# Identifying nest IDs
AYNA_unresolved_nests <- AYNA_unresolved %>%
  distinct(Nest_label) %>%
  pull(Nest_label)

# Removing nests from AYNA_sum_clean
AYNA_sum_clean <- AYNA_sum_clean %>%
  filter(!Nest_label %in% AYNA_unresolved_nests)

write.csv(AYNA_unresolved_nests, file = "AYNA_unresolved_nests.csv", row.names = FALSE)
# test code end ------------------------------------------------------------------------------

# Exporting clean summary data 
write.csv(AYNA_sum_clean, file = "AYNA_sum_clean.csv", row.names = FALSE)

# ============================================
# Discrepancy fixes for notations in AYNA_all
# ============================================

# Checking the final observation per nest stage and status outcome to find notation proportions per year
AYNA_all_last_entry <- AYNA_all %>%
  group_by(Nest_label) %>% 
  arrange(Date) %>%     
  slice_tail(n = 1)

# These tables now show there is no difference in observation recording across years so can start to calculate
# success only difference is some nests = CHIC/Fledged - can just be labelled as fledged nests in final dataset 
table(AYNA_all_last_entry$Stage, AYNA_all_last_entry$Status, AYNA_all_last_entry$Year)
table(AYNA_sum$LastStage, AYNA_sum$SUCCESS, AYNA_sum$Year)

# Creating a new table which has a nest outcome column to show how each stage and status reflects the biological 
# state of the nest when observed and recorded
AYNA_all_clean <- AYNA_all %>%
  group_by(Nest_label) %>%
  arrange(Date) %>%
  mutate(stage_number = row_number(),
         total_stages = n(),
         nest_outcome = case_when(
           Stage == "INCU" & Status == "Alive"  ~ "Egg Laid",
           Stage == "INCU" & Status == "Failed" ~ "Egg Failed",
           Stage == "CHIC" & Status == "Failed" ~ "Chick Failed",
           (Stage == "CHIC" & Status == "Fledged") | (Stage == "FLED" & Status == "Fledged") ~ "Chick Fledged",
           Stage == "CHIC" & Status == "Alive" & stage_number < total_stages ~ "Chick Hatched",
           Stage == "CHIC" & Status == "Alive" & stage_number == total_stages ~ "Chick Fledged",
           TRUE ~ "Other")) %>%
  ungroup()

# Removing nests from final dataset which were unresolved INCU/Alive nest discrepancy in AYNA_sum 
AYNA_all_clean <- AYNA_all_clean %>%
  filter(!Nest_label %in% AYNA_unresolved_nests)

# Exporting this file for checking how breeding success chnages when notations are updated - used in AYNA Breeding 
# Check.R script
write.csv(AYNA_all_clean, file = "AYNA_all_clean_discrep_test.csv", row.names = FALSE)

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

# Removing observations from the 2023 breeding season as the monitoring season was cut short in March and will not 
# produce reliable breeding estimations for this year
AYNA_final <- AYNA_final %>%
  filter(Year != "2023")

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
write.csv(AYNA_breeding_success, file = "AYNA_breeding_success.csv", row.names = FALSE)
write.csv(AYNA_all_clean, file = "AYNA_all_clean.csv", row.names = FALSE)
write.csv(AYNA_final, file = "AYNA_final.csv", row.names = FALSE)

