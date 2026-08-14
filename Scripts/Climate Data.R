# ================================================================================================================
#                                      CLIMATE DATASET CLEANING AND FORMATTING
# ================================================================================================================

rm(list=ls())

# ===========================
# Adding packages to library
# ===========================

library(readxl);library(dplyr);library(tidyr);library(stringr);library(lubridate);library(openxlsx);library(purrr)

# ===================================================
# Importing and cleaning the max daily temp data set
# ===================================================

# uploading the temp datasheet 
raw_max_temp <- read_excel("Daily Max Temp.xlsx", sheet = 1, col_names = FALSE)

# identifying the rows which mark the beginning of each year block
year_rows <- which(str_detect(raw_max_temp[[1]], "Daily Maximum Temperature"))

# converting one year block to long format 
temp_year <- function(start_row, end_row, data){
  # extracting title row and converting to raw text
  title_row <- as.character(data[start_row,1]) 
  # extracting year from title text
  year <- str_extract(title_row, "(19|20)\\d{2}") %>% 
    as.numeric()
  # isolating temp table block
  block <- data[(start_row + 3):(end_row -2), 1:13] 
  # renaming columns in block
  colnames(block) <- c("day", "JAN", "FEB", "MAR", "APR", "MAY", "JUN",
                       "JUL", "AUG", "SEP", "OCT", "NOV", "DEC") 
  # reshaping the block to long format
  long_block <- block %>% 
    # converting month columns to rows 
    pivot_longer(cols = JAN:DEC,
                 names_to = "month",
                 values_to = "max_temp") %>%
    # removing rows where day contains non-numbers
    filter(str_detect(day, "^\\d+$")) %>%
    # converting month names to numbers 
    mutate(day = as.integer(day),
           month_num = match(month,
                             c("JAN", "FEB", "MAR", "APR", "MAY", "JUN",
                               "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")),
           # replacing missing values with NA 
           max_temp = na_if(max_temp, "***"),
           max_temp = as.numeric(max_temp),
           # formatting dates to YYYY-MM-DD
           date = make_date(year, month_num, day)) %>%
    select(date, max_temp) %>%
    # removing missing temp values
    filter(!is.na(date),
           !is.na(max_temp))
  # returning the cleaned year data
  return(long_block)}

## applying to all the year blocks in data set

# creating empty list for yearly data sets
all_years <- list() 

# loop to apply to all 25 blocks of data
for(i in seq_along(year_rows)){
  start_row <- year_rows[i]
  end_row <- if(i < length(year_rows)){
    year_rows[i + 1]-1
  } else {nrow(raw_max_temp)}
  
  # running function and saving to list
  all_years[[i]] <- temp_year(start_row,
                              end_row,
                              raw_max_temp)}
# ------------------------------------
# CREATING THE FINAL DATA FRAME
daily_max_temp <- bind_rows(all_years)
# ------------------------------------

# ===================================================
# Importing and cleaning the min daily temp data set
# ===================================================

# uploading the temp datasheet 
raw_min_temp <- read_excel("Daily Min Temp.xlsx", sheet = 1, col_names = FALSE)

# identifying the rows which mark the beginning of each year block
year_rows <- which(str_detect(raw_min_temp[[1]], "Daily Minimum Temperature"))

# converting one year block to long format 
temp_year <- function(start_row, end_row, data){
  # extracting title row and converting to raw text
  title_row <- as.character(data[start_row,1])
  # extracting year from title text
  year <- str_extract(title_row, "(19|20\\d{2})") %>%
    as.numeric()
  # isolating the temp table block 
  block <- data[(start_row + 3):(end_row -2), 1:13]
  #renaming columns in the block 
  colnames(block) <- c("day", "JAN", "FEB", "MAR", "APR", "MAY", "JUN",
                       "JUL", "AUG", "SEP", "OCT", "NOV", "DEC") 
  # reshaping the block to long format
  long_block <- block %>% 
    # converting month columns to rows 
    pivot_longer(cols = JAN:DEC,
                 names_to = "month",
                 values_to = "min_temp") %>%
    # removing rows where day contains non-numbers
    filter(str_detect(day, "^\\d+$")) %>%
    # converting month names to numbers 
    mutate(day = as.integer(day),
           month_num = match(month,
                             c("JAN", "FEB", "MAR", "APR", "MAY", "JUN",
                               "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")),
           # replacing missing values with NA 
           min_temp = na_if(min_temp, "***"),
           min_temp = as.numeric(min_temp),
           # formatting dates to YYYY-MM-DD
           date = make_date(year, month_num, day)) %>%
    select(date, min_temp) %>%
    # removing missing temp values
    filter(!is.na(date),
           !is.na(min_temp))
  # returning the cleaned year data
  return(long_block)}

## applying to all the year blocks in data set

# creating empty list for yearly data sets
all_years <- list() 

# loop to apply to all 25 blocks of data
for(i in seq_along(year_rows)){
  start_row <- year_rows[i]
  end_row <- if(i < length(year_rows)){
    year_rows[i + 1]-1
  } else {nrow(raw_min_temp)}
  
  # running function and saving to list
  all_years[[i]] <- temp_year(start_row,
                              end_row,
                              raw_min_temp)}

# ------------------------------------
# CREATING FINAL DATA FRAME
daily_min_temp <- bind_rows(all_years)
# ------------------------------------

# =======================================================
# Combining max and min for final temperature data frame
# =======================================================

daily_temp <- daily_max_temp %>%
  left_join(daily_min_temp,
            by = "date") %>%
  mutate(avg_temp = (daily_max_temp$max_temp + daily_min_temp$min_temp) /2)

# ==================================================
# Importing and cleaning the precipitation data set
# ==================================================

# uploading the precip datasheet 
raw_precip <- read_excel("Daily Rainfall.xlsx", sheet = 1, col_names = FALSE)

# identifying the rows which mark the beginning of each year block
year_rows <- which(str_detect(raw_precip[[1]], "Daily Rain"))

# converting one year block to long format 
precip_year <- function(start_row, end_row, data){
  # extracting title row and converting to raw text
  title_row <- as.character(data[start_row,1])
  # extracting year from title text
  year <- str_extract(title_row, "(19|20)\\d{2}") %>% 
    as.numeric()
  # isolating precip table block
  block <- data[(start_row + 3):(end_row -2), 1:13] 
  # renaming columns in block
  colnames(block) <- c("day", "JAN", "FEB", "MAR", "APR", "MAY", "JUN",
                       "JUL", "AUG", "SEP", "OCT", "NOV", "DEC") 
  # reshaping the block to long format
  long_block <- block %>% 
    # converting month columns to rows 
    pivot_longer(cols = JAN:DEC,
                 names_to = "month",
                 values_to = "precip") %>%
    # removing rows where day contains non-numbers
    filter(str_detect(day, "^\\d+$")) %>%
    # converting month names to numbers 
    mutate(day = as.integer(day),
           month_num = match(month,
                             c("JAN", "FEB", "MAR", "APR", "MAY", "JUN",
                               "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")),
           # replacing missing values with NA 
           precip = na_if(precip, "***"),
           precip = as.numeric(precip),
           # formatting dates to YYYY-MM-DD
           date = make_date(year, month_num, day)) %>%
    select(date, precip) %>%
    # removing missing precip values
    filter(!is.na(date),
           !is.na(precip))
  # returning the cleaned year data
  return(long_block)}

## applying to all the year blocks in data set

# creating empty list for yearly data sets
all_years <- list() 

# loop to apply to all 25 blocks of data
for(i in seq_along(year_rows)){
  start_row <- year_rows[i]
  end_row <- if(i < length(year_rows)){
    year_rows[i + 1]-1
  } else {nrow(raw_precip)}
  
  # running function and saving to list
  all_years[[i]] <- precip_year(start_row,
                                end_row,
                                raw_precip)}
# ----------------------------------
# CREATING THE FINAL DATA FRAME 
daily_precip <- bind_rows(all_years)
# ----------------------------------

# =============================================================
# Importing and cleaning the daily average wind speed data set
# =============================================================

# uploading the wind speed datasheet
raw_wind_speed <- read_excel("Wind Speed.xlsx", sheet = 1, col_names= FALSE)

# identifying rows which mark the beginning of each year block 
year_rows <- which(str_detect(raw_wind_speed[[1]], "Average Wind Speed"))

# converting one year block to long format
wind_year <- function(start_row, end_row, data){
  # extracting title row and converting to raw text
  title_row <- as.character (data[start_row, 1])
  # extracting year from title text 
  year <- str_extract(title_row, "(19|20)\\d{2}") %>%
    as.numeric()
  # extracting the time from title text
  time <- str_extract(title_row, "(08|14|20):00")
  # isolating the wind speed table block
  block <- data[(start_row + 3):(end_row -2), 1:13]
  # renaming columns in block
  colnames(block) <- c("day", "JAN", "FEB", "MAR", "APR", "MAY", "JUN",
                       "JUL", "AUG", "SEP", "OCT", "NOV", "DEC") 
  # reshaping block to long format
  long_block <- block %>%
    # converting month columns to rows 
    pivot_longer(cols = JAN:DEC,
                 names_to = "month",
                 values_to = "wind_speed") %>%
    # removing rows where day contains non-numbers
    filter(str_detect(day, "^\\d+$")) %>%
    # converting month names to numbers 
    mutate( day = as.integer(day), 
            month_num = match(month,
                              c("JAN", "FEB", "MAR", "APR", "MAY", "JUN",
                                "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")),
            # replacing missing values with NA
            wind_speed = na_if(wind_speed, "***"),
            wind_speed = as.numeric(wind_speed),
            # formatting dates to YYYY-MM-DD
            date = make_date(year, month_num, day), 
            # adding the times wind was recorded 
            time = time) %>%
    select(date, time, wind_speed) %>%
    # removing missing wind speed values
    filter(!is.na(date),
           !is.na(wind_speed))
  # returning the cleaned year and time data 
  return(long_block)}

## applying to all the year blocks in data set

# creating an empty list for yearly data sets
all_years <- list()

# loop to apply to all 25 x 3 blocks of data
for(i in seq_along(year_rows)){
  start_row <- year_rows[i]
  end_row <- if(i < length(year_rows)){
    year_rows[i + 1] - 1
  } else {nrow(raw_wind_speed)}
  
  #running function and saving to list
  all_years[[i]] <- wind_year(start_row,
                              end_row,
                              raw_wind_speed)}

# --------------------------------------------------------------------
# CREATING THE FINAL DATA FRAME 
wind_speed <- bind_rows(all_years)

wind_speed <- wind_speed %>%
  pivot_wider(names_from = time,
              values_from = wind_speed,
              names_prefix = "wind_speed_")

wind_speed <- wind_speed %>%
  mutate(mean_wind_speed = rowMeans(select(., `wind_speed_08:00`,
                                           `wind_speed_14:00`,
                                           `wind_speed_20:00`),
                                    na.rm = TRUE))
# -------------------------------------------------------------------

# =================================================================
# Importing and cleaning the daily average wind direction data set
# =================================================================

# uploading wind direction datasheet
raw_wind_direction <- read_excel("Wind Direction.xlsx", sheet = 1, col_names = FALSE)

# identifying rows which mark the beginning of each year block
year_rows <- which(str_detect(raw_wind_direction[[1]], "Average Wind Direction"))

# converting one year block to long format
wind_year <- function(start_row, end_row, data){
  # extracting title row and converting to raw text
  title_row <- as.character (data[start_row, 1])
  # extracting year from title text 
  year <- str_extract(title_row, "(19|20)\\d{2}") %>%
    as.numeric()
  # extracting the time from title text
  time <- str_extract(title_row, "(08|14|20):00")
  # isolating the wind direction table block
  block <- data[(start_row + 3):(end_row -2), 1:13]
  # renaming columns in block
  colnames(block) <- c("day", "JAN", "FEB", "MAR", "APR", "MAY", "JUN",
                       "JUL", "AUG", "SEP", "OCT", "NOV", "DEC") 
  # reshaping block to long format
  long_block <- block %>%
    # converting month columns to rows 
    pivot_longer(cols = JAN:DEC,
                 names_to = "month",
                 values_to = "wind_direction") %>%
    # removing rows where day contains non-numbers
    filter(str_detect(day, "^\\d+$")) %>%
    # converting month names to numbers 
    mutate( day = as.integer(day), 
            month_num = match(month,
                              c("JAN", "FEB", "MAR", "APR", "MAY", "JUN",
                                "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")),
            # replacing missing values with NA
            wind_direction = na_if(wind_direction, "***"),
            wind_direction = as.numeric(wind_direction),
            # formatting dates to YYYY-MM-DD
            date = make_date(year, month_num, day), 
            # adding the times direction was recorded 
            time = time) %>%
    select(date, time, wind_direction) %>%
    # removing missing wind direction values
    filter(!is.na(date),
           !is.na(wind_direction))
  # returning the cleaned year and time data 
  return(long_block)}

## applying to all the year blocks in data set

# creating an empty list for yearly data sets
all_years <- list()

# loop to apply to all 25 x 3 blocks of data
for(i in seq_along(year_rows)){
  start_row <- year_rows[i]
  end_row <- if(i < length(year_rows)){
    year_rows[i + 1] - 1
  } else {nrow(raw_wind_direction)}
  
  #running function and saving to list
  all_years[[i]] <- wind_year(start_row,
                              end_row,
                              raw_wind_direction)}

# ------------------------------------------------------------------------
# CREATING THE FINAL DATA FRAME 
wind_direction <- bind_rows(all_years)

wind_direction <- wind_direction %>%
  pivot_wider(names_from = time,
              values_from = wind_direction,
              names_prefix = "wind_direction_")

wind_direction <- wind_direction %>%
  mutate(mean_wind_direction = rowMeans(select(., `wind_direction_08:00`,
                                               `wind_direction_14:00`,
                                               `wind_direction_20:00`),
                                        na.rm = TRUE))
# ------------------------------------------------------------------------

# =================================
# All of the final clean data sets
# =================================

daily_precip
daily_temp
wind_direction # Might not analyse this driver, but included if required later 
wind_speed

climate <- list(daily_precip, daily_temp, wind_direction, wind_speed)
climate_final <- reduce(climate, full_join, by = "date") 

# Exporting table 
write.csv(climate_final, file = "climate_final.csv", row.names = FALSE)
