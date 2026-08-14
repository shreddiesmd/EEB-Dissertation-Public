# ===============================================================================================================
#                                           CLIMATE PRELIMINARY MANIPULATION
# ===============================================================================================================

rm(list=ls())

# ===========================
# Adding packages to library
# ===========================

library(readxl);library(dplyr);library(tidyr);library(stringr);library(lubridate);library(ggplot2);library(zoo);library(purrr)

# ============================
# Uploading required datasets 
# ============================

climate_final <- read.csv("climate_final.csv")

# ===============================================================
# Temperature trace across all 25 years to compare av daily temp
# ===============================================================

# Converting date column
climate_final$date <- as.Date(climate_final$date)

# Filtering to include only relevant years
daily_temp_filtered <- climate_final %>%
  filter(date >= as.Date("2002-01-01"),
         date <= as.Date("2024-12-31")) %>%
  select(date, avg_temp)

ggplot(daily_temp_filtered, aes(x = date, y = avg_temp))+
  geom_line(colour = "cyan4")+
  theme_minimal()+
  labs(x = "Year",
       y = "Average Daily Temperature (\u00B0C)")+
  scale_x_date(date_labels = "%Y", date_breaks = "1 year")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ======================================================
# Heat map across all 25 years to compare av daily temp - test
# ======================================================

# separating the year and date and setting July as beginning of breeding year
temp_test <- climate_final %>%
  mutate(year = year(date),
         month = month(date),
         start_year = if_else(month >= 7, year, year - 1),
         day_of_year = as.integer(date - ymd(paste0(start_year, "-07-01"))) + 1) %>%
  select(date, avg_temp, year, month, start_year, day_of_year)

# heat map to show average daily temperature on Gough between 2000-2025

month_breaks <- c(1, 32, 63, 94, 124, 155, 185, 213, 244, 274, 305, 335)

month_labels <- c("Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
                  "Jan", "Feb", "Mar", "Apr", "May", "Jun")

ggplot(temp_test, aes(x = day_of_year, y = year, fill = avg_temp)) +
  geom_tile(color = "gray1") +
  scale_fill_viridis_c() +
  scale_x_continuous(breaks = month_breaks,
                     labels = month_labels) +
  theme_minimal() +
  labs(title = "Daily Temperature Heatmap",
       x = "Month",
       y = "Year",
       fill = "Temperature")

# ==========================================
# Monthly precipitation across all 25 years 
# ==========================================

# Filtering to include only relevant years
daily_precip_filtered <- climate_final %>%
  filter(date >= as.Date("2002-01-01"),
         date <= as.Date("2024-12-31")) %>% 
  select(date, precip)
         

# filtering to include only months Aug-Mar 
monthly_precip <- daily_precip_filtered%>%
  filter(month(date) %in% c(8:12,1:3))%>%
  mutate(month= floor_date(date, unit = "month"))%>%
  group_by(month) %>%
  summarise(mean_precip = mean(precip, na.rm=TRUE),
            .groups = "drop")

# the Aug - Mar monthly precip plot 
ggplot(monthly_precip, aes(x = month, y = mean_precip)) +
  geom_bar(stat = "identity", fill = "cyan4") +
  theme_minimal() +
  labs(x = "Breeding Season (Aug-Mar)",
       y = "Monthly Precipitation (mm)") +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))