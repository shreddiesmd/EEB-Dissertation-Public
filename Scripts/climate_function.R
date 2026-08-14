# Function to extract climate window summaries - for code in Sliding Window Analysis SGPE/AYNA.R files 

climate_window_summaries <- function(climate, start_date, end_date) {
  summary_data <- climate %>%
    filter(date >= start_date & date <= end_date) %>%
    summarise(n_days_expected = as.integer(end_date - start_date) + 1,
              n_days_available = n(),
              mean_precip = mean(precip, na.rm = TRUE),
              mean_temp = mean(avg_temp, na.rm = TRUE),
              mean_wind_speed = mean(mean_wind_speed, na.rm = TRUE))
  
  return(as.list(summary_data))}
