# ======================================================================================
# Function to produce AIC values - nest level breeding success with retained predictors
# ======================================================================================

# Comparing the three windows per breeding response and one climate variable 
all_candidate_windows_glmer_adjusted <- function(climate_window,
                                                 nest_data,
                                                 response_name,
                                                 climate_driver, 
                                                 add_predictor_data = NULL,
                                                 add_predictor_names = NULL) {
  
  # Making Year consistent across all datasets
  climate_window <- climate_window %>%
    mutate(Year = factor(Year))
  
  nest_data <- nest_data %>%
    mutate(Year = factor(Year))
  
  if (!is.null(add_predictor_data)) {
    add_predictor_data <- add_predictor_data %>%
      mutate(Year = factor(Year))}
  
  # Filtering out irrelevant data to produce a dataset required for each model for the current climate driver 
  model_data <- climate_window %>%
    filter(response == response_name) %>%
    select(Year, window, all_of(climate_driver)) %>%
    left_join(nest_data, by = "Year", relationship = "many-to-many") %>%
    drop_na(outcome, all_of(climate_driver)) %>%
    mutate(Year = factor(Year))
  
  # If an earlier predictor is retained in the model then this code will come into use: 
  if (!is.null(add_predictor_data) &&
      !is.null(add_predictor_names)) {
    model_data <- model_data %>%
      left_join(add_predictor_data, by = "Year") %>%
      drop_na(all_of(add_predictor_names)) }
  
  # Creating the dataset used by the null model
  null_data <- model_data %>%
    distinct(Nest_label, Year, .keep_all = TRUE)
  
  # This is the baseline model - if there is not a retained predictor, then use the null model. However, if a 
  # predictor is retained, then code below is used for multiple predictors
  if (is.null(add_predictor_names)) {
    base_formula <- outcome ~ 1 + (1|Year) }
  else {base_formula <- as.formula(paste0("outcome ~ ", paste(add_predictor_names, collapse = " + "),"+ (1 | Year)"))}
  
  # Fitting the baseline model using a binomial GLMM
  base_model <- glmer(base_formula, family = binomial, data = null_data)
  
  # Reformatting the null model results so it can be inserted into the windows table later 
  base_results <- tibble(response = response_name,
                         climate_driver = climate_driver,
                         window = "baseline",
                         AIC = AIC(base_model),
                         beta = NA_real_)
  
  # Separating the climate data into the candidate windows (e.g. early, late and full incubation/ chick rearing)
  climate_results <- model_data %>%
    split(.$window) %>%
    imap_dfr(\(indiv_window, window_name){
      
      # Standardising the predictors
      indiv_window <- indiv_window %>%
        mutate(climate_z = as.numeric(scale(.data[[climate_driver]])))
      
      # Determining which model to use if multiple predictors are required
      if (is.null(add_predictor_names)) {
        climate_formula <- outcome ~ climate_z + (1 | Year)} 
      else { climate_formula <- as.formula(paste0("outcome ~ ", paste(add_predictor_names, collapse = " + "), "+ climate_z + (1 | Year)"))}
      
      # Fitting one model for each candidate window
      model <- glmer(climate_formula, family = binomial, data = indiv_window)
    
      # Extracting the model details and AIC
      tibble(response = response_name,
             climate_driver = climate_driver,
             window = window_name,
             AIC = AIC(model),
             beta = unname(lme4::fixef(model)["climate_z"]))})
  
# Combining the climate and null model results into one table 
results <- bind_rows(base_results, climate_results) %>%
  arrange(AIC) %>%
  mutate(
    # delta_AIC - identifies which candidate model is best overall
    delta_AIC = AIC - min(AIC),
    # AIC_minus_null - quantifies how much better/worse the model is compared to the null baseline
    AIC_minus_baseline = AIC - base_results$AIC)

return(results)}

