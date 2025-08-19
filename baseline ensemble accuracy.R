library(dplyr)
library(parallel)
library(ggplot2)
library(tidyverse)
library(broom)
library(plotly)
library(viridis)
library(lubridate)
library(pbapply)
library(scoringutils)
library(stringr)
options(dplyr.summarise.inform = FALSE)

N_cores <- detectCores()

obs_epidemics <- readRDS('./Data/observed_alarms.rds') %>%
  rename(case_vintage = m_DHF_cases) %>%
  select(date, district, case_vintage, starts_with('epidemic_flag'), starts_with('threshold'))

obs_case <- readRDS('./Data/CONFIDENTIAL/Updated_full_data_with_new_boundaries_all_factors_cleaned_lag3_add_rows_full_pop.rds') %>%
  select(date, district, m_DHF_cases, pop)

# File paths
obs_epidemics<- inner_join(obs_case,obs_epidemics,by=c("district"="district","date"="date"))

file_names <- list.files('./Results/Baseline/')

outbreak1 <- pblapply(file_names, function(X) {
  d1 <- readRDS(file = paste0('./Results/Baseline/', file.path(X)))
  
  date_pattern <- "\\d{4}-\\d{2}-\\d{2}"
  date_match <- str_locate(X, date_pattern)
  modN <- str_sub(X, end = date_match[,'start'] - 1)
  date_test_in <- regmatches(X, regexpr(date_pattern, X))
  
  pred_iter <- d1$log.samps.inc %>%
    reshape2::melt(id.vars = c('date', 'district', 'horizon')) %>%
    left_join(obs_epidemics, by = c('date', 'district')) %>%
    group_by(date, district, horizon) %>%
    mutate(
      outbreak_predicted_epidemic_2sd = mean(quantile(value, probs = 0.50) > log(threshold / pop * 100000)),
      outbreak_predicted_epidemic_nb = mean(quantile(value, probs = 0.50) > log(threshold_nb / pop * 100000)),
      outbreak_predicted_epidemic_poisson = mean(quantile(value, probs = 0.50) > log(threshold_poisson / pop * 100000)),
      outbreak_predicted_epidemic_quant = mean(quantile(value, probs = 0.50) > log(threshold_quant / pop * 100000)),
      outbreak_predicted_epidemic_fix_100 = mean(quantile(value, probs = 0.50) > log(100)),
      outbreak_predicted_epidemic_fix_150 = mean(quantile(value, probs = 0.50) > log(150)),
      outbreak_predicted_epidemic_fix_300 = mean(quantile(value, probs = 0.50) > log(300)),
      outbreak_predicted_epidemic_fix_50 = mean(quantile(value, probs = 0.50) > log(50)),
      outbreak_predicted_epidemic_fix_20 = mean(quantile(value, probs = 0.50) > log(20)),
      outbreak_predicted_epidemic_fix_200 = mean(quantile(value, probs = 0.50) > log(200)),
      vintage_date = date_test_in,
      obs_epidemic_2sd = mean(epidemic_flag),
      obs_epidemic_nb = mean(epidemic_flag_nb),
      obs_epidemic_quant = mean(epidemic_flag_quant),
      obs_epidemic_poisson = mean(epidemic_flag_poisson)
    )
  
  outbreak_out_baseline <- cbind.data.frame(
    date = pred_iter$date, 
    modN = modN, 
    vintage_date = date_test_in,
    district = pred_iter$district, 
    horizon = pred_iter$horizon,
    outbreak_predicted_epidemic_2sd = pred_iter$outbreak_predicted_epidemic_2sd,
    outbreak_predicted_epidemic_nb = pred_iter$outbreak_predicted_epidemic_nb,
    outbreak_predicted_epidemic_poisson = pred_iter$outbreak_predicted_epidemic_poisson,
    outbreak_predicted_epidemic_quant = pred_iter$outbreak_predicted_epidemic_quant, 
    outbreak_predicted_epidemic_fix_100 = pred_iter$outbreak_predicted_epidemic_fix_100,
    outbreak_predicted_epidemic_fix_150 = pred_iter$outbreak_predicted_epidemic_fix_150, 
    outbreak_predicted_epidemic_fix_300 = pred_iter$outbreak_predicted_epidemic_fix_300,
    outbreak_predicted_epidemic_fix_50 = pred_iter$outbreak_predicted_epidemic_fix_50,
    outbreak_predicted_epidemic_fix_20 = pred_iter$outbreak_predicted_epidemic_fix_20,
    outbreak_predicted_epidemic_fix_200 = pred_iter$outbreak_predicted_epidemic_fix_200,
    obs_epidemic_2sd = pred_iter$obs_epidemic_2sd,
    obs_epidemic_nb = pred_iter$obs_epidemic_nb,
    obs_epidemic_quant = pred_iter$obs_epidemic_quant,
    obs_epidemic_poisson = pred_iter$obs_epidemic_poisson
  )
  
  return(outbreak_out_baseline)
})

outbreak_out_baseline <- do.call(rbind, outbreak1) %>%
  mutate(monthN = month(date)) %>%
  ungroup()

saveRDS(outbreak_out_baseline, "./Results/predicted_outbreak1_50_percentile_baseline.rds")

out_perc50_baseline <- outbreak_out_baseline %>%
  left_join(obs_epidemics, by = c("district" = "district", "date" = "date")) %>%
  mutate(
    Month = lubridate::month(date),
    Year = lubridate::year(date)
  ) %>%
  filter(date <= '2022-12-01')

out_50_baseline <- out_perc50_baseline %>%
  mutate(
    # 2sd model
    hit_2sd = ifelse(outbreak_predicted_epidemic_2sd == 1 & epidemic_flag_quant == 1, 1, 0),
    correct_rejection_2sd = ifelse(outbreak_predicted_epidemic_2sd == 0 & epidemic_flag_quant == 0, 1, 0),
    false_alarm_2sd = ifelse(outbreak_predicted_epidemic_2sd == 1 & epidemic_flag_quant== 0, 1, 0),
    missed_2sd = ifelse(outbreak_predicted_epidemic_2sd == 0 & epidemic_flag_quant == 1, 1, 0),
    
    
    hit_quant = ifelse(outbreak_predicted_epidemic_quant == 1 & epidemic_flag_quant == 1, 1, 0),
    correct_rejection_quant = ifelse(outbreak_predicted_epidemic_quant == 0 & epidemic_flag_quant == 0, 1, 0),
    false_alarm_quant = ifelse(outbreak_predicted_epidemic_quant == 1 & epidemic_flag_quant == 0, 1, 0),
    missed_quant = ifelse(outbreak_predicted_epidemic_quant == 0 & epidemic_flag_quant == 1, 1, 0),
    
    # Poisson model
    hit_poisson = ifelse(outbreak_predicted_epidemic_poisson == 1 & epidemic_flag_quant == 1, 1, 0),
    correct_rejection_poisson = ifelse(outbreak_predicted_epidemic_poisson == 0 & epidemic_flag_quant == 0, 1, 0),
    false_alarm_poisson = ifelse(outbreak_predicted_epidemic_poisson == 1 & epidemic_flag_quant == 0, 1, 0),
    missed_poisson = ifelse(outbreak_predicted_epidemic_poisson == 0 & epidemic_flag_quant == 1, 1, 0),
    
    # Add similar columns for other models
    hit_fix_50 = ifelse(outbreak_predicted_epidemic_fix_50 == 1 & epidemic_flag_quant== 1, 1, 0),
    correct_rejection_fix_50 = ifelse(outbreak_predicted_epidemic_fix_50 == 0 & epidemic_flag_quant== 0, 1, 0),
    false_alarm_fix_50 = ifelse(outbreak_predicted_epidemic_fix_50 == 1 & epidemic_flag_quant == 0, 1, 0),
    missed_fix_50 = ifelse(outbreak_predicted_epidemic_fix_50 == 0 & epidemic_flag_quant == 1, 1, 0),
    
    
    # Add similar columns for other models
    hit_fix_20 = ifelse(outbreak_predicted_epidemic_fix_20 == 1 & epidemic_flag_quant == 1, 1, 0),
    correct_rejection_fix_20 = ifelse(outbreak_predicted_epidemic_fix_20 == 0 & epidemic_flag_quant == 0, 1, 0),
    false_alarm_fix_20 = ifelse(outbreak_predicted_epidemic_fix_20 == 1 & epidemic_flag_quant == 0, 1, 0),
    missed_fix_20 = ifelse(outbreak_predicted_epidemic_fix_20 == 0 & epidemic_flag_quant == 1, 1, 0)
  )


summary_hits <- list(
  "2sd Model" = list(
    hits = sum(out_50_baseline$hit_2sd),
    correct_rejections = sum(out_50_baseline$correct_rejection_2sd),
    false_alarms = sum(out_50_baseline$false_alarm_2sd),
    missed = sum(out_50_baseline$missed_2sd)
  ),
  
  "Quant Model" = list(
    hits = sum(out_50_baseline$hit_quant),
    correct_rejections = sum(out_50_baseline$correct_rejection_quant),
    false_alarms = sum(out_50_baseline$false_alarm_quant),
    missed = sum(out_50_baseline$missed_quant)
  ),
  
  "Poisson Model" = list(
    hits = sum(out_50_baseline$hit_poisson),
    correct_rejections = sum(out_50_baseline$correct_rejection_poisson),
    false_alarms = sum(out_50_baseline$false_alarm_poisson),
    missed = sum(out_50_baseline$missed_poisson)
  ),
  
  "Fixed Threshold 50 Model" = list(
    hits = sum(out_50_baseline$hit_fix_50),
    correct_rejections = sum(out_50_baseline$correct_rejection_fix_50),
    false_alarms = sum(out_50_baseline$false_alarm_fix_50),
    missed = sum(out_50_baseline$missed_fix_50)
  ),
  
  "Fixed Threshold 20 Model" = list(
    hits = sum(out_50_baseline$hit_fix_20),
    correct_rejections = sum(out_50_baseline$correct_rejection_fix_20),
    false_alarms = sum(out_50_baseline$false_alarm_fix_20),
    missed = sum(out_50_baseline$missed_fix_20)
  )
)

# Calculate the summary of hits, correct rejections, false alarms, and missed
summary_hits_baseline <- list(
  "2sd Model" = list(
    hits = sum(out_50_baseline$hit_2sd, na.rm = TRUE),
    correct_rejections = sum(out_50_baseline$correct_rejection_2sd, na.rm = TRUE),
    false_alarms = sum(out_50_baseline$false_alarm_2sd, na.rm = TRUE),
    missed = sum(out_50_baseline$missed_2sd, na.rm = TRUE)
  ),
  
  "Quant Model" = list(
    hits = sum(out_50_baseline$hit_quant, na.rm = TRUE),
    correct_rejections = sum(out_50_baseline$correct_rejection_quant, na.rm = TRUE),
    false_alarms = sum(out_50_baseline$false_alarm_quant, na.rm = TRUE),
    missed = sum(out_50_baseline$missed_quant, na.rm = TRUE)
  ),
  
  "Poisson Model" = list(
    hits = sum(out_50_baseline$hit_poisson, na.rm = TRUE),
    correct_rejections = sum(out_50_baseline$correct_rejection_poisson, na.rm = TRUE),
    false_alarms = sum(out_50_baseline$false_alarm_poisson, na.rm = TRUE),
    missed = sum(out_50_baseline$missed_poisson, na.rm = TRUE)
  ),
  
  "Fixed Threshold 50 Model" = list(
    hits = sum(out_50_baseline$hit_fix_50, na.rm = TRUE),
    correct_rejections = sum(out_50_baseline$correct_rejection_fix_50, na.rm = TRUE),
    false_alarms = sum(out_50_baseline$false_alarm_fix_50, na.rm = TRUE),
    missed = sum(out_50_baseline$missed_fix_50, na.rm = TRUE)
  ),
  
  "Fixed Threshold 20 Model" = list(
    hits = sum(out_50_baseline$hit_fix_20, na.rm = TRUE),
    correct_rejections = sum(out_50_baseline$correct_rejection_fix_20, na.rm = TRUE),
    false_alarms = sum(out_50_baseline$false_alarm_fix_20, na.rm = TRUE),
    missed = sum(out_50_baseline$missed_fix_20, na.rm = TRUE)
  )
)


accuracy_results_baseline <- lapply(summary_hits_baseline, function(model) {
  total_predictions <- model$hits + model$correct_rejections + model$false_alarms + model$missed
  accuracy <- (model$hits + model$correct_rejections) / total_predictions
  round(accuracy, 2) 
})


accuracy_results_baseline

# Initialize a list to store sensitivity, specificity, and PPV for each model
performance_metrics_baseline <- lapply(summary_hits_baseline, function(model) {
  hits <- model$hits
  correct_rejections <- model$correct_rejections
  false_alarms <- model$false_alarms
  missed <- model$missed
  
  # Calculate totals
  total_positive <- hits + missed
  total_negative <- correct_rejections + false_alarms
  
  # Calculate sensitivity, specificity, and PPV
  sensitivity <- ifelse(total_positive > 0, hits / total_positive, NA)
  specificity <- ifelse(total_negative > 0, correct_rejections / total_negative, NA)
  ppv <- ifelse((hits + false_alarms) > 0, hits / (hits + false_alarms), NA)
  
  # Return as a list
  list(
    Sensitivity = round(sensitivity, 2),
    Specificity = round(specificity, 2),
    PPV = round(ppv, 2)
  )
})

performance_df_baseline <- do.call(rbind, lapply(names(performance_metrics), function(model_name) {
  c(Model = model_name, performance_metrics[[model_name]])
}))

performance_df_baseline <- as.data.frame(performance_df_baseline, stringsAsFactors = FALSE)
performance_df_baseline 




#########For ensemble 
library(dplyr)
library(parallel)
library(ggplot2)
library(tidyverse)
library(plotly)
library(viridis)
library(lubridate)
library(pbapply)
library(scoringutils)
library(stringr)
library(reshape2)

options(dplyr.summarise.inform = FALSE)

N_cores <- detectCores()


obs_epidemics <- readRDS('./Data/observed_alarms.rds') %>%
  rename(case_vintage = m_DHF_cases) %>%
  select(date, district, case_vintage, starts_with('epidemic_flag'), starts_with('threshold'))

obs_case <- readRDS('./Data/CONFIDENTIAL/Updated_full_data_with_new_boundaries_all_factors_cleaned_lag3.rds') %>%
  select(date, district, m_DHF_cases, pop)

obs_epidemics <- inner_join(obs_case, obs_epidemics, by = c("district", "date"))

d1 <- readRDS('./Results/sampled_data_all_date.rds')  # ensemble draws 

# Process Predictions
pred_iter <- d1 %>%
  left_join(obs_epidemics, by = c('date', 'district')) %>%
  group_by(date, district, horizon) %>%
  mutate(
    outbreak_predicted_epidemic_2sd = mean(quantile(value, probs = 0.50) > log(threshold / pop * 100000)),
    outbreak_predicted_epidemic_nb = mean(quantile(value, probs = 0.50) > log(threshold_nb / pop * 100000)),
    outbreak_predicted_epidemic_poisson = mean(quantile(value, probs = 0.50) > log(threshold_poisson / pop * 100000)),
    outbreak_predicted_epidemic_quant = mean(quantile(value, probs = 0.50) > log(threshold_quant / pop * 100000)),
    outbreak_predicted_epidemic_fix_100 = mean(quantile(value, probs = 0.50) > log(100)),
    outbreak_predicted_epidemic_fix_150 = mean(quantile(value, probs = 0.50) > log(150)),
    outbreak_predicted_epidemic_fix_300 = mean(quantile(value, probs = 0.50) > log(300)),
    outbreak_predicted_epidemic_fix_50 = mean(quantile(value, probs = 0.50) > log(50)),
    outbreak_predicted_epidemic_fix_20 = mean(quantile(value, probs = 0.50) > log(20)),
    outbreak_predicted_epidemic_fix_200 = mean(quantile(value, probs = 0.50) > log(200)),
    vintage_date = date_test_in,
    obs_epidemic_2sd = mean(epidemic_flag),
    obs_epidemic_nb = mean(epidemic_flag_nb),
    obs_epidemic_quant = mean(epidemic_flag_quant),
    obs_epidemic_poisson = mean(epidemic_flag_poisson)
  )

outbreak_out_ensemble <- cbind.data.frame(
  date = pred_iter$date, 
  district = pred_iter$district, 
  horizon = pred_iter$horizon,
  outbreak_predicted_epidemic_2sd = pred_iter$outbreak_predicted_epidemic_2sd,
  outbreak_predicted_epidemic_nb = pred_iter$outbreak_predicted_epidemic_nb,
  outbreak_predicted_epidemic_poisson = pred_iter$outbreak_predicted_epidemic_poisson,
  outbreak_predicted_epidemic_quant = pred_iter$outbreak_predicted_epidemic_quant, 
  outbreak_predicted_epidemic_fix_100 = pred_iter$outbreak_predicted_epidemic_fix_100,
  outbreak_predicted_epidemic_fix_150 = pred_iter$outbreak_predicted_epidemic_fix_150, 
  outbreak_predicted_epidemic_fix_300 = pred_iter$outbreak_predicted_epidemic_fix_300,
  outbreak_predicted_epidemic_fix_50 = pred_iter$outbreak_predicted_epidemic_fix_50,
  outbreak_predicted_epidemic_fix_20 = pred_iter$outbreak_predicted_epidemic_fix_20,
  outbreak_predicted_epidemic_fix_200 = pred_iter$outbreak_predicted_epidemic_fix_200,
  obs_epidemic_2sd = pred_iter$obs_epidemic_2sd,
  obs_epidemic_nb = pred_iter$obs_epidemic_nb,
  obs_epidemic_quant = pred_iter$obs_epidemic_quant,
  obs_epidemic_poisson = pred_iter$obs_epidemic_poisson
)

saveRDS(outbreak_out_ensemble, "./Results/predicted_outbreak_50_percentile_ensemble.rds")


out_perc50_ensemble <- outbreak_out_ensemble %>%
  left_join(obs_epidemics, by = c("district", "date")) %>%
  mutate(Month = month(date), Year = year(date)) %>%
  filter(date <= '2022-12-01')


out_50_ensemble <- out_perc50_ensemble %>%
  mutate(
    # 2sd model
    hit_2sd = ifelse(outbreak_predicted_epidemic_2sd == 1 & epidemic_flag_quant == 1, 1, 0),
    correct_rejection_2sd = ifelse(outbreak_predicted_epidemic_2sd == 0 & epidemic_flag_quant == 0, 1, 0),
    false_alarm_2sd = ifelse(outbreak_predicted_epidemic_2sd == 1 & epidemic_flag_quant== 0, 1, 0),
    missed_2sd = ifelse(outbreak_predicted_epidemic_2sd == 0 & epidemic_flag_quant == 1, 1, 0),
    
    
    hit_quant = ifelse(outbreak_predicted_epidemic_quant == 1 & epidemic_flag_quant == 1, 1, 0),
    correct_rejection_quant = ifelse(outbreak_predicted_epidemic_quant == 0 & epidemic_flag_quant == 0, 1, 0),
    false_alarm_quant = ifelse(outbreak_predicted_epidemic_quant == 1 & epidemic_flag_quant == 0, 1, 0),
    missed_quant = ifelse(outbreak_predicted_epidemic_quant == 0 & epidemic_flag_quant == 1, 1, 0),
    
    # Poisson model
    hit_poisson = ifelse(outbreak_predicted_epidemic_poisson == 1 & epidemic_flag_quant == 1, 1, 0),
    correct_rejection_poisson = ifelse(outbreak_predicted_epidemic_poisson == 0 & epidemic_flag_quant == 0, 1, 0),
    false_alarm_poisson = ifelse(outbreak_predicted_epidemic_poisson == 1 & epidemic_flag_quant == 0, 1, 0),
    missed_poisson = ifelse(outbreak_predicted_epidemic_poisson == 0 & epidemic_flag_quant == 1, 1, 0),
    
    # Add similar columns for other models
    hit_fix_50 = ifelse(outbreak_predicted_epidemic_fix_50 == 1 & epidemic_flag_quant== 1, 1, 0),
    correct_rejection_fix_50 = ifelse(outbreak_predicted_epidemic_fix_50 == 0 & epidemic_flag_quant== 0, 1, 0),
    false_alarm_fix_50 = ifelse(outbreak_predicted_epidemic_fix_50 == 1 & epidemic_flag_quant == 0, 1, 0),
    missed_fix_50 = ifelse(outbreak_predicted_epidemic_fix_50 == 0 & epidemic_flag_quant == 1, 1, 0),
    
    
    # Add similar columns for other models
    hit_fix_20 = ifelse(outbreak_predicted_epidemic_fix_20 == 1 & epidemic_flag_quant == 1, 1, 0),
    correct_rejection_fix_20 = ifelse(outbreak_predicted_epidemic_fix_20 == 0 & epidemic_flag_quant == 0, 1, 0),
    false_alarm_fix_20 = ifelse(outbreak_predicted_epidemic_fix_20 == 1 & epidemic_flag_quant == 0, 1, 0),
    missed_fix_20 = ifelse(outbreak_predicted_epidemic_fix_20 == 0 & epidemic_flag_quant == 1, 1, 0)
  )


summary_hits_ensemble <- list(
  "2sd Model" = list(
    hits = sum(out_50_ensemble$hit_2sd),
    correct_rejections = sum(out_50_ensemble$correct_rejection_2sd),
    false_alarms = sum(out_50_ensemble$false_alarm_2sd),
    missed = sum(out_50_ensemble$missed_2sd)
  ),
  
  "Quant Model" = list(
    hits = sum(out_50_ensemble$hit_quant),
    correct_rejections = sum(out_50_ensemble$correct_rejection_quant),
    false_alarms = sum(out_50_ensemble$false_alarm_quant),
    missed = sum(out_50_ensemble$missed_quant)
  ),
  
  "Poisson Model" = list(
    hits = sum(out_50_ensemble$hit_poisson),
    correct_rejections = sum(out_50_ensemble$correct_rejection_poisson),
    false_alarms = sum(out_50_ensemble$false_alarm_poisson),
    missed = sum(out_50_ensemble$missed_poisson)
  ),
  
  "Fixed Threshold 50 Model" = list(
    hits = sum(out_50_ensemble$hit_fix_50),
    correct_rejections = sum(out_50_ensemble$correct_rejection_fix_50),
    false_alarms = sum(out_50_ensemble$false_alarm_fix_50),
    missed = sum(out_50_ensemble$missed_fix_50)
  ),
  
  "Fixed Threshold 20 Model" = list(
    hits = sum(out_50_ensemble$hit_fix_20),
    correct_rejections = sum(out_50_ensemble$correct_rejection_fix_20),
    false_alarms = sum(out_50_ensemble$false_alarm_fix_20),
    missed = sum(out_50_baseline$missed_fix_20)
  )
)

# Calculate the summary of hits, correct rejections, false alarms, and missed
summary_hits_ensemble <- list(
  "2sd Model" = list(
    hits = sum(out_50_baseline$hit_2sd, na.rm = TRUE),
    correct_rejections = sum(out_50_baseline$correct_rejection_2sd, na.rm = TRUE),
    false_alarms = sum(out_50_baseline$false_alarm_2sd, na.rm = TRUE),
    missed = sum(out_50_baseline$missed_2sd, na.rm = TRUE)
  ),
  
  "Quant Model" = list(
    hits = sum(out_50_baseline$hit_quant, na.rm = TRUE),
    correct_rejections = sum(out_50_baseline$correct_rejection_quant, na.rm = TRUE),
    false_alarms = sum(out_50_baseline$false_alarm_quant, na.rm = TRUE),
    missed = sum(out_50_baseline$missed_quant, na.rm = TRUE)
  ),
  
  "Poisson Model" = list(
    hits = sum(out_50_baseline$hit_poisson, na.rm = TRUE),
    correct_rejections = sum(out_50_baseline$correct_rejection_poisson, na.rm = TRUE),
    false_alarms = sum(out_50_baseline$false_alarm_poisson, na.rm = TRUE),
    missed = sum(out_50_baseline$missed_poisson, na.rm = TRUE)
  ),
  
  "Fixed Threshold 50 Model" = list(
    hits = sum(out_50_baseline$hit_fix_50, na.rm = TRUE),
    correct_rejections = sum(out_50_baseline$correct_rejection_fix_50, na.rm = TRUE),
    false_alarms = sum(out_50_baseline$false_alarm_fix_50, na.rm = TRUE),
    missed = sum(out_50_baseline$missed_fix_50, na.rm = TRUE)
  ),
  
  "Fixed Threshold 20 Model" = list(
    hits = sum(out_50_baseline$hit_fix_20, na.rm = TRUE),
    correct_rejections = sum(out_50_baseline$correct_rejection_fix_20, na.rm = TRUE),
    false_alarms = sum(out_50_baseline$false_alarm_fix_20, na.rm = TRUE),
    missed = sum(out_50_baseline$missed_fix_20, na.rm = TRUE)
  )
)


accuracy_results_ensemble <- lapply(summary_hits_ensemble, function(model) {
  total_predictions <- model$hits + model$correct_rejections + model$false_alarms + model$missed
  accuracy <- (model$hits + model$correct_rejections) / total_predictions
  round(accuracy, 2) 
})


accuracy_results_ensemble

# Initialize a list to store sensitivity, specificity, and PPV for each model
performance_metrics_ensemble <- lapply(summary_hits, function(model) {
  hits <- model$hits
  correct_rejections <- model$correct_rejections
  false_alarms <- model$false_alarms
  missed <- model$missed
  
  # Calculate totals
  total_positive <- hits + missed
  total_negative <- correct_rejections + false_alarms
  
  # Calculate sensitivity, specificity, and PPV
  sensitivity <- ifelse(total_positive > 0, hits / total_positive, NA)
  specificity <- ifelse(total_negative > 0, correct_rejections / total_negative, NA)
  ppv <- ifelse((hits + false_alarms) > 0, hits / (hits + false_alarms), NA)
  
  # Return as a list
  list(
    Sensitivity = round(sensitivity, 2),
    Specificity = round(specificity, 2),
    PPV = round(ppv, 2)
  )
})

performance_df_ensemble <- do.call(rbind, lapply(names(performance_metrics_ensemble), function(model_name) {
  c(Model = model_name, performance_metrics[[model_name]])
}))

performance_df_ensemble<- as.data.frame(performance_df_ensemble, stringsAsFactors = FALSE)
performance_df_ensemble