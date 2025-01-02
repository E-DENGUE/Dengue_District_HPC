library(sads)
library(tidyverse)
library(pROC)
library(ggplot2)

all.baselines <- readRDS('./Data/all_baselines.rds') %>% filter(date <= '2016-12-01') 

obs_case <- readRDS('./Data/CONFIDENTIAL/Updated_full_data_with_new_boundaries_all_factors_cleaned_lag3_add_rows_full_pop.rds') %>%
  dplyr::select(date, district, m_DHF_cases, pop)

grouped_draws<- readRDS('./Results/sampled_data_by_date.rds')

grouped_draws<- left_join(grouped_draws, obs_case,by=c('date','district'))

grouped_draws$value<- exp(grouped_draws$value) * grouped_draws$pop / 100000

grouped_data_obs <- grouped_draws %>% group_by(date, district, horizon) %>% dplyr::summarize( obs= mean(value))

unique_districts <- unique(grouped_draws$district)

district_name = 'AN PHU'  ##check for one district 

run_analysis_for_district <- function(district_name) {
  
  grouped_draws_by_district <- grouped_draws %>%
    filter(district == district_name & horizon == '3' & date <= "2016-11-01")
  
  all.baselines_by_district <- all.baselines %>%
    filter(district == district_name & date >= "2012-04-01" & date <= "2016-11-01")
  
  grouped_data_obs<- grouped_data_obs %>%
    filter(district == district_name & horizon == '3' & date <= "2016-11-01")
  
  historic_log_mean <- all.baselines_by_district$mean_log_baseline
  historic_log_sd <- all.baselines_by_district$sd_log_baseline
  
  set <- seq(1, length(historic_log_mean))
  
  
  
  set.seed(123)
  obs <- data.frame(
    set = 1:length(set),
    obs = (grouped_data_obs$obs))
  
  
  
  gen_samps <- function(set.historic_log_mean, set.historic_log_sd, set.set, grouped_draws_by_district) {

    historic_samp_mu <- rnorm(10000, mean = set.historic_log_mean, sd = set.historic_log_sd)
    historic_samp <- rpois(10000, lambda = exp(historic_samp_mu))
    
    # Select the corresponding forecast samples for the current set
    start_row <- (set.set - 1) * 10000 + 1
    end_row <- set.set * 10000
    forecast_samp <- grouped_draws_by_district$value[start_row:end_row]
    
    # Combine into a data frame
    df <- data.frame(
      set = set.set,
      iter = 1:10000,
      historic_samp = historic_samp,
      forecast_samp = forecast_samp
    )
    
    return(df)
  }
  

  set.seed(123)
  all.samps <- mapply(
    FUN = gen_samps,
    set.historic_log_mean = historic_log_mean,
    set.historic_log_sd = historic_log_sd,
    set.set = 1:length(historic_log_mean), 
    MoreArgs = list(grouped_draws_by_district = grouped_draws_by_district),
    SIMPLIFY = FALSE
  )
  
  # Combine all results into a single data frame
  all.samps.df <- do.call(rbind, all.samps)
  
  
  all.samps.df <- bind_rows(all.samps) %>%
    left_join(obs, by = 'set') %>%
    group_by(set) %>%
    mutate(
      threshold_high = mean(historic_samp) + 1.96 * sd(historic_samp),
      threshold_med = mean(historic_samp) + 1 * sd(historic_samp),
      true_high = 1*(obs>threshold_high),
      true_med = 1*(obs>threshold_med)
    ) %>%
    dplyr::summarize(
      true_high = mean(true_high),
      true_med = mean(true_med),
      obs = mean(obs),
      pred_high = mean(forecast_samp > threshold_high),
      pred_med = mean(forecast_samp > threshold_med)
    )
  
  roc1 <- roc(data = all.samps.df, response = true_high, predictor = pred_high)
  
  roc_data <- data.frame(
    Thresholds = roc1$thresholds,
    Sensitivity = roc1$sensitivities,
    Specificity = roc1$specificities
  )
  
  
  roc_data$CZ <- roc_data$Sensitivity * roc_data$Specificity
  optimal_row <- roc_data[which.max(roc_data$CZ), ]
  
  epidemic_status <- ifelse(all.samps.df$pred_high >= optimal_row$Thresholds, "Epidemic", "Non-Epidemic")
  
  results <- data.frame(
    district = district_name,
    date = unique(grouped_draws_by_district$date),
    set = all.samps.df$set,
    true_high = all.samps.df$true_high,
    true_med = all.samps.df$true_med,
    pred_high = all.samps.df$pred_high,
    pred_med = all.samps.df$pred_med,
    Epidemic_Status_high = epidemic_status
  )
  
  list(roc1 = roc1, results = results, optimal_row = optimal_row)
}


district_results <- lapply(unique_districts, run_analysis_for_district)


combined_results <- bind_rows(lapply(district_results, function(x) x$results))


saveRDS(combined_results, "./Results/district_results_samples.rds")


unique(combined_results$district)



combined_results <- combined_results %>%
  mutate(
    year = lubridate::year(date),  
    Epidemic_Status_high_numeric = ifelse(Epidemic_Status_high == "Epidemic", 1, 0)
  )


combined_results <- combined_results %>%
  mutate(
    year = lubridate::year(date),  # Extract year from date
    Epidemic_Flag = ifelse(Epidemic_Status_high == "Epidemic", 1, 0)
  )

summarized_data <- combined_results %>%
  filter(Epidemic_Flag == 1) %>%  
  group_by(year, district) %>%
  summarize(
    has_epidemic = as.integer(n() > 0),  # Mark as 1 if any epidemic occurs in the year
    .groups = "drop"
  ) %>%
  group_by(year) %>%
  summarize(
    total_epidemics = sum(has_epidemic, na.rm = TRUE),  # Total number of districts with at least one epidemic
    .groups = "drop"
  )


ggplot(summarized_data, aes(x = as.factor(year), y = total_epidemics)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Total Epidemic Districts by Year",
    x = "Year",
    y = "Total Districts with Epidemic"
  ) +
  theme_minimal()



##