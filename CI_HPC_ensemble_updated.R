


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
library(reshape2)

options(dplyr.summarise.inform = FALSE)

N_cores <- detectCores()

# Load observed epidemics and case data
obs_epidemics <- readRDS('./Data/observed_alarms.rds') %>% 
  rename(case_vintage = m_DHF_cases) %>%
  select(date, district, case_vintage, starts_with('epidemic_flag'), starts_with('threshold'))

obs_case <- readRDS('./Data/CONFIDENTIAL/Updated_full_data_with_new_boundaries_all_factors_cleaned_lag3_add_rows_full_pop.rds') %>%
  select(date, district, m_DHF_cases, pop)

# File paths
obs_epidemics<- inner_join(obs_case,obs_epidemics,by=c("district"="district","date"="date"))

##Results from spatiotemporal models
file.names1 <- list.files('./Results/Results_spacetime/')
file.names2 <- paste0('./Results/Results_pca/',list.files('./Results/Results_pca'))
file.names3 <- list.files('./Results/Results_hhh4/')


# Function to process each file
process_file1 <- pblapply(file.names1, function(X) {
  d1 <- readRDS(file = paste0('./Results/Results_spacetime/', file.path(X)))
  
  date_pattern <- "\\d{4}-\\d{2}-\\d{2}"
  
  # Find the position of the date pattern in the input string
  date_match <- str_locate(X, date_pattern)
  
  modN <- str_sub(X, end = date_match[,'start'] - 1)
  
  date.test.in <- regmatches(X, regexpr(date_pattern, X))
  
  pred.iter <- d1$log.samps.inc %>%
    reshape2::melt(., id.vars=c('date','district','horizon')) %>%
    mutate(vintage_date=as.Date(date.test.in), #vintage.date-=date when forecast was made (date.test.in-1 month)
           modN=modN,
           form=d1$form)
  
  return(pred.iter)
})




# Process HHH4 model files
process_file_hhh4 <- lapply(file.names3,function(X){
  
  d1 <- readRDS(file=file.path(paste0('./Results/Results_hhh4/',X)))
  date_pattern <- "\\d{4}-\\d{2}-\\d{2}"
  
  # Find the position of the date pattern in the input string
  date_match <- str_locate(X, date_pattern)
  
  modN <- str_sub(X, end = date_match[,'start'] - 1)
  
  # Extract the date from the string using gsub
  date.test.in <- regmatches(X, regexpr(date_pattern, X))
  
  pred.iter <- d1$log.samps.inc %>%
    reshape2::melt(., id.vars=c('date','district','horizon')) %>%
    mutate(vintage_date=as.Date(date.test.in), #vintage.date-=date when forecast was made (date.test.in-1 month)
           modN=modN,
           form=d1$form)
  
  return(pred.iter)
})



 
# Process PCA model files
process_file_pca <- pblapply(file.names2, function(X) {
  d1 <- readRDS(file = file.path(X))
  
  if (grepl("PC_lags_weather", X)) {
    modN <- "PC_lags_weather"
  } else if (grepl("PC_lags", X)) {
    modN <- "PC_lags"
  } else if (grepl("PC_weather", X)) {
    modN <- "PC_weather"
  } else {
    modN <- NA  # Handle other cases if necessary
  }
  
  date_pattern <- "\\d{4}-\\d{2}-\\d{2}"
  # Extract the date from the string using regmatches
  date.test.in <- regmatches(X, regexpr(date_pattern, X))
  pred.iter <- d1$log.samps.inc %>%
    reshape2::melt(., id.vars=c('date','district','horizon')) %>%
    mutate(vintage_date=as.Date(date.test.in), #vintage.date-=date when forecast was made (date.test.in-1 month)
           modN=modN,
           form=d1$form)

  return(pred.iter)
})





# Combine all results and calculate quantiles
summary <-  c(process_file1, process_file_pca,process_file_hhh4) %>% 
  bind_rows() 


saveRDS(summary, './Results/summary.rds')

summary <- summary %>% 
  filter(modN != 'mod4_')


##these were calculated according to CRPS2
weights <- data.frame(
  horizon = c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3),
  modN = c("mod3_", "modhhh4_power_precip_temp_", "mod2_", "PC_lags", "mod1_",
           "mod3_", "mod2_", "modhhh4_power_precip_temp_", "PC_lags", "mod1_",
           "mod3_", "mod2_", "modhhh4_power_precip_temp_", "PC_lags", "mod1_"),
  w_i2 = c(0.243, 0.221, 0.212, 0.196, 0.128, 
           0.253, 0.207, 0.203, 0.182, 0.156, 
           0.263, 0.203, 0.189, 0.174, 0.171)
)

# Sample 1000 samples for each modN based on the weights
sampled_data <- summary %>%
  left_join(weights, by = c("horizon", "modN")) %>%
  group_by(district, date, horizon, modN) %>%
  sample_n(size = round(unique(w_i2) * 10000), replace = TRUE) %>%
  ungroup()



sampled_data<- sampled_data[sampled_data$date<= "2022-12-01",]

sampled_data <- sampled_data%>%
  left_join(obs_case, by = c("date", "district"))
  
  
quantile_summary <-sampled_data %>%
  group_by(date, district, horizon) %>%
  summarise(
    median = quantile(exp(value) * pop / 100000, probs = 0.5),
    lower_CI = quantile(exp(value) * pop / 100000, probs = 0.025),
    upper_CI = quantile(exp(value) * pop / 100000, probs = 0.975),
    .groups = 'drop'
  )

saveRDS(quantile_summary,'quantile_summary_ensemble_updated_all_date.rds')

quantile_summary <- quantile_summary %>%
  mutate(date = as.Date(date))


final_summary <- quantile_summary %>%
  left_join(obs_case, by = c("date", "district")) %>%
  select(date, district, horizon, median, lower_CI, upper_CI, m_DHF_cases, pop)




library(ggplot2)
final_summary<- final_summary %>% filter(horizon=='3')

final_summary_grouped <- final_summary %>%
  group_by(date) %>%
  summarise(
    m_DHF_cases = sum(m_DHF_cases, na.rm = TRUE),
    median = sum(median, na.rm = TRUE),
    pred_lcl = sum(lower_CI, na.rm = TRUE),
    pred_ucl = sum(upper_CI, na.rm = TRUE)
  )

# Create the plot
ggplot(final_summary_grouped, aes(x = date)) +
  geom_line(aes(y = median, color = "Predicted Median"), lwd = 1.2) +  # Median predictions
  geom_ribbon(aes(ymin = pred_lcl, ymax = pred_ucl , fill = "Credible Interval"), alpha = 0.2) +  # CI ribbon
  geom_line(aes(y = m_DHF_cases, color = "Observed Cases"), lwd = 1) +  # Observed m_DHF_cases
  theme_classic() +
  labs(title = "Observed vs Predicted Dengue Cases with Confidence Intervals",
       x = "Date", y = "Cases", color = "Legend", fill = "Legend") +  # Labels for legend
  geom_hline(yintercept = 43.75, linetype = "dashed", color = "gray") +  # Reference line
  scale_color_manual(values = c("Predicted Median" = "#1f78b4", "Observed Cases" = "red")) +  # Custom colors for lines
  scale_fill_manual(values = c("Credible  Interval" = "#a6cee3"))  # Custom color for CI ribbon


