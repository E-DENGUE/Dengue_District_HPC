


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



summary <- summary %>% 
  filter(modN != 'mod4_')   ##mod4 here is the baseline 


# These were calcualted fron crps
weights <- data.frame(
  horizon = c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3),
  modN = c("mod3_", "modhhh4_power_precip_temp_", "mod2_", "PC_lags", "mod1_",
           "mod3_", "mod2_", "modhhh4_power_precip_temp_", "PC_lags", "mod1_",
           "mod3_", "mod2_", "modhhh4_power_precip_temp_", "PC_lags", "mod1_"),
  w_i2 = c(0.243, 0.221, 0.212, 0.196, 0.128, 
           0.253, 0.207, 0.203, 0.182, 0.156, 
           0.263, 0.203, 0.189, 0.174, 0.171)  ## These values obtained from 05_evaluate_forecasts.rds for te weight of each of thr enemble models 
)

# Sample 10000 samples for each modN based on the weights
sampled_data <- summary %>%
  left_join(weights, by = c("horizon", "modN")) %>%
  group_by(district, date, horizon, modN) %>%
  sample_n(size = round(unique(w_i2) * 100000), replace = TRUE) %>%
  ungroup()


saveRDS(sampled_data,'sampled_data_all_date.rds')

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


### Find bias and sharpness for the ensemble 

pred.iter <-  sampled_data %>%
  left_join(obs_epidemics, by=c('date','district')) %>%
  mutate(pred_epidemic = value < log(m_DHF_cases / pop * 100000),
         vintage_date=vintage_date) %>%
  group_by(date,vintage_date, district, horizon) %>%
  summarize( prob_pred_epidemic = mean(pred_epidemic)
  )

pred.iter$bias <- 1 - 2 * pred.iter$prob_pred_epidemic

ensemble_bais<- pred.iter %>% group_by(horizon) %>% summarise(bias=mean (bias))
ensemble_bais

sharpness_by_district <- sampled_data %>%
  left_join(obs_epidemics, by=c('date','district'))%>%
  mutate( vintage_date=vintage_date)%>%
  group_by(date,vintage_date, district, horizon) %>%
  summarize(
    sharpness = {
      
      abs_deviations <- abs(value - median(value, na.rm = TRUE))
      
      mad <- mad(value, na.rm = TRUE)
      
      
      (1 / 0.675) * mad
    }
  )

pred.iter$Sharpness<-  sharpness_by_district$sharpness
ensemble_sharpness<- pred.iter %>% group_by(horizon) %>% summarise(sharpness=mean (Sharpness))
ensemble_sharpness

bias.out <- cbind.data.frame('date'=pred.iter$date, 'modN'=modN,'district'=pred.iter$district, 'horizon'=pred.iter$horizon, 'sharpness'=pred.iter$Sharpness, 'bias'=pred.iter$bias)


###Brier score for ensemble 

obs_epidemics<- inner_join(obs_case,obs_epidemics,by=c("district"="district","date"="date"))

obs_epidemics$epidemic_fix_150<- ifelse((obs_epidemics$m_DHF_cases/obs_epidemics$pop*100000) >(150),1,0)
obs_epidemics$epidemic_fix_300<- ifelse((obs_epidemics$m_DHF_cases/obs_epidemics$pop*100000) >(300),1,0)
obs_epidemics$epidemic_fix_20<- ifelse((obs_epidemics$m_DHF_cases/obs_epidemics$pop*100000) >(20),1,0)
obs_epidemics$epidemic_fix_50<- ifelse((obs_epidemics$m_DHF_cases/obs_epidemics$pop*100000) >(50),1,0)
obs_epidemics$epidemic_fix_100<- ifelse((obs_epidemics$m_DHF_cases/obs_epidemics$pop*100000) >(100),1,0)
obs_epidemics$epidemic_fix_200<- ifelse((obs_epidemics$m_DHF_cases/obs_epidemics$pop*100000) >(200),1,0)



pred.iter <- sampled_data %>%
  left_join(obs_epidemics, by=c('date','district')) %>%
  mutate(pred_epidemic_2sd = value > log( threshold/pop*100000),
         pred_epidemic_nb = value > log( threshold_nb/pop*100000),
         pred_epidemic_poisson = value > log(threshold_poisson / pop * 100000),
         pred_epidemic_quant = value > log(threshold_quant / pop * 100000),
         pred_epidemic_fix_100 = value > log(100),
         pred_epidemic_fix_150 = value > log(150),
         pred_epidemic_fix_300 = value > log(300),
         pred_epidemic_fix_50 = value > log(50),
         pred_epidemic_fix_20 = value > log(20),
         pred_epidemic_fix_200 = value > log(200),
         vintage_date=vintage_date) %>%
  dplyr::group_by(date, vintage_date, district, horizon) %>%
  dplyr::summarize( prob_pred_epidemic_2sd = mean(pred_epidemic_2sd),
                    prob_pred_epidemic_nb= mean(pred_epidemic_nb),
                    prob_pred_epidemic_poisson = mean(pred_epidemic_poisson),
                    prob_pred_epidemic_quant = mean(pred_epidemic_quant),
                    prob_pred_epidemic_fix_100 = mean(pred_epidemic_fix_100),
                    prob_pred_epidemic_fix_150 = mean(pred_epidemic_fix_150),
                    prob_pred_epidemic_fix_300 = mean(pred_epidemic_fix_300),
                    prob_pred_epidemic_fix_50 = mean(pred_epidemic_fix_50),
                    prob_pred_epidemic_fix_20 = mean(pred_epidemic_fix_20),
                    prob_pred_epidemic_fix_200 = mean(pred_epidemic_fix_200),
                    obs_epidemic_2sd=mean(epidemic_flag),
                    obs_epidemic_nb = mean(epidemic_flag_nb),
                    obs_epidemic_quant=mean(epidemic_flag_quant) ,
                    obs_epidemic_poisson=mean(epidemic_flag_poisson),
                    epidemic_fix_100=mean(epidemic_fix_100),
                    epidemic_fix_150=mean(epidemic_fix_150),
                    epidemic_fix_200=mean(epidemic_fix_200),
                    epidemic_fix_20=mean(epidemic_fix_20),
                    epidemic_fix_50=mean(epidemic_fix_50),
                    epidemic_fix_300=mean(epidemic_fix_300)
                    
  )

brier_2sd <- brier_score( pred.iter$obs_epidemic_2sd,pred.iter$prob_pred_epidemic_2sd )
brier_nb <- brier_score( pred.iter$obs_epidemic_nb,pred.iter$prob_pred_epidemic_nb )
brier_poisson <- brier_score( pred.iter$obs_epidemic_poisson,pred.iter$prob_pred_epidemic_poisson )
brier_quant <- brier_score( pred.iter$obs_epidemic_quant,pred.iter$prob_pred_epidemic_quant )
brier_fix_100 <- brier_score( pred.iter$epidemic_fix_100,pred.iter$prob_pred_epidemic_fix_100 )
brier_fix_150 <- brier_score( pred.iter$epidemic_fix_150,pred.iter$prob_pred_epidemic_fix_150 )
brier_fix_200 <- brier_score( pred.iter$epidemic_fix_200,pred.iter$prob_pred_epidemic_fix_200 )
brier_fix_300 <- brier_score( pred.iter$epidemic_fix_300,pred.iter$prob_pred_epidemic_fix_300 )
brier_fix_20 <- brier_score( pred.iter$epidemic_fix_20,pred.iter$prob_pred_epidemic_fix_20 )
brier_fix_50 <- brier_score( pred.iter$epidemic_fix_50,pred.iter$prob_pred_epidemic_fix_50 )


brier.out <- cbind.data.frame('date'=pred.iter$date,'district'=pred.iter$district, 'horizon'=pred.iter$horizon, brier_nb, brier_2sd,brier_poisson, brier_quant,'obs_epidemic_2sd'=pred.iter$obs_epidemic_2sd,'prob_pred_epidemic_2sd'=pred.iter$prob_pred_epidemic_2sd,'prob_pred_epidemic_nb'=pred.iter$prob_pred_epidemic_nb ,'obs_epidemic_nb'=pred.iter$obs_epidemic_nb
                              ,  'obs_epidemic_quant'=pred.iter$obs_epidemic_quant ,
                              'obs_epidemic_poisson'=pred.iter$obs_epidemic_poisson, 'prob_pred_epidemic_poisson'= pred.iter$prob_pred_epidemic_poisson
                              ,'prob_pred_epidemic_quant'=pred.iter$prob_pred_epidemic_quant,
                              'brier_fix_100'=brier_fix_100, 'brier_fix_200'=brier_fix_200,
                              'brier_fix_300'=brier_fix_300,'brier_fix_20'=brier_fix_20,
                              'brier_fix_50'=brier_fix_50,'brier_fix_150'=brier_fix_150,
                              'obs_epidemic_fix_100'=pred.iter$epidemic_fix_100,'prob_pred_epidemic_fix_100'=pred.iter$prob_pred_epidemic_fix_100 ,
                              'obs_epidemic_fix_150'=pred.iter$epidemic_fix_150,'prob_pred_epidemic_fix_150'=pred.iter$prob_pred_epidemic_fix_150, 
                              'obs_epidemic_fix_200'=pred.iter$epidemic_fix_200,'prob_pred_epidemic_fix_200'=pred.iter$prob_pred_epidemic_fix_200, 
                              'obs_epidemic_fix_20'=pred.iter$epidemic_fix_20,'prob_pred_epidemic_fix_20'=pred.iter$prob_pred_epidemic_fix_20 ,
                              'obs_epidemic_fix_50'=pred.iter$epidemic_fix_50,'prob_pred_epidemic_fix_50'=pred.iter$prob_pred_epidemic_fix_50,
                              'obs_epidemic_fix_300'=pred.iter$epidemic_fix_300,'prob_pred_epidemic_fix_300'=pred.iter$prob_pred_epidemic_fix_300 )

saveRDS(brier.out,'brier_out.rds')

##plot brier

b2 <- inner_join(brier.out, obs_case, by = c("district", "date"))

b2$monthN<- month(b2$date)

b3 <- b2 %>% 
  dplyr::group_by(district, horizon) %>%
  dplyr::summarize(brier_nb=mean(brier_nb),
                   brier_2sd =mean(brier_2sd),
                   m_DHF_cases=sum(m_DHF_cases))



b2$year<- year(b2$date)
p2 <- b2 %>%
  dplyr::group_by(horizon,year) %>%
  dplyr::summarize(brier_2sd=mean(brier_2sd)) %>%
  #filter(modN %in% ensemble_mods) %>%
  ggplot( aes(x=year, y=brier_2sd, group=interaction(horizon), color=interaction(horizon)))+
  geom_line()+
  theme_minimal()
ggplotly(p2)



b1_summary <- b2 %>%
  group_by(horizon) %>%
  dplyr::summarize(brier_nb=mean(brier_nb),
                   brier_2sd =mean(brier_2sd))


b1 <- b2 %>%
  mutate(month_name = factor(monthN, levels = 1:12, 
                             labels = c("Jan", "Feb", "Mar", "Apr", "May", 
                                        "Jun", "Jul", "Aug", "Sep", "Oct", 
                                        "Nov", "Dec")))

b1_summary <- b1 %>%
  group_by(month_name, horizon) %>%
  dplyr::summarise(
    brier_2sd=mean(brier_2sd),
    brier_nb=mean(brier_nb),
    .groups = 'drop'
  )



ggplot(b1_summary, aes(x = month_name, y = brier_2sd, color = factor(horizon), group = factor(horizon))) +
  geom_line(aes(linetype = factor(horizon)), size = 1) +
  geom_point(size = 3) +ylim(0, NA) +
  labs(x = "Month of the year", y = "Brier score", color = "Horizon", linetype = "Horizon") +
  theme_minimal() +
  theme(legend.position = "bottom")



###CRPS for the ensemble 

library(dplyr)
library(tidyr)
library(scoringRules)

# Join with obs_case and calculate obs_inc
qq <- left_join(sampled_data, obs_case, by = c('date' = 'date', 'district' = 'district'))

# Calculate observed incidence and exponentiate the value column
qq <- qq %>%
  mutate(obs_inc = m_DHF_cases / pop * 100000,
         value = exp(value)) %>% group_by(district, date, horizon) %>%  mutate(variable = paste0("rep", row_number()))

qq<- qq[,-c(6,7,8,9)] ## just keep values variable 

qq_wider <- qq %>%
  pivot_wider(names_from = variable, values_from = value)


# Convert the pivoted data into a matrix for predictions
pred_inc_matrix <- as.matrix(qq_wider[, -c(1:6)],ncol = 10000)  


obs_inc <- qq_wider$obs_inc


crps_sample(obs_inc[1], matrix(pred_inc_matrix[1, 1:1000], nrow = 1))

results<- c()
for (i in 1: length(obs_inc)){
  results[i]=crps_sample(obs_inc[i], matrix(pred_inc_matrix[i, 1:1000], nrow = 1))
}

CRPS_ens <- cbind(qq_wider, results = results)

CRPS_ens %>% group_by(horizon) %>% summarise(crps=mean(results))
