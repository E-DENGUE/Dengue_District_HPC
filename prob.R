prob<- readRDS('C:/Users/uqwareed/OneDrive - The University of Queensland/Two months_predictions/HPC_District_monthly_update_lag/Data/cleaned_scores/prob_summary_updated.rds')
filtered_data <- prob %>%
  filter(horizon==2 & modN %in% c('mod6_', 'mod60_', 'mod61_', 'PC_lags', 'modhhh4_power_precip_temp_'))

library(dplyr)

# Assuming filtered_data is your dataframe

# Calculate mean values for each district and date
summary_data <- filtered_data %>%
  dplyr::group_by(district, date) %>%
  dplyr::summarise(
    mean_prob_pred_epidemic_2sd = mean(prob_pred_epidemic_2sd, na.rm = TRUE),
    mean_prob_pred_epidemic_nb = mean(prob_pred_epidemic_nb, na.rm = TRUE),
    mean_prob_epidemic_poisson = mean(prob_epidemic_poisson, na.rm = TRUE),
    mean_prob_epidemic_quant = mean(prob_epidemic_quant, na.rm = TRUE),
    mean_prob_epidemic_fix_100 = mean(prob_epidemic_fix_100, na.rm = TRUE),
    mean_prob_epidemic_fix_150 = mean(prob_epidemic_fix_150, na.rm = TRUE),
    mean_prob_epidemic_fix_300 = mean(prob_epidemic_fix_300, na.rm = TRUE),
    mean_prob_epidemic_fix_50 = mean(prob_epidemic_fix_50, na.rm = TRUE),
    mean_prob_epidemic_fix_20 = mean(prob_epidemic_fix_20, na.rm = TRUE),
    mean_prob_epidemic_fix_200 = mean(prob_epidemic_fix_200, na.rm = TRUE),
  )

# Print the summary data
print(summary_data)

library(sf)
MDR_NEW <- st_read(dsn = "C:/Users/uqwareed/OneDrive - The University of Queensland/Two months_predictions/HPC_District_monthly_update_lag/Data/shapefiles/MDR_NEW_Boundaries_Final.shp") 

# Create a new variable 'District_province' by concatenating 'VARNAME' and 'NAME_En' with an underscore
MDR_NEW <- MDR_NEW %>%
  dplyr::mutate(District_province = paste( VARNAME,NAME_En, sep = " "))

MDR_NEW$VARNAME<- toupper(MDR_NEW$VARNAME)
MDR_NEW$NAME_En<- toupper(MDR_NEW$NAME_En)

MDR_NEW  <- MDR_NEW  %>%
  mutate(VARNAME = ifelse(VARNAME == 'CHAU THANH' & NAME_En == "AN GIANG", 
                          "CHAU THANH AN GIANG", 
                          ifelse(VARNAME == 'CHAU THANH' & NAME_En == "BEN TRE", 
                                 "CHAU THANH BEN TRE", 
                                 ifelse(VARNAME == 'CHAU THANH' & NAME_En == "CA MAU", 
                                        "CHAU THANH CA MAU",
                                        ifelse(VARNAME == 'CHAU THANH' & NAME_En == "DONG THAP", 
                                               "CHAU THANH DONG THAP",
                                               ifelse(VARNAME == 'CHAU THANH' & NAME_En == "HAU GIANG", 
                                                      "CHAU THANH HAU GIANG",
                                                      ifelse(VARNAME == 'CHAU THANH' & NAME_En == "LONG AN", 
                                                             "CHAU THANH LONG AN",
                                                             ifelse(VARNAME == 'CHAU THANH' & NAME_En == "TIEN GIANG", 
                                                                    "CHAU THANH TIEN GIANG",
                                                                    ifelse(VARNAME == 'CHAU THANH' & NAME_En == "TRA VINH", 
                                                                           "CHAU THANH TRA VINH",
                                                                           ifelse(VARNAME == 'PHU TAN' & NAME_En == "CAM MAU", 
                                                                                  "PHU TAN CA MAU",
                                                                                  ifelse(VARNAME == 'PHU TAN' & NAME_En == "AN GIANG", 
                                                                                         "PHU TAN AN GIANG",
                                                                                         as.character(VARNAME)
                                                                                  )
                                                                           )
                                                                    )))))))))




library(ggplot2)


obs_epidemics <- readRDS( 'C:/Users/uqwareed/OneDrive - The University of Queensland/Two months_predictions/HPC_District_monthly_update_lag/Data/observed_alarms.rds') %>% #observed alarms, as flagged in outbreak_quant.R
  rename(case_vintage=m_DHF_cases) %>%
  dplyr::select(date, district,case_vintage, starts_with('epidemic_flag'), starts_with('threshold'))

obs_case <- readRDS('C:/Users/uqwareed/OneDrive - The University of Queensland/Two months_predictions/HPC_District_monthly_update_lag/Data/CONFIDENTIAL/Updated_full_data_with_new_boundaries_all_factors_cleaned.rds') %>%
  dplyr::select(date, district,m_DHF_cases, pop)

obs_epidemics <- inner_join(obs_case,obs_epidemics,by=c('district'='district','date'='date'))
obs_epidemics<- obs_epidemics %>%  mutate(obs_inc=m_DHF_cases/pop*100000)

obs_epidemics  <- obs_epidemics  %>% 
  mutate(epidemic_flag_ensamble_fix_150 = ifelse(obs_inc > log(150), 1, 0),
         epidemic_flag_ensamble_fix_300 = ifelse(obs_inc > log(300), 1, 0),
         epidemic_flag_ensamble_fix_100 = ifelse(obs_inc > log(100), 1, 0),
         epidemic_flag_ensamble_fix_50 = ifelse(obs_inc > log(50), 1, 0),
         epidemic_flag_ensamble_fix_20 = ifelse(obs_inc > log(20), 1, 0),
         epidemic_flag_ensamble_fix_200 = ifelse(obs_inc > log(200), 1, 0))

b6<- inner_join(summary_data,obs_epidemics,by=c('district'='district','date'='date'))

b6$mean_prob_pred_epidemic_2sd <- ifelse(b6$epidemic_flag == '1', b6$mean_prob_pred_epidemic_2sd, 0)
b6$mean_prob_pred_epidemic_nb <- ifelse(b6$epidemic_flag_nb == '1', b6$ mean_prob_pred_epidemic_nb, 0)
b6$mean_prob_epidemic_poisson <- ifelse(b6$epidemic_flag_poisson== '1', b6$mean_prob_epidemic_poisson, 0)
b6$mean_prob_epidemic_quant <- ifelse(b6$epidemic_flag_quant == '1', b6$mean_prob_epidemic_quant, 0)
b6$mean_prob_epidemic_fix_100<- ifelse(b6$epidemic_flag_ensamble_fix_100 == '1', b6$mean_prob_epidemic_fix_100, 0)
b6$mean_prob_epidemic_fix_150 <- ifelse(b6$epidemic_flag_ensamble_fix_150 == '1', b6$mean_prob_epidemic_fix_150, 0)
b6$mean_prob_epidemic_fix_300 <- ifelse(b6$epidemic_flag_ensamble_fix_300 == '1', b6$mean_prob_epidemic_fix_300, 0)
b6$mean_prob_epidemic_fix_50 <- ifelse(b6$epidemic_flag_ensamble_fix_50 == '1', b6$mean_prob_epidemic_fix_50, 0)
b6$mean_prob_epidemic_fix_20 <- ifelse(b6$epidemic_flag_ensamble_fix_20 == '1', b6$mean_prob_epidemic_fix_20, 0)
b6$mean_prob_epidemic_fix_200 <- ifelse(b6$epidemic_flag_ensamble_fix_200 == '1', b6$mean_prob_epidemic_fix_200, 0)

b3<- b6

###For one date 
date.select<- unique(b3$date)[1]

b4<- b6[ b6$date==date.select,]

b5<- inner_join(MDR_NEW,b4,by=c('VARNAME'='district'))

ggplot(data = b5) +
  geom_sf(aes(fill =epidemic_flag_ensamble_fix_50)) +
  scale_fill_gradient(low = "white", high = "darkblue") + # Continuous color scale from white to dark blue
  theme_minimal() +
  labs(fill = "Probability of Epidemic (2SD)")

ggplot(data = b5) +
  geom_sf(aes(fill =mean_prob_epidemic_fix_50)) +
  scale_fill_gradient(low = "white", high = "darkblue") + # Continuous color scale from white to dark blue
  theme_minimal() +
  labs(fill = "Probability of Epidemic (2SD)")
  