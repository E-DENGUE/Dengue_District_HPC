library(dplyr)
library(lubridate)

source('./R/99_helper_funcs.R')
source('./R/99_define_inla_spacetime_mods.R')
source('./R/01_fun_inla_spacetime.R')
source('./R/02_fun_hhh4.R')
source('./R/03_fun_lag_district_pca.R')

#cleaned in 00_format_input_data.R
d2 <- readRDS('./Data/CONFIDENTIAL/Updated_full_data_with_new_boundaries_all_factors_cleaned.rds') %>%
  rename(District_province = prov_dis) %>%
  arrange(district, date) %>%
  dplyr::select(year, month, province, district,pop,
                avg_daily_temp,avg_max_daily_temp,avg_min_daily_temp,
                avg_daily_humid,avg_max_daily_humid,avg_min_daily_humid,
                monthly_cum_ppt,
                BI_larvae ,CI_larvae,HI_larvae,
                Population_density,Poverty_Rate,Hygienic_Water_Access,
                communication_or_training,breeding_site_elimination_campaign,large_scale_spraying_for_epidemic_response,active_spraying,
                Monthly_Average_Income_Percapita,
                m_DHF_cases)
saveRDS(d2,'./Data/loza_district.rds')
