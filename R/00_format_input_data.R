library(parallel)
library(stats)
library(dplyr)
library(readr)
library(readxl)
library(tidyr)
library(tidyverse)
library(zoo)
library(lubridate)
library(pbapply)
library(INLA)
#inla.setOption(mkl=TRUE)
library(MASS)
library(scoringutils)
library(sf)
library(spdep)
library(ggmap) # plotting shapefiles 
library(lattice)  # Load the lattice package if you are using lattice graphics
library(stringr)

source('C:/Users/uqwareed/OneDrive - The University of Queensland/Two months_predictions/HPC_District_monthly_update_lag3/R/99_helper_funcs.R')

d1 <- readRDS('C:/Users/uqwareed/OneDrive - The University of Queensland/Two months_predictions/HPC_District_monthly_update_lag3/Data/CONFIDENTIAL/Updated_full_data_with_new_boundaries_all_factors.rds') 
  
names(d1)[names(d1) == "Dengue"] <- "m_DHF_cases"
names(d1)[names(d1) == "total_population"] <- "pop"
names(d1)[names(d1) == "t2m_avg"] <- "avg_daily_temp"
names(d1)[names(d1) == "t2m_max"] <- "avg_max_daily_temp"
names(d1)[names(d1) == "t2m_min" ] <- "avg_min_daily_temp"
names(d1)[names(d1) == "ws_avg"] <- "avg_daily_wind"
names(d1)[names(d1) == "ws_max"] <-  "avg_max_daily_wind"
names(d1)[names(d1) ==  "ws_min"] <- "avg_min_daily_wind"
names(d1)[names(d1) == "rh_avg"] <- "avg_daily_humid"
names(d1)[names(d1) == "rh_max" ] <- "avg_max_daily_humid"
names(d1)[names(d1) == "rh_min"  ] <- "avg_min_daily_humid"
names(d1)[names(d1) == "tp_accum"  ] <- "monthly_cum_ppt"
names(d1)[names(d1) == "District"  ] <- "district"
names(d1)[names(d1) == "Province"  ] <- "province"
names(d1)[names(d1) == "Month"  ] <- "month"
names(d1)[names(d1) == "Year"  ] <- "year"
# Assuming 'data' is your data frame containing the regions


# Define the renaming condition for all rows
d1 <- d1 %>%
  mutate(district = ifelse(district == 'CHAU THANH' & province == "AN GIANG", 
                           "CHAU THANH AN GIANG", 
                           ifelse(district == 'CHAU THANH' & province == "BEN TRE", 
                                  "CHAU THANH BEN TRE", 
                                  ifelse(district == 'CHAU THANH' & province == "CA MAU", 
                                         "CHAU THANH CA MAU",
                                         ifelse(district == 'CHAU THANH' & province == "DONG THAP", 
                                                "CHAU THANH DONG THAP",
                                                ifelse(district == 'CHAU THANH' & province == "HAU GIANG", 
                                                       "CHAU THANH HAU GIANG",
                                                       ifelse(district == 'CHAU THANH' & province == "LONG AN", 
                                                              "CHAU THANH LONG AN",
                                                              ifelse(district == 'CHAU THANH' & province == "TIEN GIANG", 
                                                                     "CHAU THANH TIEN GIANG",
                                                                     ifelse(district == 'CHAU THANH' & province == "TRA VINH", 
                                                                            "CHAU THANH TRA VINH",
                                                                            ifelse(district == 'PHU TAN' & province == "CA MAU", 
                                                                                   "PHU TAN CA MAU",
                                                                                   ifelse(district == 'PHU TAN' & province == "AN GIANG", 
                                                                                          "PHU TAN AN GIANG",
                                                                                          as.character(district)
                                                                                   )
                                                                            )
                                                                     )))))))))




d2 <- d1 %>%
  mutate(date = paste(year, month, '01', sept='-'),
         year = as_factor(year)) %>%
  filter(district != "KIEN HAI", 
         district != "PHU QUOC") %>%
  distinct(year, month, NAME_2, NAME_1, ENGTYPE, .keep_all = T) %>%
  arrange(month, year)%>%
  ungroup() %>%
  dplyr::select(year, month,province, district,m_DHF_cases,pop,avg_daily_temp,avg_max_daily_temp,avg_min_daily_temp,avg_daily_wind,avg_max_daily_wind,
                avg_min_daily_wind,avg_daily_humid,avg_max_daily_humid,avg_min_daily_humid,monthly_cum_ppt,
                Population_density,Outmigration_Rate, No..DEN1,No..DEN2,No..DEN3,No..DEN4,              
                Inmigration_Rate,NetImmigration_Rate,BI_larvae,CI_larvae ,HI_larvae,DI, prov_dis,         
                Poverty_Rate,   Hygienic_Water_Access, breeding_site_elimination_campaign,          
                Monthly_Average_Income_Percapita,Total_Passenger, communication_or_training,number_of_outbreak_response,               
                Hygienic_Toilet_Access, Urbanization_Rate,large_scale_spraying_for_epidemic_response  , land.scan.population ,number_of_outbreak_detection,active_spraying
  ) %>%
  ungroup() %>%
  arrange(province,district, year, month) %>%
  group_by(district) %>%
  mutate(date= as.Date(paste(year,month, '01',sep='-'), '%Y-%m-%d'),
         m_DHF_cases =ifelse(!is.na(pop) & is.na(m_DHF_cases),0, m_DHF_cases ) ,
         first_date=min(date),
         last_date =max(date),
  ) %>%
  ungroup() %>%
  filter(!is.na(district) &first_date==as.Date('2004-01-01') & last_date=='2022-12-01')   #filter out regions with partial time series


rain1 <- deseasonalize_climate("monthly_cum_ppt",ds=d2) %>% rename(total_rainfall_ab = climate_aberration)
#rain2 <- deseasonalize_climate("mean_ppt", ds=d2)  %>% rename(daily_rainfall_ab = climate_aberration)
temp1 <- deseasonalize_climate("avg_daily_temp", ds=d2)  %>% rename( ave_temp_ab = climate_aberration)
temp2 <- deseasonalize_climate("avg_min_daily_temp", ds=d2)  %>% rename( max_temp_ab = climate_aberration)
temp3 <- deseasonalize_climate("avg_max_daily_temp", ds=d2)  %>% rename( min_ave_temp_ab = climate_aberration)
#temp4 <- deseasonalize_climate("mean_max_temp")  %>% rename( max_abs_temp_abb = climate_aberration)
#temp5 <- deseasonalize_climate("mean_min_temp")  %>% rename( min_abs_temp_abb= climate_aberration)
humid1 <- deseasonalize_climate("avg_daily_humid", ds=d2)  %>% rename(ave_humid_ab = climate_aberration)
humid2 <- deseasonalize_climate("avg_min_daily_humid", ds=d2)  %>% rename(min_humid_abb = climate_aberration)
humid3 <- deseasonalize_climate("avg_max_daily_humid", ds=d2)  %>% rename(max_humid_abb = climate_aberration)

wind1 <- deseasonalize_climate("avg_daily_wind", ds=d2)  %>% rename( ave_wind_ab = climate_aberration)
wind2 <- deseasonalize_climate("avg_min_daily_wind", ds=d2)  %>% rename( min_wind_ab = climate_aberration)
wind3 <- deseasonalize_climate("avg_max_daily_wind", ds=d2)  %>% rename( max_wind_ab = climate_aberration)

d2 <- d2 %>%
  left_join(rain1, by=c('district', 'date')) %>%
  left_join(temp1, by=c('district', 'date')) %>%
  left_join(temp2, by=c('district', 'date')) %>%
  left_join(temp3, by=c('district', 'date')) %>%
  left_join(humid1, by=c('district', 'date')) %>%
  left_join(humid2, by=c('district', 'date')) %>%
  left_join(humid3, by=c('district', 'date')) %>%
  left_join(wind1, by=c('district', 'date')) %>%
  left_join(wind2, by=c('district', 'date')) %>%
  left_join(wind3, by=c('district', 'date')) %>%
  mutate( 
    #redefine the lag variables
    avg_daily_wind = as.vector(scale(avg_daily_wind)),
    lag1_avg_daily_wind = dplyr::lag(avg_daily_wind,1,default=NA),
    lag2_avg_daily_wind = dplyr::lag(avg_daily_wind,2,default=NA),
    lag3_avg_daily_wind = dplyr::lag(avg_daily_wind,3),
    lag4_avg_daily_wind = dplyr::lag(avg_daily_wind,4),
    lag5_avg_daily_wind = dplyr::lag(avg_daily_wind,5),
    lag6_avg_daily_wind = dplyr::lag(avg_daily_wind,6),
    #redefine the lag variables
    avg_daily_humid = as.vector(scale(avg_daily_humid)),
    lag1_avg_daily_humid = dplyr::lag(avg_daily_humid,1,default=NA),
    lag2_avg_daily_humid = dplyr::lag(avg_daily_humid,2,default=NA),
    lag3_avg_daily_humid = dplyr::lag(avg_daily_humid,3),
    lag4_avg_daily_humid = dplyr::lag(avg_daily_humid,4),
    lag5_avg_daily_humid = dplyr::lag(avg_daily_humid,5),
    lag6_avg_daily_humid = dplyr::lag(avg_daily_humid,6),
    
    avg_daily_temp= as.vector(scale(avg_daily_temp)),
    lag1_avg_daily_temp= dplyr::lag(avg_daily_temp,1),
    lag2_avg_daily_temp= dplyr::lag(avg_daily_temp,2),
    lag3_avg_daily_temp= dplyr::lag(avg_daily_temp,3),
    lag4_avg_daily_temp= dplyr::lag(avg_daily_temp,4),
    lag5_avg_daily_temp= dplyr::lag(avg_daily_temp,5),
    lag6_avg_daily_temp= dplyr::lag(avg_daily_temp,6),
    
    monthly_cum_ppt=as.vector(scale(monthly_cum_ppt)),
    lag1_monthly_cum_ppt= dplyr::lag(monthly_cum_ppt,1),
    lag2_monthly_cum_ppt= dplyr::lag(monthly_cum_ppt,2),
    lag3_monthly_cum_ppt= dplyr::lag(monthly_cum_ppt,3),
    lag4_monthly_cum_ppt= dplyr::lag(monthly_cum_ppt,4),
    lag5_monthly_cum_ppt= dplyr::lag(monthly_cum_ppt,5),
    lag6_monthly_cum_ppt= dplyr::lag(monthly_cum_ppt,6),
    
    avg_min_daily_temp=as.vector(scale(avg_min_daily_temp)),
    lag1_avg_min_daily_temp= dplyr::lag(avg_min_daily_temp,1),
    lag2_avg_min_daily_temp= dplyr::lag(avg_min_daily_temp,2),
    lag3_avg_min_daily_temp= dplyr::lag(avg_min_daily_temp,3),
    lag4_avg_min_daily_temp= dplyr::lag(avg_min_daily_temp,4),
    lag5_avg_min_daily_temp= dplyr::lag(avg_min_daily_temp,5),
    lag6_avg_min_daily_temp= dplyr::lag(avg_min_daily_temp,6),
    
    avg_max_daily_temp= as.vector(scale(avg_max_daily_temp)),
    lag1_avg_max_daily_temp= dplyr::lag(avg_max_daily_temp,1),
    lag2_avg_max_daily_temp= dplyr::lag(avg_max_daily_temp,2),
    lag3_avg_max_daily_temp= dplyr::lag(avg_max_daily_temp,3),
    lag4_avg_max_daily_temp= dplyr::lag(avg_max_daily_temp,4),
    lag5_avg_max_daily_temp= dplyr::lag(avg_max_daily_temp,5),
    lag6_avg_max_daily_temp= dplyr::lag(avg_max_daily_temp,6),
    
    lag1_total_rainfall_ab= dplyr::lag(total_rainfall_ab,1),
    lag2_total_rainfall_ab= dplyr::lag(total_rainfall_ab,2),
    
    lag2_breeding_site_elimination_campaign= dplyr::lag(breeding_site_elimination_campaign,2),	
    lag2_active_spraying= dplyr::lag(active_spraying,2),
    lag2_large_scale_spraying_for_epidemic_response= dplyr::lag(large_scale_spraying_for_epidemic_response,2),
    lag2_communication_or_training= dplyr::lag(communication_or_training,2),
    lag2_number_of_outbreak_detection= dplyr::lag(number_of_outbreak_detection,2),
    lag2_number_of_outbreak_response= dplyr::lag(number_of_outbreak_response,2),
    
    
  )%>%
  filter(!is.na(lag6_monthly_cum_ppt) & first_date==as.Date('2004-01-01') & last_date=='2022-12-01')   #filter out regions with partial time series
saveRDS(d2, './Data/CONFIDENTIAL/Updated_full_data_with_new_boundaries_all_factors_cleaned_lag3.rds') 


###############################
#SPATIAL MATRIX:
MDR_NEW <- st_read(dsn = "C:/Users/uqwareed/OneDrive - The University of Queensland/Two months_predictions/HPC_District_monthly_update_lag3/Data/shapefiles/MDR_NEW_Boundaries_Final.shp") 
  
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
# Remove island districts (no neighbors) from the dataset
spat_IDS <- MDR_NEW %>%
  dplyr::filter(VARNAME != "KIEN HAI",
                VARNAME != "PHU QUOC") %>%
  rename(district=VARNAME) %>%
  mutate(districtID= row_number(), district=(district)) %>%
  as.data.frame() %>%
  dplyr::select(district,districtID)

setdiff(toupper(d2$district),toupper(spat_IDS$district))
setdiff(toupper(spat_IDS$district),toupper(d2$district))
sort(spat_IDS$district) ==sort(unique(d2$district))

MDR_NEW<- MDR_NEW %>%
  dplyr::filter(VARNAME != "KIEN HAI",
                VARNAME != "PHU QUOC")

saveRDS(MDR_NEW, "C:/Users/uqwareed/OneDrive - The University of Queensland/Two months_predictions/HPC_District_monthly_update_lag3/Data/MDR_NEW.rds")
saveRDS(spat_IDS, "C:/Users/uqwareed/OneDrive - The University of Queensland/Two months_predictions/HPC_District_monthly_update_lag3/Data/spatial_IDS.rds")
