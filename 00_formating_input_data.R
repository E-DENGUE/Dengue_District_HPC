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
library(raster)
library(Hmisc)
library(ggdendro)
library(dtwclust)
library(sf)
library(cluster)
library(dplyr)
source('./R/predict.rlm.R')
source('./R/deseasonalize_climate.R')
#source('./R/all_district_fwd2.R')
#source('./R/scoring_func.R')

######## Load data
d1 <- readRDS('./Data/CONFIDENTIAL/full_data_with_new_boundaries_all_factors.rds')


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

d1 <- d1 %>%
  mutate(date = paste(year, month, '01', sept='-'),
         year = as_factor(year)) %>%
  filter(district != "KIEN HAI", 
         district != "PHU QUOC") %>%
  distinct(year, month, province, district, .keep_all = T) 

###Fix the Bac Lieu province 
data <- read_excel("./Data/20231130_ED_MDR Dengue district data_2000-2023_original modification_updated 20May24.xlsx", sheet=1)
filtered_data <- data[data$year >= 2004, ]
dim(filtered_data)

unique(filtered_data$district)

sum(is.na(filtered_data$Dengue))
sum(is.na(filtered_data$SevereDHF))


library(dplyr)
filtered_data$district<- toupper(filtered_data$district)

library(dplyr)
filtered_data <- filtered_data %>%
  arrange(district, province, year, month)



# Filter the old and new data for BAC Lieu province and mach the boundaries 

BAC_LIEU_old <- d1[d1$province == 'BAC LIEU',]
BAC_LIEU_New <- filtered_data[filtered_data$province == 'BAC LIEU',]

filtered_data_summarized <- BAC_LIEU_New %>%  group_by(province, district, year, month)%>%
  mutate(
    district = ifelse(
      (district %in% c("VINH LOI", "HOA BINH")) & (province == "BAC LIEU"),
      "VINH LOI",district) )%>%
  
  summarise(Dengue = sum(Dengue,na.rm = TRUE), SevereDHF = sum(SevereDHF,na.rm = TRUE)) %>%
  ungroup()


BAC_LIEU_New<- filtered_data_summarized

BAC_LIEU_New <- BAC_LIEU_New 

BAC_LIEU_old$year<- as.factor(BAC_LIEU_old$year)
BAC_LIEU_New$year<- as.factor(BAC_LIEU_New$year)

BAC_LIEU_New$province <- toupper(BAC_LIEU_New$province)
BAC_LIEU_old$province <- toupper(BAC_LIEU_old$province)


BAC_LIEU_old_subset <- BAC_LIEU_old[BAC_LIEU_old$province == "BAC LIEU", ]


BAC_LIEU_old_subset_updated <- merge(
  BAC_LIEU_old_subset,
  BAC_LIEU_New[, c("year", "month", "district", "Dengue")],
  by = c("year", "month", "district"),
  all.x = TRUE
)


BAC_LIEU_old_subset_updated$m_DHF_cases <- BAC_LIEU_old_subset_updated$Dengue

# Drop the extra Dengue column after updating
BAC_LIEU_old_subset_updated$Dengue <- NULL


BAC_LIEU_old_subset <- BAC_LIEU_old_subset_updated

BAC_LIEU_old_subset_updated <- BAC_LIEU_old_subset_updated[, names(d1)]



d1[d1$province == "BAC LIEU", ] <- BAC_LIEU_old_subset_updated

b<- d1[d1$province == "BAC LIEU", ]


unique(b$year)
unique(b$month)




bac_lieu_data <- d1 %>% filter(province == "BAC LIEU")


bac_lieu_summary <- bac_lieu_data %>%
  group_by(year, month) %>%
  dplyr::summarize(total_cases = sum(m_DHF_cases, na.rm = TRUE)) %>%
  arrange(year, month)


bac_lieu_summary <- bac_lieu_summary %>%
  mutate(date = as.Date(paste(year, month, "01", sep = "-")))


ggplot(bac_lieu_summary, aes(x = date, y = total_cases)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  labs(
    title = "Time Series of m_DHF_cases in BAC LIEU",
    x = "Date",
    y = "Total Cases"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )



d2 <- d1 %>%  arrange(month, year)%>%
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

rain1 <- deseasonalize_climate("monthly_cum_ppt") %>% rename(total_rainfall_ab = climate_aberration)

temp1 <- deseasonalize_climate("avg_daily_temp" )  %>% rename( ave_temp_ab = climate_aberration)
temp2 <- deseasonalize_climate("avg_min_daily_temp")  %>% rename( max_temp_ab = climate_aberration)
temp3 <- deseasonalize_climate("avg_max_daily_temp" )  %>% rename( min_ave_temp_ab = climate_aberration)

humid1 <- deseasonalize_climate("avg_daily_humid")  %>% rename(ave_humid_ab = climate_aberration)
humid2 <- deseasonalize_climate("avg_min_daily_humid")  %>% rename(min_humid_abb = climate_aberration)
humid3 <- deseasonalize_climate("avg_max_daily_humid")  %>% rename(max_humid_abb = climate_aberration)

wind1 <- deseasonalize_climate("avg_daily_wind")  %>% rename( ave_wind_ab = climate_aberration)
wind2 <- deseasonalize_climate("avg_min_daily_wind")  %>% rename( min_wind_ab = climate_aberration)
wind3 <- deseasonalize_climate("avg_max_daily_wind")  %>% rename( max_wind_ab = climate_aberration)

d3 <- d2 %>%
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
  arrange(province,district, year, month) %>%
  group_by(district)%>% mutate( 
    #redefine the lag variables
    avg_daily_wind = as.vector(scale(avg_daily_wind)),
    lag1_avg_daily_wind = dplyr::lag(avg_daily_wind,1,default=NA),
    lag2_avg_daily_wind = dplyr::lag(avg_daily_wind,2,default=NA),
    lag3_avg_daily_wind = dplyr::lag(avg_daily_wind,3,default=NA),
    
    
    #redefine the lag variables
    avg_daily_humid = as.vector(scale(avg_daily_humid)),
    lag1_avg_daily_humid = dplyr::lag(avg_daily_humid,1,default=NA),
    lag2_avg_daily_humid = dplyr::lag(avg_daily_humid,2,default=NA),
    lag3_avg_daily_humid = dplyr::lag(avg_daily_humid,3,default=NA),
    
    avg_daily_temp= as.vector(scale(avg_daily_temp)),
    lag1_avg_daily_temp= dplyr::lag(avg_daily_temp,1),
    lag2_avg_daily_temp= dplyr::lag(avg_daily_temp,2),
    lag3_avg_daily_temp= dplyr::lag(avg_daily_temp,3),
    
    
    monthly_cum_ppt=as.vector(scale(monthly_cum_ppt)),
    lag1_monthly_cum_ppt= dplyr::lag(monthly_cum_ppt,1),
    lag2_monthly_cum_ppt= dplyr::lag(monthly_cum_ppt,2),
    lag3_monthly_cum_ppt= dplyr::lag(monthly_cum_ppt,3),
    
    avg_min_daily_temp=as.vector(scale(avg_min_daily_temp)),
    lag1_avg_min_daily_temp= dplyr::lag(avg_min_daily_temp,1),
    lag2_avg_min_daily_temp= dplyr::lag(avg_min_daily_temp,2),
    lag3_avg_min_daily_temp= dplyr::lag(avg_min_daily_temp,3),
    
    avg_max_daily_temp= as.vector(scale(avg_max_daily_temp)),
    lag1_avg_max_daily_temp= dplyr::lag(avg_max_daily_temp,1),
    lag2_avg_max_daily_temp= dplyr::lag(avg_max_daily_temp,2),
    lag3_avg_max_daily_temp= dplyr::lag(avg_max_daily_temp,3),
    
    
    total_rainfall_ab=as.vector(scale(total_rainfall_ab)),
    lag1_total_rainfall_ab= dplyr::lag(total_rainfall_ab,1),
    lag2_total_rainfall_ab= dplyr::lag(total_rainfall_ab,2),
    lag3_total_rainfall_ab= dplyr::lag(total_rainfall_ab,3),
    
    lag2_breeding_site_elimination_campaign= dplyr::lag(scale(breeding_site_elimination_campaign),2),	
    lag2_active_spraying= dplyr::lag(scale(active_spraying),2),
    lag2_large_scale_spraying_for_epidemic_response= dplyr::lag(scale(large_scale_spraying_for_epidemic_response),2),
    lag2_communication_or_training= dplyr::lag(scale(communication_or_training),2),
    lag2_number_of_outbreak_detection= dplyr::lag(scale(number_of_outbreak_detection),2),
    lag2_number_of_outbreak_response= dplyr::lag(scale(number_of_outbreak_response),2),
    
    lag3_breeding_site_elimination_campaign= dplyr::lag(scale(breeding_site_elimination_campaign),3),	
    lag3_active_spraying= dplyr::lag(active_spraying,3),
    lag3_large_scale_spraying_for_epidemic_response= dplyr::lag(scale(large_scale_spraying_for_epidemic_response),3),
    lag3_communication_or_training= dplyr::lag(scale(communication_or_training),3),
    lag3_number_of_outbreak_detection= dplyr::lag(scale(number_of_outbreak_detection),3),
    lag3_number_of_outbreak_response= dplyr::lag(scale(number_of_outbreak_response),3),
    
  )%>%
  ungroup() %>%
  arrange(district, date) %>%
  group_by(district) %>%
  mutate(   cumsum_cases_12m =  roll::roll_sum(m_DHF_cases,12, min_obs=1), #partial backward moving sum
            cumsum_pop_12m =  roll::roll_sum(pop,12, min_obs=1), #partial backward moving sum
            cum_inc_12m = (cumsum_cases_12m+1)/cumsum_pop_12m*100000,
            cumsum_cases_24m =  roll::roll_sum(m_DHF_cases,24, min_obs=1), #partial backward moving sum
            cumsum_pop_24m =  roll::roll_sum(pop,24, min_obs=1), #partial backward moving sum
            cum_inc_24m = (cumsum_cases_24m+1)/cumsum_pop_24m*100000,
            cumsum_cases_36m =  roll::roll_sum(m_DHF_cases,36, min_obs=1), #partial backward moving sum
            cumsum_pop_36m =  roll::roll_sum(pop,36, min_obs=1), #partial backward moving sum
            cum_inc_36m = (cumsum_cases_36m+1)/cumsum_pop_36m*100000
  ) %>%
  ungroup() %>%
  arrange(district, date) %>%
  group_by(district) %>%
  mutate(log_cum_inc_12m=scale(log(cum_inc_12m)),
         log_cum_inc_24m=scale(log(cum_inc_24m)),
         log_cum_inc_36m=scale(log(cum_inc_36m)),
         lag2_log_cum_inc_12m=lag(log_cum_inc_12m,2),
         lag2_log_cum_inc_24m=lag(log_cum_inc_24m,2),
         lag2_log_cum_inc_36m=lag(log(cum_inc_36m,2)),
         lag3_log_cum_inc_12m=lag(log_cum_inc_12m,3),
         lag3_log_cum_inc_24m=lag(log_cum_inc_24m,3),
         lag3_log_cum_inc_36m=lag(log(cum_inc_36m,3))
  ) %>%
  ungroup() %>%
  filter(!is.na(lag3_monthly_cum_ppt) & first_date==as.Date('2004-01-01') & last_date=='2022-12-01')


d3_filtered <- d3[is.na(d3$pop), ]

d3$pop[is.na(d3$pop) & d3$year == 2022 & d3$district == "CAO LANH CITY" & d3$province == "DONG THAP"] <- 165163   #this value fro the population data set, it was missing in the file


##Handel the stereotype vectors 

d<- d3[ , c("No..DEN1", "No..DEN2", "No..DEN3", "No..DEN4")]


d$podem <- NA  


for (i in 1:nrow(d)) {
  row <- d[i, ]  
  
  if (all(row[-length(row)] == 0)) {  # Exclude the last column (podem column) from the check
    # If all zeros, return the value from the previous row's "podem" column
    if (i == 1) {
      d$podem[i] <- 2  # For the first row, assign 2
    } else {
      d$podem[i] <- d$podem[i - 1]  # For subsequent rows, assign the previous row's value
    }
  } else {
    # If not all zeros, return the column number with the maximum value
    max_col <- which.max(row[-length(row)])  # Exclude the last column (podem column)
    d$podem[i] <- max_col
  }
}

d3<- d3%>% 
  dplyr::mutate(prediomentent=as.factor(d$podem))


library(dplyr)
d3 <- d3 %>%
  arrange(district, province, year, month)

d3$pop<- as.numeric(d3$pop)
##Find the clusters
d4 <- d3 %>% mutate(  log_df_rate = log((m_DHF_cases +1 ) / d3$pop * 100000))


d4$log_df_rate <- ifelse(is.na(d4$log_df_rate),
                         0, # Replace NA values with zero
                         d4$log_df_rate)

names(d4)
#DTW 
#hierarchical clustering with dynamic time-warping (DTW)


d.c <- d4 %>%
  reshape2::dcast(., date ~ district, value.var='log_df_rate') %>%
  dplyr::select_if(~ !any(is.na(.))) %>%
  dplyr::select(-date) %>%
  t()


set.seed(123)
dtw_hc <- dtwclust::tsclust(d.c,
                            type = "hierarchical",
                            k = 3,
                            distance = "dtw_basic",
                            control = hierarchical_control(method = "average"),
                            args = tsclust_args(dist = list(window.size = 12), cent = dba)
)

clusters = cbind.data.frame('district'=row.names(d.c), 'cluster'=dtw_hc@cluster)

unique_clusters_districts <- unique(clusters[, c("cluster", "district")])

d4<- full_join(d4,unique_clusters_districts,by=('district'='district'))


saveRDS(d4, 'Updated_full_data_with_new_boundaries_all_factors_cleaned_lag3.rds') 


###############################
#SPATIAL MATRIX:


MDR_NEW <- st_read(dsn = "./Data/shapefiles/MDR_NEW_Boundaries_Final.shp") 

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

setdiff(toupper(d3$district),toupper(spat_IDS$district))
setdiff(toupper(spat_IDS$district),toupper(d3$district))
sort(spat_IDS$district) ==sort(unique(d3$district))

MDR_NEW<- MDR_NEW %>%
  dplyr::filter(VARNAME != "KIEN HAI",
                VARNAME != "PHU QUOC")

saveRDS(MDR_NEW, "./Data/MDR_NEW.rds")
saveRDS(spat_IDS, "./Data/spatial_IDS.rds")
#####