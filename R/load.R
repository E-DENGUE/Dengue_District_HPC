source('./R/predict.rlm.R')
source('./R/deseasonalize_climate.R')
source('./R/all_district_fwd1.R')
source('./R/scoring_func.R')
source('./0_specify_models.R')

######## Load data
#d1 <- readRDS('./Data/CONFIDENTIAL/full_climate_model.rds')
d1 <- readRDS('./Data/CONFIDENTIAL/full_data_with_new_boundaries.rds')

# Rename columns in the data frame
names(d1)[names(d1) == "Dengue"] <- "m_DHF_cases"
names(d1)[names(d1) == "Population"] <- "pop"
names(d1)[names(d1) == "t2m_avg"] <- "avg_daily_temp"
names(d1)[names(d1) == "t2m_max"] <- "avg_max_daily_temp"
names(d1)[names(d1) == "t2m_min"] <- "avg_min_daily_temp"
names(d1)[names(d1) == "ws_avg"] <- "avg_daily_wind"
names(d1)[names(d1) == "ws_max"] <- "avg_max_daily_wind"
names(d1)[names(d1) == "ws_min"] <- "avg_min_daily_wind"
names(d1)[names(d1) == "rh_avg"] <- "avg_daily_humid"
names(d1)[names(d1) == "rh_max"] <- "avg_max_daily_humid"
names(d1)[names(d1) == "rh_min"] <- "avg_min_daily_humid"
names(d1)[names(d1) == "tp_accum"] <- "monthly_cum_ppt"

d2a <- d1 %>%
  mutate(date = paste(year, month, '01', sep='-'),
         year = as_factor(year),
         District_province = paste(district, province, sep = "_")) %>%
  filter(district != "KIEN HAI", district != "PHU QUOC") %>%
  distinct(province,district,year, month, NAME_2, NAME_1, ENGTYPE, .keep_all = TRUE) %>%
  arrange(month, year) %>%
  ungroup() %>%
  dplyr::select(year, month, province, district,District_province, m_DHF_cases, pop, avg_daily_temp, avg_max_daily_temp, avg_min_daily_temp, avg_daily_wind, avg_max_daily_wind,
                avg_min_daily_wind, avg_daily_humid, avg_max_daily_humid, avg_min_daily_humid, monthly_cum_ppt
  ) %>%
  ungroup() %>%
  arrange(province, district, year, month) %>%
  group_by(district) %>%
  mutate(date = as.Date(paste(year, month, '01', sep='-'), '%Y-%m-%d'),
         m_DHF_cases = m_DHF_cases,
         first_date = min(date),
         last_date = max(date),
  ) %>%
  ungroup() %>%
  filter(!is.na(district) & first_date == as.Date('2004-01-01') & last_date == '2022-12-01')

rain1 <- deseasonalize_climate("monthly_cum_ppt") %>% rename(total_rainfall_ab = climate_aberration)
#rain2 <- deseasonalize_climate("mean_ppt")  %>% rename(daily_rainfall_ab = climate_aberration)
#temp1 <- deseasonalize_climate("mean_daily_temp")  %>% rename( ave_temp_ab = climate_aberration)
temp2 <- deseasonalize_climate("avg_min_daily_temp")  %>% rename( max_temp_ab = climate_aberration)
temp3 <- deseasonalize_climate("avg_max_daily_temp")  %>% rename( min_ave_temp_ab = climate_aberration)
#temp4 <- deseasonalize_climate("mean_max_temp")  %>% rename( max_abs_temp_abb = climate_aberration)
#temp5 <- deseasonalize_climate("mean_min_temp")  %>% rename( min_abs_temp_abb= climate_aberration)
humid1 <- deseasonalize_climate("avg_daily_humid")  %>% rename(min_humid_abb = climate_aberration)


# test <- merge(d2a, rain1,by=c('district', 'date') )

d2 <- d2a %>%
  #arrange(district, date) %>%
  left_join(rain1, by=c('District_province', 'date')) %>%
  left_join(temp2, by=c('District_province', 'date')) %>%
  left_join(temp3, by=c('District_province', 'date')) %>%
  left_join(humid1, by=c('District_province', 'date')) %>%
  mutate( 
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
    lag2_total_rainfall_ab= dplyr::lag(total_rainfall_ab,2)

    
        
    )%>%
    filter(!is.na(lag6_monthly_cum_ppt) )   
  


# d1.agg <- d2 %>%
#   group_by(date) %>%
#   summarize(N=n())
# 
#   summarize( df_rates = m_DHF_cases/ pop*100000,
#     mean_dengue = mean(df_rates)) %>%
#   mutate(lag1_mean_dengue = lag(mean_dengue,1),
#          log_lag1_mean_dengue = log(lag1_mean_dengue +1 ))

# d2 <- d2 %>%
#   left_join(d1.agg, by='date')


### Run the models


# Generate a sequence of monthly dates from January 2012 to December 2022
date.test2 <- seq.Date(from = as.Date('2012-01-01'), to = as.Date('2022-12-01'), by = 'month')


# Read district-level spatial data from a shapefile
MDR_NEW <- st_read(dsn = "./Data/CONFIDENTIAL/MDR_NEW_Boundaries_Final.shp")

# Create a new variable 'District_province' by concatenating 'VARNAME' and 'NAME_En' with an underscore
MDR_NEW <- MDR_NEW %>%
  dplyr::mutate(District_province = toupper(paste(VARNAME, NAME_En, sep = "_")))

# Remove island districts (no neighbors) from the dataset
MDR_NEW <- MDR_NEW %>%
  dplyr::filter(VARNAME != "Kien Hai", 
                VARNAME != "Phu Quoc")

toupper(sort(MDR_NEW$District_province))==toupper(sort(unique(d2$District_province)))

# Transform MDR_NEW to WGS 84 coordinate reference system
#MDR_NEW <- st_transform(MDR_NEW, crs = st_crs(MDR_2))

# Create a spatial neighbors object 'neighb' using queen contiguity based on valid geometries
neighb <- poly2nb(st_make_valid(MDR_NEW), queen = T, snap = sqrt(0.001))

# Create a new neighbors object 'W.nb' using non-queen contiguity based on valid geometries
W.nb <- poly2nb(st_make_valid(MDR_NEW), row.names = MDR_NEW$ID)
neighb <- poly2nb(st_make_valid(MDR_NEW), queen = F, snap = sqrt(0.001))

# Use 'nb2INLA' function to convert the neighbors object to a format suitable for the 'INLA' package
nb2INLA("MDR.graph", neighb)

# Set the file path for the adjacency graph file
MDR.adj <- paste(getwd(), "/MDR.graph", sep = "")


##Priors from Gibb ms 
# iid model 
hyper.iid = list(theta = list(prior="pc.prec", param=c(1, 0.01)))

# ar1 model
hyper.ar1 = list(theta1 = list(prior='pc.prec', param=c(0.5, 0.01)),
                  rho = list(prior='pc.cor0', param = c(0.5, 0.75)))

# bym model
hyper.bym = list(theta1 = list(prior="pc.prec", param=c(1, 0.01)),
                 theta2 = list(prior="pc.prec", param=c(1, 0.01)))

# bym2 model
# probability of SD of theta1 > 1 = 0.01
hyper.bym2 = list(theta1 = list(prior="pc.prec", param=c(1, 0.01)),
                  theta2 = list(prior="pc", param=c(0.5, 0.5)))

# hyperpriors for model grouping (iid / ar1) if used
# group.control.iid = list(model='iid', hyper = list(prec = list(prior='pc.prec',param=c(1, 0.01))))
# group.control.ar1 = list(model='ar1', hyper = list(theta1 = list(prior='pc.prec', param=c(1, 0.01)), rho = list(prior='pc.cor0', param = c(0.5, 0.75))))

# rw1/rw2 model: three levels of constraint on precision parameter 
# (puts more or less prior probability density on more or less wiggly)
hyper1.rw = list(prec = list(prior='pc.prec', param=c(0.1, 0.01))) # strictest smoothing; sd constrained to be low
hyper2.rw = list(prec = list(prior='pc.prec', param=c(0.3, 0.01))) # medium
hyper3.rw = list(prec = list(prior='pc.prec', param=c(1, 0.01))) # weaker (suggested INLA default) 
hyper4.rw = list(prec = list(prior='pc.prec', param=c(2, 0.01))) # weakest; sd can be quite wide 

#All models


