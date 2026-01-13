
library(dplyr)
library(parallel)
library(ggplot2)
library(tidyverse)
library(broom)
library(plotly)
library(viridis)
library(lubridate)
library(gganimate)
library(pbapply)
library(scoringutils)
options(dplyr.summarise.inform = FALSE)
library(dplyr)
N_cores = detectCores()
library(dplyr)
library(ggplot2)
library(lubridate)

obs_epidemics <- readRDS( './Data/observed_alarms.rds') %>% #observed alarms, as flagged in outbreak_quant.R
  dplyr::rename(case_vintage=m_DHF_cases) %>%
  dplyr::select(date, district,case_vintage, starts_with('epidemic_flag'), starts_with('threshold'))

obs_case <- readRDS('./Data/CONFIDENTIAL/Updated_full_data_with_new_boundaries_all_factors_cleaned_lag3_add_rows_full_pop.rds') %>%
  dplyr::select(date, district, m_DHF_cases, pop)

if (!"month" %in% names(obs_case)) {
  obs_case <- obs_case %>%
    mutate(month = month(date))
}

# Now, create the box plot for m_DHF_cases by month
fig1<- ggplot(obs_case, aes(x = factor(month, levels = 1:12, labels = month.abb), y = m_DHF_cases)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(
    title = "Monthly Distribution of Dengue Cases",
    x = "Month",
    y = "Number of Dengue Cases"
  ) +
  theme_minimal()


library(ggplot2)

# Create a new column with log-transformed cases
obs_case$log_cases <- log(obs_case$m_DHF_cases + 1)

obs_case$month_2 <- factor(as.numeric(obs_case$month), 
                     levels = 1:12, 
                     labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

# Plot using the transformed values with natural scale on the y-axis
library(ggplot2)
library(scales)  # For label formatting

# Create a new column with log-transformed cases
obs_case$log_cases <- log(obs_case$m_DHF_cases + 1)

# Plot using log-transformed values but with natural scale y-axis labels
fig1<- ggplot(obs_case, aes(x = month_2, y = log_cases)) +
  geom_violin(fill = "lightblue", color = "black", alpha = 0.7) +
  labs(x = "Month", y = "log(dengue cases)") +
  theme_minimal(base_size = 15) + # Minimal theme with white background
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid = element_line(color = "grey80")
  )



#### to find the incidences

 ggsave("fig1.png", plot = fig1, width = 10, height = 8, dpi = 300, bg = "white")

obs_case$incidence_rate <- obs_case$m_DHF_cases / obs_case$pop * 100000

# Extract year from the date
obs_case$date <- as.Date(obs_case$date)  # Ensure date is in Date format
obs_case$year <- format(obs_case$date, "%Y")

# Summarize by year
annual_incidence <- obs_case %>%
  group_by(year) %>% filter(year != c('2004','2023'))%>%
  dplyr::summarize(total_incidence = sum(incidence_rate, na.rm = TRUE)) %>%
  ungroup()

annual_incidence <- obs_case %>%
  group_by(month,year) %>% filter(year != c("2023"))%>%
  dplyr::summarize(total_incidence = sum(incidence_rate, na.rm = TRUE)) %>%
  ungroup()

# Find high and low incidence years
high_incidence_year <- annual_incidence[which.max(annual_incidence$total_incidence), ]
low_incidence_year <- annual_incidence[which.min(annual_incidence$total_incidence), ]
low_incidence_year
# Summarize by district and year
district_incidence <- obs_case %>%
  group_by(district, year) %>%
  dplyr::summarize(total_incidence = sum(incidence_rate, na.rm = TRUE)) %>%
  ungroup()

# Calculate average annual incidence for each district
average_district_incidence <- district_incidence %>%
  group_by(district,year) %>%
  dplyr::summarize(average_incidence = mean(total_incidence, na.rm = TRUE)) %>%
  ungroup()


average_district_incidence <- district_incidence %>%
  group_by(district) %>%
  plyr::summarize(average_incidence = mean(total_incidence, na.rm = TRUE)) %>%
  ungroup()

# Find high and low incidence districts
high_incidence_district <- average_district_incidence[which.max(average_district_incidence$average_incidence), ]
low_incidence_district <- average_district_incidence[which.min(average_district_incidence$average_incidence), ]

print(high_incidence_year)
print(low_incidence_year)

print(high_incidence_district)
print(low_incidence_district)


# Extract month from the date
obs_case$month <- format(obs_case$date, "%m")

# Summarize by month
monthly_incidence <- obs_case %>%
  group_by(month) %>%
  dplyr::summarize(total_incidence = sum(incidence_rate, na.rm = TRUE)) %>%
  ungroup()

# Find high and low incidence months
high_incidence_month <- monthly_incidence[which.max(monthly_incidence$total_incidence), ]
low_incidence_month <- monthly_incidence[which.min(monthly_incidence$total_incidence), ]
print(high_incidence_month)
print(low_incidence_month)

###Figure S1 in appendix

obs_case_monthly <- obs_case %>%
  group_by(year, month) %>%
  dplyr::summarize(total_cases = sum(m_DHF_cases, na.rm = TRUE)) %>%
  mutate(date = as.Date(paste(year, month, "01", sep = "-"))) %>%
  ungroup()

# Create a base time series plot from 2004-2022 with clear x-axis labels
p <- ggplot(obs_case_monthly, aes(x = date, y = total_cases)) +
  geom_line(size = 1, color = "black") +
  
  # Add vertical lines for key time periods (end of 2011 and start of 2012 for TSCV)
  geom_vline(xintercept = as.Date(c("2011-12-31", "2012-01-01", "2016-12-31")),
             linetype = "dotted", color = "red", size = 1) +
  
  annotate("text", x = as.Date("2007-01-01"), y = max(obs_case_monthly$total_cases, na.rm = TRUE) * 0.9, 
           label = "Initial training", color = "red", size = 5, hjust = 0) +
  annotate("text", x = as.Date("2013-01-01"), y = max(obs_case_monthly$total_cases, na.rm = TRUE) * 0.9, 
           label = "TSCV", color = "blue", size = 5, hjust = 0) +
  annotate("text", x = as.Date("2018-01-01"), y = max(obs_case_monthly$total_cases, na.rm = TRUE) * 0.9, 
           label = "Evaluation period", color = "grey", size = 5, hjust = 0) +
  
  # Customize x-axis to mark specific periods
  scale_x_date(breaks = as.Date(c("2004-01-01", "2011-12-31", "2016-12-31", "2022-12-01")),
               labels = c("2004", "2011", "2016", "2022")) +
  
  # Customize the axis labels
  labs(x = "Time", y = "Number of Dengue Cases") +
  
  # Adjust the theme for clarity and aesthetics
  theme_minimal() +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    panel.grid = element_line(color = "lightgrey"),
    panel.grid.minor.x = element_blank(), # Remove minor grid lines on x-axis
    panel.grid.major.x = element_line(size = 0.8, color = "grey80")
  )

# Display the plot
print(p)


####plots S2 and S3 the tested model was using data from 2012-2016 and it applied till horizon=2

out<- readRDS('./Data/cleaned_scores/all_crps_slim_updated_all_tested_models.rds')
out<- out %>%
  filter( date <= '2016-12-01')

miss.mod <- out %>%
  filter(horizon%in% c(1,2) & date <= '2016-12-01') %>%
  group_by(modN) %>%
  dplyr::summarize(N=n()) %>%
  mutate(exclude_miss_mod = N<max(N))

miss.dates <- out %>% 
  left_join(miss.mod, by='modN') %>%
  filter(exclude_miss_mod==F) %>%
  group_by(date, horizon) %>%   
  filter(  horizon %in% c(1,2)) %>%
  dplyr::summarize(N_mods=n(), N_cases=mean(m_DHF_cases)) %>%
  ungroup() %>%
  group_by(horizon) %>%
  mutate(miss_date = if_else(N_mods< max(N_mods),1,0 )) %>%
  ungroup()


miss_models<- miss.mod %>% filter(miss.mod$exclude_miss_mod==TRUE)

#note this is not a proper time series--we are double counting cases across models.
ggplot(miss.dates, aes(x=date, y=N_cases)) +
  theme_classic()+
  geom_line()+
  facet_wrap(~horizon) +
  geom_point(aes(x=date, y=N_cases, color=miss_date))

#FILTER OUT months when an epidemic has been recognized by the time forecast is made in a specific district (using fixed epidemic threshold)
out_1a <- out %>%
  dplyr::select(-pop,-m_DHF_cases) %>%
  left_join(obs_epidemics, by=c('district'='district','vintage_date'='date'))   

#Overall
out2 <- out_1a %>%
  group_by(horizon, modN, form) %>%
  dplyr::summarize(crps1 = mean(crps1),crps2 = mean(crps2) ,N=n() ) %>%
  ungroup() %>%
  arrange(horizon, crps2) %>%
  group_by(horizon) %>%
  dplyr::mutate( w_i1 = (1/crps1^2)/sum(1/crps1^2),w_i2 = (1/crps2^2)/sum(1/crps2^2) ) %>%
  filter(horizon==2)
  

#By calendar month
out3 <- out_1a %>%
  filter( !is.na(crps2)) %>%
  mutate(month=lubridate::month(date)) %>%
  group_by(horizon, month, modN, form) %>%
  dplyr:: summarize(crps1 = mean(crps1),crps2=mean(crps2), N=n() ) %>%
  arrange(horizon,month, crps2)%>%
  ungroup() %>%
  group_by(horizon,month) %>%
  mutate(w_i1 = (1/crps1^2)/sum(1/crps1^2),
         w_i2 = (1/crps2^2)/sum(1/crps2^2),rel_wgt2= w_i2/max(w_i2) )%>%
  filter(horizon==2)%>%
  mutate(rw_season = grepl('cyclic=TRUE', form),
         harm_season = grepl('sin12', form),
         lag2_y = grepl('lag2_y', form),
         lag_y = grepl('lag_y', form),
         lag2_monthly_cum_ppt =grepl('lag2_monthly_cum_ppt', form),
         iid_spat_intercept=grepl('f(districtID,model = "iid")',form, fixed=T),
         rw_time_spatial=grepl(' f(t, replicate=districtID3, model="rw1", hyper = hyper2.rw) ',form, fixed=T),
         type4_spatial_bym = grepl('model="bym"', form, fixed=T) *grepl('control.group=list(model="ar1"', form, fixed=T) *grepl('group=time_id1', form, fixed=T)
  )

out3<- out3 %>% filter(modN !='mod61')
out3$modN <- factor(out3$modN, levels = c("mod1_", "mod2_", "mod3_", "mod4_", "mod5_", "mod6_", "mod7_", "mod8_", "mod9_", "mod10_", 
                                          "mod11_", "mod12_", "mod13_", "mod14_", "mod15_", "mod16_", "mod17_", "mod18_","mod19_"
                                          ,"mod20_", "mod21_","mod22_", "mod23_", "mod24_",
                                          "mod25_", 
                                          "mod26_", "mod27_", "mod28_", "mod29_", "mod30_", "mod31_", "mod32_", "mod33_", "mod34_", 
                                          "mod35_", "mod36_", "mod37_", "mod38_", "mod39_", "mod40_", "mod41_", "mod42_", "mod43_", 
                                          "mod44_", "mod45_", "mod46_", "mod47_", "mod48_", "mod49_", "mod50_", "mod51_", "mod52_", 
                                          "mod53_", "mod54_", "mod55_", "mod56_", "mod57_", "mod58_", "mod59_", "mod60_", "mod61_", 
                                          "modhhh4_basic_", "modhhh4_np_", "modhhh4_power_", "modhhh4_power_precip_temp_", 
                                          "modhhh4_power_precip_temp_dist_", "modhhh4_power_precip_temp_endmc1_", "modhhh4_power_precip_temp_endmc2_", 
                                          "modhhh4_power_lag12_", "modhhh4_power_cum_lag24_", "PC_lags", "PC_lags_weather", "PC_weather"))

out3 <- out3[order(out3$modN), ]

out3$month <- factor(out3$month, 
                     levels = 1:12, 
                     labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))


##this is S3 heat map
z<- ggplot(out3, aes(x = month, y = modN, fill = rel_wgt2)) +
  geom_tile() +
  scale_fill_viridis(discrete = FALSE)+
  labs(title = "CRPS Heatmap",
       x = "Month",
       y = "ModelN")

ggsave(
  filename =("z.png"),
  plot =z,
  width = 9,  # Width in inches (107 mm)
  height = 9,  # Height in inches
  units = "in", 
  dpi = 300 ,bg = "white" # Resolution in dpi
)

out_1a<- out_1a %>% dplyr::filter(modN !='mod61_')

## How does best model differ by district?
out4 <- out_1a %>%
  mutate(month=lubridate::month(date)) %>%
  group_by(horizon, district, modN, form) %>%
  dplyr::summarize(crps1 = mean(crps1),crps2=mean(crps2), N=n() ) %>%
  arrange(horizon,district, crps2)%>%
  ungroup() %>%
  group_by(horizon,district) %>%
  mutate(w_i1 = (1/crps1^2)/sum(1/crps1^2),w_i2 = (1/crps2^2)/sum(1/crps2^2), rel_wgt2= w_i2/max(w_i2) )%>%
  filter(horizon==2)%>%
  mutate(rw_season = grepl('cyclic=TRUE', form),
         harm_season = grepl('sin12', form),
         lag2_y = grepl('lag2_y', form),
         lag_y = grepl('lag_y', form),
         lag2_monthly_cum_ppt =grepl('lag2_monthly_cum_ppt', form),
         iid_spat_intercept=grepl('f(districtID,model = "iid")',form, fixed=T),
         rw_time_spatial=grepl(' f(t, replicate=districtID3, model="rw1", hyper = hyper2.rw) ',form, fixed=T),
         type4_spatial_bym = grepl('model="bym"', form, fixed=T) *grepl('control.group=list(model="ar1"', form, fixed=T) *grepl('group=time_id1', form, fixed=T)
  ) %>%
  dplyr::select(-form) %>%
  ungroup() %>%
  arrange(district, crps2) %>%
  group_by(district) %>%
  mutate(mod_rank = row_number()) %>%
  ungroup()


out4$modN <- factor(out4$modN, levels = c("mod1_", "mod2_", "mod3_", "mod4_", "mod5_", "mod6_", "mod7_", "mod8_", "mod9_", "mod10_", 
                                          "mod11_", "mod12_", "mod13_", "mod14_", "mod15_", "mod16_", "mod17_", "mod18_","mod19_"
                                          ,"mod20_", "mod21_","mod22_", "mod23_", "mod24_",
                                          "mod25_", 
                                          "mod26_", "mod27_", "mod28_", "mod29_", "mod30_", "mod31_", "mod32_", "mod33_", "mod34_", 
                                          "mod35_", "mod36_", "mod37_", "mod38_", "mod39_", "mod40_", "mod41_", "mod42_", "mod43_", 
                                          "mod44_", "mod45_", "mod46_", "mod47_", "mod48_", "mod49_", "mod50_", "mod51_", "mod52_", 
                                          "mod53_", "mod54_", "mod55_", "mod56_", "mod57_", "mod58_", "mod59_", "mod60_", "mod61_", 
                                          "modhhh4_basic_", "modhhh4_np_", "modhhh4_power_", "modhhh4_power_precip_temp_", 
                                          "modhhh4_power_precip_temp_dist_", "modhhh4_power_precip_temp_endmc1_", "modhhh4_power_precip_temp_endmc2_", 
                                          "modhhh4_power_lag12_", "modhhh4_power_cum_lag24_", "PC_lags", "PC_lags_weather", "PC_weather"))

# Step 2: Reorder the data frame 'out4' based on the newly ordered 'modN' factor levels
out4 <- out4[order(out4$modN), ]

# Check the result to confirm the order is correct
head(out4)


###By month
out4%>%
  ggplot(aes(x=modN,y=rel_wgt2, group=district )) +
  geom_line()+
  theme_classic()

out4<- out4 %>% dplyr::filter(modN !='mod61_')
##By district (S2 hatmap)
a<- ggplot(out4, aes(x = district, y = modN, fill = rel_wgt2)) +
  geom_tile() +
  scale_fill_viridis(discrete = FALSE)+
  labs(title = "CRPS Heatmap",
       x = "District",
       y = "ModelN")

ggsave(
  filename =("a.png"),
  plot = a,
  width = 10,  # Width in inches (107 mm)
  height = 10,  # Height in inches
  units = "in", 
  dpi = 300 ,bg = "white" # Resolution in dpi
)
###Figure 2 (CRPSS,bias,Diffuseness) for TSCV  using five models, baseline and ensemble
#Ensemble bias and diffuseness

ensemble_bias_diff<- readRDS('./Data/cleaned_scores/bais_diff_ensemble_TSCV.rds')

ensemble_bias_diff<- ensemble_bias_diff %>% group_by(horizon)%>%  summarize(bias = mean(bias), sharpness = mean(sharpness))

ensemble_bias_diff <- tibble(
  modN = "ensemble model",  
  horizon = c(1, 2, 3),     
  bias = c(0.265, 0.268, 0.272),  
  sharpness = c(0.67, 0.76, 0.80)  
)

#bias_sharpness_TSCV<- readRDS('./Data/cleaned_scores/sharpness_bias_summary (5).rds')

bias_sharpness_TSCV<- readRDS('./Data/cleaned_scores/sharpness_bias_summary_with_basline_2.rds')

b1 <- inner_join(bias_sharpness_TSCV, obs_case, by = c("district", "date"))


ensemble_mods <- c ('mod4_','mod1_','mod2_','mod3_','PC_lags','modhhh4_power_precip_temp_')


library(ggplot2)
library(dplyr)

# Summarizing bias and sharpness per horizon and model
b1_summary <- b1 %>%
  group_by(modN,horizon) %>%
  summarize(bias = mean(bias), sharpness = mean(sharpness))


b1_summary  <- b1_summary   %>%
  dplyr::mutate(modN = case_when(
    modN == "mod4_" ~ "Baseline",
    modN == "mod1_" ~ "spatio-temporal 1",
    modN == "mod2_" ~ "spatio-temporal 2",
    modN == "mod3_" ~ "spatio-temporal 3",
    modN == "PC_lags" ~ "PCA model",
    modN == "modhhh4_power_precip_temp_" ~ "hhh4 model"
  ))
b1_summary_all_models_ensemble<- rbind(b1_summary,ensemble_bias_diff)
# Plot for Bias
bias_plot <- ggplot(b1_summary_all_models_ensemble, aes(x = horizon, y = bias, group = modN)) +
  geom_line(aes(color = modN, linetype = modN), size = 2, alpha = 0.8) + 
  geom_point(aes(shape = modN), size = 3) +
  ylim(0, 0.70) +  # Set y-axis limits
  scale_x_continuous(breaks = c( 1, 2, 3), limits = c(1, 3)) +  # Set limits for better control
  labs(
    x = "Lead time (months)",
    y = "Bias",
    color = "Model",
    shape = "Model",
    linetype = "Model"
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14, face = "bold"),                  # Bold x-axis title
    axis.title.y = element_text(size = 14, face = "bold"),                  # Bold y-axis title
    axis.text.x = element_text(size = 12, face = "bold"),                   # Larger x-axis tick labels
    axis.text.y = element_text(size = 12, face = "bold"),                   # Larger y-axis tick labels
    legend.position = "bottom",
    legend.key.size = unit(1.5, "lines"),                                   # Larger legend symbols
    legend.title = element_text(size = 14, face = "bold"),                  # Larger legend title
    legend.text = element_text(size = 15),                                  # Larger legend text
    panel.grid.major = element_line(color = "grey80", size = 0.6),          # Thicker major grid lines
    panel.grid.minor = element_line(color = "grey90", size = 0.4)  
  ) +
  scale_color_manual(values = c(
    "spatio-temporal 1" = "red", 
    "spatio-temporal 2" = "green", 
    "spatio-temporal 3" = "blue", 
    "PCA model" = "purple", 
    "hhh4 model" = "orange",
    "Baseline" = "grey",
    "ensemble model" = "darkred"
  )) +
  scale_shape_manual(values = c(
    "Baseline" = 16,  # Circle
    "ensemble model" = 17,  # Triangle
    "spatio-temporal 1" = 18,  # Square
    "spatio-temporal 2" = 18,  # Filled circle
    "spatio-temporal 3" = 18,  # Solid circle
    "PCA model" = 18,  # Circle with fill
    "hhh4 model" = 18   # Diamond
  )) +
  scale_linetype_manual(values = c(
    "Baseline" = "dashed",
    "ensemble model" = "solid",
    "spatio-temporal 1" = "dashed",
    "spatio-temporal 2" = "dashed",
    "spatio-temporal 3" = "dashed",
    "PCA model" = "dashed",
    "hhh4 model" = "dashed"
  )) 
bias_plot




# Plot for Sharpness
sharpness_plot <- ggplot(b1_summary_all_models_ensemble, aes(x = horizon, y = sharpness, group = modN)) +
  geom_line(aes(color = modN, linetype = modN), size = 2, alpha = 0.8) +   geom_point(aes(shape = modN), size = 3) +
  ylim(0, 0.90) +  # Set y-axis limits
  scale_x_continuous(breaks = c( 1, 2, 3), limits = c(1, 3)) +  # Set limits for better control
  labs(
    x = "Lead time (months)",
    y = "Diffuseness",
    color = "Model",
    shape = "Model",
    linetype = "Model"
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14, face = "bold"),                  # Bold x-axis title
    axis.title.y = element_text(size = 14, face = "bold"),                  # Bold y-axis title
    axis.text.x = element_text(size = 12, face = "bold"),                   # Larger x-axis tick labels
    axis.text.y = element_text(size = 12, face = "bold"),                   # Larger y-axis tick labels
    legend.position = "bottom",
    legend.key.size = unit(1.5, "lines"),                                   # Larger legend symbols
    legend.title = element_text(size = 14, face = "bold"),                  # Larger legend title
    legend.text = element_text(size = 15),                                  # Larger legend text
    panel.grid.major = element_line(color = "grey80", size = 0.6),          # Thicker major grid lines
    panel.grid.minor = element_line(color = "grey90", size = 0.4)  
  ) +
  scale_color_manual(values = c(
    "spatio-temporal 1" = "red", 
    "spatio-temporal 2" = "green", 
    "spatio-temporal 3" = "blue", 
    "PCA model" = "purple", 
    "hhh4 model" = "orange",
    "Baseline" = "grey",
    "ensemble model" = "darkred"
  )) +
  scale_shape_manual(values = c(
    "Baseline" = 16,  # Circle
    "ensemble model" = 17,  # Triangle
    "spatio-temporal 1" = 18,  # Square
    "spatio-temporal 2" = 18,  # Filled circle
    "spatio-temporal 3" = 18,  # Solid circle
    "PCA model" = 18,  # Circle with fill
    "hhh4 model" = 18   # Diamond
  )) +
  scale_linetype_manual(values = c(
    "Baseline" = "dashed",
    "ensemble model" = "solid",
    "spatio-temporal 1" = "dashed",
    "spatio-temporal 2" = "dashed",
    "spatio-temporal 3" = "dashed",
    "PCA model" = "dashed",
    "hhh4 model" = "dashed"
  )) 
sharpness_plot
####CRPSS
out <- readRDS( "./Data/cleaned_scores/all_crps_slim_updated_lag3_Final_with_baseline_TSCV.rds") 

out<- out %>% filter(vintage_date <= '2016-12-01')

miss.mod <- out %>%
  filter(horizon %in% c(1,2,3) ) %>%
  group_by(modN, horizon) %>%
  dplyr::summarize(N=n()) %>%
  mutate(exclude_miss_mod = N<max(N)) %>%
  ungroup()

miss.dates <- out %>% 
  full_join(miss.mod, by=c('modN', 'horizon')) %>%
  filter(exclude_miss_mod==F) %>%
  group_by(date, horizon) %>%   
  filter(  horizon %in% c(1,2,3)) %>%
  dplyr::summarize(N_mods=n(), N_cases=mean(m_DHF_cases)) %>%
  ungroup() %>%
  group_by(horizon) %>%
  mutate(miss_date = if_else(N_mods< max(N_mods),1,0 )) %>%
  ungroup()

#note this is not a proper time series--we are double counting cases across models.
ggplot(miss.dates, aes(x=date, y=N_cases)) +
  theme_classic()+
  geom_line()+
  facet_wrap(~horizon) +
  geom_point(aes(x=date, y=N_cases, color=miss_date))

#FILTER OUT months when an epidemic has been recognized by the time forecast is made in a specific district (using fixed epidemic threshold)
out_1a <- out %>%
  left_join(miss.mod, by=c('horizon','modN') )%>%
  #filter(exclude_miss_mod!=1) %>%
  dplyr::select(-pop,-m_DHF_cases) %>%
  left_join(obs_epidemics, by=c('district'='district','vintage_date'='date'))   #%>%


out2 <- out_1a %>%
  filter(epidemic_flag==0 ) %>%
  left_join(miss.dates, by=c('date','horizon')) %>%
  #filter(miss_date==0 & exclude_miss_mod==0)  %>%
  group_by(horizon, modN, form) %>%
  dplyr::summarize(crps1 = mean(crps1),crps2 = mean(crps2) ,N=n() ) %>%
  ungroup() %>%
  arrange(horizon, crps2) %>%
  group_by(horizon) %>%
  dplyr::mutate( w_i1 = (1/crps1^2)/sum(1/crps1^2),w_i2 = (1/crps2^2)/sum(1/crps2^2) ) 
#View(out2)


# First, filter out the baseline model for each horizon

all_models<- out2[,c(1,2,4)]

ensemble_crps <- tibble(
  modN = "ensemble model",  
  horizon = c(1, 2, 3),     
  crps1 = c(2.4, 2.9, 3.2),  
)

all_models_crps<- rbind(all_models,ensemble_crps)


all_models_crps <- all_models_crps  %>%
  dplyr::mutate(modN = case_when(
    modN == "mod4_" ~ "Baseline",
    modN == "mod1_" ~ "spatio-temporal 1",
    modN == "mod2_" ~ "spatio-temporal 2",
    modN == "mod3_" ~ "spatio-temporal 3",
    modN == "PC_lags" ~ "PCA model",
    modN == "modhhh4_power_precip_temp_" ~ "hhh4 model",
    modN == "ensemble model" ~ "ensemble model"
  ))

baseline_crps <- all_models_crps %>%
  filter(modN == "Baseline") %>%
  dplyr::select(horizon, crps1)  %>%rename(baseline_crps1 = crps1)

# Join the baseline CRPS1 with the original data to calculate CRPSS2
all_models_crps<- all_models_crps %>%
  left_join(baseline_crps, by = "horizon") %>%
  mutate(CRPSS = 1 - (crps1 / baseline_crps1))

# Plot the CRPSS over the lead time (horizon)
CRPSS<- ggplot(all_models_crps, aes(x = horizon, y = CRPSS, group = modN)) +
  geom_line(aes(color = modN, linetype = modN), size = 2, alpha = 0.8) +   geom_point(aes(shape = modN), size = 3) +
  ylim(0, 0.90) +  # Set y-axis limits
  scale_x_continuous(breaks = c( 1, 2, 3), limits = c(1, 3)) +  # Set limits for better control
  labs(
    x = "Lead time (months)",
    y = "CRPSS",
    color = "Model",
    shape = "Model",
    linetype = "Model"
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14, face = "bold"),                  # Bold x-axis title
    axis.title.y = element_text(size = 14, face = "bold"),                  # Bold y-axis title
    axis.text.x = element_text(size = 12, face = "bold"),                   # Larger x-axis tick labels
    axis.text.y = element_text(size = 12, face = "bold"),                   # Larger y-axis tick labels
    legend.position = "bottom",
    legend.key.size = unit(1.5, "lines"),                                   # Larger legend symbols
    legend.title = element_text(size = 14, face = "bold"),                  # Larger legend title
    legend.text = element_text(size = 15),                                  # Larger legend text
    panel.grid.major = element_line(color = "grey80", size = 0.6),          # Thicker major grid lines
    panel.grid.minor = element_line(color = "grey90", size = 0.4)  
  ) +
  scale_color_manual(values = c(
    "spatio-temporal 1" = "red", 
    "spatio-temporal 2" = "green", 
    "spatio-temporal 3" = "blue", 
    "PCA model" = "purple", 
    "hhh4 model" = "orange",
    "Baseline" = "grey",
    "ensemble model" = "darkred"
  )) +
  scale_shape_manual(values = c(
    "Baseline" = 16,  # Circle
    "ensemble model" = 17,  # Triangle
    "spatio-temporal 1" = 18,  # Square
    "spatio-temporal 2" = 18,  # Filled circle
    "spatio-temporal 3" = 18,  # Solid circle
    "PCA model" = 18,  # Circle with fill
    "hhh4 model" = 18   # Diamond
  )) +
  scale_linetype_manual(values = c(
    "Baseline" = "dashed",
    "ensemble model" = "solid",
    "spatio-temporal 1" = "dashed",
    "spatio-temporal 2" = "dashed",
    "spatio-temporal 3" = "dashed",
    "PCA model" = "dashed",
    "hhh4 model" = "dashed"
  )) 
CRPSS


library(ggpubr)

brier<-   readRDS('./Data/cleaned_scores/brier_out_ensemble_F.rds') %>% mutate(monthN=month(date))
b1_5models<-  readRDS('./Data/cleaned_scores/brier_summary_updated_brier_lag3_Final_with_baseline (2).rds')

b1  <- b1_5models  %>%
  dplyr::mutate(modN = case_when(
    modN == "mod4_" ~ "Baseline",
    modN == "mod1_" ~ "spatio-temporal 1",
    modN == "mod2_" ~ "spatio-temporal 2",
    modN == "mod3_" ~ "spatio-temporal 3",
    modN == "PC_lags" ~ "PCA model",
    modN == "modhhh4_power_precip_temp_" ~ "hhh4 model"
  ))


b2 <- inner_join(b1, obs_case, by = c("district", "date"))

brier$modN <- "ensemble model"

combined_data <- rbind(brier, b1)





A<- combined_data  %>%
  dplyr::group_by(horizon, modN) %>%
  dplyr::summarize(brier_2sd = mean(brier_2sd)) %>%
  ggplot(aes(x = horizon, y = brier_2sd, group = modN, color = modN)) +
  geom_line(aes(color = modN, linetype = modN), size = 2, alpha = 0.8) +   geom_point(aes(shape = modN), size = 3) +
  ylim(0, 0.20) +  # Set y-axis limits
  scale_x_continuous(breaks = c( 1, 2, 3), limits = c(1, 3)) +  # Set limits for better control
  labs(
    x = "Lead time (months)",
    y = "Brier score",
    color = "Model",
    shape = "Model",
    linetype = "Model"
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14, face = "bold"),                  # Bold x-axis title
    axis.title.y = element_text(size = 14, face = "bold"),                  # Bold y-axis title
    axis.text.x = element_text(size = 12, face = "bold"),                   # Larger x-axis tick labels
    axis.text.y = element_text(size = 12, face = "bold"),                   # Larger y-axis tick labels
    legend.position = "bottom",
    legend.key.size = unit(1.5, "lines"),                                   # Larger legend symbols
    legend.title = element_text(size = 14, face = "bold"),                  # Larger legend title
    legend.text = element_text(size = 15),                                  # Larger legend text
    panel.grid.major = element_line(color = "grey80", size = 0.6),          # Thicker major grid lines
    panel.grid.minor = element_line(color = "grey90", size = 0.4)  
  ) +
  scale_color_manual(values = c(
    "spatio-temporal 1" = "red", 
    "spatio-temporal 2" = "green", 
    "spatio-temporal 3" = "blue", 
    "PCA model" = "purple", 
    "hhh4 model" = "orange",
    "Baseline" = "grey",
    "ensemble model" = "darkred"
  )) +
  scale_shape_manual(values = c(
    "Baseline" = 16,  # Circle
    "ensemble model" = 17,  # Triangle
    "spatio-temporal 1" = 18,  # Square
    "spatio-temporal 2" = 18,  # Filled circle
    "spatio-temporal 3" = 18,  # Solid circle
    "PCA model" = 18,  # Circle with fill
    "hhh4 model" = 18   # Diamond
  )) +
  scale_linetype_manual(values = c(
    "Baseline" = "dashed",
    "ensemble model" = "solid",
    "spatio-temporal 1" = "dashed",
    "spatio-temporal 2" = "dashed",
    "spatio-temporal 3" = "dashed",
    "PCA model" = "dashed",
    "hhh4 model" = "dashed"
  )) 




combined_plot_1 <- ggarrange(CRPSS,bias_plot, sharpness_plot,A, 
                           common.legend = TRUE,  # Use a common legend for both plots
                           legend = "bottom",labels = c('a','b','c','d'),     # Place the legend at the bottom
                           ncol = 2, nrow = 2)  

# To display the combined plot

 ggsave("combined_plot_1.png", plot = combined_plot_1, width = 10, height = 8, dpi = 1000, bg = "white")

###Figure S4 in the appendix

b1  <- b1  %>%
  dplyr::mutate(modN = case_when(
    modN == "mod4_" ~ "Baseline",
    modN == "mod1_" ~ "spatio-temporal 1",
    modN == "mod2_" ~ "spatio-temporal 2",
    modN == "mod3_" ~ "spatio-temporal 3",
    modN == "PC_lags" ~ "PCA model",
    modN == "modhhh4_power_precip_temp_" ~ "hhh4 model"
  ))

# Assuming 'b1' is your dataset
# Convert monthN to factor with month names
b1 <- b1 %>%
  mutate(month_name = factor(monthN, levels = 1:12, 
                             labels = c("Jan", "Feb", "Mar", "Apr", "May", 
                                        "Jun", "Jul", "Aug", "Sep", "Oct", 
                                        "Nov", "Dec")))

b1_summary <- b1 %>%
  group_by(month_name, horizon) %>%
  summarise(
    mean_bias = mean(bias, na.rm = TRUE),
    mean_sharpness = mean(sharpness, na.rm = TRUE),
    .groups = 'drop'
  )


# Plot Bias
p_bias <- ggplot(b1_summary, aes(x = month_name, y = mean_bias, color = factor(horizon), group = factor(horizon))) +
  geom_line(size = 1, linetype = "solid") +
  geom_point(size = 3) +
  labs(x = "Month of the year", y = "Bias", color = "Horizon", linetype = "Horizon") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Plot Sharpness
p_sharpness <- ggplot(b1_summary, aes(x = month_name, y = mean_sharpness, color = factor(horizon), group = factor(horizon))) +
  geom_line(size = 1, linetype = "solid") +
  geom_point(size = 3) +
  labs(x = "Month of the year", y = "Diffuseness", color = "Horizon", linetype = "Horizon") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Print plots
p_bias
p_sharpness

ggarrange(p_bias,p_sharpness, 
          common.legend = TRUE,  # Use a common legend for both plots
          legend = "bottom",labels = c('a','b'),     # Place the legend at the bottom
          ncol = 2, nrow = 1) 

##############################Fro figure 3

quantile_summary <-readRDS( './Data/cleaned_scores/quantile_summary_ensemble_updated_TSCV_10000samples.rds')

print(quantile_summary)

quantile_summary <- quantile_summary %>%
  mutate(date = as.Date(date))


final_summary_all <- quantile_summary %>%
  left_join(obs_case, by = c("date", "district")) %>%
  select(date, district, horizon, median, lower_CI, upper_CI, m_DHF_cases, pop,mean)



final_summary_grouped <- final_summary_all %>%
  group_by(date, horizon) %>%
  summarise(
    mean=sum(mean),
    m_DHF_cases = sum(m_DHF_cases, na.rm = TRUE),
    median = sum(median, na.rm = TRUE),
    pred_lcl = sum(lower_CI, na.rm = TRUE),
    pred_ucl = sum(upper_CI, na.rm = TRUE)
  ) %>%
  ungroup()


combined_plot_CI <- ggplot(final_summary_grouped, aes(x = date)) +
  geom_line(aes(y = median, color = factor(horizon)), lwd = 1.2) +  # Median predictions for each horizon (solid lines)
  geom_ribbon(aes(ymin = pred_lcl, ymax = pred_ucl, fill = factor(horizon)), alpha = 0.2) +  # CI ribbon for each horizon
  geom_line(aes(y = m_DHF_cases), lwd = 1.5, color = "red") +  # Observed m_DHF_cases as a solid line
  theme_classic() +
  labs(
    x = "Date", y = "Dengue Cases", color = "Legend", fill = "Horizon") +  # Labels for legend
  geom_hline(yintercept = 43.75, linetype = "solid", color = "gray") +  # Reference line
  scale_color_manual(values = c("1" = "#ff7f00", "2" = "#33a02c", "3" = "darkblue", "Observed Cases" = "red")) +  # Custom colors for lines
  scale_fill_manual(values = c("1" = "#fdbf6f", "2" = "#b2df8a", "3" = "#a6cee3")) +  # Custom colors for CI ribbons
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")  # Custom x-axis with year labels

# Display the combined plot
print(combined_plot_CI)

# Combined Plot with Observed Cases in the Legend
combined_plot_CI <- ggplot(final_summary_grouped, aes(x = date)) +
  geom_line(aes(y = median, color = factor(horizon)), lwd = 1.2) +  # Median predictions for each horizon
  geom_ribbon(aes(ymin = pred_lcl, ymax = pred_ucl, fill = factor(horizon), color = factor(horizon)), 
              alpha = 0.2, show.legend = FALSE) +  # CI ribbon for each horizon
  geom_line(aes(y = m_DHF_cases, color = "Observed Cases"), lwd = 1.5) +  # Observed cases with color mapped
  theme_classic() +
  labs(
    x = "Date", y = "Dengue Cases", color = "Legend", fill = "Horizon") +  # Unified legend labels
  geom_hline(yintercept = 43.75, linetype = "solid", color = "gray") +  # Reference line
  scale_color_manual(values = c("1" = "#ff7f00", "2" = "#33a02c", "3" = "darkblue", "Observed Cases" = "red")) +  # Custom colors
  scale_fill_manual(values = c("1" = "#fdbf6f", "2" = "#b2df8a", "3" = "#a6cee3")) +  # Custom fill colors
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")  # Custom x-axis with year labels

# Display the combined plot
print(combined_plot_CI)

# Combined Plot with Observed Cases in the Legend and Dashed Bounds
combined_plot_CI <- ggplot(final_summary_grouped, aes(x = date)) +
  geom_line(aes(y = median, color = factor(horizon)), lwd = 1.2) +  # Median predictions for each horizon
  geom_line(aes(y = pred_lcl, color = factor(horizon)), linetype = "dashed", lwd = 0.8) +  # Lower bound as dashed line
  geom_line(aes(y = pred_ucl, color = factor(horizon)), linetype = "dashed", lwd = 0.8) +  # Upper bound as dashed line
  geom_line(aes(y = m_DHF_cases, color = "Observed Cases"), lwd = 1.5) +  # Observed cases with color mapped
  theme_classic() +
  labs(
    x = "Date", y = "Dengue Cases", color = "Horizon", fill = "Horizon") +  # Unified legend labels
  geom_hline(yintercept = 43.75, linetype = "solid", color = "gray") +  # Reference line
  scale_color_manual(values = c("1" = "#ff7f00", "2" = "#33a02c", "3" = "darkblue", "Observed Cases" = "red")) +  # Custom colors
  scale_fill_manual(values = c("1" = "#fdbf6f", "2" = "#b2df8a", "3" = "#a6cee3")) +  # Custom fill colors
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")  # Custom x-axis with year labels

# Display the combined plot
print(combined_plot_CI)


  ggsave("combined_plot_CI2.png", plot = combined_plot_CI,
  width = 8,  # Width in inches (107 mm)
  height = 9,  # Height in inches
  units = "in", 
  dpi = 300  # Resolution in dpi
)

 #ggsave("combined_plot_CI.png", plot = combined_plot_CI, width = 10, height = 8, dpi = 300, bg = "white")

##Figure S5 in appendix from shiny app

##figure 4 brier score by month

#brier<-  readRDS('./Data/cleaned_scores/brier_ensemble_TSCV.rds') %>% mutate(monthN=month(date))
brier<-  readRDS('./Data/cleaned_scores/brier_out_ensemble_F.rds') %>% mutate(monthN=month(date))

b2_brier <- inner_join(brier, obs_case, by = c("district", "date"))


ensemble_mods <- c ('mod1_','mod2_','mod3_','PC_lags','modhhh4_power_precip_temp_')


monthly_brier <- b2_brier %>%
  group_by(monthN) %>%
  summarise(
    mean_brier_nb = mean(brier_nb, na.rm = TRUE),
    mean_brier_2sd = mean(brier_2sd, na.rm = TRUE),
    mean_brier_poisson = mean(brier_poisson, na.rm = TRUE),
    mean_brier_quant = mean(brier_quant, na.rm = TRUE),
    mean_brier_fix_100 = mean(brier_fix_100, na.rm = TRUE),
    mean_brier_fix_150 = mean(brier_fix_150, na.rm = TRUE),
    mean_brier_fix_200 = mean(brier_fix_200, na.rm = TRUE),
    mean_brier_fix_300 = mean(brier_fix_300, na.rm = TRUE),
    mean_brier_fix_20 = mean(brier_fix_20, na.rm = TRUE),
    mean_brier_fix_50 = mean(brier_fix_50, na.rm = TRUE)
  )


plottt<- ggplot(monthly_brier, aes(x = monthN)) +
  geom_line(aes(y = mean_brier_2sd, color = "brier_2sd"), size = 1, linetype = "dashed") +
  geom_point(aes(y = mean_brier_2sd, color = "brier_2sd"), size = 2) +
  geom_line(aes(y = mean_brier_poisson, color = "brier_poisson"), size = 1, linetype = "dotted") +
  geom_point(aes(y = mean_brier_poisson, color = "brier_poisson"), size = 2) +
  geom_line(aes(y = mean_brier_quant, color = "brier_quant"), size = 1, linetype = "dotdash") +
  geom_point(aes(y = mean_brier_quant, color = "brier_quant"), size = 2) +
  geom_line(aes(y = mean_brier_fix_20, color = "brier_fix_20"), size = 1, linetype = "dashed") +
  geom_point(aes(y = mean_brier_fix_20, color = "brier_fix_20"), size = 2) +
  geom_line(aes(y = mean_brier_fix_50, color = "brier_fix_50"), size = 1, linetype = "solid") +
  geom_point(aes(y = mean_brier_fix_50, color = "brier_fix_50"), size = 2) +
  scale_color_manual(values = c(
    "brier_2sd" = "red",
    "brier_poisson" = "green",
    "brier_quant" = "purple",
    "brier_fix_20" = "black",
    "brier_fix_50" = "magenta"
  ),
  labels = c(
    "brier_2sd" = "Mean + 2 SD",
    "brier_poisson" = "Poisson threshold",
    "brier_quant" = "95th percentile",
    "brier_fix_20" = "20 cases/100k per two weeks",
    "brier_fix_50" = "50 cases/100k per two weeks"
  )) +
  scale_x_continuous(breaks = 1:12, 
                     labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  labs(
    x = "Month of the Year",
    y = "Brier Score",
    title = "Brier Score by Month",
    color = "Threshold Definition"
  ) +
  theme_minimal() +theme(
    axis.title.x = element_text(size = 14, face = "bold"),                  # Bold x-axis title
    axis.title.y = element_text(size = 14, face = "bold"),                  # Bold y-axis title
    axis.text.x = element_text(size = 12, face = "bold"),                   # Larger x-axis tick labels
    axis.text.y = element_text(size = 12, face = "bold"),                   # Larger y-axis tick labels                                  # Larger legend symbols
    legend.title = element_text(size = 14, face = "bold"),                  # Larger legend title
    legend.text = element_text(size = 15),                                  # Larger legend text
  )

ggsave(
  filename =("b1_.png"),
  plot = plottt,
  width = 9,  # Width in inches (107 mm)
  height = 7,  # Height in inches
  units = "in", 
  dpi = 1000 ,bg = "white" # Resolution in dpi
)
####Figure S6 in the appendix 

brier<-   readRDS('./Data/cleaned_scores/brier_out_ensemble_F.rds') %>% mutate(monthN=month(date))
b1_5models<-  readRDS('./Data/cleaned_scores/brier_summary_updated_brier_lag3_Final_with_baseline (2).rds')

b1  <- b1_5models  %>%
  dplyr::mutate(modN = case_when(
    modN == "mod4_" ~ "Baseline",
    modN == "mod1_" ~ "spatio-temporal 1",
    modN == "mod2_" ~ "spatio-temporal 2",
    modN == "mod3_" ~ "spatio-temporal 3",
    modN == "PC_lags" ~ "PCA model",
    modN == "modhhh4_power_precip_temp_" ~ "hhh4 model"
  ))


b2 <- inner_join(b1, obs_case, by = c("district", "date"))

brier$modN <- "ensemble model"

combined_data <- rbind(brier, b1)





A<- combined_data  %>%
  dplyr::group_by(horizon, modN) %>%
  dplyr::summarize(brier_2sd = mean(brier_2sd)) %>%
  ggplot(aes(x = horizon, y = brier_2sd, group = modN, color = modN)) +
  geom_line(aes(color = modN, linetype = modN), size = 1, alpha = 0.7) +   geom_point(aes(shape = modN), size = 3) +
  ylim(0, 0.20) +  # Set y-axis limits
  scale_x_continuous(breaks = c( 1, 2, 3), limits = c(1, 3)) +  # Set limits for better control
  labs(
    x = "Lead time (months)",
    y = "Brier score",
    color = "Model",
    shape = "Model",
    linetype = "Model"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.major = element_line(color = "grey80", size = 0.5),  # Major grid lines
    panel.grid.minor = element_line(color = "grey90", size = 0.25)   # Minor grid lines
  ) +
  scale_color_manual(values = c(
    "spatio-temporal 1" = "red", 
    "spatio-temporal 2" = "green", 
    "spatio-temporal 3" = "blue", 
    "PCA model" = "purple", 
    "hhh4 model" = "orange",
    "Baseline" = "grey",
    "ensemble model" = "darkred"
  )) +
  scale_shape_manual(values = c(
    "Baseline" = 16,  # Circle
    "ensemble model" = 17,  # Triangle
    "spatio-temporal 1" = 18,  # Square
    "spatio-temporal 2" = 18,  # Filled circle
    "spatio-temporal 3" = 18,  # Solid circle
    "PCA model" = 18,  # Circle with fill
    "hhh4 model" = 18   # Diamond
  )) +
  scale_linetype_manual(values = c(
    "Baseline" = "dashed",
    "ensemble model" = "solid",
    "spatio-temporal 1" = "dashed",
    "spatio-temporal 2" = "dashed",
    "spatio-temporal 3" = "dashed",
    "PCA model" = "dashed",
    "hhh4 model" = "dashed"
  )) +
  theme(legend.title = element_text(size = 10), legend.text = element_text(size = 9))

A

combined_data <- rbind(brier, b1)

combined_data  <- combined_data  %>%
  dplyr::mutate(modN = case_when(
    modN == "mod4_" ~ "Baseline",
    modN == "mod1_" ~ "spatio-temporal 1",
    modN == "mod2_" ~ "spatio-temporal 2",
    modN == "mod3_" ~ "spatio-temporal 3",
    modN == "PC_lags" ~ "PCA model",
    modN == "modhhh4_power_precip_temp_" ~ "hhh4 model"
  ))

# Assuming 'b1' is your dataset
# Convert monthN to factor with month names
combined_data <- combined_data %>%
  mutate(month_name = factor(monthN, levels = 1:12, 
                             labels = c("Jan", "Feb", "Mar", "Apr", "May", 
                                        "Jun", "Jul", "Aug", "Sep", "Oct", 
                                        "Nov", "Dec")))

b1_summary <- combined_data %>%
  group_by(month_name, horizon) %>% filter(modN=='ensemble model') %>%
  dplyr::summarise(
    brier_2sd=mean(brier_2sd),
    brier_nb=mean(brier_nb),
    .groups = 'drop'
  )



B <- ggplot(b1_summary, aes(x = month_name, y = brier_2sd, color = factor(horizon), group = factor(horizon))) +
  geom_line(size = 1, linetype = "solid") +  # Set linetype to solid
  geom_point(size = 3) +
  labs(x = "Month of the year", y = "Brier score", color = "Horizon", linetype = "Horizon") +
  theme_minimal() +
  theme(legend.position = "bottom")

library(ggpubr)
com<- ggarrange(A,B ,
          labels = c('a','b'),     # Place the legend at the bottom
          ncol = 2, nrow = 1) 

ggsave(
  filename =("com.png"),
  plot =com,
  width = 10,  # Width in inches (107 mm)
  height = 6,  # Height in inches
  units = "in", 
  dpi = 300 ,bg = "white" # Resolution in dpi
)
##Figure S7 is in evaluate_threshold_for_paper.R

##########Out of sample
##figure 5

bias_sharpness<- readRDS('./Data/cleaned_scores/sharpness_bias_summary_all_dates_F (2).rds') %>% filter(date <='2022-12-01')


b1 <- inner_join(bias_sharpness, obs_case, by = c("district", "date"))


ensemble_mods <- c ('mod1_','mod2_','mod3_','PC_lags','modhhh4_power_precip_temp_')

library(ggplot2)
library(dplyr)

# Summarizing bias and sharpness per horizon and model
b1_summary <- b1 %>%
  group_by(modN,horizon) %>%
  dplyr::summarize(bias = mean(bias), sharpness = mean(sharpness))


b1_summary  <- b1_summary   %>%
  dplyr::mutate(modN = case_when(
    modN == "mod4_" ~ "Baseline",
    modN == "mod1_" ~ "spatio-temporal 1",
    modN == "mod2_" ~ "spatio-temporal 2",
    modN == "mod3_" ~ "spatio-temporal 3",
    modN == "PC_lags" ~ "PCA model",
    modN == "modhhh4_power_precip_temp_" ~ "hhh4 model"
  ))


ensemble_bias_diff_OOB <- tibble(
  modN = "ensemble model",  
  horizon = c(1, 2, 3),     
  bias = c(0.208, 0.204, 0.202),  
  sharpness = c(0.671, 0.755, 0.815)  
)

b1_summary_all_models_ensemble_OOB<- rbind(b1_summary,ensemble_bias_diff_OOB)


# Plot for Bias
bias_plot_OOB <- ggplot(b1_summary_all_models_ensemble_OOB, aes(x = horizon, y = bias, group = modN)) +
  geom_line(aes(color = modN, linetype = modN), size = 2, alpha = 0.8) + 
  geom_point(aes(shape = modN), size = 3) +
  scale_x_continuous(breaks = c( 1, 2, 3), limits = c(1, 3)) +  # Set limits for better control
  labs(
    x = "Lead time (months)",
    y = "Bias",
    color = "Model",
    shape = "Model",
    linetype = "Model"
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14, face = "bold"),                  # Bold x-axis title
    axis.title.y = element_text(size = 14, face = "bold"),                  # Bold y-axis title
    axis.text.x = element_text(size = 12, face = "bold"),                   # Larger x-axis tick labels
    axis.text.y = element_text(size = 12, face = "bold"),                   # Larger y-axis tick labels
    legend.position = "bottom",
    legend.key.size = unit(1.5, "lines"),                                   # Larger legend symbols
    legend.title = element_text(size = 14, face = "bold"),                  # Larger legend title
    legend.text = element_text(size = 15),                                  # Larger legend text
    panel.grid.major = element_line(color = "grey80", size = 0.6),          # Thicker major grid lines
    panel.grid.minor = element_line(color = "grey90", size = 0.4)  
  ) +
  scale_color_manual(values = c(
    "spatio-temporal 1" = "red", 
    "spatio-temporal 2" = "green", 
    "spatio-temporal 3" = "blue", 
    "PCA model" = "purple", 
    "hhh4 model" = "orange",
    "Baseline" = "grey",
    "ensemble model" = "darkred"
  )) +
  scale_shape_manual(values = c(
    "Baseline" = 16,  # Circle
    "ensemble model" = 17,  # Triangle
    "spatio-temporal 1" = 18,  # Square
    "spatio-temporal 2" = 18,  # Filled circle
    "spatio-temporal 3" = 18,  # Solid circle
    "PCA model" = 18,  # Circle with fill
    "hhh4 model" = 18   # Diamond
  )) +
  scale_linetype_manual(values = c(
    "Baseline" = "dashed",
    "ensemble model" = "solid",
    "spatio-temporal 1" = "dashed",
    "spatio-temporal 2" = "dashed",
    "spatio-temporal 3" = "dashed",
    "PCA model" = "dashed",
    "hhh4 model" = "dashed"
  )) 

bias_plot_OOB


# Plot for Sharpness
sharpness_plot <- ggplot(b1_summary_all_models_ensemble_OOB, aes(x = horizon, y = sharpness, group = modN)) +
  geom_line(aes(color = modN, linetype = modN), size = 2, alpha = 0.8) +   geom_point(aes(shape = modN), size = 3) +
  ylim(0, 0.90) +  # Set y-axis limits
  scale_x_continuous(breaks = c( 1, 2, 3), limits = c(1, 3)) +  # Set limits for better control
  labs(
    x = "Lead time (months)",
    y = "Diffuseness",
    color = "Model",
    shape = "Model",
    linetype = "Model"
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14, face = "bold"),                  # Bold x-axis title
    axis.title.y = element_text(size = 14, face = "bold"),                  # Bold y-axis title
    axis.text.x = element_text(size = 12, face = "bold"),                   # Larger x-axis tick labels
    axis.text.y = element_text(size = 12, face = "bold"),                   # Larger y-axis tick labels
    legend.position = "bottom",
    legend.key.size = unit(1.5, "lines"),                                   # Larger legend symbols
    legend.title = element_text(size = 14, face = "bold"),                  # Larger legend title
    legend.text = element_text(size = 15),                                  # Larger legend text
    panel.grid.major = element_line(color = "grey80", size = 0.6),          # Thicker major grid lines
    panel.grid.minor = element_line(color = "grey90", size = 0.4)  
  ) +
  scale_color_manual(values = c(
    "spatio-temporal 1" = "red", 
    "spatio-temporal 2" = "green", 
    "spatio-temporal 3" = "blue", 
    "PCA model" = "purple", 
    "hhh4 model" = "orange",
    "Baseline" = "grey",
    "ensemble model" = "darkred"
  )) +
  scale_shape_manual(values = c(
    "Baseline" = 16,  # Circle
    "ensemble model" = 17,  # Triangle
    "spatio-temporal 1" = 18,  # Square
    "spatio-temporal 2" = 18,  # Filled circle
    "spatio-temporal 3" = 18,  # Solid circle
    "PCA model" = 18,  # Circle with fill
    "hhh4 model" = 18   # Diamond
  )) +
  scale_linetype_manual(values = c(
    "Baseline" = "dashed",
    "ensemble model" = "solid",
    "spatio-temporal 1" = "dashed",
    "spatio-temporal 2" = "dashed",
    "spatio-temporal 3" = "dashed",
    "PCA model" = "dashed",
    "hhh4 model" = "dashed"
  )) 

sharpness_plot
####CRPSS
#out <- readRDS( "./Data/cleaned_scores/all_crps_slim_updated_lag3_check.rds") %>% filter(date <= '2022-10-01')
out <- readRDS( "./Data/cleaned_scores/all_crps_slim_updated_lag3_check_all_dateswithbaseline_2022_OOB.rds") %>% filter(date <= '2022-10-01')


miss.mod <- out %>%
  filter(horizon %in% c(3) & date <= '2022-10-01') %>%
  group_by(modN, horizon) %>%
  dplyr::summarize(N=n()) %>%
  mutate(exclude_miss_mod = N<max(N)) %>%
  ungroup()

miss.dates <- out %>% 
  full_join(miss.mod, by=c('modN', 'horizon')) %>%
  #filter(exclude_miss_mod==F) %>%
  group_by(date, horizon) %>%   
  filter(  horizon %in% c(3) ) %>%
  dplyr::summarize(N_mods=n(), N_cases=mean(m_DHF_cases)) %>%
  ungroup() %>%
  group_by(horizon) %>%
  mutate(miss_date = if_else(N_mods< max(N_mods),1,0 )) %>%
  ungroup()





#note this is not a proper time series--we are double counting cases across models.
ggplot(miss.dates, aes(x=date, y=N_cases)) +
  theme_classic()+
  geom_line()+
  facet_wrap(~horizon) +
  geom_point(aes(x=date, y=N_cases, color=miss_date))

#FILTER OUT months when an epidemic has been recognized by the time forecast is made in a specific district (using fixed epidemic threshold)
out_1a <- out %>%
  left_join(miss.mod, by=c('horizon','modN') )%>%
  #filter(exclude_miss_mod!=1) %>%
  dplyr::select(-pop,-m_DHF_cases) %>%
  left_join(obs_epidemics, by=c('district'='district','vintage_date'='date'))   #%>%
#filter(epidemic_flag==0) #ONLY EVALUATE MONTHS WHERE EPIDEMIC HAS NOT YET BEEN OBSERVED IN THE DISTRICT

#View(out_1a %>% group_by(district,date, horizon) %>% dplyr::summarize(N=n()))


#Overall
out2 <- out_1a %>%
  filter(epidemic_flag==0 ) %>%
  left_join(miss.dates, by=c('date','horizon')) %>%
  #filter(miss_date==0 & exclude_miss_mod==0)  %>%
  group_by(horizon, modN, form) %>%
  dplyr::summarize(crps1 = mean(crps1),crps2 = mean(crps2) ,N=n() ) %>%
  ungroup() %>%
  arrange(horizon, crps2) %>%
  group_by(horizon) %>%
  dplyr::mutate( w_i1 = (1/crps1^2)/sum(1/crps1^2),w_i2 = (1/crps2^2)/sum(1/crps2^2) ) %>%
  # filter(horizon==3)%>%
  mutate(rw_season = grepl('cyclic=TRUE', form),
         harm_season = grepl('sin12', form),
         lag2_y = grepl('lag2_y', form),
         lag_y = grepl('lag_y', form),
         lag2_monthly_cum_ppt =grepl('lag2_monthly_cum_ppt', form),
         iid_spat_intercept=grepl('f(districtID,model = "iid")',form, fixed=T),
         rw_time_spatial=grepl(' f(t, replicate=districtID3, model="rw1", hyper = hyper2.rw) ',form, fixed=T),
         type4_spatial_bym = grepl('model="bym"', form, fixed=T) *grepl('control.group=list(model="ar1"', form, fixed=T) *grepl('group=time_id1', form, fixed=T)
  )
#View(out2)


# First, filter out the baseline model for each horizon

all_models<- out2[,c(1,2,4)]

ensemble_crps <- tibble(
  modN = "ensemble model",  
  horizon = c(1, 2, 3),     
  crps1 = c(2.4, 3.4, 4.5),  
)

all_models_crps<- rbind(all_models,ensemble_crps)


all_models_crps <- all_models_crps  %>%
  dplyr::mutate(modN = case_when(
    modN == "mod4_" ~ "Baseline",
    modN == "mod1_" ~ "spatio-temporal 1",
    modN == "mod2_" ~ "spatio-temporal 2",
    modN == "mod3_" ~ "spatio-temporal 3",
    modN == "PC_lags" ~ "PCA model",
    modN == "modhhh4_power_precip_temp_" ~ "hhh4 model",
    modN == "ensemble model" ~ "ensemble model"
  ))

baseline_crps <- all_models_crps %>%
  filter(modN == "Baseline") %>%
  dplyr::select(horizon, crps1)  %>%rename(baseline_crps1 = crps1)

# Join the baseline CRPS1 with the original data to calculate CRPSS2
all_models_crps<- all_models_crps %>%
  left_join(baseline_crps, by = "horizon") %>%
  mutate(CRPSS = 1 - (crps1 / baseline_crps1))

all_models_crps[all_models_crps$horizon=='3',]
# Plot the CRPSS over the lead time (horizon)
CRPSS<- ggplot(all_models_crps, aes(x = horizon, y = CRPSS, group = modN)) +
  geom_line(aes(color = modN, linetype = modN), size = 2, alpha = 0.8) +   geom_point(aes(shape = modN), size = 3) +
  scale_x_continuous(breaks = c( 1, 2, 3), limits = c(1, 3)) +  # Set limits for better control
  labs(
    x = "Lead time (months)",
    y = "CRPSS",
    color = "Model",
    shape = "Model",
    linetype = "Model"
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14, face = "bold"),                  # Bold x-axis title
    axis.title.y = element_text(size = 14, face = "bold"),                  # Bold y-axis title
    axis.text.x = element_text(size = 12, face = "bold"),                   # Larger x-axis tick labels
    axis.text.y = element_text(size = 12, face = "bold"),                   # Larger y-axis tick labels
    legend.position = "bottom",
    legend.key.size = unit(1.5, "lines"),                                   # Larger legend symbols
    legend.title = element_text(size = 14, face = "bold"),                  # Larger legend title
    legend.text = element_text(size = 15),                                  # Larger legend text
    panel.grid.major = element_line(color = "grey80", size = 0.6),          # Thicker major grid lines
    panel.grid.minor = element_line(color = "grey90", size = 0.4)  
  ) +
  scale_color_manual(values = c(
    "spatio-temporal 1" = "red", 
    "spatio-temporal 2" = "green", 
    "spatio-temporal 3" = "blue", 
    "PCA model" = "purple", 
    "hhh4 model" = "orange",
    "Baseline" = "grey",
    "ensemble model" = "darkred"
  )) +
  scale_shape_manual(values = c(
    "Baseline" = 16,  # Circle
    "ensemble model" = 17,  # Triangle
    "spatio-temporal 1" = 18,  # Square
    "spatio-temporal 2" = 18,  # Filled circle
    "spatio-temporal 3" = 18,  # Solid circle
    "PCA model" = 18,  # Circle with fill
    "hhh4 model" = 18   # Diamond
  )) +
  scale_linetype_manual(values = c(
    "Baseline" = "dashed",
    "ensemble model" = "solid",
    "spatio-temporal 1" = "dashed",
    "spatio-temporal 2" = "dashed",
    "spatio-temporal 3" = "dashed",
    "PCA model" = "dashed",
    "hhh4 model" = "dashed"
  )) 
CRPSS



brier<-  readRDS('./Data/cleaned_scores/brier_ensemble_OOB.rds') 
brier<- brier[1:5]%>% mutate(monthN=month(date))
b1_5models<-  readRDS('./Data/cleaned_scores/brier_summary_lag3_all_dates_check_all_dates_2022_OOB.rds')
brier_base  <- readRDS('./Data/cleaned_scores/brier_summary_baseline_OOB.rds') 
brier_base<- brier_base [,c(1:6)]%>% mutate(monthN=month(date))
b1_5models<- rbind(b1_5models,brier_base)

b1  <- b1_5models  %>%
  dplyr::mutate(modN = case_when(
    modN == "mod4_" ~ "Baseline",
    modN == "mod1_" ~ "spatio-temporal 1",
    modN == "mod2_" ~ "spatio-temporal 2",
    modN == "mod3_" ~ "spatio-temporal 3",
    modN == "PC_lags" ~ "PCA model",
    modN == "modhhh4_power_precip_temp_" ~ "hhh4 model"
  ))


b2 <- inner_join(b1, obs_case, by = c("district", "date"))

brier$modN <- "ensemble model"

combined_data <- rbind(brier, b1)

unique(combined_data$modN)


A<- combined_data  %>%
  dplyr::group_by(horizon, modN) %>%
  dplyr::summarize(brier_2sd = mean(brier_2sd)) %>%
  ggplot(aes(x = horizon, y = brier_2sd, group = modN, color = modN)) +
  geom_line(aes(color = modN, linetype = modN), size = 2, alpha = 0.8) +   geom_point(aes(shape = modN), size = 3) +
  ylim(0, 0.35) +  # Set y-axis limits
  scale_x_continuous(breaks = c( 1, 2, 3), limits = c(1, 3)) +  # Set limits for better control
  labs(
    x = "Lead time (months)",
    y = "Brier score",
    color = "Model",
    shape = "Model",
    linetype = "Model"
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14, face = "bold"),                  # Bold x-axis title
    axis.title.y = element_text(size = 14, face = "bold"),                  # Bold y-axis title
    axis.text.x = element_text(size = 12, face = "bold"),                   # Larger x-axis tick labels
    axis.text.y = element_text(size = 12, face = "bold"),                   # Larger y-axis tick labels
    legend.position = "bottom",
    legend.key.size = unit(1.5, "lines"),                                   # Larger legend symbols
    legend.title = element_text(size = 14, face = "bold"),                  # Larger legend title
    legend.text = element_text(size = 15),                                  # Larger legend text
    panel.grid.major = element_line(color = "grey80", size = 0.6),          # Thicker major grid lines
    panel.grid.minor = element_line(color = "grey90", size = 0.4)  
  ) +
  scale_color_manual(values = c(
    "spatio-temporal 1" = "red", 
    "spatio-temporal 2" = "green", 
    "spatio-temporal 3" = "blue", 
    "PCA model" = "purple", 
    "hhh4 model" = "orange",
    "Baseline" = "grey",
    "ensemble model" = "darkred"
  )) +
  scale_shape_manual(values = c(
    "Baseline" = 16,  # Circle
    "ensemble model" = 17,  # Triangle
    "spatio-temporal 1" = 18,  # Square
    "spatio-temporal 2" = 18,  # Filled circle
    "spatio-temporal 3" = 18,  # Solid circle
    "PCA model" = 18,  # Circle with fill
    "hhh4 model" = 18   # Diamond
  )) +
  scale_linetype_manual(values = c(
    "Baseline" = "dashed",
    "ensemble model" = "solid",
    "spatio-temporal 1" = "dashed",
    "spatio-temporal 2" = "dashed",
    "spatio-temporal 3" = "dashed",
    "PCA model" = "dashed",
    "hhh4 model" = "dashed"
  )) 

A

combined_plot <- ggarrange(CRPSS,bias_plot_OOB, sharpness_plot, A,
                           common.legend = TRUE,  # Use a common legend for both plots
                           legend = "bottom",labels = c('a','b','c','d'),     # Place the legend at the bottom
                           ncol = 2, nrow = 2)  

# To display the combined plot
print(combined_plot)

# Save the combined plot in high resolution
ggsave("combined_plot22.png", plot = combined_plot, width = 10, height = 10, dpi = 1000,bg = "white" )


##Figure 6 is in prob_for_outbreaks_percentiles.R