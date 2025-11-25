library(shiny)
library(dplyr)
library(ggplot2)
library(gridExtra)


a1 <- read_csv('../Data/risk scores and alerts.csv') %>%
  mutate(month = month(pred_date)) %>%
  filter(Forecast_horizon =='3 months')


ggplot(a1) +
  geom_point(aes(x=risk_value_z, y=risk_value_absoulte , color=epidemic_flag_from_three_methods))+
  facet_wrap(~month)


# Load the data
all.baselines <- readRDS('all_baselines.rds')
grouped_data <- readRDS('grouped_data_Full.rds')

d2 <-  readRDS('../Data/Full_data_set_with_covariates_and_lags.rds') %>%
  dplyr::select(date, pop_total, fcode) %>%
  mutate(fcode = gsub('_', ' ', fcode))  


df1 <- tibble(long = unique(d2$fcode))   # your long names
df2 <- tibble(short = unique(grouped_data$district)) # your short names

#crosswalk; loses some districts
cw <- df2 %>% 
  mutate(
    long = sapply(short, function(s) {
      hits <- df1$long[str_detect(df1$long, fixed(s, ignore_case = TRUE))]
      if(length(hits) == 0) NA else hits
    })
  ) %>% 
  unnest(long) %>%
  mutate(N_words_short = str_count(short, "\\w+"),
         long2 = str_remove(long, "\\s+\\S+$"),
         short2 = word(long2, -N_words_short, -1)
  ) %>%
  filter(short2==short) %>%
  dplyr::select(short, long) %>%
  rename(fcode = long,
         district = short)

d3 <- d2 %>% left_join(cw, by='fcode')

date.vector <- seq.Date(from=as.Date('2012-04-01'), to=as.Date('2022-12-01'), by='month')
MonthN <- 1:length(date.vector)

date.vector <- seq.Date(from = as.Date('2012-04-01'), to = as.Date('2021-12-01'), by = 'month')
MonthN <- 1:length(date.vector)

grouped_data_filter <- grouped_data %>%
  filter(horizon==3) %>%
  left_join(all.baselines, by=c('date', 'district')) %>%
  left_join(d3, by=c('date','district')) %>%
  rowwise() %>%
  reframe(
    date,
    district,
    pop_total,
    forecast_log_mean,
    forecast_log_sd,
    mean_log_baseline,
    sd_log_baseline,
    
    sim = 1:10000,
    forecast1 = rpois(
      10000,
      pop_total/100000 * exp(rnorm(10000, forecast_log_mean, forecast_log_sd))
    ),
    historic1 = rpois(
      10000,
      pop_total/100000 * exp(rnorm(10000, mean_log_baseline, sd_log_baseline))
    ),
    RR1 = (forecast1 + 1) / (historic1 + 1)
  )

summary_measures <- grouped_data_filter %>%
  group_by(date, district) %>%
  mutate(alert_RR = RR1 >1 ) %>%
  summarize(
            historic_mean = mean(historic1, na.rm=T),
            ucl = quantile(historic1,probs=0.975, na.rm=T),
            prob_RR1_gt_1 =  mean(alert_RR)                 
 )
  
#Risk scores
pop1_scores <- grouped_data_filter %>%
  group_by(date, district, forecast1) %>%
  summarize(
    N_obs = n()
  ) %>%
  ungroup() %>%
  group_by(date, district) %>%
  mutate(
    probability1 = N_obs / sum(N_obs),
  ) %>%
  rename(predN =forecast1) %>%
  dplyr::select(date, district,predN, probability1)

pop2_scores <- pop1_scores %>%
  arrange(district, date, predN) %>%
  left_join(summary_measures, by = c('date','district')) %>%
  group_by(district, date) %>%
  mutate(cum_prob = cumsum(probability1),
         prob_above_level = 1 - cum_prob,
         risk_profile = prob_above_level * predN ,
         risk_profile_z = prob_above_level*(predN - historic_mean)/sqrt(historic_mean)
         ) %>%
  summarize(
            max_risk_absolute = max(risk_profile, na.rm=T),
            max_risk_z = max(risk_profile_z, na.rm=T)
      ) %>%
  mutate(month = lubridate::month(date)
         ) %>%
  ungroup()


ggplot(pop2_scores) +
  geom_point(aes(x=max_risk_z, y=max_risk_absolute))+
  facet_wrap(~month)+
  ylim(0, 60)+
  xlim(0,15)

