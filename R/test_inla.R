source('./R/99_load.R')


vintage_date <- as.Date('2022-05-01')

fit_mod <- function(complex_seasonal = T) {
  if (complex_seasonal == T) {
    formula1 = 'obs_dengue_cases_hold ~ lag3_y+
          f(t,  model="ar1", hyper = hyper.ar1,constr=TRUE) + #shared AR(1) across fcodes
         f(fcodeID, model="iid") +
         f(monthN2, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=fcodeID2)
'
  } else{
    formula1 = 'obs_dengue_cases_hold ~ lag3_y+
        f(t,  model="ar1", hyper = hyper.ar1,constr=TRUE) + #shared AR(1) across fcodes
         f(fcodeID, model="iid") +
         f(monthN2, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE)'
  }
  
  
  
  c1 <- d2 %>%
    filter(date >= '2004-09-01') %>%
    left_join(spat_IDS, by = 'fcode') %>%
    arrange(fcode, date) %>%
    mutate(t = lubridate::interval(min(date), date) %/% months(1) + 1) %>%
    group_by(fcode) %>%
    mutate(
      fcode2 = fcode,
      Dengue_fever_rates = obs_dengue_cases / pop_total * 100000,
      log_df_rate = log((obs_dengue_cases + 1) / pop_total * 100000),
      log_pop_total = log(pop_total / 100000),
      year = lubridate::year(date),
      obs_dengue_cases_hold = ifelse(date > vintage_date, NA_real_, obs_dengue_cases),
      lag_y = lag(log_df_rate, 1),
      lag2_y = lag(log_df_rate, 2),
      lag3_y = lag(log_df_rate, 3),
      max_allowed_lag = ifelse(any(
        grepl('lag_y', formula1) | grepl('lag1', formula1)
      ), 1, 3),
      horizon = ifelse(
        date == (vintage_date %m+% months(1)),
        1,
        ifelse(date == (vintage_date %m+% months(2)), 2, ifelse(date == (
          vintage_date %m+% months(3)
        ), 3, 0))
      ),
      sin12 = sin(2 * pi * t / 12),
      cos12 = cos(2 * pi * t / 12),
      month = as.factor(month(date)),
      monthN = month(date),
      monthN2 = monthN,
      offset1 = pop_total / 100000,
      #log_offset=log(pop_total/100000)
    ) %>%
    filter(date <= (vintage_date %m+% months(3)) &
             !is.na(lag3_y) &
             horizon <= max_allowed_lag) %>%  #only keep test date and 1 month ahead of that
    ungroup() %>%
    mutate(
      fcodeID2 = fcodeID,
      fcodeID3 = fcodeID,
      fcodeID4 = fcodeID,
      t = t - min(t, na.rm = TRUE) + 1,
      #make sure timeID starts at 1
      
      time_id1 = t ,
      time_id2 = t,
      time_id3 = t,
      time_id4 = t
    ) %>%
    arrange(date, fcodeID) %>% #SORT FOR SPACE_TIME
    mutate(
      fcodeIDpad = str_pad(fcodeID, 3, pad = "0", side = 'left'),
      timeIDpad = str_pad(time_id1, 5, pad = "0", side = 'left'),
      Population_density = scale(pop_density),
    )
  
  c1$fcodeID <- as.numeric(c1$fcodeID)
  
  
  form2 <- as.formula (formula1)
  
  # form2 <- as.formula(y ~ f(t, group = fcodeID2, model = "ar1",
  # hyper = list(theta1 = list(prior = "loggamma", param = c(3,
  #    2)))))
  
  
  #nbinomial or poisson
  offset1 <- c1$offset1
  
  mod1 <- inla(
    form2,
    data = c1,
    family = "poisson",
    E = offset1,
    control.compute = list(
      dic = FALSE,
      waic = FALSE,
      config = T,
      return.marginals = F
    ),
    # save predicted values on response scale
    control.predictor = list(compute = TRUE, link = 1),
    control.inla = list(strategy = 'adaptive', # adaptive gaussian
                        cmin = 0),
    control.fixed = list(
      mean.intercept = 0,
      prec.intercept = 0.04,
      # precision 1
      mean = 0,
      prec = 1
    ),
    # weakly regularising on fixed effects (sd of 1)
    inla.mode = "experimental",
    # new version of INLA algorithm (requires R 4.1 and INLA testing version)
    num.threads = 8
  )
  
  mod.family <- mod1$.args$family
  
  
  c1$pred_mean_count <- mod1$summary.fitted.values$mean*offset1
  
  
  preds_overall <- c1 %>%
    group_by(date) %>%
    summarize(
      pred_mean_count = sum(pred_mean_count ),
      obs_dengue_cases = sum(obs_dengue_cases)
    ) %>%
    mutate(
      obs_fit = if_else(date <= vintage_date, obs_dengue_cases , NA_real_),
      obs_future = if_else(date > vintage_date, obs_dengue_cases , NA_real_)
      
    )
  
  preds_district <- c1 %>%
    mutate(
      obs_fit = if_else(date <= vintage_date, obs_dengue_cases , NA_real_),
      obs_future = if_else(date > vintage_date, obs_dengue_cases , NA_real_)
    )
  
  out.list = list('preds_district' = preds_district, 'preds_overall' = preds_overall)
  
}

mod1 <- fit_mod(complex_seasonal = T)
mod2 <- fit_mod(complex_seasonal = F)


#Show preds by district#


december_dates <- preds_overall %>%
  filter(month(date) == 12) %>%
  group_by(year = lubridate::year(date)) %>%
  summarize(dec_date = min(date))  # first December date each year


all_districts <- unique(c1$fcode)



pdf("./Results/district_predictions.pdf",
    width = 8,
    height = 6)  # open PDF device

for (i in all_districts) {
  ds1_plot <- mod1$preds_district %>%    filter(fcode == i  & date>='2021-01-01') 
  ds2_plot <- mod2$preds_district %>%    filter(fcode == i  & date>='2021-01-01') 
  
  
 p <- ds1_plot %>%
    ggplot() +
    geom_line(aes(x = date, y = pred_mean_count), color="#377eb8") +
    geom_line(data=ds2_plot,aes(x = date, y = pred_mean_count), color='#e41a1c') +
    geom_point(aes(x = date, y = obs_fit)) +
    geom_point(aes(x = date, y = obs_future), color = 'gray') +
    
    geom_vline(
      data = december_dates,
      aes(xintercept = dec_date),
      color = "red",
      linetype = "dashed",
      alpha = 0.7
    ) +
    theme_classic() +
    ggtitle(paste(vintage_date, i))
 print(p)
 
}
dev.off()  # close the PDF device


# Plot with vertical lines
ggplot(mod1$preds_overall) +
  geom_line(aes(x = date, y = pred_mean_count), color="#377eb8") +
 geom_line(data=mod2$preds_overall,aes(x = date, y = pred_mean_count), color='#e41a1c') +
  
    geom_point(aes(x = date, y = obs_fit)) +
  geom_point(aes(x = date, y = obs_future), color = 'gray') +
  xlim(as.Date('2021-01-01'),NA) +
  geom_vline(
    data = december_dates,
    aes(xintercept = dec_date),
    color = "red",
    linetype = "dashed",
    alpha = 0.7
  ) +
  theme_classic()
