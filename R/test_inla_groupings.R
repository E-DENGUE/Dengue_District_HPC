source('./R/99_load.R')



grps <- read_csv('./Data/mdr_cluster_assignments_dtw.csv')

fit_mod <- function(vintage_date, formula1 ) {


  c1 <- d2 %>%
    filter(date >= '2004-09-01') %>%
    left_join(spat_IDS, by = 'fcode') %>%
    left_join(grps, by=c('fcode'='district_name')) %>%
    group_by(date, grp, year, month) %>%
    summarize(obs_dengue_cases = sum(obs_dengue_cases, na.rm=T),
              pop_total = sum(pop_total, na.rm=T),
              pop_density = mean(pop_density),
              lag3_log_cum_inc_24m = mean(lag3_log_cum_inc_24m),
              lag3_monthly_cum_ppt= mean(lag3_monthly_cum_ppt),
              lag3_avg_min_daily_temp = mean(lag3_avg_min_daily_temp),
              lag3_monthly_cum_ppt = mean(lag3_monthly_cum_ppt)) %>%
    ungroup() %>%
    arrange(grp, date) %>%
    mutate(t = lubridate::interval(min(date), date) %/% months(1) + 1) %>%
    group_by(grp) %>%
    mutate(
      grp2 = grp,
      Dengue_fever_rates = obs_dengue_cases / pop_total * 100000,
      log_df_rate = log((obs_dengue_cases + 1) / pop_total * 100000),
      log_pop_total = log(pop_total / 100000),
      year = lubridate::year(date),
      obs_dengue_cases_hold = ifelse(date > vintage_date, NA_real_, obs_dengue_cases),
      lag_y = lag(log_df_rate, 1),
      lag2_y = lag(log_df_rate, 2),
      lag3_y = lag(log_df_rate, 3),
      pandemic = if_else(date >= '2020-03-01' & date <= "2022-12-01",1,0),
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
      grpID = grp +1,
      grpID2 = grpID,
      grpID3 = grpID,
      grpID4 = grpID,
      t = t - min(t, na.rm = TRUE) + 1,
      #make sure timeID starts at 1
      
      time_id1 = t ,
      time_id2 = t,
      time_id3 = t,
      time_id4 = t,
      grp=as.factor(grp)
    ) %>%
    arrange(date, grpID) %>% #SORT FOR SPACE_TIME
    mutate(
      grpIDpad = str_pad(grpID, 3, pad = "0", side = 'left'),
      timeIDpad = str_pad(time_id1, 5, pad = "0", side = 'left'),
      Population_density = scale(pop_density),
    )
  
  c1$grpID <- as.numeric(c1$grpID)
  
  
  form2 <- as.formula(formula1)
  
  # form2 <- as.formula(y ~ f(t, group = grpID2, model = "ar1",
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
    ) %>%
    filter(date> vintage_date & date <= (vintage_date %m+% months(3) ) ) %>%
    dplyr::select(date,grp, obs_dengue_cases,pred_mean_count,offset1 ) %>%
    mutate(horizon = interval(vintage_date, date) %/% months(1) )
  
  out.list = list('preds_district' = preds_district, 'preds_overall' = preds_overall, 'mod.formula'=formula1)
  
}

#mod1 <- fit_mod(vintage_date = as.Date('2022-05-01'))

form0 = 'obs_dengue_cases_hold ~ lag3_y+ grp +
         f(t,  model="ar1", hyper = hyper.ar1,constr=TRUE) + 
          f(grpID, model="iid") +
          f(monthN2, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE)'

form1 = 'obs_dengue_cases_hold ~ lag3_y+ grp +
  lag3_monthly_cum_ppt+
  lag3_avg_min_daily_temp*grp + 
         f(t,  model="ar1", hyper = hyper.ar1,constr=TRUE, replicate=grpID2) + #shared AR(1) across fcodes
          f(grpID, model="iid") +
          f(monthN2, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE)'

form2 = 'obs_dengue_cases_hold ~ lag3_y+ grp +
  lag3_monthly_cum_ppt+
  lag3_avg_min_daily_temp*grp + 
         f(t,  model="ar1", hyper = hyper.ar1,constr=TRUE, replicate=grpID2) + #shared AR(1) across fcodes
          f(grpID, model="iid") +
          f(monthN2, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=grpID3)'

form3 = 'obs_dengue_cases_hold ~ lag3_y+ grp +
         f(t,  model="ar1", hyper = hyper.ar1,constr=TRUE, replicate=grpID2) + #shared AR(1) across fcodes
          f(grpID, model="iid") +
          f(monthN2, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE)'

form4 = 'obs_dengue_cases_hold ~ lag3_y+ grp +
         f(t,  model="ar1", hyper = hyper.ar1,constr=TRUE, replicate=grpID2) + #shared AR(1) across fcodes
          f(grpID, model="iid") +
          f(monthN2, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=grpID3)'

#same as mod 1, but without interaction for climate
form5 = 'obs_dengue_cases_hold ~ lag3_y+ grp +
  lag3_monthly_cum_ppt+
         f(t,  model="ar1", hyper = hyper.ar1,constr=TRUE, replicate=grpID2) + #shared AR(1) across fcodes
          f(grpID, model="iid") +
          f(monthN2, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE)'

#same as mod 5, but with interaction for pandemic
form6 = 'obs_dengue_cases_hold ~ lag3_y+ grp +
  lag3_monthly_cum_ppt+ pandemic + lag3_monthly_cum_ppt*pandemic +
         f(t,  model="ar1", hyper = hyper.ar1,constr=TRUE, replicate=grpID2) + #shared AR(1) across fcodes
          f(grpID, model="iid") +
          f(monthN2, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE)'

#same as mod 5 with RW2
form7 = 'obs_dengue_cases_hold ~ lag3_y+ grp +
  lag3_monthly_cum_ppt+
         f(t,  model="rw2", constr=TRUE, replicate=grpID2) + #shared AR(1) across fcodes
          f(grpID, model="iid") +
          f(monthN2, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE)'

#same as mod 5, no lag3_y
form8 = 'obs_dengue_cases_hold ~  grp +
  lag3_monthly_cum_ppt+
         f(t,  model="ar1", hyper = hyper.ar1,constr=TRUE, replicate=grpID2) + #shared AR(1) across fcodes
          f(grpID, model="iid") +
          f(monthN2, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE)'

#same as mod5 no ar1
form9 = 'obs_dengue_cases_hold ~ lag3_y+ grp +
  lag3_monthly_cum_ppt+
          f(grpID, model="iid") +
          f(monthN2, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE)'

#same as mod 8, no seasonality
form10 = 'obs_dengue_cases_hold ~  grp +
  lag3_monthly_cum_ppt+
         f(t,  model="ar1", hyper = hyper.ar1,constr=TRUE, replicate=grpID2) + #shared AR(1) across fcodes
          f(grpID, model="iid") 
          '
#same as mod 8, no precipitation
form11 = 'obs_dengue_cases_hold ~  grp +
         f(t,  model="ar1", hyper = hyper.ar1,constr=TRUE, replicate=grpID2) + #shared AR(1) across fcodes
          f(grpID, model="iid") +
          f(monthN2, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE)'


#same as mod 8, adds rw2
form12 = 'obs_dengue_cases_hold ~  grp +
  lag3_monthly_cum_ppt+
         f(t,  model="ar1", hyper = hyper.ar1,constr=TRUE, replicate=grpID2) + #shared AR(1) across fcodes
         f(time_id2,  model="rw2", constr=TRUE, replicate=grpID2) + #shared AR(1) across fcodes

          f(grpID, model="iid") +
          f(monthN2, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE)'

#same as mod 12, uses rw1 instead of AR1
form13 = 'obs_dengue_cases_hold ~  grp +
  lag3_monthly_cum_ppt+
         f(t,  model="rw1", constr=TRUE, replicate=grpID2) + #shared AR(1) across fcodes
         f(time_id2,  model="rw2", constr=TRUE, replicate=grpID2) + #shared AR(1) across fcodes

          f(grpID, model="iid") +
          f(monthN2, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE)'


all_dates <- seq.Date(from=as.Date('2020-01-01'), to= as.Date('2025-05-01'), by='month')

 all_mods0 <- lapply(all_dates,fit_mod, formula1=form0)
 all_mods1 <- lapply(all_dates,fit_mod, formula1=form1)
 all_mods2 <- lapply(all_dates,fit_mod, formula1=form2)
 all_mods3 <- lapply(all_dates,fit_mod, formula1=form3)
 all_mods4 <- lapply(all_dates,fit_mod, formula1=form4)
 all_mods5 <- lapply(all_dates,fit_mod, formula1=form5)
 all_mods6 <- lapply(all_dates,fit_mod, formula1=form6)
 all_mods7 <- lapply(all_dates,fit_mod, formula1=form7)
 all_mods8 <- lapply(all_dates,fit_mod, formula1=form8) ##WINNER
 all_mods9 <- lapply(all_dates,fit_mod, formula1=form9)
 all_mods10 <- lapply(all_dates,fit_mod, formula1=form10) 
 all_mods11 <- lapply(all_dates,fit_mod, formula1=form11) 
 all_mods12 <- lapply(all_dates,fit_mod, formula1=form12) 
 all_mods13 <- lapply(all_dates,fit_mod, formula1=form13) 
 
  
#all_mods_combined <- list(all_mods0,all_mods1,all_mods2,all_mods3,all_mods4, all_mods5,all_mods6,all_mods7,all_mods8,all_mods9,all_mods10,all_mods11)

#all_mods_combined <- list(all_mods0,all_mods1,all_mods2,all_mods3,all_mods4, all_mods5,all_mods6,all_mods7,all_mods8,all_mods9,all_mods10,all_mods11)
all_mods_combined <- list(all_mods8,all_mods12,all_mods13)
 
saveRDS(all_mods_combined, './Results/all_results.rds' )

all_mods_combined <- readRDS( './Results/all_results.rds' )

#all_mods_combined <- c(all_mods_combined,all_mods12,all_mods13)




all_preds_df <- imap_dfr(   # imap_dfr gives both element and its name
  all_mods_combined,
  ~ map2_dfr(.x, seq_along(.x), function(mod_time, time_index) {
    mod_time$preds_district %>%
      mutate(model_name = .y, time_index = time_index)
  })
)


mae <- all_preds_df %>%
  mutate(abs_error = abs(obs_dengue_cases - pred_mean_count )) %>%
  group_by(horizon, model_name) %>%
  summarize(mae = mean(abs_error)) %>%
  filter(horizon==3)

print(mae)  #Mods 1,3,4 all similar at 3 month horizon; Mod 3 is the simplest of these
#Show preds by district#

mae_post_2023 <- all_preds_df %>%
  filter(date>='2023-01-01') %>%
  mutate(abs_error = abs(obs_dengue_cases - pred_mean_count )) %>%
  group_by(horizon, model_name) %>%
  summarize(mae = mean(abs_error))%>%
  filter(horizon==3)

print(mae_post_2023)  #Mods 1,3,4 all similar at 3 month horizon; Mod 3 is the simplest of these
#Show preds by district#



all_preds_df %>%
  filter(horizon==3 & model_name !='all_mods7') %>%
ggplot() +
  geom_point(aes(x=date, y=obs_dengue_cases))+
  geom_line(aes(x=date, y=pred_mean_count, group=model_name, color=model_name))+
  facet_wrap(~grp)


p1 <-all_preds_df %>%
  filter(horizon==3 & model_name !='all_mods7' & date>='2023-01-01') %>%
  ggplot() +
  geom_point(aes(x=date, y=obs_dengue_cases))+
  geom_line(aes(x=date, y=pred_mean_count, group=model_name, color=model_name))+
  facet_wrap(~grp)
p1
plotly::ggplotly(p1)

