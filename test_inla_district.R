source('./R/99_load.R')



fcodes <- read_csv('./Data/mdr_cluster_assignments_dtw.csv')

fit_mod <- function(vintage_date, formula1 ) {
  
  
  c1 <- d2 %>%
    filter(date >= '2004-09-01') %>%
    left_join(spat_IDS, by = 'fcode') %>%
    left_join(fcodes, by=c('fcode'='district_name')) %>%
    #group_by(date, fcode, year, month) %>%
    # summarize(obs_dengue_cases = sum(obs_dengue_cases, na.rm=T),
    #           pop_total = sum(pop_total, na.rm=T),
    #           pop_density = mean(pop_density),
    #           lag3_log_cum_inc_24m = mean(lag3_log_cum_inc_24m),
    #           lag3_monthly_cum_ppt= mean(lag3_monthly_cum_ppt),
    #           lag3_avg_min_daily_temp = mean(lag3_avg_min_daily_temp),
    #           lag3_monthly_cum_ppt = mean(lag3_monthly_cum_ppt)) %>%
    #ungroup() %>%
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
      grpID1 = grp +1 ,
      grpID2 = grpID1,
      fcodeID = as.numeric(as.factor(fcode)),
      fcodeID2 = fcodeID,
      fcodeID3 = fcodeID,
      fcodeID4 = fcodeID,
      t = t - min(t, na.rm = TRUE) + 1,
      #make sure timeID starts at 1
      
      time_id1 = t ,
      time_id2 = t,
      time_id3 = t,
      time_id4 = t,
      fcode=as.factor(fcode),
      grp = as.factor(grp)
    ) %>%
    arrange(date, fcodeID) %>% #SORT FOR SPACE_TIME
    mutate(
      fcodeIDpad = str_pad(fcodeID, 3, pad = "0", side = 'left'),
      timeIDpad = str_pad(time_id1, 5, pad = "0", side = 'left'),
      Population_density = scale(pop_density),
    )
  
  c1$fcodeID <- as.numeric(c1$fcodeID)
  
  
  form2 <- as.formula(formula1)
  
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
    ) %>%
    filter(date> vintage_date & date <= (vintage_date %m+% months(3) ) ) %>%
    dplyr::select(date,obs_dengue_cases,pred_mean_count,offset1, fcode, grp ) %>%
    mutate(horizon = interval(vintage_date, date) %/% months(1) )
  
  out.list = list('preds_district' = preds_district, 'preds_overall' = preds_overall, 'mod.formula'=formula1)
  
}

#mod1 <- fit_mod(vintage_date = as.Date('2022-05-01'))


form3_grp = 'obs_dengue_cases_hold ~ lag3_y+ grp +
         f(t,  model="ar1", hyper = hyper.ar1,constr=TRUE, replicate=grpID1) + #shared AR(1) across grp
          f(fcodeID, model="iid") +
          f(monthN2, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE)'


##Reference model 
form0 <- 'obs_dengue_cases_hold ~  f(fcodeID, model="iid") +
        f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=fcodeID2)'


form1 <- 'obs_dengue_cases_hold ~ lag3_y+
         f(t, model="ar1") + f(fcodeID, model="iid") +
         f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=fcodeID2)'




form2 <- 'obs_dengue_cases_hold~   lag3_y + 
                            f(fcodeID,
                                   model="besag",
                                   constr= TRUE,
                                   graph=MDR.adj,
                                    hyper = hyper.besag ,
                                   scale.model = TRUE) +
                     lag3_avg_min_daily_temp + lag3_monthly_cum_ppt +
                        f(t, replicate=fcodeID3, model="ar1", hyper = hyper.ar1,constr=TRUE) + #shared AR(1) across fcodes
                      f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=fcodeID2)'

form3 <- 'obs_dengue_cases_hold~   lag3_y + log_cum_inc_12m +log_cum_inc_24m +log_cum_inc_36m +
                            f(fcodeID,
                                   model="besag",
                                   constr= TRUE,
                                   graph=MDR.adj,
                                    hyper = hyper.besag ,
                                   scale.model = TRUE) +
                     lag3_avg_min_daily_temp + lag3_monthly_cum_ppt +
                        f(t, replicate=fcodeID3, model="ar1", hyper = hyper.ar1,constr=TRUE) + #shared AR(1) across fcodes
                      f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=fcodeID2)'




all_dates <- seq.Date(from=as.Date('2020-01-01'), to= as.Date('2025-05-01'), by='month')

all_mods0 <- lapply(all_dates,fit_mod, formula1=form0)
all_mods1 <- lapply(all_dates,fit_mod, formula1=form1)
all_mods2 <- lapply(all_dates,fit_mod, formula1=form2)
all_mods3 <- lapply(all_dates,fit_mod, formula1=form3)
all_mods3_grp <- lapply(all_dates,fit_mod, formula1=form3_grp)

all_mods_combined <- list(
  all_mods0 = all_mods0,
  all_mods1 = all_mods1,
  #all_mods2 = all_mods2,
  #all_mods3 = all_mods3,
  all_mods3_grp = all_mods3_grp
)

saveRDS(all_mods_combined, './Results/all_results_district_grps.rds' )



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
  summarize(mae = mean(abs_error, na.rm=T))

print(mae)  #Mods 1,3,4 all similar at 3 month horizon; Mod 3 is the simplest of these
#Show preds by district#







december_dates <- preds_overall %>%
  filter(month(date) == 12) %>%
  group_by(year = lubridate::year(date)) %>%
  summarize(dec_date = min(date))  # first December date each year


all_districts <- unique(c1$fcode)



pdf("./Results/district_grouped_predictions.pdf",
    width = 8,
    height = 6)  # open PDF device

for (i in all_districts) {
  ds1_plot <- mod1$preds_district %>%    
    # filter( date>='2021-01-01') %>%
    filter(fcode == i ) 
  
  
  p <- ds1_plot %>%
    ggplot() +
    geom_line(aes(x = date, y = pred_mean_count), color="#377eb8") +
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



##Test groupings
