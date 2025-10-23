library(sads)
library(tidyverse)

## SEASONAL DECOMPOSITION IN INLA TO GET THE BASELINE
ts_decomposition_inla <- function(forecast_year, fcode.select){
  set.seed(8123)  # Global R seed for reproducibility
  
  c1 <- d2 %>%
    filter( date>='2004-09-01')%>%
    left_join(spat_IDS, by='fcode') %>%
    arrange(fcode, date) %>%
    mutate( t = lubridate::interval(min(date), date) %/% months(1) + 1) %>%
    group_by(fcode) %>%
    mutate(fcode2=fcode,
           Dengue_fever_rates = obs_dengue_cases / pop_total * 100000,
           log_df_rate = log((obs_dengue_cases + 1) / pop_total * 100000),
           log_pop_total = log(pop_total / 100000),
           year = lubridate::year(date),
           lag_y = lag(log_df_rate, 1),
           lag2_y = lag(log_df_rate, 2),
           lag3_y = lag(log_df_rate, 3),
           
           sin12 = sin(2*pi*t/12),
           cos12 = cos(2*pi*t/12),
           month=as.factor(month(date)),
           monthN=month(date),
           offset1 = pop_total/100000,
           #log_offset=log(pop_total/100000)
    ) %>%
    ungroup() %>%
    mutate(
      fcodeID2 = fcodeID,
      fcodeID3 = fcodeID,
      fcodeID4 = fcodeID,
      t = t - min(t, na.rm = TRUE) + 1, #make sure timeID starts at 1
      
      time_id1= t , 
      time_id2=t,
      time_id3= t,
      time_id4= t,
      yearN= as.numeric(as.factor(year))) %>%
    arrange(date,fcodeID) %>% #SORT FOR SPACE_TIME
    mutate(fcodeIDpad=str_pad(fcodeID, 3, pad = "0", side='left'),
           timeIDpad=str_pad(time_id1, 5, pad = "0", side='left')
           
    )
  
  c2 <- c1 %>%
    filter(fcode==fcode.select & year <=forecast_year) %>%
    mutate(obs_dengue_cases_fit = if_else(year>=forecast_year, NA_real_, obs_dengue_cases))
  
  offset1 <- c2$offset1
  
  #single fcode version
  form2 <- as.formula( 'obs_dengue_cases_fit ~ 1+
            f(time_id1, model="ar1",constr=TRUE) +
            f(yearN, model="rw2", constr=TRUE)+
            f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE)'
  )
  
  n_times <- length(unique(c2$time_id1))
  n_years <- length(unique(c2$year))
  
  pred.time_id1 <- c2$time_id1[(n_times-11):n_times]
  pred.yearN <- c2$yearN[(n_times-11):n_times]
  pred.monthN <- c2$monthN[(n_times-11):n_times]
  
  time.mat <- model.matrix(~ -1 + as.factor(time_id1) , data=c2)
  year.mat <- model.matrix(~ -1 + as.factor(yearN) , data=c2)
  month.mat <- model.matrix(~ -1 + as.factor(monthN) , data=c2)
  
  #confirm linear combs gives same results as summary.linear.predictor
  # lc.lin.pred = inla.make.lincombs("(Intercept)"=rep(1,12),
  #                              'time_id1' = time.mat[(n_times-11):n_times,], 
  #                              'yearN' = year.mat[(n_times-11):n_times,],
  #                              'monthN'=month.mat[(n_times-11):n_times,])
  
  lc.lin.pred.no.ar1 = inla.make.lincombs("(Intercept)"=rep(1,nrow(c2)),
                                          'time_id1' = rep(0,nrow(c2)), 
                                          'yearN' = year.mat,
                                          'monthN'=month.mat)
  
  mod1 <- inla(form2, data = c2,  family = "poisson",E=offset1,
               lincomb = lc.lin.pred.no.ar1,
               control.compute = list(dic = FALSE, 
                                      waic = FALSE, 
                                      config = T,
                                      return.marginals=F
               ),
               # save predicted values on response scale
               control.predictor = list(compute=TRUE, link=1),
               control.fixed = list(mean.intercept=0, 
                                    prec.intercept=1e-4, # precision 1
                                    mean=0, 
                                    prec=1), # weakly regularising on fixed effects (sd of 1)
               inla.mode = "experimental", # new version of INLA algorithm (requires R 4.1 and INLA testing version)
               num.threads=8
  )    
  mod.family <- mod1$.args$family
  
  #need the linear predictor minus the AR(1) using lincomb; ar(1) random effect should be uncorrelated from rest of model;
    lin.pred.no.ar1 <- mod1$summary.lincomb.derived[,c('mean','sd')] %>%
    dplyr::rename(mean.no.ar1_inc=mean, sd.no.ar1_inc=sd) %>%
    mutate(
      mean_log_baseline = mean.no.ar1_inc*offset1, #lambda, mean of cases
      var.no.ar1_inc = sd.no.ar1_inc^2,
      sd_log_baseline = sqrt(var.no.ar1_inc*offset1)
    )
  
  ##dataset with the baseline expected values!
  baseline <- cbind.data.frame('date'=c2$date ,'obs_dengue_cases'=c2$obs_dengue_cases,c2$obs_dengue_cases, lin.pred.no.ar1) %>%
    
    mutate(year=lubridate::year(date),
           fcode=fcode.select) %>%
    filter(year==forecast_year) %>%
    saveRDS( paste0('./Output/Results/baselines/baseline_', forecast_year,'_',fcode.select,'.rds'))
  
  # lin.pred <- mod1$summary.linear.predictor[,c('mean','sd')]%>%
  #   rename(mean.lin.pred=mean, sd.lin.pred=sd) 
  # 
  # comb.pred <- cbind.data.frame('date'=c2$date ,lin.pred.no.ar1, lin.pred) %>%
  #   mutate(t=row_number())
  # 
  # ggplot(comb.pred, aes(x=date, y=mean.no.ar1))+
  #   geom_line(color='red')+
  #   geom_line(aes(x=date, y=mean.lin.pred))
}


source('./Model/R/99_load.R')

##to test
forecast_year=2025
fcode.select="ED_TRA_VINH_TRA_VINH_CITY" 

for(i in 2025:max(d2$year)){
  for(j in unique(d2$fcode)){
    print(i)
    print(j)
    ts_decomposition_inla(forecast_year=i, fcode.select=j)
  }
}



baseline_dir <- "./Output/Results/baselines"

baseline.files <- list.files(path = baseline_dir, pattern = "\\.rds$", full.names = TRUE)



all.baselines <- list()
for(i in 1:length(baseline.files)){
  print(i)
  all.baselines[[i]] <- readRDS(baseline.files[i])
}
all.baselines <- bind_rows(all.baselines)
saveRDS(all.baselines,'./Model/Data/all_baselines_2025.rds')


