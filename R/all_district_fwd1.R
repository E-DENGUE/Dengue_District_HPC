all_district_fwd1 <- function(vintage.date, modN,formula1='y ~ -1 +  X +   f(t,model = "ar1", hyper = list(theta1 = list(prior = "loggamma", param = c(3, 2))))'){

  c1 <- d2 %>%
filter( date>='2004-09-01')%>%
    left_join(spat_IDS, by='district') %>%
    arrange(district, date) %>%
    mutate( t = interval(min(date), date) %/% weeks(1) + 1) %>%
    group_by(district) %>%
    mutate(district2=district,
           Dengue_fever_rates = m_DHF_cases / pop *100000,
           log_df_rate = log((m_DHF_cases +1)/ pop *100000) , 
           log_pop=log(pop/100000),
           year = year(date) ,
           m_DHF_cases_hold= ifelse( date> (vintage.date[1]), NA_real_,
                                      m_DHF_cases),
           lag8_y = lag(log_df_rate, 8),
           lag9_y = lag(log_df_rate, 9),
           lag10_y = lag(log_df_rate, 10),
           lag11_y = lag(log_df_rate, 11),
           
                      max_allowed_lag = if_else(grepl('lag_y',formula1 )|grepl('lag1',formula1 ),1,2),
           horizon = if_else(if_else(date== (vintage.date[1] %m+% weeks(8)),8, 0
                             )
           ),
           sin12 = sin(2*pi*t/52.1775),
           cos12 = cos(2*pi*t/52.1775),
           month=as.factor(month(date)),
           monthN=month(date),
           offset1 = pop/100000,
           #log_offset=log(pop/100000)
    ) %>%
    filter(date<= (vintage.date[1] %m+% weeks(8) ) ) %>%  #only keep test date and 1 month ahead of that
    ungroup() %>%
    mutate(
           districtID2 = districtID,
           districtID3 = districtID,
           districtID4 = districtID,
           t = t - min(t, na.rm = TRUE) + 1, #make sure timeID starts at 1
           
           time_id1= t , 
           time_id2=t,
           time_id3= t) %>%
    arrange(date,districtID) %>% #SORT FOR SPACE_TIME
    mutate(districtIDpad=str_pad(districtID, 3, pad = "0", side='left'),
           timeIDpad=str_pad(time_id1, 5, pad = "0", side='left'),
           ) 
  
  # check_times <- c1 %>% group_by(district) %>% summarize(N=n())
   #check_districts <- c1 %>% group_by(date) %>% summarize(N=n())
   
  form2 <- as.formula (formula1)
  
  # form2 <- as.formula(y ~ f(t, group = districtID2, model = "ar1", 
  # hyper = list(theta1 = list(prior = "loggamma", param = c(3, 
  #    2)))))    
  
  
  #nbinomial or poisson
  
  offset1 <- c1$offset1
  mod1 <- inla(form2, data = c1,  family = "poisson",E=offset1,
               control.compute = list(dic = FALSE, 
                                      waic = FALSE, 
                                      config = T,
                                      return.marginals=F
               ),
               # save predicted values on response scale
               control.predictor = list(compute=TRUE, link=1),
            control.inla = list(strategy='adaptive', # adaptive gaussian
                   cmin=0),
            control.fixed = list(mean.intercept=0, 
                   prec.intercept=1, # precision 1
                   mean=0, 
                   prec=1), # weakly regularising on fixed effects (sd of 1)
            inla.mode = "experimental", # new version of INLA algorithm (requires R 4.1 and INLA testing version)
            num.threads=8
               )    
  
    mod.family <- mod1$.args$family

    c1 <- c1 %>%
      ungroup() %>%
      mutate(forecast= as.factor(if_else(is.na(m_DHF_cases_hold),1,0)),
             horizon = if_else(date== (vintage.date[1] %m+% weeks(8)),8, 0
              )
      )

  
  score.list =list ('ds'=c1, mod=mod1, 'fixed.eff'=mod1$summary.fixed,'mod.family'=mod.family)

  scores <- scoring_func(score.list)
  
  #plot(c1$date, mod1$summary.fitted.values$mean)
 # plot((c1$m_DHF_cases[is.na(c1$m_DHF_cases_hold)]), ((mod1$summary.fitted.values$mean[is.na(c1$m_DHF_cases_hold)])))
  
  c1.out <- c1 %>%
    dplyr::select(date, district, Dengue_fever_rates, forecast,horizon ) 

  out.list =  list ('ds'=c1.out, 'scores'=scores, 'vintage.date'=vintage.date, 'fixed.eff'=mod1$summary.fixed, 'form'=formula1)
  saveRDS(out.list,paste0('./Results/', 'mod',modN,'_',vintage.date  ,'.rds' )   )
  return(out.list)
}
