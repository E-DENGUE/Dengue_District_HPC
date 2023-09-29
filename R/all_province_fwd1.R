all_district_fwd1 <- function(date.test.in, modN, formula1='y ~ -1 +  X +   f(t,model = "ar1", hyper = list(theta1 = list(prior = "loggamma", param = c(3, 2))))'){

  c1 <- d2 %>%
    # filter(district %in% select.districts) %>%
    arrange(district, date) %>%
    group_by(district) %>%
    mutate(district2=district,
           Dengue_fever_rates = m_DHF_cases / pop *100000,
           log_df_rate = log(Dengue_fever_rates+1) , 
           log_pop=log(pop/100000),
           year = year(date) ,
           m_DHF_cases_hold= if_else( date>= (date.test.in[1]), NA_real_,
                                             m_DHF_cases),
           lag_y = lag(log_df_rate, 1),
           t=row_number(),
           t2=t,
           month=as.factor(month(date)),
           monthN=month(date),
           offset1 = pop/100000,
           #log_offset=log(pop/100000)
    ) %>%
    filter(date<= (date.test.in[1]) & !is.na(lag_y)) %>%
    ungroup() %>%
    mutate(districtID = as.numeric(as.factor(district)),
           districtID2 = districtID,
           districtID3 = districtID)
  
  form2 <- as.formula (formula1)
  
  # form2 <- as.formula(y ~ f(t, group = districtID2, model = "ar1", 
  # hyper = list(theta1 = list(prior = "loggamma", param = c(3, 
  #    2)))))    
  
  
  offset1 <- c1$offset1
  mod1 <- inla(form2, data = c1,  family = "nbinomial",E=offset1,
               control.compute = list(dic = FALSE, 
                                      waic = FALSE, 
                                      config = T,
                                      return.marginals=F
               ),
               # save predicted values on response scale
               control.predictor = list(compute=TRUE, 
                                        link=1),
            control.inla = list(strategy='adaptive', # adaptive gaussian
                   cmin=0),
            control.fixed = list(mean.intercept=0, 
                   prec.intercept=1, # precision 1
                   mean=0, 
                   prec=1), # weakly regularising on fixed effects (sd of 1)
            inla.mode = "experimental", # new version of INLA algorithm (requires R 4.1 and INLA testing version)
            num.threads=8
               )    
  
  
  
  
  c1 <- c1 %>%
    ungroup() %>%
    mutate(forecast= as.factor(if_else(is.na(m_DHF_cases_hold),1,0)))
  
  score.list =list ('ds'=c1, mod=mod1, 'fixed.eff'=mod1$summary.fixed)

  scores <- scoring_func(score.list)
  
  c1.out <- c1 %>%
    dplyr::select(date, district, Dengue_fever_rates, log_df_scale,forecast ) %>%
    mutate(preds = exp(mod1$summary.linear.predictor * sd.log.df + mean.log.df))
  
  out.list =  list ('ds'=c1.out, 'scores'=scores,  'fixed.eff'=mod1$summary.fixed)
  saveRDS(out.list,paste0('./Results/', 'mod',modN,'_',date.test.in  ,'.rds' )   )
  return(out.list)
}
