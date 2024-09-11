source('./R/99_load.R')




c1 <- d2 %>%
  filter( date>='2004-09-01')%>%
  left_join(spat_IDS, by='district') %>%
  arrange(district, date) %>%
  mutate( t = lubridate::interval(min(date), date) %/% months(1) + 1) %>%
  group_by(district) %>%
  mutate(district2=district,
         Dengue_fever_rates = m_DHF_cases / pop * 100000,
         log_df_rate = log((m_DHF_cases + 1) / pop * 100000),
         log_pop = log(pop / 100000),
         year = lubridate::year(date),
         lag_y = lag(log_df_rate, 1),
         lag2_y = lag(log_df_rate, 2),
         lag3_y = lag(log_df_rate, 3),
        
         sin12 = sin(2*pi*t/12),
         cos12 = cos(2*pi*t/12),
         month=as.factor(month(date)),
         monthN=month(date),
         offset1 = pop/100000,
         #log_offset=log(pop/100000)
  ) %>%
  ungroup() %>%
  mutate(
    districtID2 = districtID,
    districtID3 = districtID,
    districtID4 = districtID,
    t = t - min(t, na.rm = TRUE) + 1, #make sure timeID starts at 1
    
    time_id1= t , 
    time_id2=t,
    time_id3= t,
    time_id4= t,
    yearN= as.numeric(as.factor(year)),
    
    urban_dic = as.factor(if_else(Urbanization_Rate>=40,1,0))) %>%
  arrange(date,districtID) %>% #SORT FOR SPACE_TIME
  mutate(districtIDpad=str_pad(districtID, 3, pad = "0", side='left'),
         timeIDpad=str_pad(time_id1, 5, pad = "0", side='left')
   
  )



#fit baseline through 2021, project to 2022
forecast_year=2012
c2 <- c1 %>%
  filter(district=='CHO MOI' & year <=forecast_year) %>%
  mutate(m_DHF_cases_fit = if_else(year>=forecast_year, NA_real_, m_DHF_cases))


ggplot(c2, aes(x=date, y=m_DHF_cases))+
  geom_line()+
  theme_classic()


offset1 <- c2$offset1

form2 <- as.formula( 'm_DHF_cases_fit ~ 
        f(time_id1, model="ar1",constr=TRUE) + 
        f(time_id2, model="ar1",constr=TRUE, replicate=districtID2) + 
        f(yearN, model="rw2",constr=TRUE) + 
        f(districtID, model="iid") +
        f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'
)

#single district version
form2 <- as.formula( 'm_DHF_cases_fit ~
        f(time_id1, model="ar1",constr=TRUE) +
        f(yearN, model="rw2", constr=TRUE)+
        f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE)'
)


mod1 <- inla(form2, data = c2,  family = "poisson",E=offset1,
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
                                  prec.intercept=1e-4, # precision 1
                                  mean=0, 
                                  prec=1), # weakly regularising on fixed effects (sd of 1)
             inla.mode = "experimental", # new version of INLA algorithm (requires R 4.1 and INLA testing version)
             num.threads=8
)    

mod.family <- mod1$.args$family


   #this function extracts the samples for the mean of lambda ('Predictor'), and then generates samples
  #from the predictive distribution using rpois or rnbinom
  
   

  pred.interval.func <- function(sample.ds, dist=c('nbinomial','poisson')){
    lambda0 <-  sample.ds$latent[grep('Predictor', row.names(sample.ds$latent))]
    latent_t1 <-  sample.ds$latent[grep('time_id1:', row.names(sample.ds$latent), fixed=T)]
    #latent_t2 <-  sample.ds$latent[grep('time_id2:', row.names(sample.ds$latent), fixed=T)]
      endemic <- lambda0-latent_t1
    
    lambda1 <- exp(endemic)*c2$pop/100000 #"Predictor"=lambda/E 
    lambda2 <- lambda1
    if(dist=='nbinomial'){
      nb.size1 = sample.ds$hyperpar['size for the nbinomial observations (1/overdispersion)']
      pred <- replicate(10, rnbinom(n=length(lambda2), mu=lambda2, size=nb.size1), simplify = 'array')
    }else{
      pred <- replicate(10, rpois(n=length(lambda2), lambda=lambda2), simplify = 'array')
    }
    return(pred)
  }
  
  
  #Generate samples from the predictive districution for the count
  
  test1 <-inla.posterior.sample(1000, mod1, seed=0)
  
  samps <- sapply(test1,pred.interval.func,dist=mod.family, simplify='array')
  
  samps <- matrix(samps, dim(samps)[1], dim(samps)[2]*dim(samps)[3])
  
  #convert the count to incidence
  samps.inc <- apply(samps,2, function(x) x/c2$pop*100000)
  
  
  prob_epidemic <- NA
  for(i in 1:nrow(c2)){
    prob_epidemic[i] <- mean(c2$m_DHF_cases[i] > samps[i,])
  }

  cbind.data.frame('prob'=prob_epidemic[89:100],'cases'=c2$m_DHF_cases[89:100], 'prod'=c2$m_DHF_cases[89:100]*prob_epidemic[89:100]) %>%
    ggplot(aes(x=prob, y=cases , color=prod))+
    geom_point()+
    theme_classic()

  out_ds <- c2 %>%
    dplyr::select(date, district,  pop, m_DHF_cases)%>%
    mutate( pred_mean = apply(samps,1,mean),
            pred_lcl = apply(samps,1,quantile, probs=0.025),
            pred_ucl = apply(samps,1,quantile, probs=0.975))

  out_ds %>%
   # filter(district=='AN MINH') %>%
  ggplot(aes(x=date, y=m_DHF_cases) )+
    geom_ribbon(aes(x=date, ymin=pred_lcl, ymax=pred_ucl))+
  geom_line(color='red')+
    theme_classic()
    
  