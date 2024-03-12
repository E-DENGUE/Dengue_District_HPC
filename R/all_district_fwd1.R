all_district_fwd1 <- function(date.test.in, modN,type4mod=F, formula1='y ~ -1 +  X +   f(t,model = "ar1", hyper = list(theta1 = list(prior = "loggamma", param = c(3, 2))))'){

  c1 <- d2 %>%
filter( date>='2004-09-01')%>%
    left_join(spat_IDS, by='district') %>%
    arrange(district, date) %>%
    mutate( t = interval(min(date), date) %/% months(1) + 1) %>%
    group_by(district) %>%
    mutate(district2=district,
           Dengue_fever_rates = m_DHF_cases / pop *100000,
           log_df_rate = log((m_DHF_cases +1)/ pop *100000) , 
           log_pop=log(pop/100000),
           year = year(date) ,
           m_DHF_cases_hold= ifelse( date>= (date.test.in[1]), NA_real_,
                                      m_DHF_cases),
           lag_y = lag(log_df_rate, 1),
           lag2_y = lag(log_df_rate, 2),
           max_allowed_lag = if_else(grepl('lag_y',formula1 )|grepl('lag1',formula1 ),1,2),
           horizon = if_else(date== (date.test.in[1]),1,
                             if_else(date== (date.test.in[1] %m+% months(1)),2, 0
                             )
           ),
           sin12 = sin(2*pi*t/12),
           cos12 = cos(2*pi*t/12),
           month=as.factor(month(date)),
           monthN=month(date),
           offset1 = pop/100000,
           #log_offset=log(pop/100000)
    ) %>%
    filter(date<= (date.test.in[1] %m+% months(1) ) & !is.na(lag2_y) & horizon <= max_allowed_lag) %>%  #only keep test date and 1 month ahead of that
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
  if(type4mod==F){
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
  }else if(type4mod==T){
    #Type 4 spatial model in INLA; code provided by 	Esmail Abdul Fattah
    #Called when all_district_fwd1() has a value of T for type4mod
    
    mydata <- list()
    mydata$m_DHF_cases_hold <- c1$m_DHF_cases_hold
    mydata$E <- c1$offset1
    mydata$cov <- c1$time_id1
    mydata$lag2_y <- c1$lag2_y
    mydata$lag2_avg_min_daily_temp  <- c1$lag2_avg_min_daily_temp
    mydata$lag2_monthly_cum_ppt <- c1$lag2_monthly_cum_ppt
    mydata$sin12 <- c1$sin12
    mydata$cos12<- c1$cos12
    
    ##---------------------------
    #-------> Time
    ##---------------------------
    order_t <- 2
    nt <- length(unique(c1$time_id1))
    Qt <- INLA:::inla.rw(nt, order = order_t)
    con_t <- eigen(Qt)$vectors[,(nt-order_t+1):nt]
    Qt = inla.scale.model(Qt,list(A=t(con_t),e=rep(0,order_t)))
    mydata$id_t <- c1$time_id1
    
    ##---------------------------
    #-------> Space
    ##---------------------------
    order_s <- 1
    R <- inla.read.graph(paste(getwd(),"/MDR.graph", sep=""))
    graph_M <- inla.graph2matrix(R)
    graph_M[graph_M != 0] <- -1
    diag(graph_M) <- 0
    diag(graph_M) <- abs(rowSums(graph_M))
    Qs <- graph_M
    ns <- dim(Qs)[1]
    con_s <- eigen(Qs)$vectors[,ns]
    Qs = inla.scale.model(Qs,list(A=t(con_s),e=0))
    #id_s=rep(seq(1,ns),each=1)
    mydata$id_s <- c1$districtID
    
    
    ##---------------------------
    #-------> Time x Space
    ##---------------------------
    #id_ts=rep(seq(1,ns*nt),each=1)
    R_QtQs <- Qt%x%Qs
    nts <- dim(R_QtQs)[1]
    num_con_ts <- nts - (nt-order_t)*(ns-1)
    con_ts <- eigen(R_QtQs)$vectors[,(dim(R_QtQs)[1]-num_con_ts+1):dim(R_QtQs)[1]]
    mydata$id_ts <- as.numeric(as.factor(paste(c1$timeIDpad,c1$districtIDpad, sep='_'))) #should be arranged as t1_s1, t1_s2, t1_s3...t2_s1, t2_d2, t2_d3...
    
    
    ##---------------------------
    #-------> Formula
    ##---------------------------
    INIT_THETA <- rep(1,3)
    fix_th <- FALSE
    
    mod1 <- inla(form2,
                 family = "poisson",
                 E = E,
                 data =mydata,
                 control.compute = list(dic = FALSE, 
                                        waic = FALSE, 
                                        config = T,
                                        return.marginals=F
                 ),
                 control.inla=list(strategy="gaussian",
                                   control.vb=list(enable=TRUE),
                                   h = 5E-3,use.directions = TRUE,num.hessian="central"),
                 control.predictor=list(compute=TRUE, 
                                        link=1), 
                 control.fixed = list(prec.intercept =1e-3),
                 verbose=T)
  }
  
    mod.family <- mod1$.args$family

  
    c1 <- c1 %>%
      ungroup() %>%
      mutate(forecast= as.factor(if_else(is.na(m_DHF_cases_hold),1,0)),
             horizon = if_else(date== (date.test.in[1]),1,
                               if_else(date== (date.test.in[1] %m+% months(1)),2, 0
                               )
             ),
             max_allowed_lag = if_else(grepl('lag_y',formula1 ),1,2)
      )%>%
      filter(horizon <= max_allowed_lag) #get rid of lag2 if lag1 is included as covariate
    #View(c1 %>% dplyr::select(district, date,m_DHF_cases_hold,Dengue_fever_rates,log_df_rate,lag_y,lag2_y, forecast, horizon)  %>% filter(date>=as.Date('2012-01-01')))
    
  
  score.list =list ('ds'=c1, mod=mod1, 'fixed.eff'=mod1$summary.fixed,'mod.family'=mod.family)

  scores <- scoring_func(score.list)
  
  #plot(c1$date, mod1$summary.fitted.values$mean)
  plot((c1$m_DHF_cases[is.na(c1$m_DHF_cases_hold)]), ((mod1$summary.fitted.values$mean[is.na(c1$m_DHF_cases_hold)])))
  
  c1.out <- c1 %>%
    dplyr::select(date, district, Dengue_fever_rates, forecast,horizon ) %>%
    mutate(preds = mod1$summary.linear.predictor)
  
  out.list =  list ('ds'=c1.out, 'scores'=scores,  'fixed.eff'=mod1$summary.fixed, 'form'=formula1)
  saveRDS(out.list,paste0('./Results/', 'mod',modN,'_',date.test.in  ,'.rds' )   )
  return(out.list)
}
