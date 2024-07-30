lag_district_pca_short <- function(vintage_date, district.select, modN){
  
#max_index=84 #should dynamically generate this based on vintage date
max_horizon =2
lookback <- 12 +max_horizon #fits to 12 dates + max horizon forecast data

biglist <- list()

e1 <- d2 %>%
  dplyr::select(date,m_DHF_cases, district) %>%
  mutate(date_index = interval(as.Date('2004-09-01'),date) %/% months(1) +1
  ) %>%
  filter(date>='2004-09-01' & date<= (vintage_date %m-% months(max_horizon ) )) %>% #can't use data past vintage date minus max horizon
  dplyr::select(date,m_DHF_cases, district, date_index) 
 
district.index <-  1:length(all.districts)

max_index <- max(e1$date_index) #works out to vintage_Date - max_horizon

lookback_index <-  (lookback): (max_index) 

combinations <- expand.grid( lookback_index,district.index) %>%
  mutate(listindex=row_number()) %>%
  rename(district.index=Var2, lookback_index=Var1) 

combinations.ls <- split(combinations, combinations$listindex)

 
ts_lookback_func <- function(in.ls){
    f1 <- e1 %>%
            filter( district == all.districts[in.ls[1,2]] & date_index>(in.ls[1,1]-lookback) & date_index<=(in.ls[1,1]) ) %>%
    pull(m_DHF_cases)
  return(f1)
}

all.vec <- sapply(combinations.ls,ts_lookback_func)

scaled.vecs <- as.data.frame(apply(all.vec,2, function(x) scale(log(x+1)) ))

names(scaled.vecs ) <- paste(combinations[,1],combinations[,2], sep='_')
#matplot(scaled.vecs, type='l')


#filter so keep vintagedate-12 through vintagedate+max_horizon
c1a <- d2 %>%
  filter( date>='2004-09-01' & date <= vintage_date  %m+% months(max_horizon ) & date >= (vintage_date  %m-% months(lookback-max_horizon-1)) ) %>%
  left_join(spat_IDS, by='district') %>%
  arrange(district, date) %>%
  mutate( date_index = lubridate::interval(min(date), date) %/% months(1) + 1) %>%
  group_by(district) %>%
  mutate(district2=district,
         Dengue_fever_rates = m_DHF_cases / pop *100000,
  ) %>%
  ungroup() %>%
  filter( district==district.select) 


c1b <- cbind.data.frame(c1a , scaled.vecs) %>%
  mutate(offset1= pop/100000,
         m_DHF_cases_hold= ifelse( date> (vintage_date), NA_real_,
                                   m_DHF_cases)
  )
# dplyr::select(-contains(district.select)) #filters out lags from the select district--fix this to work with tidy names

##Y-AWARE PCA
df2 <- c1b 
x <- df2[,names(scaled.vecs)]
Y <- df2$m_DHF_cases_hold
y.aware.scale<- apply(x[,-1], 2, function(x1){
  x1 <- as.vector(x1)
  log.y.pre.scale<- scale(log(Y+0.5))
  log.y.pre.scale<- as.vector(log.y.pre.scale)
  reg<-lm(log.y.pre.scale~x1)
  slope<- reg$coefficients[2]
  x.scale<-x1*slope - mean(x1*slope)
  return(x.scale)
})
pca1<- prcomp(y.aware.scale, center = FALSE,scale. = FALSE)
# plot(pca1$sdev)
n.pcs.keep<-3
pcs<-pca1$x
pcs<- as.data.frame(apply(pcs,2, scale)) #SCALE THE PCS prior to regression!
names(pcs) <- paste0('PC', 1:ncol(pcs))
pc.df <- cbind.data.frame('date'=df2$date, pcs[,1:n.pcs.keep])

c1 <- c1b %>%
  left_join(pc.df, by='date') 


all.vars <- paste(names(pc.df)[-1], collapse="+")

mod.select = mods[as.numeric(modN)]

c1$t = 1:nrow(c1)
  
if(mod.select == 'PC_lags_weather'){
  form2 <- as.formula(paste0("m_DHF_cases_hold ~", all.vars, " +lag2_monthly_cum_ppt + lag2_avg_daily_temp + f(t, model='ar1')"))
}else   if(mod.select == 'PC_lags'){
  form2 <- as.formula(paste0("m_DHF_cases_hold ~", all.vars, " +  f(t, model='ar1')"))
}else   if(mod.select == 'PC_weather'){
  form2 <- as.formula(paste0("m_DHF_cases_hold ~", "lag2_monthly_cum_ppt + lag2_avg_daily_temp  + f(t, model='ar1')"))
  
}

offset1 <- c1$offset1
#ptm <- proc.time()

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
                                  prec.intercept=0.04, # precision 1
                                  mean=0, 
                                  prec=1), # weakly regularising on fixed effects (sd of 1)
             inla.mode = "experimental", # new version of INLA algorithm (requires R 4.1 and INLA testing version)
             num.threads=7
)    


mod.family <- mod1$.args$family


c1 <- c1 %>%
  ungroup() %>%
  mutate(forecast= as.factor(if_else(is.na(m_DHF_cases_hold),1,0)),
         horizon = if_else(date== (vintage_date %m+% months(1)),1,
                           if_else(date== (vintage_date %m+% months(2)),2,
                                   if_else(date== (vintage_date %m+% months(3)),3, 0
                                   )
                           )),
         max_allowed_lag = 3
  )%>%
  filter(horizon <= max_allowed_lag) #get rid of lag2 if lag1 is included as covariate
#View(c1 %>% dplyr::select(district, date,m_DHF_cases_hold,Dengue_fever_rates,log_df_rate,lag_y,lag2_y, forecast, horizon)  %>% filter(date>=as.Date('2012-01-01')))


score.list =list ('ds'=c1, mod=mod1, 'fixed.eff'=mod1$summary.fixed,'mod.family'=mod.family)

scores <- scoring_func(score.list)

#plot(c1$date, mod1$summary.fitted.values$mean)
# plot((c1$m_DHF_cases[is.na(c1$m_DHF_cases_hold)]), ((mod1$summary.fitted.values$mean[is.na(c1$m_DHF_cases_hold)])))

c1.out <- c1 %>%
  dplyr::select(date, district, Dengue_fever_rates, forecast,horizon ) 

out.list =  list ('ds'=c1.out, 'scores'=scores$crps3,'log.samps.inc'=scores$log.samps.inc,  'fixed.eff'=mod1$summary.fixed, 'form'==as.character(form2))

saveRDS(out.list,paste0('./Results/Results_pca2/', mod.select,'_',district.select,'_',vintage_date  ,'.rds' )   )
}