library(parallel)
library(stats)
library(dplyr)
library(readr)
library(readxl)
library(tidyr)
library(tidyverse)
library(zoo)
library(lubridate)
library(pbapply)
library(INLA)
#inla.setOption(mkl=TRUE)
library(MASS)
library(scoringutils)
library(sf)
library(spdep)
library(ggmap) # plotting shapefiles 
library(lattice)  # Load the lattice package if you are using lattice graphics


source('./R/load.R')

k=1
j=1
date.test.in = date.test2[j]
formula1 = 'm_DHF_cases_hold~   lag_y + sin12 + cos12 +
                        f(districtID,model = "iid")+
                        f(t, model="ar1") + #shared AR(1) across districts
                      f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'

modN=as.numeric(gsub("mod","",names(all.mods)[k]) )


c1 <- d2 %>%
  # filter(district %in% select.districts) %>%
  arrange(district, date) %>%
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
         t=row_number(),
         t2=t,
         sin12=sin(2*pi*t/12),
         cos12=cos(2*pi*t/12),
         month=as.factor(month(date)),
         monthN=month(date),
         offset1 = pop/100000,
         #log_offset=log(pop/100000)
  ) %>%
  filter(date<= (date.test.in[1] %m+% months(1) ) & !is.na(lag2_y) & horizon <= max_allowed_lag) %>% #only keep test date and 1 month ahead of that
  ungroup() %>%
  mutate(districtID = as.numeric(as.factor(district)),
         districtID2 = districtID,
         districtID3 = districtID,
         time_id1= t - min(t, na.rm=T) + 1, #make sure timeID starts at 1
         time_id2=time_id1)

form2 <- as.formula (formula1)

# form2 <- as.formula(y ~ f(t, group = districtID2, model = "ar1", 
# hyper = list(theta1 = list(prior = "loggamma", param = c(3, 
#    2)))))    

#to set initial values for hyperparameters:  control.mode = list(theta = mod1$mode$theta, restart = TRUE),

#nbinomial or poisson

ptm <- proc.time() #full dataset with uninformative priors 49 seconds

offset1 <- c1$offset1
mod1 <- inla(form2, data = c1,  family = "poisson",E=offset1,
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

proc.time() - ptm

plot(mod1$summary.random$t$mean, type='l')
     
posterior_samples <- inla.posterior.sample(mod1, n = 10000, intern=T)

posterior.mean <- function(m, name) m$summary.fixed[name, "mean"]
posterior.prec <- function(m, name) 1/m$summary.fixed[name, "sd"]^2

posterior.mean(mod1, 'lag_y' )
posterior.prec(mod1, 'lag_y' )

fixed.priors <- list()
fixed.priors$mean = list()
fixed.priors$prec = list()
for (name in c("lag_y","sin12","cos12")) {
  fixed.priors$mean[[name]] <- posterior.mean(mod1, name)
  fixed.priors$prec[[name]] <- posterior.prec(mod1, name)
}

prior.fixed <- list(mean.intercept = posterior.mean(mod1, '(Intercept)' ), 
 prec.intercept = posterior.prec(mod1, '(Intercept)' ),
                    mean = list(lag_y = fixed.priors$mean[['lag_y']]), prec = list(lag_y = fixed.priors$prec[['lag_y']]))

#Extract the density of the priors
ih.prec.t <-  paste(c("table:",as.vector(mod1$internal.marginals.hyperpar[["Log precision for t"]])), collapse='')
ih.prec.district <- paste(c("table:",as.vector(mod1$internal.marginals.hyperpar[["Log precision for districtID"]])), collapse='')
ih.prec.monthN <- paste(c("table:",as.vector(mod1$internal.marginals.hyperpar[["Log precision for monthN"]])), collapse='')
ih.prec.rho <- paste(c("table:",as.vector(mod1$internal.marginals.hyperpar[["Rho_intern for t"]])), collapse='')

hyper2.rw.monthN.inf = list(prec = ih.prec.monthN) # medium
hyper.ar1.t = list(theta1 = ih.prec.t,
                 rho = ih.prec.rho)
hyper.iid.district = list(theta = ih.prec.district)

#prior with initial values
hyper2.rw.monthN.inf = list(prec = ih.prec.monthN) # medium
hyper.ar1.t = list(theta1 = ih.prec.t,
                   rho = ih.prec.rho)
hyper.iid.district = list(theta = ih.prec.district)


form3 <- as.formula('m_DHF_cases_hold ~ lag_y +sin12 +cos12 +
  f(districtID, model = "iid", hyper=list(prec=hyper.iid.district)) + 
  f(t, model = "ar1", hyper=list(hyper.ar1.t)) +
  f(monthN, model = "rw1", hyper = list(hyper2.rw.monthN.inf), cyclic = TRUE, scale.model = TRUE, constr = TRUE, 
                          replicate = districtID2)'
                    )

#run on full dataset with informative priors:62.3 sec
ptm <- proc.time()
mod2 <- inla(form3, data = c1,  family = "poisson",E=offset1,
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
             control.fixed=list(mean.intercept = posterior.mean(mod1, '(Intercept)' ), 
                                prec.intercept = posterior.prec(mod1, '(Intercept)' ),
                                mean = list(lag_y = fixed.priors$mean[['lag_y']],
                                            sin12 = fixed.priors$mean[['sin12']],
                                            cos12 = fixed.priors$mean[['cos12']]
                                ), 
                                prec = list(lag_y = fixed.priors$prec[['lag_y']],
                                            sin12 = fixed.priors$prec[['sin12']],
                                            cos12 = fixed.priors$prec[['cos12']]
                                )),
             inla.mode = "experimental", # new version of INLA algorithm (requires R 4.1 and INLA testing version)
             num.threads=8
)    
proc.time() - ptm


#now just rerun on the last 2 time points--15 sec


form3.test <- as.formula('m_DHF_cases_hold ~ lag_y +sin12 +cos12  '
)

ptm <- proc.time()
c1a <- c1 %>%
  filter(date>="2011-11-01")
offset1a <- c1a$pop/100000
mod2a <- inla(form3.test, data = c1a,  family = "poisson",E=offset1a,
             control.compute = list(dic = FALSE, 
                                    waic = FALSE, 
                                    config = T,
                                    return.marginals=F
             ),
             # save predicted values on response scale
             control.predictor = list(compute=TRUE, 
                                      link=1),
             control.fixed=list(mean.intercept = posterior.mean(mod1, '(Intercept)' ), 
                                prec.intercept = posterior.prec(mod1, '(Intercept)' ),
                                mean = list(lag_y = fixed.priors$mean[['lag_y']],
                                            sin12 = fixed.priors$mean[['sin12']],
                                            cos12 = fixed.priors$mean[['cos12']]
                                            ), 
                                prec = list(lag_y = fixed.priors$prec[['lag_y']],
                                            sin12 = fixed.priors$prec[['sin12']],
                                            cos12 = fixed.priors$prec[['cos12']]
                                            )),
             
             inla.mode = "experimental", # new version of INLA algorithm (requires R 4.1 and INLA testing version)
             num.threads=8
)    
proc.time() - ptm

summary(mod2a)
#check prior used
mod2a$all.hyper$fixed

