###############best model based on CRPS (full posterior) ############
mod1 <- 'm_DHF_cases_hold~   lag_y + 
                        f(districtID,model = "iid")+
                        f(t, model="ar1") + #shared AR(1) across districts
                      f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'


#same as mod 9 without AR1
mod2 <- 'm_DHF_cases_hold~   lag_y +
                        f(districtID,model = "iid")+
                      f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'


#building off of model 9, adds rainfall
mod3 <- 'm_DHF_cases_hold~   lag_y +
                        f(districtID,model = "iid")+
                        lag1_total_rainfall_ab + lag2_total_rainfall_ab +
                        f(t, model="ar1") + #shared AR(1) across districts
                      f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'


#building off of model 9, allow AR1 to vary by district
mod4 <- 'm_DHF_cases_hold~   lag_y +
                        f(districtID,model = "iid")+
                        f(t, replicate=districtID3, model="rw1", hyper = hyper2.rw) + #shared AR(1) across districts
                      f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'


#building off of model 9, allow AR1 to vary by district AND add rainfall
mod5 <- 'm_DHF_cases_hold~   lag_y +
                        f(districtID,model = "iid")+
                        lag1_total_rainfall_ab + lag2_total_rainfall_ab +
                        f(t, replicate=districtID3, model="rw1", hyper = hyper2.rw) + #shared AR(1) across districts
                      f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'


## try adding different combos of climate vars
mod6 <- 'm_DHF_cases_hold ~   lag_y + lag1_monthly_cum_ppt +lag2_monthly_cum_ppt +
                        f(districtID,model = "iid")+
                        f(t, model="ar1") + #shared AR(1) across districts
                      f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'


mod7 <- 'm_DHF_cases_hold ~   lag_y +  lag1_avg_min_daily_temp +lag2_avg_min_daily_temp +
                        f(districtID,model = "iid")+
                        f(t, model="ar1") + #shared AR(1) across districts
                      f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'


mod8 <- 'm_DHF_cases_hold ~   lag_y +  lag1_avg_max_daily_temp +lag2_avg_max_daily_temp +
                        f(districtID,model = "iid")+
                        f(t, model="ar1") + #shared AR(1) across districts
                      f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'

## district AR(1) without  lagged cases           
mod9 <- 'm_DHF_cases_hold~    
                        f(districtID,model = "iid")+
                        f(t, replicate=districtID3, model="rw1", hyper = hyper2.rw) + #shared AR(1) across districts
                      f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'


## district AR(1) without  lagged cases    +rain       
mod10 <- 'm_DHF_cases_hold~    
                        f(districtID,model = "iid")+ lag1_monthly_cum_ppt +lag2_monthly_cum_ppt +
                        f(t, replicate=districtID3, model="rw1", hyper = hyper2.rw) + #shared AR(1) across districts
                      f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'


## district AR(1) without  lagged cases    +temp       
mod11 <- 'm_DHF_cases_hold~    
                        f(districtID,model = "iid")+ lag1_avg_min_daily_temp +lag2_avg_min_daily_temp +
                        f(t, replicate=districtID3, model="rw1", hyper = hyper2.rw) + #shared AR(1) across districts
                      f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'

## district AR(1) without  lagged cases    +temp   +rain    
mod12 <- 'm_DHF_cases_hold~    
                        f(districtID,model = "iid")+ lag1_avg_min_daily_temp +lag2_avg_min_daily_temp + lag1_monthly_cum_ppt +lag2_monthly_cum_ppt +
                        f(t, replicate=districtID3, model="rw1", hyper = hyper2.rw) + #shared AR(1) across districts
                        f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'


## district AR(1) without  lagged cases    +all 
mod13 <- 'm_DHF_cases_hold~    lag_y + 
                        f(districtID,model = "iid")+ lag1_avg_min_daily_temp +lag2_avg_min_daily_temp + lag1_monthly_cum_ppt +lag2_monthly_cum_ppt +
                        lag1_avg_max_daily_temp +lag2_avg_max_daily_temp + lag1_total_rainfall_ab + lag2_total_rainfall_ab +
                        f(t, model="ar1") + #shared AR(1) across districts
                        f(monthN, group=districtID2,model = "ar1", hyper = list(theta1 = list(prior = "loggamma", param = c(3, 2)))) 
                         ' 
#Same as mod 1 with AR1 instead of RW1                         
mod14 <- 'm_DHF_cases~   lag_y + 
                        f(districtID,model = "iid")+
                        f(t, model="ar1") + #shared AR(1) across districts
                        f(monthN, group=districtID2,model = "ar1", hyper = list(theta1 = list(prior = "loggamma", param = c(3, 2)))) 
                        '

## district AR(1) without  lagged cases  , with correlated AR(1)     GROUP district 
mod15 <- 'm_DHF_cases_hold~    
                        f(districtID,model = "iid")+ 
                        f(t, group=districtID3, model="rw1", hyper = hyper2.rw,control.group=list(model="iid" )) + #shared AR(1) across districts
                      f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, group=districtID2,control.group=list(model="iid" )) 
                      '     

#building off of model 15, adds covariates
mod16 <- 'm_DHF_cases_hold~    
                        f(districtID,model = "iid")+ 
                        f(t, group=districtID3, model="rw1", hyper = hyper2.rw,control.group=list(model="iid" )) + #shared AR(1) across districts
                      f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, group=districtID2,control.group=list(model="iid" ))+
                      lag1_avg_min_daily_temp +lag2_avg_min_daily_temp + lag1_monthly_cum_ppt +lag2_monthly_cum_ppt +
                        lag1_avg_max_daily_temp +lag2_avg_max_daily_temp + lag1_total_rainfall_ab + lag2_total_rainfall_ab 
                        '

mod17 <- 'm_DHF_cases_hold~    
                        f(districtID,model = "iid")+ 
                        f(t, group=districtID3, model="rw1", hyper = hyper3.rw,control.group=list(model="iid" )) + #shared AR(1) across districts
                      f(monthN, model="rw1", hyper=hyper3.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, group=districtID2,control.group=list(model="iid" ))+
                      lag1_avg_min_daily_temp +lag2_avg_min_daily_temp + lag1_monthly_cum_ppt +lag2_monthly_cum_ppt +
                        lag1_avg_max_daily_temp +lag2_avg_max_daily_temp + lag1_total_rainfall_ab + lag2_total_rainfall_ab 
                '
#use hyper3 prior add lagy to model 15

mod18 <- 'm_DHF_cases_hold~    lag_y +
                        f(districtID,model = "iid")+ 
                        f(t, group=districtID3, model="rw1", hyper = hyper2.rw,control.group=list(model="iid" )) + #shared AR(1) across districts
                      f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, group=districtID2,control.group=list(model="iid" ))
                        '
#Use lag2 instead of lag1 for y                        
mod19 <- 'm_DHF_cases_hold~    lag2_y +
                        f(districtID,model = "iid")+ 
                        f(t, group=districtID3, model="rw1", hyper = hyper2.rw,control.group=list(model="iid" )) + #shared AR(1) across districts
                      f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, group=districtID2,control.group=list(model="iid" ))
                        '
#mod 4 but using lag2 y
mod20 <- 'm_DHF_cases_hold~   lag2_y +
                        f(districtID,model = "iid")+
                        f(t, replicate=districtID3, model="rw1", hyper = hyper2.rw) + #shared AR(1) across districts
                      f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'


## mod12, with AR1 nstead of RW1
mod21 <- 'm_DHF_cases_hold~    
                            f(districtID,model = "iid")+ 
                            lag1_avg_min_daily_temp +lag2_avg_min_daily_temp + lag1_monthly_cum_ppt +lag2_monthly_cum_ppt +
                            f(t, replicate=districtID3, model="ar1", hyper = hyper.ar1) + #shared AR(1) across districts
                      f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'

#mod21 but with                    
mod22 <- 'm_DHF_cases_hold~    
                            f(districtID,model = "iid")+ 
                            lag1_avg_min_daily_temp +lag2_avg_min_daily_temp + lag1_monthly_cum_ppt +lag2_monthly_cum_ppt +
                            f(t, replicate=districtID3, model="ar1", hyper =hyper.ar1 ) + #shared AR(1) across districts
                      f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'

#same as 20 but with AR1
mod23 <- 'm_DHF_cases_hold~   lag2_y +
                        f(districtID,model = "iid")+
                        f(t, replicate=districtID3, model="ar1", hyper = hyper2.rw) + #shared AR(1) across districts
                      f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'

#same as 19 with AR1                        
mod24 <- 'm_DHF_cases_hold~    lag2_y +
                        f(districtID,model = "iid")+ 
                        f(t, group=districtID3, model="ar1", hyper =hyper.ar1 ,control.group=list(model="iid" )) + #shared AR(1) across districts
                      f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'


#mod 20 wih AR1 and 2 month lagged weather covariates
mod25 <- 'm_DHF_cases_hold~   lag2_y +
                        f(districtID,model = "iid")+ lag2_avg_min_daily_temp + lag2_monthly_cum_ppt +
                        f(t, replicate=districtID3, model="ar1", hyper = hyper.ar1) + #shared AR(1) across districts
                      f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'

#Same as 22, but with grouped RE instead of replicate RE          
mod26 <- 'm_DHF_cases_hold~    
                            f(districtID,model = "iid")+ 
                            lag1_avg_min_daily_temp +lag2_avg_min_daily_temp + lag1_monthly_cum_ppt +lag2_monthly_cum_ppt +
                        f(t, group=districtID3, model="rw1", hyper = hyper2.rw,control.group=list(model="iid" )) + #shared AR(1) across districts
                      f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'     

#Same as 26, but 2 month lagged only         
mod27 <- 'm_DHF_cases_hold~    
                            f(districtID,model = "iid")+ 
                            lag2_avg_min_daily_temp + lag2_monthly_cum_ppt +
                        f(t, group=districtID3, model="rw1", hyper = hyper2.rw,control.group=list(model="iid" )) + #shared AR(1) across districts
                      f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'    

#Model proposed by Rue and Fattah 
fix_th <- FALSE
hyper1 = list(prec=list(prior="pc.prec", param=c(1,0.5), 
                        fixed = fix_th,
                        initial = 1))
mod28 <- 'm_DHF_cases_hold~ 1+    
                            f(districtID, 
                                   model="besag", 
                                   constr= TRUE, 
                                   graph=MDR.adj
                                   group=time_id1, 
                                   control.group=list(model="ar1"),
                                   hyper = hyper1,
                                   scale.model = TRUE) +
                                f(districtID2, 
                                  model="iid", 
                                  group=time_id2, 
                                  control.group=list(model="iid"),
                                  hyper = hyper1,
                                  scale.model = TRUE)'    
#mod 28 adding in lagged weather
mod29 <- 'm_DHF_cases_hold~ 1+    
                             lag2_avg_min_daily_temp + lag2_monthly_cum_ppt +
                            f(districtID, 
                                   model="besag", 
                                   constr= TRUE, 
                                   graph=MDR.adj
                                   group=time_id1, 
                                   control.group=list(model="ar1"),
                                   hyper = hyper1,
                                   scale.model = TRUE) +
                                f(districtID2, 
                                  model="iid", 
                                  group=time_id2, 
                                  control.group=list(model="iid"),
                                  hyper = hyper1,
                                  scale.model = TRUE)'    


#all.mods <- list('mod1'=mod1,'mod2'=mod2,'mod3'=mod3,'mod4'=mod4,'mod5'=mod5,'mod6'=mod6,'mod7'=mod7,
#'mod8'=mod8,'mod9'=mod9,'mod10'=mod10, 'mod11'=mod11, 'mod12'=mod12, 'mod13'=mod13, 'mod14'=mod14, 'mod15'=mod15, 'mod16'=mod16, 'mod17'=mod17, 'mod18'=mod18, 'mod19'=mod19, 'mod20'=mod20)

all.mods <- list('mod28'=mod28,'mod29'=mod29)


