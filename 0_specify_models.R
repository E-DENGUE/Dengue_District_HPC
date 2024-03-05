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
#same as 27 with AR1
mod28 <- 'm_DHF_cases_hold~    
                            f(districtID,model = "iid")+ 
                            lag2_avg_min_daily_temp + lag2_monthly_cum_ppt +
                        f(t, group=districtID3, model="ar1", hyper =  hyper.ar1,control.group=list(model="iid" )) + #shared AR(1) across districts
                      f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'    


#Model proposed by Rue and Fattah 
fix_th <- FALSE
hyper1 = list(prec=list(prior="pc.prec", param=c(1,0.5), 
                        fixed = fix_th,
                        initial = 1))
mod29 <- 'm_DHF_cases_hold~ 1+    
                            f(districtID, 
                                   model="besag", 
                                   constr= TRUE, 
                                   graph=MDR.adj,
                                   group=time_id1, 
                                   control.group=list(model="ar1"),
                                   hyper = hyper1,
                                   scale.model = TRUE) +
                                f(districtID2, 
                                  model="iid", 
                                  group=time_id2, 
                                  control.group=list(model="iid"),
                                  hyper = hyper1)'    
#mod 28 adding in lagged weather
mod30 <- 'm_DHF_cases_hold~ 1+    
                             lag2_avg_min_daily_temp + lag2_monthly_cum_ppt +
                            f(districtID, 
                                   model="besag", 
                                   constr= TRUE, 
                                   graph=MDR.adj,
                                   group=time_id1, 
                                   control.group=list(model="ar1"),
                                   hyper = hyper1,
                                   scale.model = TRUE) +
                                f(districtID2, 
                                  model="iid", 
                                  group=time_id2, 
                                  control.group=list(model="iid"),
                                  hyper = hyper1)'    

#mod29 adding seasonality
 mod31 <- 'm_DHF_cases_hold~ 1+    
                            f(districtID, 
                                   model="besag", 
                                   constr= TRUE, 
                                   graph=MDR.adj,
                                   group=time_id1, 
                                   control.group=list(model="ar1"),
                                   hyper = hyper1,
                                   scale.model = TRUE) +
                                f(districtID2, 
                                  model="iid", 
                                  group=time_id2, 
                                  control.group=list(model="iid"),
                                  hyper = hyper1)+
                                 f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, 
                                   scale.model=TRUE, constr=TRUE, replicate=districtID2)'    
                                  
#mod 30 adding in seasonality
mod32 <- 'm_DHF_cases_hold~ 1+    
                             lag2_avg_min_daily_temp + lag2_monthly_cum_ppt +
                            f(districtID, 
                                   model="besag", 
                                   constr= TRUE, 
                                   graph=MDR.adj,
                                   group=time_id1, 
                                   control.group=list(model="ar1"),
                                   hyper = hyper1,
                                   scale.model = TRUE) +
                                f(districtID2, 
                                  model="iid", 
                                  group=time_id2, 
                                  control.group=list(model="iid"),
                                  hyper = hyper1)+
                                 f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, 
                                   scale.model=TRUE, constr=TRUE, replicate=districtID2)'  
                                   
#mod 25 wih besag random itercept
                    mod33 <- 'm_DHF_cases_hold~   lag2_y +
                            f(districtID, 
                                   model="besag", 
                                   constr= TRUE, 
                                   graph=MDR.adj,
                                    hyper = hyper1,
                                   scale.model = TRUE) +
                     lag2_avg_min_daily_temp + lag2_monthly_cum_ppt +
                        f(t, replicate=districtID3, model="ar1", hyper = hyper.ar1) + #shared AR(1) across districts
                      f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'
                      
#mod 33 wih besag rAR1 for time--THIS IS A TYPE IV STRUCTURE!
                    mod34 <- 'm_DHF_cases_hold~   lag2_y +
                           f(districtID, 
                                   model="besag", 
                                   constr= TRUE, 
                                   graph=MDR.adj,
                                   group=time_id1, 
                                   control.group=list(model="ar1"),
                                   hyper = hyper1,
                                   scale.model = TRUE) +
                     lag2_avg_min_daily_temp + lag2_monthly_cum_ppt +
                      f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'  
 
 #save as 34 but addign spatial intercept from 33 as well                     
             mod35 <- 'm_DHF_cases_hold~   lag2_y +
                      f(districtID, 
                                   model="besag", 
                                   constr= TRUE, 
                                   graph=MDR.adj,
                                    hyper = hyper1,
                                   scale.model = TRUE)+
                           f(districtID2, 
                                   model="besag", 
                                   constr= TRUE, 
                                   graph=MDR.adj,
                                   group=time_id1, 
                                   control.group=list(model="ar1"),
                                   hyper = hyper1,
                                   scale.model = TRUE) +
                     lag2_avg_min_daily_temp + lag2_monthly_cum_ppt +
                      f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'  
            
            mod36 <- 'm_DHF_cases_hold~   lag2_y +
                      f(districtID, 
                                   model="bym", 
                                   constr= TRUE, 
                                   graph=MDR.adj,
                                  #  hyper = hyper1,
                                   scale.model = TRUE)+
                           f(districtID2, 
                                   model="bym", 
                                   constr= TRUE, 
                                   graph=MDR.adj,
                                   group=time_id1, 
                                   control.group=list(model="ar1"),
                                  # hyper = hyper1,
                                   scale.model = TRUE) +
                     lag2_avg_min_daily_temp + lag2_monthly_cum_ppt +
                      f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=districtID2)'  
          
        #same as 36 but substitute sin12, cos12 random effects for rw random effect for season            
        mod37 <- 'm_DHF_cases_hold~   lag2_y + sin12 +cos12 +
                      f(districtID, 
                                   model="bym", 
                                   constr= TRUE, 
                                   graph=MDR.adj,
                                   scale.model = TRUE)+
                           f(districtID2, 
                                   model="bym", 
                                   constr= TRUE, 
                                   graph=MDR.adj,
                                   group=time_id1, 
                                   control.group=list(model="ar1"),
                                   scale.model = TRUE) +
                     lag2_avg_min_daily_temp + lag2_monthly_cum_ppt +
                      f(districtID3,sin12, model="iid")+
                      f(districtID4,cos12, model="iid")'  
                
                 #same as 37 but only have fixed effect for seasonal terms            
           
         mod38 <- 'm_DHF_cases_hold~   lag2_y + sin12 +cos12 +
                      f(districtID, 
                                   model="bym", 
                                   constr= TRUE, 
                                   graph=MDR.adj,
                                   scale.model = TRUE)+
                           f(districtID2, 
                                   model="bym", 
                                   constr= TRUE, 
                                   graph=MDR.adj,
                                   group=time_id1, 
                                   control.group=list(model="ar1"),
                                   scale.model = TRUE) +
                     lag2_avg_min_daily_temp + lag2_monthly_cum_ppt '  

    mod39_type4 <- 'm_DHF_cases_hold~ 1 + sin12 +cos12 + #lag2_y+ lag2_avg_min_daily_temp + lag2_monthly_cum_ppt + sin12 +cos12 +
      f(id_t,model="generic0",
        Cmatrix= Qt,
        constr= FALSE,
        rankdef = order_t,
        diagonal = 1e-4,
        extraconstr= list(A=t(con_t),e=rep(0,nrow(t(con_t)))),
        hyper = list(prec=list(prior="pc.prec", param=c(1,0.5), 
                               fixed = fix_th,
                               initial = INIT_THETA[1] ))) +
      
      f(id_s,model="generic0",
        Cmatrix= Qs,
        constr= FALSE,
        diagonal = 1e-4,
        rankdef = order_s,
        extraconstr= list(A=t(con_s),e=rep(0,nrow(t(con_s)))),
        hyper = list(prec=list(prior="pc.prec", param=c(1,0.5), 
                               fixed = fix_th,
                               initial = INIT_THETA[2]))) +
      
      f(id_ts,model="generic0",
        Cmatrix= R_QtQs,
        constr= FALSE,
        diagonal = 1e-4,
        rankdef = num_con_ts,
        extraconstr= list(A=t(con_ts),e=rep(0,nrow(t(con_ts)))),
        hyper = list(prec=list(prior="pc.prec", param=c(1,0.5), 
                               fixed = fix_th,
                               initial = INIT_THETA[5]))) '

#all.mods <- list('mod1'=mod1,'mod2'=mod2,'mod3'=mod3,'mod4'=mod4,'mod5'=mod5,'mod6'=mod6,'mod7'=mod7,
#'mod8'=mod8,'mod9'=mod9,'mod10'=mod10, 'mod11'=mod11, 'mod12'=mod12, 'mod13'=mod13, 'mod14'=mod14, 'mod15'=mod15, 'mod16'=mod16, 'mod17'=mod17, 'mod18'=mod18, 'mod19'=mod19, 'mod20'=mod20)

#all.mods <- list( 'mod28'=mod28,'mod29'=mod29, 'mod30'=mod30,'mod31'=mod31,'mod32'=mod32)
#all.mods <- list( 'mod37'=mod37,'mod38'=mod38)
all.mods <- list('mod28'=mod28,'mod29'=mod29, 'mod30'=mod30,'mod31'=mod31,'mod32'=mod32,  'mod33'=mod33,  'mod34'=mod34,  'mod35'=mod35,  'mod36'=mod36,  'mod37'=mod37,  'mod38'=mod38,     'mod39_type4'=mod39_type4)

#all.mods <- list('mod39_type4'=mod39_type4)

