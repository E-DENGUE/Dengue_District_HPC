

mod1 <- 'obs_dengue_cases_hold ~ lag3_y+
         f(t, model="ar1") + f(fcodeID, model="iid") +
         f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=fcodeID2)'




mod2<- 'obs_dengue_cases_hold~   lag3_y + 
                            f(fcodeID,
                                   model="besag",
                                   constr= TRUE,
                                   graph=MDR.adj,
                                    hyper = hyper.besag ,
                                   scale.model = TRUE) +
                     lag3_avg_min_daily_temp + lag3_monthly_cum_ppt +
                        f(t, replicate=fcodeID3, model="ar1", hyper = hyper.ar1,constr=TRUE) + #shared AR(1) across fcodes
                      f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=fcodeID2)'

mod3 <- 'obs_dengue_cases_hold~   lag3_y + log_cum_inc_12m +log_cum_inc_24m +log_cum_inc_36m +
                            f(fcodeID,
                                   model="besag",
                                   constr= TRUE,
                                   graph=MDR.adj,
                                    hyper = hyper.besag ,
                                   scale.model = TRUE) +
                     lag3_avg_min_daily_temp + lag3_monthly_cum_ppt +
                        f(t, replicate=fcodeID3, model="ar1", hyper = hyper.ar1,constr=TRUE) + #shared AR(1) across fcodes
                      f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=fcodeID2)'


##Reference model 
mod0 <- 'obs_dengue_cases_hold ~  f(fcodeID, model="iid") +
        f(monthN, model="rw1", hyper=hyper2.rw, cyclic=TRUE, scale.model=TRUE, constr=TRUE, replicate=fcodeID2)'