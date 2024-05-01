deseasonalize_climate <- function(climate_var, ds=d1){
  
  seas.mod <-ds %>% 
    arrange(district, date) %>%
    group_by(district) %>%
    mutate( Climate_Train = if_else(date<as.Date('2005-01-01'), .data[[climate_var]], NA_real_),
            t=row_number(),
            sin12=sin(2*pi*t/52.1775),
            cos12=cos(2*pi*t/52.1775),
            sin6=sin(2*pi*t*2/52.1775),
            cos6=cos(2*pi*t*2/52.1775),
    )  %>%
    ungroup()
  
  form1 <-as.formula(paste0('Climate_Train', '~ sin12 + cos12+ sin6 +cos6'))
  
  fitted_models = seas.mod %>% 
    group_by(district) %>% 
    do(mod1 = rlm(form1, data=.))
  
  
  fun1 <- function(X, Y){
    seas.mod %>% filter(district==X) %>%
      cbind.data.frame(., predict.rlm( Y, newdata = seas.mod[seas.mod$district==X,], interval = "prediction"))
  }
  
  all_preds <-  mapply( fun1, fitted_models$district, fitted_models$mod1, SIMPLIFY=F) %>%
    bind_rows()
  
  all_mods <- all_preds %>%
    mutate(climate_diff = (.data[[climate_var]] - upr),
           climate_aberration = if_else(.data[[climate_var]] > upr,climate_diff , 0 ) 
    ) %>% 
    dplyr::select(district,district, date,climate_aberration)
  
  return(all_mods)
}  