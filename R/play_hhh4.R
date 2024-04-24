library(surveillance)
library(sf)
library(dplyr)
library(lubridate)
library(fanplot)
MDR_NEW <- readRDS( "./Data/MDR_NEW.rds") %>%
  arrange(ID)

row.names(MDR_NEW) <- MDR_NEW$VARNAME

neighb <- poly2adjmat(st_make_valid(MDR_NEW))

dist_nbOrder <- nbOrder(neighb)

colnames(dist_nbOrder) <- MDR_NEW$VARNAME

map1 <- sf:::as_Spatial(MDR_NEW)

#rowSums(neighb)
d2 <- readRDS('./Data/CONFIDENTIAL/full_data_with_new_boundaries_all_factors_cleaned.rds') %>%
  rename(District_province = prov_dis) %>%
  filter(date<= "2012-12-01")

start.date <- '2004-07-01'
start.year <- year(start.date)
start.week <- week(start.date)
start.month <- month(start.date)


cases <- d2 %>% 
          reshape2::dcast(date~district, value.var= 'm_DHF_cases') %>%
  filter(date>=start.date) %>%
  dplyr::select(unique(MDR_NEW$VARNAME))%>%
  as.matrix()

pop <- d2 %>% 
  mutate(pop2=pop/100000) %>%
  reshape2::dcast(date~district, value.var= 'pop2') %>%
  filter(date>=start.date) %>%
  dplyr::select(unique(MDR_NEW$VARNAME))%>%
  as.matrix()

temp_lag2 <- d2 %>% 
  mutate(lag2_avg_daily_temp = scale(lag2_avg_daily_temp)) %>%
  reshape2::dcast(date~district, value.var= 'lag2_avg_daily_temp') %>%
  filter(date>=start.date) %>%
  dplyr::select(unique(MDR_NEW$VARNAME))%>%
  as.matrix()

unique(MDR_NEW$VARNAME) == colnames(pop)

#Define STS object
dengue_df <- sts(cases, start = c(start.year, start.month), frequency = 12,
                        population = pop, neighbourhood = dist_nbOrder, map=map1)

plot(dengue_df, type = observed ~ time)

plot(dengue_df, type = observed ~ unit,
#      population = dengue_df@map$POPULATION / 100000,
      labels = list(font = 2), colorkey = list(space = "right"),
      sp.layout = layout.scalebar(dengue_df@map, corner = c(0.05, 0.05),
                                    scale = 50, labels = c("0", "50 km"), height = 0.03))

dengue_mod_basic <- list(
   end = list(f = addSeason2formula(~1 + t, period = dengue_df@freq),
                offset = population(dengue_df)),
   ar = list(f = ~1),
   ne = list(f = ~1, weights = neighbourhood(dengue_df) == 1),
   family = "NegBin1")

dengueFit_basic <- hhh4(stsObj = dengue_df, control = dengue_mod_basic)

summary(dengueFit_basic, idx2Exp = TRUE, amplitudeShift = TRUE, maxEV = TRUE)

#decomposition of the fitted model ; orange=spatiotempiral, gray=endemic, blue=autoregressive
districts2plot <- which(colSums(observed(dengue_df)) > 50)
 par(mfrow = c(2,3), mar = c(3, 5, 2, 1), las = 1)
 plot(dengueFit_basic, type = "fitted", units = districts2plot,
         hide0s = TRUE, par.settings = NULL, legend = 1)
 
 #overall
 plot(dengueFit_basic, type = "fitted", total = TRUE,
        hide0s = TRUE, par.settings = NULL, legend = FALSE) -> fitted_components

 
 #Update model, by AIC is worse than basic model
 dengueFit_nepop <- update(dengueFit_basic,
                             ne = list(f = ~log(pop)), data = list(pop = population(dengue_df)))

 #Power law weights; this improves a lot
 dengueFit_nepop_powerlaw <- update(dengueFit_nepop,
                                ne = list(weights = W_powerlaw(maxlag = 5)))
 
 #unconstrianed weights
 dengueFit_np2 <- update(dengueFit_nepop,
                           ne = list(weights = W_np(maxlag = 2)))

 AIC(dengueFit_basic)
 AIC(dengueFit_nepop)
 AIC(dengueFit_nepop_powerlaw)
 AIC(dengueFit_np2) #winner
 
 #covariate--best is if the ar1 and neighbor are effects of temp
 dengueFit_temp1 <- update(dengueFit_np2,
                        end = list(f = update(formula(dengueFit_np2)$end, ~. +1)),
                        ar = list(f = update(formula(dengueFit_np2)$ar, ~. + temp_lag2)),
                        ne = list(f = update(formula(dengueFit_np2)$ne, ~. + temp_lag2))
  )
 
 dengueFit_temp2 <- update(dengueFit_np2,
                           end = list(f = update(formula(dengueFit_np2)$end, ~. +1 + temp_lag2)),
                           ar = list(f = update(formula(dengueFit_np2)$ar, ~. +1)),
                           ne = list(f = update(formula(dengueFit_np2)$ne, ~. + temp_lag2))
 )
 dengueFit_temp3 <- update(dengueFit_np2,
                           end = list(f = update(formula(dengueFit_np2)$end, ~. +1)),
                           ar = list(f = update(formula(dengueFit_np2)$ar, ~. + 1)),
                           ne = list(f = update(formula(dengueFit_np2)$ne, ~. + temp_lag2))
 )
 dengueFit_temp4 <- update(dengueFit_np2,
                           end = list(f = update(formula(dengueFit_np2)$end, ~. +1)),
                           ar = list(f = update(formula(dengueFit_np2)$ar, ~. + temp_lag2)),
                           ne = list(f = update(formula(dengueFit_np2)$ne, ~. + 1))
 )
 summary(dengueFit_temp1)
 AIC(dengueFit_temp1)
 AIC(dengueFit_temp2)
 AIC(dengueFit_temp3)
 AIC(dengueFit_temp4)
 
 #add random intercept
 dengueFit_ri <- update(dengueFit_temp1,
                          end = list(f = update(formula(dengueFit_temp1)$end, ~. + ri() - 1)),
                          ar = list(f = update(formula(dengueFit_temp1)$ar, ~. + ri() - 1)),
                          ne = list(f = update(formula(dengueFit_temp1)$ne, ~. + ri() - 1)))
  summary(dengueFit_ri)
 par(mfrow = c(2,3), mar = c(3, 5, 2, 1), las = 1)
  plot(dengueFit_ri, type = "fitted", units = districts2plot,
          hide0s = TRUE, par.settings = NULL, legend = 1)
  plot(dengueFit_ri, type = "fitted", total = TRUE,
          hide0s = TRUE, par.settings = NULL, legend = FALSE)
 
  
  models2compare <- c("dengueFit_np2","dengueFit_temp", "dengueFit_ri", "dengueFit_basic")
  tp = c(80,100)
  
  #This takes some time when doing rolling; random intercept model is best in both instances
  dengue_preds1 <- lapply(mget(models2compare), oneStepAhead,    tp = tp, type = "final")
  
  SCORES <- c("logs", "rps", "dss", "ses")
  dengueScores1 <- lapply(dengue_preds1, scores, which = SCORES, individual = TRUE)
  t(sapply(dengueScores1, colMeans, dims = 2))  
  
  #Forward simulation; fit up to time t, then simulate forward
  last_fit_t = 90
  dengue_mod_ri_temp <- list(
    end = list(f = addSeason2formula(~0 + t + ri() , period = dengue_df@freq),
               offset = population(dengue_df)),
    ar = list(f = ~0 + temp_lag2 + ri() ),
    ne = list(f = ~0 + temp_lag2 + ri() - 1, weights = W_np(maxlag = 2)),
    family = "NegBin1",
    subset = 2:last_fit_t
    )
  
  #fit the model to time t
  dengueFit_ri <- hhh4(stsObj = dengue_df, control = dengue_mod_ri_temp)
  
  #simulate forward
    dengueSim <- simulate(dengueFit_ri,
                          nsim = 999, seed = 1, subset = (last_fit_t+1):(last_fit_t+12))

  par(mfrow = c(1,1), mar = c(3, 5, 2, 1), las = 1)
  
  plot(dengueFit_ri, type = "fitted", total = TRUE,
       hide0s = TRUE, par.settings = NULL, legend = FALSE)
  plot(dengueSim, "fan", means.args = list(), key.args = list(), add=F)

  #for CRPS evaluation:
  horizon=2
  samps <- matrix(dengueSim[horizon,,], nrow=dim(dengueSim)[2])
  pop_forecast <- population(dengue_df)[last_fit_t+horizon,]
  obs_forecast <- observed(dengue_df)[last_fit_t+horizon,]
  