library(dplyr)
library(shinydashboard)
library(shiny)
library(ggplot2)
library(lubridate)

ensemble_mods <- c ('mod6_','mod60_','mod61_','PC_lags','modhhh4_power_precip_temp_')

obs_case <- readRDS('../../Data/CONFIDENTIAL/Updated_full_data_with_new_boundaries_all_factors_cleaned.rds') %>%
  dplyr::select(date, district,m_DHF_cases, pop)

obs_epidemics <- readRDS( '../../Data/observed_alarms.rds') %>% #observed alarms, as flagged in outbreak_quant.R
  rename(case_vintage=m_DHF_cases) %>%
  dplyr::select(date, district,case_vintage, starts_with('epidemic_flag'), starts_with('threshold'))

out <- readRDS( "../../Data/cleaned_scores/all_crps_slim_updated_Final.rds") %>%  #CRPS score from model
  dplyr::select(-pop,-m_DHF_cases) %>%
  full_join(obs_case, by=c('date','district')) %>%
  filter(date<='2016-12-01')

miss.mod <- out %>%
  filter(horizon==2 & date <= '2016-12-01') %>%
  group_by(modN) %>%
  dplyr::summarize(N=n()) %>%
  mutate(exclude_miss_mod = N<max(N))

miss.dates <- out %>% 
  left_join(miss.mod, by='modN') %>%
  filter(exclude_miss_mod==F) %>%
  group_by(date, horizon) %>%   
  filter(  horizon %in% c(1,2)) %>%
  dplyr::summarize(N_mods=n(), N_cases=mean(m_DHF_cases)) %>%
  ungroup() %>%
  group_by(horizon) %>%
  mutate(miss_date = if_else(N_mods< max(N_mods),1,0 )) %>%
  ungroup()

#FILTER OUT months when an epidemic has been recognized by the time forecast is made in a specific district (using fixed epidemic threshold)
out_1a <- out %>%
  left_join(miss.mod, by='modN') %>%
  filter(exclude_miss_mod!=1) %>%
  dplyr::select(-pop,-m_DHF_cases) %>%
  left_join(obs_epidemics, by=c('district'='district','vintage_date'='date'))   #%>%
#filter(epidemic_flag==0) #ONLY EVALUATE MONTHS WHERE EPIDEMIC HAS NOT YET BEEN OBSERVED IN THE DISTRICT

out3 <- out_1a %>%
  left_join(miss.dates, by=c('date','horizon')) %>%
  filter(miss_date==0 & exclude_miss_mod==0)  %>%
  filter(epidemic_flag==0) %>%
  filter( !is.na(crps2)) %>%
  mutate(month=lubridate::month(date)) %>%
  group_by(horizon, month, modN, form) %>%
  dplyr:: summarize(crps1 = mean(crps1),crps2=mean(crps2), N=n() ) %>%
  arrange(horizon,month, crps2)%>%
  ungroup() %>%
  group_by(horizon,month) %>%
  mutate(w_i1 = (1/crps1^2)/sum(1/crps1^2),
         w_i2 = (1/crps2^2)/sum(1/crps2^2) )%>%
  filter(horizon==2)%>%
  mutate(rw_season = grepl('cyclic=TRUE', form),
         harm_season = grepl('sin12', form),
         lag2_y = grepl('lag2_y', form),
         lag_y = grepl('lag_y', form),
         lag2_monthly_cum_ppt =grepl('lag2_monthly_cum_ppt', form),
         iid_spat_intercept=grepl('f(districtID,model = "iid")',form, fixed=T),
         rw_time_spatial=grepl(' f(t, replicate=districtID3, model="rw1", hyper = hyper2.rw) ',form, fixed=T),
         type4_spatial_bym = grepl('model="bym"', form, fixed=T) *grepl('control.group=list(model="ar1"', form, fixed=T) *grepl('group=time_id1', form, fixed=T)
  )

mod.weights_t <- out3 %>%
  ungroup() %>%
  filter(modN %in% ensemble_mods) %>%
  filter(horizon==2) %>%
  dplyr::select(w_i2, modN,  month)

p1.ds <- out_1a %>%
  filter(modN %in% ensemble_mods) %>%
  left_join(miss.dates, by=c('date','horizon')) %>%
  filter(miss_date==0 & exclude_miss_mod==0)  %>%
  filter( horizon==2 ) %>%
  full_join(obs_case, by=c('date','district')) %>%
  dplyr::select(-form) %>%
  group_by(modN,date,vintage_date,horizon) %>%
  dplyr::summarize(m_DHF_cases=sum(m_DHF_cases),pop=sum(pop), pred_count=sum(pred_mean)) %>%
  mutate(month=month(date)) %>%
  left_join(mod.weights_t, by=c('modN','month')) %>% #weights determined by month-specific  predictions
  ungroup() %>%
  group_by(date, vintage_date,horizon) %>%
  mutate(sum_wgts=sum(w_i2)) %>%
  dplyr::summarize(ensemble_month = sum(w_i2/sum_wgts *pred_count),
                   m_DHF_cases=mean(m_DHF_cases),pop=mean(pop)) %>%
  ungroup() %>%
  arrange( date)

date.vector <- seq.Date(from=as.Date('2012-03-01'), to=as.Date('2015-12-01'), by='month')

district.vector = unique(out_1a$district)
MonthN <- 1:length(date.vector)
#####################################################################


ui = dashboardPage(
  dashboardHeader(title = "Dengue predictions in the Mekong Delta Region",titleWidth=500),
  dashboardSidebar(
    sliderInput("MonthN", "Select Time Index:",
                min=min(MonthN),
                max=max(MonthN), value=min(MonthN),
                timeFormat="%Y-%m-%d",
                step=1, animate=T),
    selectInput("district.select", "Select District:",
                district.vector)
  ),
  dashboardBody(
    fluidRow(
      box(tabPanel("MDR", plotOutput("ts1"))),
      box(tabPanel("district", plotOutput("ts2")))
      
    ))
)
#SERVER
server = function(input, output) {
  output$ts1 = renderPlot({
    date.select <- date.vector[input$MonthN]
    p2.ds <- p1.ds %>%
      filter(date<=date.select) %>%
      mutate(horizon=max(horizon, na.rm=T),
             vintage_date = as.Date(date.select) %m-% months(horizon),
             cases_hold = if_else(date>vintage_date, NA_real_, m_DHF_cases),
             cases_hold_dot = if_else(date==vintage_date, m_DHF_cases, NA_real_),
             ensemble_month_dot = if_else(date==date.select, ensemble_month, NA_real_)) %>%
      filter(date>='2008-01-01')
    
    p1b <- p2.ds %>%
      ggplot()+
      geom_line(aes(x=date, y=cases_hold)) +
      theme_classic()+
      ylim(0,NA)+
      geom_line(aes(x=date, y=ensemble_month), alpha=0.5 , col='red')+
      geom_point(aes(x=date, y=ensemble_month_dot), alpha=1 , col='red', cex=2)+
      geom_point(aes(x=date, y=cases_hold_dot), alpha=1 , col='black', cex=2)+
      ggtitle(paste0("Predictions for MDR for " , date.select))
    p1b
  })
  
  output$ts2 = renderPlot({
    date.select <- date.vector[input$MonthN]
    
    p1.ds.district <- out_1a %>%
      filter(modN %in% ensemble_mods ) %>%
      left_join(miss.dates, by=c('date','horizon')) %>%
      filter(miss_date==0 & exclude_miss_mod==0)  %>%
      filter( horizon==2 ) %>%
      full_join(obs_case, by=c('date','district')) %>%
      filter(  district==input$district.select) %>%
      dplyr::select(-form) %>%
      group_by(modN,date,vintage_date,horizon) %>%
      dplyr::summarize(m_DHF_cases=sum(m_DHF_cases),pop=sum(pop), pred_count=sum(pred_mean)) %>%
      mutate(month=month(date)) %>%
      left_join(mod.weights_t, by=c('modN','month')) %>% #weights determined by month-specific  predictions
      ungroup() %>%
      group_by(date, vintage_date,horizon) %>%
      mutate(sum_wgts=sum(w_i2)) %>%
      dplyr::summarize(ensemble_month = sum(w_i2/sum_wgts *pred_count),
                       m_DHF_cases=mean(m_DHF_cases),pop=mean(pop)) %>%
      ungroup() %>%
      arrange( date) %>% 
      filter(date<=date.select) %>%
      mutate(horizon=max(horizon, na.rm=T),
             vintage_date = as.Date(date.select) %m-% months(horizon),
             cases_hold = if_else(date>vintage_date, NA_real_, m_DHF_cases),
             cases_hold_dot = if_else(date==vintage_date, m_DHF_cases, NA_real_),
             ensemble_month_dot = if_else(date==date.select, ensemble_month, NA_real_)) %>%
      filter(date>='2008-01-01')
    
    p1c <- p1.ds.district %>%
      ggplot()+
      geom_line(aes(x=date, y=cases_hold)) +
      theme_classic()+
      ylim(0,NA)+
      geom_line(aes(x=date, y=ensemble_month), alpha=0.5 , col='red')+
      geom_point(aes(x=date, y=ensemble_month_dot), alpha=1 , col='red', cex=2)+
      geom_point(aes(x=date, y=cases_hold_dot), alpha=1 , col='black', cex=2)+
      ggtitle(paste0("Predictions for ",input$district.select,  " for " , date.select))
    p1c
  })
  
}
shinyApp(ui, server)