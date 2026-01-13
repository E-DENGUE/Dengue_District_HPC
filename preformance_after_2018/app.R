#setwd('C:/Users/DMW63/Downloads/Dengue_Updated_Data-main/Dengue_Updated_Data-main/APP/')
library(plyr)
library(dplyr)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(plotly)
library(shiny)

ensemble_mods <- c ('mod1_','mod2_','mod3_','PC_lags','modhhh4_power_precip_temp_')

obs_epidemics <- readRDS( 'observed_alarms.rds') %>% #observed alarms, as flagged in outbreak_quant.R
  dplyr::rename(case_vintage=m_DHF_cases) %>%
  dplyr::select(date, district,case_vintage, starts_with('epidemic_flag'), starts_with('threshold'))

obs_case <- readRDS('Updated_full_data_with_new_boundaries_all_factors_cleaned_lag3.rds') %>%
  dplyr::select(date, district,m_DHF_cases, pop)



out <- readRDS( "all_crps_slim_updated_lag3_check.rds") %>%  #CRPS score from model
  dplyr::select(-pop,-m_DHF_cases) %>%
  full_join(obs_case, by=c('date','district')) %>%
  filter(date<='2022-12-01')

miss.mod <- out %>%
  filter(horizon==3 & date <= '2022-12-01') %>%
  group_by(modN) %>%
  dplyr::summarize(N=n()) %>%
  dplyr::mutate(exclude_miss_mod = N<max(N))

miss.dates <- out %>% 
  left_join(miss.mod, by='modN') %>%
  filter(exclude_miss_mod==F) %>%
  group_by(date, horizon) %>%   
  filter(  horizon %in% c(1,2,3)) %>%
  dplyr::summarize(N_mods=n(), N_cases=mean(m_DHF_cases)) %>%
  ungroup() %>%
  group_by(horizon) %>%
  dplyr::mutate(miss_date = if_else(N_mods< max(N_mods),1,0 )) %>%
  ungroup()

#FILTER OUT months when an epidemic has been recognized by the time forecast is made in a specific district (using fixed epidemic threshold)
out_1a <- out %>%
  left_join(miss.mod, by='modN') %>%
  filter(exclude_miss_mod!=1) %>%
  dplyr::select(-pop,-m_DHF_cases) %>%
  left_join(obs_epidemics, by=c('district'='district','vintage_date'='date'))   #%>%
#filter(epidemic_flag==0) #ONLY EVALUATE MONTHS WHERE EPIDEMIC HAS NOT YET BEEN predicted IN THE DISTRICT

out3 <- out_1a %>%
  left_join(miss.dates, by=c('date','horizon')) %>%
  filter(miss_date==0 & exclude_miss_mod==0)  %>%
  filter(epidemic_flag==0) %>%
  filter( !is.na(crps2)) %>%
  dplyr::mutate(month=lubridate::month(date)) %>%
  group_by(horizon, month, modN, form) %>%
  dplyr:: summarize(crps1 = mean(crps1),crps2=mean(crps2), N=n() ) %>%
  arrange(horizon,month, crps2)%>%
  ungroup() %>%
  group_by(horizon,month) %>%
  dplyr::mutate(w_i1 = (1/crps1^2)/sum(1/crps1^2),
                w_i2 = (1/crps2^2)/sum(1/crps2^2) )%>%
  filter(horizon==3)%>%
  dplyr::mutate(rw_season = grepl('cyclic=TRUE', form),
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
  filter(horizon==3) %>%
  dplyr::select(w_i2, modN,  month)

p1.ds <- out_1a %>%
  left_join(miss.dates, by=c('date','horizon')) %>%
  filter(miss_date==0 & exclude_miss_mod==0)  %>%
  filter( horizon==3  ) %>%
  filter(modN %in% ensemble_mods ) %>%
  full_join(obs_case, by=c('date','district')) %>%
  dplyr::select(-form) %>%
  group_by(modN,date,vintage_date,horizon) %>%
  dplyr::summarize(m_DHF_cases=sum(m_DHF_cases),pop=sum(pop), pred_count=sum(pred_mean)) %>%
  dplyr::mutate(month=month(date)) %>%
  ungroup() %>% 
  left_join(mod.weights_t, by=c('modN','month')) %>% #weights determined by month-specific  predictions
  group_by(date, vintage_date,horizon) %>%
  dplyr::mutate(sum_wgts=sum(w_i2), N=n()) %>%
  dplyr::summarize(ensemble_month = sum(w_i2/sum_wgts *pred_count, na.rm=T),
                   m_DHF_cases=mean(m_DHF_cases, na.rm=T),pop=mean(pop, na.rm=T)) %>%
  ungroup() %>%
  arrange( date) %>%
  mutate( ensemble_month = if_else(is.na(vintage_date), NA_real_,ensemble_month ))

#ensemble, weight based on overall performance


## How does best model differ by district?
out4 <- out_1a %>%
  left_join(miss.dates, by=c('date','horizon')) %>%
  filter(miss_date==0 & exclude_miss_mod==0 &!is.na(crps2))  %>%
  filter(epidemic_flag==0) %>%
  dplyr::mutate(month=lubridate::month(date)) %>%
  group_by(horizon, district, modN, form) %>%
  dplyr::summarize(crps1 = mean(crps1),crps2=mean(crps2), N=n() ) %>%
  arrange(horizon,district, crps2)%>%
  ungroup() %>%
  group_by(horizon,district) %>%
  dplyr::mutate(w_i1 = (1/crps1^2)/sum(1/crps1^2),w_i2 = (1/crps2^2)/sum(1/crps2^2), rel_wgt2= w_i2/max(w_i2) )%>%
  filter(horizon==3)%>%
  dplyr::mutate(rw_season = grepl('cyclic=TRUE', form),
                harm_season = grepl('sin12', form),
                lag2_y = grepl('lag2_y', form),
                lag_y = grepl('lag_y', form),
                lag2_monthly_cum_ppt =grepl('lag2_monthly_cum_ppt', form),
                iid_spat_intercept=grepl('f(districtID,model = "iid")',form, fixed=T),
                rw_time_spatial=grepl(' f(t, replicate=districtID3, model="rw1", hyper = hyper2.rw) ',form, fixed=T),
                type4_spatial_bym = grepl('model="bym"', form, fixed=T) *grepl('control.group=list(model="ar1"', form, fixed=T) *grepl('group=time_id1', form, fixed=T)
  ) %>%
  dplyr::select(-form) %>%
  ungroup() %>%
  arrange(district, crps2) %>%
  dplyr::group_by(district) %>%
  dplyr::mutate(mod_rank = row_number()) %>%
  ungroup()


mod.weights_dist<- out4 %>%
  ungroup() %>%
  filter(horizon==3) %>%
  dplyr::select(w_i2, modN,   district) %>%
  arrange(district, -w_i2)


p3.ds <- out_1a %>%
  filter(modN %in% ensemble_mods) %>%
  left_join(miss.dates, by=c('date','horizon')) %>%
  filter(miss_date==0 & exclude_miss_mod==0)  %>%
  filter( horizon==3 ) %>%
  full_join(obs_case, by=c('date','district')) %>%
  dplyr::select(-form) %>%
  group_by(modN,date,vintage_date,district) %>%
  dplyr::summarize(m_DHF_cases=sum(m_DHF_cases),pop=sum(pop), pred_count=sum(pred_mean),threshold_quant=threshold_quant) %>%
  dplyr::mutate(month=month(date)) %>%
  left_join(mod.weights_dist, by=c('modN','district')) %>% #weights determined by month-specific  predictions
  #filter(w_i2>=0.05) %>%
  ungroup() %>%
  group_by(date,vintage_date, district) %>%
  dplyr::mutate(sum_wts=sum(w_i2)) %>%
  dplyr::summarize(ensemble_dist_wgt = sum(w_i2/sum_wts *pred_count) #summarize across the different models to get date and district-specific estimate
  ) %>%
  ungroup() %>%
  group_by(date,vintage_date,district) %>%
  dplyr::summarize( ensemble_dist_wgt=sum(ensemble_dist_wgt)) %>% #sum across the districts
  dplyr::right_join(p1.ds, by='date') 

p3.ds<- inner_join(p3.ds,obs_case,by=c('district'='district','date'='date'))



pp.ds <- out_1a %>%
  left_join(obs_case, by=c('date','district')) %>%
  filter( horizon==3 & modN %in% ensemble_mods ) %>% #RESTRICTS TO THE SELECTED ENSEMBLE
  dplyr::select(-form) %>%
  group_by(modN,date,vintage_date,district) %>%
  dplyr::summarize(m_DHF_cases=sum(m_DHF_cases),pop=sum(pop), pred_count=sum(pred_mean),threshold_quant=threshold_quant) %>%
  mutate(month=month(date)) %>%
  left_join(mod.weights_dist, by=c('modN','district')) %>% #weights determined by month-specific  predictions
  #filter(w_i2>=0.05) %>%
  ungroup() %>%
  group_by(date,vintage_date, district) %>%
  mutate(sum_wts=sum(w_i2)) %>%
  dplyr::summarize(ensemble_dist_wgt = sum(w_i2/sum_wts*pred_count/pop*100000)) #summarize across the different models to get date and district-specific estimate



date.vector <- seq.Date(from=as.Date('2018-01-01'), to=as.Date('2022-12-01'), by='month')

district.vector = unique(out_1a$district)
MonthN <- 1:length(date.vector)

MonthN <- 1:length(date.vector)



library(shiny)
library(shinydashboard)
library(ggplot2)

# Define UI for the application
ui <- fluidPage(
  titlePanel("Dengue Predictions in the Mekong Delta Region"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("MonthN", 
                  "Select Time Index:", 
                  min = 1, 
                  max = 60, 
                  value = 1, 
                  step = 1, 
                  animate = TRUE)
    ),
    
    mainPanel(
      tags$head(
        tags$style(HTML("
          .main-panel {
            padding: 0;
            margin: 0;
          }
          .shiny-plot-output {
            width: 100%;
            height: 60%
          }
        "))
      ),
      plotOutput("ts1")
    )
  )
)



# Define server logic
server <- function(input, output) {
  output$ts1 <- renderPlot({
    date.select <- date.vector[input$MonthN]
    
    # Ensure `p1.ds` and `date.vector` are preloaded and defined in your environment
    p2.ds <- p1.ds %>%
      filter(date <= date.select) %>%
      dplyr::mutate(
        horizon = max(horizon, na.rm = TRUE),
        vintage_date = as.Date(date.select) %m-% months(horizon),
        cases_hold = if_else(date > vintage_date, NA_real_, m_DHF_cases),
        cases_hold_dot = if_else(date == vintage_date, m_DHF_cases, NA_real_),
        ensemble_month_dot = if_else(date == date.select, ensemble_month, NA_real_)
      ) %>%
      filter(date >= as.Date('2018-01-01') & date <= as.Date('2022-12-01'))
    
    ggplot(p2.ds) +
      geom_line(aes(x = date, y = cases_hold), col = 'red') +
      geom_line(aes(x = date, y = ensemble_month), alpha = 0.5, col = 'blue') +
      geom_point(aes(x = date, y = ensemble_month_dot), col = 'blue', size = 2) +
      geom_point(aes(x = date, y = cases_hold_dot), col = 'red', size = 2) +
      ylim(0, NA) +
      theme_classic() +
      ggtitle(paste0("Predictions for MDR for ", date.select)) +
      labs(y = "Dengue Cases") +
      scale_x_date(
        breaks = as.Date(c("2018-01-01", "2019-01-01", "2020-01-01", "2021-01-01", "2022-01-01","2023-01-01")),
        labels = c("2018", "2019", "2020", "2021", "2022","2023"),
        limits = as.Date(c("2018-01-01", "2022-12-01"))
      )
  }, height = 700, width = 800)
}

# Run the application
shinyApp(ui = ui, server = server)




