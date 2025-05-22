library(dplyr)
library(ggplot2)
library(RcppRoll)
library(lubridate)

out.ds<- readRDS('thres_ds1.rds')

app2 <- shinyApp(
ui = fluidPage(    
  sliderInput("threshold", "Threshold (Cases/100,000 per month):",
              min=10, max=120, value=40),
  plotOutput("threshPlot")  
),

server = function(input, output) {
  output$threshPlot = renderPlot({
    ds.plot <- out.ds %>%
      filter( threshold_quant !=9999 ) %>%
      mutate(epidemic_flag_fixed = if_else(obs_inc>input$threshold,1,0)) %>% # is assume 1 week has 100 cases/100K, and other 3 weeks in month have 25
      arrange(district, date) %>%
      mutate(year=year(date)) %>%
      group_by(district) %>%
      mutate( alarmN_fixed=RcppRoll::roll_sum(epidemic_flag_fixed, n=4,align = "right", fill = NA,partial = FALSE))%>%
      ungroup()%>%
      #Count how many cases occur during the alarm period
      mutate( inc=m_DHF_cases/pop*100000,
              N_epidemic_fixed = if_else(alarmN_fixed>0,m_DHF_cases,NA_real_) 
      ) %>%
      group_by(district, year) %>% 
      summarize(
        N_epidemic_fixed=sum(N_epidemic_fixed, na.rm=T),
        N_cases=sum(m_DHF_cases, na.rm=T),
        pop=mean(pop),
        
        prop_epidemic_fixed = N_epidemic_fixed/N_cases,
        
        inc_epidemic_fixed = N_epidemic_fixed/pop*100000,
      ) %>%
      ungroup()
    ##What we want is stuff in upper quadrant: large proportion of cases occur after epidemic is declared and incidence is high after epidemic is declared.
    p4<- ggplot(ds.plot, aes(x=inc_epidemic_fixed, y=prop_epidemic_fixed))+
      geom_point(alpha=0.2)+
      ylim(0,1)+
      ggtitle('Fixed threshold')+
      theme_classic()
    p4  
  },width = "auto", height = "auto")
}
)
