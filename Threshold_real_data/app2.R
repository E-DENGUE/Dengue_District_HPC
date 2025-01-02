library(shiny)
library(dplyr)
library(ggplot2)
library(gridExtra)

draws <- readRDS('./Results/sampled_data_TSCV.rds')
# Load the data
all.baselines <- readRDS('./Data/all_baselines.rds')
grouped_data <- readRDS('./Data/grouped_data_Full.rds')

grouped_data<- draws%>%
  left_join(grouped_data, by = c('date', 'district','horizon',"vintage_date"))

date.vector <- seq.Date(from=as.Date('2012-04-01'), to=as.Date('2022-12-01'), by='month')
MonthN <- 1:length(date.vector)

date.vector <- seq.Date(from = as.Date('2012-04-01'), to = as.Date('2021-12-01'), by = 'month')
MonthN <- 1:length(date.vector)

ui <- fluidPage(
  titlePanel("Risk Profiles and Thresholds"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("district", "Select District:", choices = unique(grouped_data$district)),
      selectInput("date", 
                  "Select Date:", 
                  choices = date.vector, 
                  selected = min(date.vector))
      ,
      numericInput("horizon", "Select Horizon:", value = 3, min = 1, max = 3),
      # sliderInput("RR1_in", "RR forecast Mod1:", min = 1, max = 2, value = 1.5, step = 0.1),
      sliderInput("bimodal", "Bimodal prediction?:", min = 0, max = 1, value = 0, step = 0.1)
    ),
    
    mainPanel(
      verbatimTextOutput("probability_output"),
      plotOutput("risk_plots", height = "800px")
    )
  ),
  
  div(
    style = "margin-bottom: 20px; padding: 10px; background-color: #f9f9f9; border: 1px solid #ddd; border-radius: 5px;",
    p(
      "This app demonstrates how the characteristics of forecasts of case counts affect risk scores. 
      We simulate data from a mixture of a log-normal/Poisson distribution for one data point. 
      The controls allow adjusting the mean number of cases (lambda), data dispersion (SD), 
      and how far above historical baselines the mean forecasts are (RR). 
      The histogram on the top shows the posterior distribution of forecasts for two populations, 
      with the dotted line representing the mean + 2SD of the historic baseline (97.5th percentile). 
      The risk profile below shows P(X > A) * A, where A are different threshold values. 
      The maximum of these profiles gives the risk score. 
      Additionally, we calculate the probability that forecasts fall into pre-defined epidemic alert categories 
      (e.g., mean + 2SD)."
    )
  )
)





server <- function(input, output, session) {
  calc_scores <- reactive({
    selected_date <- as.Date(input$date)
    selected_district <- input$district
    selected_horizon <- input$horizon
    RR1.in <- input$RR1_in
    bimodal <- input$bimodal
    
    grouped_data_filter <- grouped_data %>%
      filter(date == selected_date, district == selected_district, horizon == selected_horizon)
    
    baseline_filter <- all.baselines %>%
      filter(date == selected_date, district == selected_district)
    
    lambda1 <- grouped_data_filter$forecast_log_mean
    sd_historic <- baseline_filter$sd_log_baseline
    sd_forecast <- grouped_data_filter$forecast_log_sd
    lambda2 <- baseline_filter$mean_log_baseline
    
    b1a <- tibble(.rows = 10000) %>%
      mutate(
        forecast1 = sample(grouped_data_filter$value, size = 10000, replace = TRUE),
        historic1 = rpois(10000, exp(rnorm(10000, (lambda2), sd = sd_historic))),
        RR1 = (forecast1 + 1) / (historic1 + 1)
      )
    
    
    if(input$bimodal>0){
      b1b <- b1b[sample(1:10000,size=round(input$bimodal*10000)),]
      b1 <- bind_rows(b1a, b1b)
      
    }else{
      b1 <- b1a
    }    
    
    ucl = quantile(b1$historic1,probs=0.975)
    historic_mean = mean(b1$historic1, na.rm=T)
    
    prob_RR1_gt_1 <- mean(b1$RR1 > 1)
    
    historic1_mean = mean(b1$historic1, na.rm=T)
    
    pop1_scores <- b1 %>%
      group_by(forecast1) %>%
      summarize(
        N_obs = n()
      ) %>%
      ungroup() %>%
      mutate(
        probability1 = N_obs / sum(N_obs),
      ) %>%
      rename(predN =forecast1) %>%
      dplyr::select(predN, probability1)
    
    pop_score_compare <- pop1_scores 
    
    probs_high_risk <- pop1_scores %>%
      filter(predN>ucl) %>%
      summarize(probs_high_risk=sum(probability1))
    
    probs_med_risk <- pop1_scores %>%
      filter(predN>historic_mean & predN<=ucl) %>%
      summarize(probs_med_risk=sum(probability1))
    
    thresholds <- 1:max(pop_score_compare$predN,na.rm=T)
    
    risk.threshold1 <- vector()
    
    risk.threshold1z <- vector()
    
    risk.threshold1.exp <- vector()
    
    risk.threshold1z.log <- vector()
    
    for(threshold in thresholds){
      
      probs_sum1 <- sum(pop_score_compare$probability1[pop_score_compare$predN>threshold], na.rm=T)
      
      risk.threshold1[threshold] <- probs_sum1*threshold 
      
      risk.threshold1.exp[threshold] <- probs_sum1*(threshold)^2
      
      
      risk.threshold1z[threshold] <- probs_sum1*(threshold - historic1_mean)/sqrt(historic1_mean)
      
      risk.threshold1z.log[threshold] <- probs_sum1*log((threshold+1)/(historic1_mean+1))
      
    }
    
    
    list(
      prob_RR1_gt_1 = prob_RR1_gt_1,
      risk.threshold1 = risk.threshold1,
      risk.threshold1z = risk.threshold1z,
      risk.threshold1z.log=risk.threshold1z.log,
      risk.threshold1.exp=risk.threshold1.exp,
      thresholds=thresholds,
      probs_high_risk=probs_high_risk,
      probs_med_risk=probs_med_risk,
      b1=b1
    )
  })
  
  # Output probabilities
  output$probability_output <- renderPrint({
    scores <- calc_scores()
    cat("Max Score:", max(scores$risk.threshold1, na.rm=T) ,"\n")
    
    "\n"
    cat("Prob greater than baseline:", scores$prob_RR1_gt_1, "\n")
    
    "\n"
    
    cat("Max Score (Z):", max(scores$risk.threshold1z, na.rm=T) ,"\n")
    
    cat("P(Mean->Mean+2SD):", scores$probs_med_risk$probs_med_risk ,"\n")
    
    cat("P(>Mean+2sd):", scores$probs_high_risk$probs_high_risk ,"\n")
    
    cat("P(High) vs P(moderate):", scores$probs_high_risk$probs_high_risk/scores$probs_med_risk$probs_med_risk ,"\n")
    
    
    
  })
  
  # Output plots
  output$risk_plots <- renderPlot({
    scores <- calc_scores()
    pop_score_compare <- cbind.data.frame('thresholds'=scores$thresholds,
                                          'risk.threshold1'= scores$risk.threshold1,
                                          'risk.threshold1z'= scores$risk.threshold1z,
                                          'risk.threshold1.exp'= scores$risk.threshold1.exp,
                                          'risk.threshold1z.log'= scores$risk.threshold1z.log
                                          
    )
    
    ucl = quantile(scores$b1$historic1,probs=0.975)
    historic_mean = mean(scores$b1$historic1, na.rm=T)
    max.risk <- max(scores$risk.threshold1, na.rm=T)
    max.risk.exp <- max(scores$risk.threshold1.exp, na.rm=T)
    
    max.risk.z <- max(scores$risk.threshold1z, na.rm=T)
    max.risk.z.log <- max(scores$risk.threshold1z.log, na.rm=T)
    
    index= 1:length(scores$risk.threshold1)
    
    max.risk.index =  index[max.risk==scores$risk.threshold1]
    max.risk.index.exp =  index[max.risk.exp==scores$risk.threshold1.exp]
    
    max.risk.index.z =  index[max.risk.z==scores$risk.threshold1z]
    max.risk.index.z.log =  index[max.risk.z.log==scores$risk.threshold1z.log]
    
    riskdf <- cbind.data.frame(max.risk.index,max.risk)
    riskdf.exp <- cbind.data.frame(max.risk.index.exp,max.risk.exp)
    
    riskdfz <- cbind.data.frame(max.risk.index.z,max.risk.z)
    riskdfz.log <- cbind.data.frame(max.risk.index.z.log,max.risk.z.log)
    
    lims1=cbind.data.frame(historic_mean,ucl)
    
    p1 <- ggplot(pop_score_compare) +
      geom_rect(data=lims1,aes(xmin=0,
                               xmax = historic_mean,
                               ymin = -Inf,
                               ymax = Inf), fill = '#31a35420')+
      geom_rect(data=lims1,aes(xmin=historic_mean,
                               xmax = ucl,
                               ymin = -Inf,
                               ymax = Inf), fill = '#fec44f20')+
      geom_rect(data=lims1,aes(xmin=ucl,
                               xmax = Inf,
                               ymin = -Inf,
                               ymax = Inf), fill = '#d7191c20')+
      geom_vline(xintercept=ucl, lty=3, col='black', alpha=0.5)+
      geom_vline(xintercept=historic_mean, lty=3, col='black', alpha=0.5)+
      geom_line(aes(x = thresholds, y = risk.threshold1), color = "#2b83ba") +
      xlim(0,max(pop_score_compare$thresholds, na.rm=T))+
      theme_classic() +
      geom_point(data=riskdf,aes(x=max.risk.index, y=max.risk), col='#2b83ba')+
      ggtitle("Risk profile (Absolute)")
    
    p1a <- ggplot(pop_score_compare) +
      geom_rect(data=lims1,aes(xmin=0,
                               xmax = historic_mean,
                               ymin = -Inf,
                               ymax = Inf), fill = '#31a35420')+
      geom_rect(data=lims1,aes(xmin=historic_mean,
                               xmax = ucl,
                               ymin = -Inf,
                               ymax = Inf), fill = '#fec44f20')+
      geom_rect(data=lims1,aes(xmin=ucl,
                               xmax = Inf,
                               ymin = -Inf,
                               ymax = Inf), fill = '#d7191c20')+
      geom_vline(xintercept=ucl, lty=3, col='black', alpha=0.5)+
      geom_vline(xintercept=historic_mean, lty=3, col='black', alpha=0.5)+
      geom_line(aes(x = thresholds, y = risk.threshold1.exp), color = "#2b83ba") +
      xlim(0,max(pop_score_compare$thresholds, na.rm=T))+
      theme_classic() +
      geom_point(data=riskdf.exp,aes(x=max.risk.index.exp, y=max.risk.exp), col='#2b83ba')+
      ggtitle("Risk profile ((Absolute^2))")
    
    p2 <- ggplot(scores$b1) +
      geom_rect(data=lims1,aes(xmin=0,
                               xmax = historic_mean,
                               ymin = -Inf,
                               ymax = Inf), fill = '#31a35420')+
      geom_rect(data=lims1,aes(xmin=historic_mean,
                               xmax = ucl,
                               ymin = -Inf,
                               ymax = Inf), fill = '#fec44f20')+
      geom_rect(data=lims1,aes(xmin=ucl,
                               xmax = Inf,
                               ymin = -Inf,
                               ymax = Inf), fill = '#d7191c20')+
      geom_histogram(aes(x=historic1),binwidth=5, col='gray')+
      geom_histogram(aes(x=forecast1),binwidth=5, col='#2b83ba',fill='#2b83ba', alpha=0.5)+
      geom_vline(xintercept=ucl, lty=3, col='black', alpha=0.5)+
      geom_vline(xintercept=historic_mean, lty=3, col='black', alpha=0.5)+
      theme_classic()+
      ggtitle('Posterior predictions compared with historical') +
      xlim(0,max(pop_score_compare$thresholds, na.rm=T))
    
    p3 <- ggplot(pop_score_compare) +
      geom_rect(data=lims1,aes(xmin=0,
                               xmax = historic_mean,
                               ymin = -Inf,
                               ymax = Inf), fill = '#31a35420')+
      geom_rect(data=lims1,aes(xmin=historic_mean,
                               xmax = ucl,
                               ymin = -Inf,
                               ymax = Inf), fill = '#fec44f20')+
      geom_rect(data=lims1,aes(xmin=ucl,
                               xmax = Inf,
                               ymin = -Inf,
                               ymax = Inf), fill = '#d7191c20')+
      geom_line(aes(x = thresholds, y = risk.threshold1z), color = "#2b83ba") +
      theme_classic() +
      geom_vline(xintercept=ucl, lty=3, col='black', alpha=0.5)+
      geom_vline(xintercept=historic_mean, lty=3, col='black', alpha=0.5)+
      xlim(0,max(pop_score_compare$thresholds, na.rm=T))+
      ggtitle("Risk profile (Z-score)")+
      geom_point(data=riskdfz,aes(x=max.risk.index.z, y=max.risk.z), col='#2b83ba')
    
    p3a <- ggplot(pop_score_compare) +
      geom_rect(data=lims1,aes(xmin=0,
                               xmax = historic_mean,
                               ymin = -Inf,
                               ymax = Inf), fill = '#31a35420')+
      geom_rect(data=lims1,aes(xmin=historic_mean,
                               xmax = ucl,
                               ymin = -Inf,
                               ymax = Inf), fill = '#fec44f20')+
      geom_rect(data=lims1,aes(xmin=ucl,
                               xmax = Inf,
                               ymin = -Inf,
                               ymax = Inf), fill = '#d7191c20')+
      geom_line(aes(x = thresholds, y = risk.threshold1z.log), color = "#2b83ba") +
      theme_classic() +
      geom_vline(xintercept=ucl, lty=3, col='black', alpha=0.5)+
      geom_vline(xintercept=historic_mean, lty=3, col='black', alpha=0.5)+
      xlim(0,max(pop_score_compare$thresholds, na.rm=T))+
      ggtitle("Risk profile (log(RR))")+
      geom_point(data=riskdfz.log,aes(x=max.risk.index.z.log, y=max.risk.z.log), col='#2b83ba')
    
    
    gridExtra::grid.arrange( p2,p1,p1a,p3,p3a, ncol=1)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
