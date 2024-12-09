library(shiny)
library(dplyr)
library(ggplot2)

# Define UI for the app
ui <- fluidPage(
  titlePanel("Risk profiles and thresholds"),
  
   sidebarLayout(
    sidebarPanel(
      sliderInput("lambda_historic", "Lambda Historic:", min = 10, max = 500, value = 60, step = 10),
      sliderInput("RR1_in", "RR forecast Mod1:", min = 1, max = 2, value = 1.5, step = 0.1),
      #sliderInput("RR2_in", "RR forecast Mod2:", min = 1, max = 2, value = 1.5, step = 0.1),
      
      sliderInput("sd_historic", "SD historic:", min = 0.0001, max = 1, value = 0.0001, step = 0.1),
      sliderInput("sd_forecast", "SD forecast:", min = 0.0001, max = 1, value = 0.0001, step = 0.1),

      sliderInput("bimodal", "Bimodal prediction?:", min = 0, max = 1, value = 0, step = 0.1),

      sliderInput("w1", "Weight for relative vs absolute:", min = 0, max = 1, value = 0, step = 0.1),
      
      
    ),
    
    mainPanel(
      verbatimTextOutput("probability_output"),
      plotOutput("risk_plots")
    )
  ),
  div(
    style = "margin-bottom: 20px; padding: 10px; background-color: #f9f9f9; border: 1px solid #ddd; border-radius: 5px;",
    p("This app demonstrates how the characteristics of forecasts of cases counts
      affects risk scores. We simulate data from a mixture of a log-normal/Poisson distribution for one data point. 
      We can control
      the mean number of cases (lambda) and the amount of dispersion in the data (SD). We can also control
      how far above a historical baselines the mean of these forecasts are (RR). The historgram on the
      top shows the posterior distribution of the forecasts for the two populations. The dotted line shows the mean+2SD of
      the historic baseline (97.5th percentile of the historic).
      The plots below shows the risk profile, which is P(X>A)*A where A are different
      threshold values. The risk profile is calculate based on overall incidence, or relative to the mean (Z-score). The maximum of these profiles gives the risk score.
      Separately, we can calculate the probability that the forecast falls into each of the pre-defined epidemic alert categories (e.g., mean+2SD)")
  )
)

#Define the server
server <- function(input, output) {
  
  # Reactive expression to calculate risk scores
  calc_scores <- reactive({
    lambda1 <- input$lambda_historic
    RR1.in <- input$RR1_in
    RR2.in <- 1
    sd_historic <- input$sd_historic
    sd_forecast <- input$sd_forecast
    set.seed(123)
    
    b1a <- tibble(.rows = 10000) %>%
      mutate(
        forecast1 = rpois(10000, exp(rnorm(10000, log(lambda1*RR1.in), sd = sd_forecast))),
        historic1 = rpois(10000, exp(rnorm(10000, log(lambda1 ), sd = sd_historic))),
        RR1 = (forecast1 + 1) / (historic1 + 1),
      )
    
    b1b <- tibble(.rows = 10000) %>%
      mutate(
        forecast1 = rpois(10000, exp(rnorm(10000, log(lambda1*RR2.in), sd = sd_forecast))),
        historic1 = rpois(10000, exp(rnorm(10000, log(lambda1 ), sd = sd_historic))),

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

    risk.threshold2z <- vector()
    
        
    risk.threshold1.exp <- vector()
    
    risk.threshold1z.log <- vector()
    
    for(threshold in thresholds){
      
      probs_sum1 <- sum(pop_score_compare$probability1[pop_score_compare$predN>threshold], na.rm=T)
      
      risk.threshold1[threshold] <- probs_sum1*threshold 

      risk.threshold1.exp[threshold] <- probs_sum1*(threshold)^2
      
            
      risk.threshold1z[threshold] <- probs_sum1*(threshold - historic1_mean)/sqrt(historic1_mean)

      risk.threshold1z.log[threshold] <- probs_sum1*log((threshold+1)/(historic1_mean+1))
     
      risk.threshold2z[threshold] <- input$w1*probs_sum1*(threshold - historic1_mean)/sqrt(historic1_mean) + (1-input$w1)*probs_sum1*threshold 
      
    }
    
    
    list(
      prob_RR1_gt_1 = prob_RR1_gt_1,
      risk.threshold1 = risk.threshold1,
      risk.threshold1z = risk.threshold1z,
      risk.threshold2z = risk.threshold2z,
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

    cat("Max Score (Z2):", max(scores$risk.threshold2z, na.rm=T) ,"\n")
    
    
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
                                          'risk.threshold1z.log'= scores$risk.threshold1z.log,
                                          'risk.threshold2z'= scores$risk.threshold2z
                                          
    )
    
    ucl = quantile(scores$b1$historic1,probs=0.975)
    historic_mean = mean(scores$b1$historic1, na.rm=T)
    max.risk <- max(scores$risk.threshold1, na.rm=T)
    max.risk.exp <- max(scores$risk.threshold1.exp, na.rm=T)
    
        max.risk.z <- max(scores$risk.threshold1z, na.rm=T)
        max.risk.2z <- max(scores$risk.threshold2z, na.rm=T)
        max.risk.z.log <- max(scores$risk.threshold1z.log, na.rm=T)
        
    index= 1:length(scores$risk.threshold1)
    
    max.risk.index =  index[max.risk==scores$risk.threshold1]
    max.risk.index.exp =  index[max.risk.exp==scores$risk.threshold1.exp]
    
        max.risk.index.z =  index[max.risk.z==scores$risk.threshold1z]
        max.risk.index.2z =  index[max.risk.2z==scores$risk.threshold2z]
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
      ggtitle("Risk profile Relative(Z-score)")+
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
      geom_line(aes(x = thresholds, y = risk.threshold2z), color = "#2b83ba") +
      theme_classic() +
      geom_vline(xintercept=ucl, lty=3, col='black', alpha=0.5)+
      geom_vline(xintercept=historic_mean, lty=3, col='black', alpha=0.5)+
      xlim(0,max(pop_score_compare$thresholds, na.rm=T))+
      ggtitle("Risk profile Combine relative and absolute")+
      geom_point(data=riskdfz.log,aes(x=max.risk.index.2z, y=max.risk.2z), col='#2b83ba')
    
    
    gridExtra::grid.arrange( p2,p1,p3,p3a, ncol=1)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
