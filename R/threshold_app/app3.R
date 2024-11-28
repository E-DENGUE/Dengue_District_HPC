library(shiny)
library(dplyr)
library(ggplot2)

# Define UI for the app
ui <- fluidPage(
  titlePanel("Interactive Model Adjustment"),
  
  div(
    style = "margin-bottom: 20px; padding: 10px; background-color: #f9f9f9; border: 1px solid #ddd; border-radius: 5px;",
    p("This app demonstrates how the characteristics of forecasts of cases counts
      affects risk scores. We simulate data from a mixture of a log-normal/Poisson distribution for one data point, which is a mixture of two underlying distributions governed by parameters lambda1 and lambda2 and sd1 and sd2.
      We can control
      the mean number of cases (lambda) and the amount of dispersion in the data (SD). We can also control
      how far above a historical baselines the mean of these forecasts are (RR). The historgram on the
      left shows the posterior distribution of the forecasts for the two populations. The dotted line shows the mean+2SD of
      the historic baseline (97.5th percentile of the historic).
      The plot on the right shows the risk profile, which is P(X>A)*A where A are different
      threshold values. The maximum of these profiles gives the risk score.
      Separately, we can calculate the probability that the forecast is greater than the
      historical distribution, and we can create a combined risk score by multiplying that
      probability by the risk score estimated from the profile")
  ),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("lambda_historic", "Lambda Historic:", min = 10, max = 500, value = 60, step = 10),
      sliderInput("RR1_in", "RR forecast Mod1:", min = 1, max = 2, value = 1.5, step = 0.1),
      sliderInput("RR2_in", "RR forecast Mod2:", min = 1, max = 2, value = 1.5, step = 0.1),
      
      sliderInput("sd_historic", "SD historic:", min = 0.0001, max = 1, value = 0.0001, step = 0.1),
      sliderInput("sd_forecast", "SD forecast:", min = 0.0001, max = 1, value = 0.0001, step = 0.1),
      
      
    ),
    
    mainPanel(
      verbatimTextOutput("probability_output"),
      plotOutput("risk_plots")
    )
  )
)

#Define the server
server <- function(input, output) {
  
  # Reactive expression to calculate risk scores
  calc_scores <- reactive({
    lambda1 <- input$lambda_historic
    RR1.in <- input$RR1_in
    RR2.in <- input$RR2_in
    sd_historic <- input$sd_historic
    sd_forecast <- input$sd_forecast
    
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
    
    b1 <- bind_rows(b1a, b1b)
    
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
    
    thresholds <- 1:max(pop_score_compare$predN,na.rm=T)
    
    risk.threshold1 <- vector()
    
    risk.threshold1z <- vector()
    
    for(threshold in thresholds){
      
      probs_sum1 <- sum(pop_score_compare$probability1[pop_score_compare$predN>threshold], na.rm=T)
      
      risk.threshold1[threshold] <- probs_sum1*threshold 
      
      risk.threshold1z[threshold] <- probs_sum1*(threshold - historic1_mean)/sqrt(historic1_mean)
      
    }
    
    
    list(
      prob_RR1_gt_1 = prob_RR1_gt_1,
      risk.threshold1 = risk.threshold1,
      risk.threshold1z = risk.threshold1z,
      thresholds=thresholds,
      b1=b1
    )
  })
  
  # Output probabilities
  output$probability_output <- renderPrint({
    scores <- calc_scores()
    cat("Max Score Pop 1:", max(scores$risk.threshold1, na.rm=T) ,"\n")
    
    "\n"
    cat("Prob greater than baseline Pop1:", scores$prob_RR1_gt_1, "\n")
    
    "\n"
    cat("Combined score Pop1:", scores$prob_RR1_gt_1*max(scores$risk.threshold1, na.rm=T), "\n")
    
    cat("Max Score (Z) Pop 1:", max(scores$risk.threshold1z, na.rm=T) ,"\n")
    
  })
  
  # Output plots
  output$risk_plots <- renderPlot({
    scores <- calc_scores()
    pop_score_compare <- cbind.data.frame('thresholds'=scores$thresholds,
                                          'risk.threshold1'= scores$risk.threshold1,
                                          'risk.threshold1z'= scores$risk.threshold1z
    )
    
    ucl = quantile(scores$b1$historic1,probs=0.975)
    historic_mean = mean(scores$b1$historic1, na.rm=T)
    max.risk <- max(scores$risk.threshold1, na.rm=T)
    max.risk.z <- max(scores$risk.threshold1z, na.rm=T)
    
    index= 1:length(scores$risk.threshold1)
    
    max.risk.index =  index[max.risk==scores$risk.threshold1]
    max.risk.index.z =  index[max.risk.z==scores$risk.threshold1z]
    
    riskdf <- cbind.data.frame(max.risk.index,max.risk)
    riskdfz <- cbind.data.frame(max.risk.index.z,max.risk.z)
    
    p1 <- ggplot(pop_score_compare) +
      geom_vline(xintercept=ucl, lty=3, col='black', alpha=0.5)+
      geom_vline(xintercept=historic_mean, lty=3, col='black', alpha=0.5)+
      geom_line(aes(x = thresholds, y = risk.threshold1), color = "#e41a1c") +
      geom_point(data=riskdf,aes(x=max.risk.index, y=max.risk), col='red')+
      xlim(0,max(pop_score_compare$thresholds, na.rm=T))+
      theme_classic() +
      ggtitle("Risk profile")
    
    p2 <- ggplot(scores$b1) +
      geom_histogram(aes(x=forecast1),binwidth=5, col='#e41a1c',fill='#e41a1c', alpha=0.5)+
      geom_histogram(aes(x=historic1),binwidth=5, col='gray', alpha=0.1)+
      geom_vline(xintercept=ucl, lty=3, col='black', alpha=0.5)+
      geom_vline(xintercept=historic_mean, lty=3, col='black', alpha=0.5)+
      theme_classic()+
      ggtitle('Posterior predictions compared with historical') +
      xlim(0,max(pop_score_compare$thresholds, na.rm=T))
    
    p3 <- ggplot(pop_score_compare) +
      geom_line(aes(x = thresholds, y = risk.threshold1z), color = "#e41a1c") +
      theme_classic() +
      geom_vline(xintercept=ucl, lty=3, col='black', alpha=0.5)+
      geom_vline(xintercept=historic_mean, lty=3, col='black', alpha=0.5)+
      xlim(0,max(pop_score_compare$thresholds, na.rm=T))+
      ggtitle("Risk profile (Z-score)")+
      geom_point(data=riskdfz,aes(x=max.risk.index.z, y=max.risk.z), col='red')
    
    
    gridExtra::grid.arrange( p2,p3,p1, ncol=1)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
