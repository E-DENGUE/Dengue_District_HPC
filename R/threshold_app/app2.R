library(shiny)
library(dplyr)
library(ggplot2)

# Define UI for the app
ui <- fluidPage(
  titlePanel("Interactive Model Adjustment"),
  
  div(
    style = "margin-bottom: 20px; padding: 10px; background-color: #f9f9f9; border: 1px solid #ddd; border-radius: 5px;",
    p("This app demonstrates how the characteristics of forecasts of cases counts
      affects risk scores. We simulate data from a mixture of a log-normal/Poisson distribution for two data points. We can control
      the mean number of cases (lambda) and the amount of dispersion in the data (SD). We can also control
      how far above a historical baselines the mean of these forecasts are (RR). The historgram on the
      left shows the posterior distribution of the forecasts for the two populations.
      The plot on the right shows the risk profile, which is P(X>A)*A where A are different
      threshold values. The maximum of these profiles gives the risk score.
      Separately, we can calculate the probability that the forecast is greater than the
      historical distribution, and we can create a combined risk score by multiplying that
      probability by the risk score estimated from the profile")
  ),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("lambda1", "Lambda 1:", min = 10, max = 1000, value = 1000, step = 10),
      sliderInput("lambda2", "Lambda 2:", min = 10, max = 560, value = 100, step = 50),
      sliderInput("sd1", "SD 1:", min = 0.0001, max = 1, value = 0.0001, step = 0.1),
      sliderInput("sd2", "SD 2:", min = 0.0001, max = 1, value = 0.0001, step = 0.1),
      sliderInput("RR1_in", "RR 1 (Input):", min = 1, max = 2, value = 1.5, step = 0.1),
      sliderInput("RR2_in", "RR 2 (Input):", min = 1, max = 2, value = 1.5, step = 0.1)
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
    lambda1 <- input$lambda1
    lambda2 <- input$lambda2
    sd1 <- input$sd1
    sd2 <- input$sd2
    RR1.in <- input$RR1_in
    RR2.in <- input$RR2_in
    
    b1 <- tibble(.rows = 10000) %>%
      mutate(
        forecast1 = rpois(10000, exp(rnorm(10000, log(lambda1), sd = sd1))),
        historic1 = rpois(10000, exp(rnorm(10000, log(lambda1 / RR1.in), sd = 0.001))),
        forecast2 = rpois(10000, exp(rnorm(10000, log(lambda2), sd = sd2))),
        historic2 = rpois(10000, exp(rnorm(10000, log(lambda2 / RR2.in), sd = 0.001))),
        
        RR1 = (forecast1 + 1) / (historic1 + 1),
        RR2 = (forecast2 + 1) / (historic2 + 1)
      )
    
    prob_RR1_gt_1 <- mean(b1$RR1 > 1)
    prob_RR2_gt_1 <- mean(b1$RR2 > 1)
    
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
    
    pop2_scores <- b1 %>%
      group_by(forecast2) %>%
      summarize(
        N_obs = n()
      ) %>%
      ungroup() %>%
      mutate(
        probability2 = N_obs / sum(N_obs),
      ) %>%
      rename(predN =forecast2) %>%
      dplyr::select(predN, probability2)
    
    pop_score_compare <- pop1_scores %>%
      full_join(pop2_scores, by = 'predN') 
    
    thresholds <- 1:max(pop_score_compare$predN,na.rm=T)
    
    risk.threshold1 <- vector()
    risk.threshold2 <- vector()
    
    for(threshold in thresholds){
      
      probs_sum1 <- sum(pop_score_compare$probability1[pop_score_compare$predN>threshold], na.rm=T)
      probs_sum2 <- sum(pop_score_compare$probability2[pop_score_compare$predN>threshold], na.rm=T)
      
      risk.threshold1[threshold] <- probs_sum1*threshold 
      risk.threshold2[threshold] <- probs_sum2*threshold 
      
    }
    
    
    list(
      prob_RR1_gt_1 = prob_RR1_gt_1,
      prob_RR2_gt_1 = prob_RR2_gt_1,
      risk.threshold1 = risk.threshold1,
      risk.threshold2=risk.threshold2,
      thresholds=thresholds,
      b1=b1
    )
  })
  
  # Output probabilities
  output$probability_output <- renderPrint({
    scores <- calc_scores()
    cat("Max Score Pop 1:", max(scores$risk.threshold1, na.rm=T) ,"\n")
    cat("Max Score Pop 2:", max(scores$risk.threshold2, na.rm=T) ,"\n")
    
    "\n"
    cat("Prob greater than baseline Pop1:", scores$prob_RR1_gt_1, "\n")
    cat("Prob greater than baseline Pop2:", scores$prob_RR2_gt_1, "\n")
    
    "\n"
    cat("Combined score Pop1:", scores$prob_RR1_gt_1*max(scores$risk.threshold1, na.rm=T), "\n")
    cat("Combined score Pop2:", scores$prob_RR2_gt_1*max(scores$risk.threshold2, na.rm=T), "\n")
    
  })
  
  # Output plots
  output$risk_plots <- renderPlot({
    scores <- calc_scores()
    pop_score_compare <- cbind.data.frame('thresholds'=scores$thresholds,
                                          'risk.threshold1'= scores$risk.threshold1,
                                          'risk.threshold2'= scores$risk.threshold2
    )
    
    p1 <- ggplot(pop_score_compare) +
      geom_line(aes(x = thresholds, y = risk.threshold1), color = "#e41a1c") +
      geom_line(aes(x = thresholds, y = risk.threshold2), color = '#377eb8') +
      theme_classic() +
      ggtitle("Risk profile")
    
    p2 <- ggplot(scores$b1) +
      geom_histogram(aes(x=forecast1),binwidth=10, col='#e41a1c', alpha=0.5)+
      geom_histogram(aes(x=forecast2), binwidth=10, col='#377eb8', alpha=0.5)+
      theme_classic()+
      ggtitle('Posterior predictions for the two values')
    
    gridExtra::grid.arrange( p2,p1, ncol=2) 
  })
}

# Run the application
shinyApp(ui = ui, server = server)