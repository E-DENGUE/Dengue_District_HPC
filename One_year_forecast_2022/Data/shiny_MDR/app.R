library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(shinydashboard)
library(lubridate)
library(patchwork)
library(plotly)
library(RColorBrewer)
library(shinydashboard)


# a1 <- readRDS('./full_data_with_new_boundaries_all_factors_cleaned.rds') %>%
#   arrange(district, date) %>%
# dplyr::select(district, date, m_DHF_cases) %>%
#   saveRDS(.,'./case_counts_month.rds')

a1 <-  readRDS('./case_counts_month.rds')

all.districts <- unique(a1$district)

## The interactive part of the code starts here
app2 <- shinyApp(
  
  
  ## UI IS USED TO CONTROL THE FRONT END
  ui = dashboardPage(
    
    dashboardHeader(title = "",titleWidth=500),
    
    #see input options: https://shiny.posit.co/r/gallery/widgets/widget-gallery/
    dashboardSidebar(
   
      selectInput("districts", "Select district(s):", 
                  all.districts, multiple=T, selected=all.districts[1:3]),
      
      dateRangeInput("plotdates", label = h3("Date range"), start='2004-01-01', end='2022-12-01')
      
    ),
    dashboardBody(
      fluidRow(
        box(tabPanel("Plot1", plotlyOutput("plot1")), width=12),
        valueBoxOutput("TallyBox")
        
      )
    )
  ), 
  
  
  ##SERVER IS USED TO DEFINE THE BACK END
  server = function(input, output) {
    output$plot1 = renderPlotly({
      p1 <- a1 %>%
        filter(district %in% input$districts
               & date>= input$plotdates[1] & date <= input$plotdates[2]) %>%
        ggplot(aes(x=date, y=m_DHF_cases)) +
        geom_line() +
        theme_classic() +
        facet_wrap(~district, scales='free') +
        theme(axis.text.x=element_text(angle = 45, vjust = 0.5))
      ggplotly(p1)
    })

  }
)