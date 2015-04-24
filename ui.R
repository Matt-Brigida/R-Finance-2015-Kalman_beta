# Based on RStudio's stockVis app, Shiny lesson 6, here: http://shiny.rstudio.com/tutorial/lesson6/ ----

library(shiny)

shinyUI(fluidPage(
  titlePanel("Kalman Filtered Beta Coefficient"),
  
  sidebarLayout(
    sidebarPanel(
		 #       helpText("Select a stock to examine. 
		 #         Information will be collected from yahoo finance."),
    
      textInput("symb", "Ticker", "C"),
    
      dateRangeInput("dates", 
        "Date range",
        start = "2003-01-01", 
        end = as.character(Sys.Date())),
   
		 #        actionButton("get", "Get Stock"),
br(),

sliderInput("init", "Initial Beta Value", min = -5, max = 5, value = 1, step= 0.1),

sliderInput("init.sd", "Initial Standard Deviation of Beta", min = 0.01, max = 1, value = 0.05, step= 0.01)

  ),
    
    mainPanel(plotOutput("plot"))
  )
))

