#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Exploring mtcars: how combined variables influence mpg"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
            width=4,
                 sliderInput("nvar",
                   "Number of variables to use:",
                   min = 1,
                   max = 10,
                   value = 1),
       radioButtons("indic","Choose the indicator you want to explore :",
                    choiceNames = c("Adjusted R-Squared","Multiple R Squared",
                                    "Residual Standard Error","p-value","Variance Inflation Factor",
                                    "F-statistic","Residuals"),
                    choiceValues = c(3:9)
                    ),
       actionButton("showhigh","Show Highest Values"),
       actionButton("showlow","Show Lowest Values"),
       
       textOutput("text"),
       verbatimTextOutput("text2") 
    ),
    
    # Show a plot of the generated distribution
    mainPanel(tabsetPanel(type="tabs",
              tabPanel("Mpg Explorer",br(),
        plotOutput("distPlot",brush = brushOpts(id = "plot_brush")),
        h4("Points you've just selected :"),
        verbatimTextOutput("select_points"),
        verbatimTextOutput("plot_brushinfo")),
        tabPanel("Documentation",br(), verbatimTextOutput("documentation")))
    )
  )
))
