library(shiny)
library(lattice)
library(ggplot2) 
library(geosphere)
library(maps)

shinyUI(pageWithSidebar(
  headerPanel("Fare Analysis"),
  sidebarPanel(
    fileInput('file1','choose Airfare file',accept = c('text/csv', 
                                                   'text/comma-separated-values,text/plain', 
                                                   '.csv')),
    fileInput('file2','choose Airport code file',accept = c('text/csv', 
                                                   'text/comma-separated-values,text/plain', 
                                                   '.csv')),
    
    uiOutput("choose_from"),
    uiOutput("choose_to"),
    radioButtons("output","Choose output type",choices = c("Plots","Fare Predictor")),
    conditionalPanel(condition = "input.output == 'Plots'",
                     selectInput("plot","Choose Plot",choices = c("Histogram","Bar Chart","Map"))),
    conditionalPanel(condition = "input.output == 'Fare Predictor'",
                     selectInput("fare","Choose quarter",choices = c("Show Data Table","Average Fare","summary")))
   
  ),
  mainPanel(
   conditionalPanel(condition = "input.output == 'Plots'",
    plotOutput("result"), plotOutput("result5")),
   conditionalPanel(condition = "input.output == 'Fare Predictor'", 
    tableOutput("result1"), tableOutput("result2"), tableOutput("result3"), verbatimTextOutput("result4"))
  
  )
))
