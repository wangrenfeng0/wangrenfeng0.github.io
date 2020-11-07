library(shiny)
library(tidyr)
library(ggplot2)
library(usmap)
library(magrittr)
library(dplyr)


ui <- fluidPage(
  
  
  titlePanel("Beer Data"),
  
  
  sidebarLayout(
    
    
    sidebarPanel(
      
     
      selectInput("select", label = h3("ABV OR IBU"), 
                  choices = c("ABV", "IBU"), 
                  selected = 'ABV'),
      
      selectInput('states',label =h3('States Selection'),
                  choices=c('ALL',read.csv('Breweries.csv',header=T)$State),
                  selected='All'),
      
      radioButtons('plotType', label=h3('Histogram or Boxplot'),
                   choices=c('Histogram','Boxplot'),
                   selected='Histogram'),
      
      checkboxInput('showLM','Show regression Line',value=T),
      
      hr(),
      fluidRow(column(3, verbatimTextOutput("value")))
      
    ),
    
    
    mainPanel(
      
      
      plotOutput(outputId = "distPlot1"),
      plotOutput(outputId = 'distPlot2'),
      plotOutput(outputId = 'distPlot3'),
      plotOutput(outputId = 'distPlot4')
    )
  )
)




#shinyApp(ui, server)