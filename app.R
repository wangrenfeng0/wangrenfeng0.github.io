library(shiny)
library(tidyr)
library(ggplot2)
library(usmap)
library(magrittr)
library(dplyr)
library(stringr)
library(stringi)

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


server <- function(input, output,session) {
  


    Beers <<- read.csv('Beers.csv',header=T)
    Breweries <<- read.csv('Breweries.csv',header=T)
    
    colnames(Beers)[colnames(Beers)=='Brewery_id']='Brew_ID'  
    dfbeer <<- merge(Beers, Breweries, by='Brew_ID', all=T) 
    colnames(dfbeer)[colnames(dfbeer)=='Name.x']='Beer_Name'
    colnames(dfbeer)[colnames(dfbeer)=='Name.y']='Brewery_Name'
    
    beer1 <<- 
        dfbeer %>% 
        filter(!is.na(ABV) & !is.na(IBU))
    
    Mean_ABV_IBU=dfbeer %>% 
        group_by(State) %>% summarise(Mean_ABV=mean(ABV, na.rm=TRUE), Mean_IBU=mean(IBU,na.rm=TRUE))
    
    colnames(Mean_ABV_IBU)[colnames(Mean_ABV_IBU)=='State']='abbr'
    statedata= statepop[,c(2,3)]
    statedata$abbr=as.factor(statedata$abbr)
    Mean_ABV_IBU$abbr=as.factor(Mean_ABV_IBU$abbr)
    statedata1=merge(statedata,Mean_ABV_IBU,by='abbr',all.x=T)
    statedata2=cbind(statedata1,Mean_ABV_IBU)
    statedata3=statedata2[,c(1,2,6,7)]
    colnames(statedata3)[colnames(statedata3)=='full']='state'
    
    
    output$distPlot1 <- renderPlot({
        
        if(input$select=='ABV')
        {
            
            if(input$plotType=='Histogram')
            {
                if(input$states=='ALL')
                {
                    ggplot(beer1,aes(ABV))+geom_histogram(aes(y=..density..),colour='black',fill='white')+
                        geom_density(alpha=.5, fill='#FF6666')+xlab('ABV of the country')
                }
                else
                {
                    beer1 %>% filter(State==input$states) %>% 
                        ggplot(aes(ABV))+geom_histogram(aes(y=..density..),colour='black',fill='white')+
                        geom_density(alpha=.5, fill='#FF6666')+xlab(paste('ABV of', input$states))
                }
            }
            else
            {
                if(input$states=='ALL')
                {
                    ggplot(beer1,aes(ABV))+geom_boxplot()+xlab('ABV of the country')
                }
                else
                {
                    beer1 %>% filter(State==input$states) %>%
                        ggplot(aes(ABV))+geom_boxplot()+xlab(paste('ABV of', input$states))
                }
            }
        }
        
        else
        {
            
            if(input$plotType=='Histogram')
            {
                if(input$states=='ALL')
                {
                    ggplot(beer1,aes(IBU))+geom_histogram(aes(y=..density..),colour='black',fill='white')+
                        geom_density(alpha=.5, fill='#FF6666')+xlab('IBU of the country')
                }
                else
                {
                    beer1 %>% filter(State==input$states) %>%
                        ggplot(aes(IBU))+geom_histogram(aes(y=..density..),colour='black',fill='white')+
                        geom_density(alpha=.5, fill='#FF6666')+xlab(paste('IBU of', input$states))
                }
            }
            else
            {
                if(input$states=='ALL')
                {
                    ggplot(beer1,aes(IBU))+geom_boxplot()+xlab('IBU of the country')
                }
                else
                {
                    beer1 %>% filter(State==input$states) %>%
                        ggplot(aes(IBU))+geom_boxplot()+xlab(paste('IBU of', input$states))
                }
            }
        }
    })
    
    output$distPlot2 <- renderPlot({
        if(input$showLM)
        {
            if(input$states=='ALL')
            {
                ggplot(beer1,aes(y=ABV, x=IBU))+geom_point(position='jitter')+geom_smooth(method=lm)+
                    ggtitle('ABV v. IBU')
            }
            else
            {
                beer1 %>% filter(State==input$states) %>%
                    ggplot(aes(y=ABV, x=IBU))+geom_point(position='jitter')+geom_smooth(method=lm)+
                    ggtitle(paste('ABV v. IBU in',input$states))
            }
        }
        else
        {
            if(input$states=='ALL')
            {
                ggplot(beer1,aes(y=ABV, x=IBU))+geom_point(position='jitter')+ggtitle('ABV v. IBU')
            }
            else
            {
                beer1 %>% filter(State==input$states) %>%
                    ggplot(aes(y=ABV, x=IBU))+geom_point(position='jitter')+ggtitle(paste('ABV v. IBU in',input$states))
            }
        }
    })
    
    output$distPlot3 <- renderPlot({
        plot_usmap(data = statedata3, values = "Mean_ABV", color = "red") + 
            scale_fill_continuous(
                low = "white", high = "red", name = "Mean_ABV", label = scales::comma
            ) + theme(legend.position = "right") + ggtitle('Mean ABV by State')
    })
    
    output$distPlot4 <- renderPlot({
        plot_usmap(data = statedata3, values = "Mean_IBU", color = "black") + 
            scale_fill_continuous(
                low = "white", high = "blue", name = "Mean_IBU", label = scales::comma
            ) + theme(legend.position = "right") + ggtitle('Mean IBU by State')
    })
}


shinyApp(ui, server)