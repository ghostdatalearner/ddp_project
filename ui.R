# DDP Project
# Author: ghostdatalearner
# Date: October 2014
# Code at https://github.com/ghostdatalearner/ddp_project
# Shiny app at https://ghostdatalearner.shinyapps.io/ddp_project/

library(shiny)

shinyUI(fluidPage(
  # Introductory text and basic usage instructions
  titlePanel("Unemployent evolution in Spain 2005-2014"),
  p("Unemployment has been a severe problem for Spanish economy since the financial crisis of 2008.
    Heated political debated focuses on the monthly unemployment figure rise or fall when only mid and long term
    analysis may discover underlying trends.
    This application uses the monthly data published by the Spanish Employment Public Service."),
  p("You may create 4 kinds of graphs. Time evolution by gender and by age group show how abrupt 
    was unemployment growth after 2008. Due to structural reasons, female unemployment was higher
    before that date. Crisis reduced that ratio, but late signs of recuperation show that the gap is
    growing again. Despite the undesirable gender bias, this ratio seems to be a predictor
    of the turning point and that is what Female/Male Ratio graph shows. Finally, the Monthly Speed
    graph helps to locate the critical periods of Spanish unemployment growth."),
  p(""),p(""),
  a(href="usermanual.pdf","User Manual",target="_blank"),p(""),
  sidebarLayout(
    sidebarPanel(
      # Year selection
      fluidRow(
        column(10,
        sliderInput("slideryears", label = h4("Time Lapse"), min = 2005, 
                    max = 2014, value = c(2005, 2014), format = "####") ) ),
      # Select Gender Graph
      fluidRow( 
        column(10,
               h5("Evolution by Gender")
              )
      ),
      # Gender Options
      fluidRow( 
        column(5,
               checkboxInput("choosegendGraph", label = "Graph", value = TRUE)
               ),

        column(5,
               checkboxGroupInput("checkGender",  label="",
                                  choices = list("Male" = "M", "Female" = "F", "Total" = "TOTAL"),
                                  selected = c("M","F")) 
        )
      ),
      # Select Age Group Graph
      fluidRow( 
        column(10,
               h5("Evolution by Age")
              )
      ),     
      # Age Group Options
      fluidRow(
        column(5,
               checkboxInput("chooseageGraph", label = "Graph", value = FALSE)
        ),
        column(5,
               checkboxGroupInput("checkAge",  label="",
                                  choices = list("Young" = "Y", "Adult" = "A", "Total" = "TOTAL"),
                                 selected = c("Y","A")) 
        ), 
      # Select F/M ratio Graph
      fluidRow( 
        column(10,
               h5("Female/Male ratio")
        )
      ),     
      fluidRow(
        column(5,
               checkboxInput("chooserateGraph", label = "Graph", value = FALSE)
        ) 
      ),
      # Select Speed Graph
      fluidRow( 
        column(10,
               h5("Monthly Speed")
        )
      ),     
      fluidRow(
        column(5,
               checkboxInput("choosespeedGraph", label = "Graph", value = FALSE)
        ) 
      ),
      fluidRow(
        hr(),
        a(href="http://www.sepe.es/contenidos/que_es_el_sepe/estadisticas/datos_avance/paro/index.html", "Original sheet at Spanish Employment Public Service"),
        br(""),
        a(href="unemployment_sp_2005_2014.csv","Raw data")
      )
      )
      
    ),
    
  mainPanel(
    # Display Gender Graph
     conditionalPanel(
        condition="input.choosegendGraph",
        fluidRow(
        column(12, plotOutput("gendPlot"))
        )
      ),
    # Disply Age Group Graph
    conditionalPanel(
    condition="input.chooseageGraph",
        fluidRow(
        column(12,
               plotOutput("agePlot"))
        )
      ),
    # Display F/M ratio Graph
    conditionalPanel(
      condition="input.chooserateGraph",
      fluidRow(
        column(12,
               plotOutput("quotPlot"))
      )
    ),
    # Display Speed Graph
    conditionalPanel(
      condition="input.choosespeedGraph",
      fluidRow(
        column(12,
               plotOutput("speedPlot"))
      )
    )

     )
  )
))