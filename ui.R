library(shiny)

shinyUI(fluidPage(
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
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(10,
        sliderInput("slideryears", label = h4("Time Lapse"), min = 2005, 
                    max = 2014, value = c(2005, 2014), format = "####") ) ),
      fluidRow( 
        column(10,
               h5("Evolution by Gender")
              )
      ),
      fluidRow( 
        column(5,
               checkboxInput("choosegendGraph", label = "Graph", value = TRUE)
               ),

        column(5,
               checkboxGroupInput("checkGender",  label="",
                                  choices = list("Male" = "M", "Female" = "F", "Both" = "TOTAL"),
                                  selected = c("M","F")) 
        )
      ),
      fluidRow( 
        column(10,
               h5("Evolution by Age")
              )
      ),     
      fluidRow(
        column(5,
               checkboxInput("chooseageGraph", label = "Graph", value = TRUE)
        ),
        column(5,
               checkboxGroupInput("checkAge",  label="",
                                  choices = list("Young" = "Y", "Adult" = "A", "Both" = "TOTAL"),
                                 selected = c("Y","A")) 
        ), 
      
      
      fluidRow( 
        column(10,
               h5("Female/Male rate")
        )
      ),     
      fluidRow(
        column(5,
               checkboxInput("chooserateGraph", label = "Graph", value = TRUE)
        ) 
      ),
      
      fluidRow( 
        column(10,
               h5("Monthly Speed")
        )
      ),     
      fluidRow(
        column(5,
               checkboxInput("choosespeedGraph", label = "Graph", value = TRUE)
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
     conditionalPanel(
        condition="input.choosegendGraph == true",
        fluidRow(
        column(12, plotOutput("gendPlot"))
        )
      ),
    conditionalPanel(
    condition="input.chooseageGraph == true",
        fluidRow(
        column(12,
               plotOutput("agePlot"))
        )
      ),
    conditionalPanel(
      condition="input.chooserateGraph == true",
      fluidRow(
        column(12,
               plotOutput("quotPlot"))
      )
    ),
    conditionalPanel(
      condition="input.choosespeedGraph == true",
      fluidRow(
        column(12,
               plotOutput("speedPlot"))
      )
    )

     )
  )
))