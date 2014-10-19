library(shiny)

shinyUI(fluidPage(
  titlePanel("Basic widgets"),
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
        ) 
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
      )
      
    ),
    
  
  mainPanel(
     h3(textOutput("caption")),
#     verbatimTextOutput("range"),
#     verbatimTextOutput("koko"),


    
     verbatimTextOutput("displaygend"),
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
# ,
#     fluidRow(
#       column(12,
#              plotOutput("quotPlot")
#       ),
#       column(12,
#              plotOutput("speedPlot")
#       ) 
#       )
     )
  )
))