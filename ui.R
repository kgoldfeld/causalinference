#ui.R
library(shiny)
library(ggplot2)
library(data.table)
library(reshape2)
library(Matching)

bwidth = 120

# Define UI for slider demo application
shinyUI(fluidPage(
  
  #  Application title
  headerPanel(h5("Causal effects and confounding")) ,
  
  fluidRow(
  
  # Sidebar with sliders that demonstrate various available options
  column(4,
    
    wellPanel(
     h6("Outcome without treatment"),
     fluidRow(
       column(6,img(src = "RedDots.png", height = 3, width = bwidth)),
       column(6,img(src = "RedLine.png", height = 3, width = bwidth))
     ),
     fluidRow(
       column(6,sliderInput("RD", "",  min=-50, max=50, value=0,width=bwidth)),
       column(6,sliderInput("RS", "",  min=-50, max=50, value=5,width=bwidth))
     ),
     fluidRow(
       column(6,img(src = "GreenDots.png", height = 10, width = bwidth)),
       column(6,img(src = "GreenLine.png", height = 10, width = bwidth))
     ),
     fluidRow(
       column(6,sliderInput("GD", "",min=-50, max=50, value=10,width=bwidth)),
       column(6,sliderInput("GS", "", min=-50, max=50, value=15,width=bwidth))
     )
    ),
    wellPanel(
     h6("Treatment effect"),
     fluidRow(
       column(6,img(src = "RedDots.png", height = 10, width = bwidth)),
       column(6,img(src = "RedLine.png", height = 10, width = bwidth))
     ),
     fluidRow(
       column(6,sliderInput("RD_effect", "", min=-20, max=20, value=5,width=bwidth)),
       column(6,sliderInput("RS_effect", "", min=-20, max=20, value=5,width=bwidth))
     ),
     fluidRow(
       column(6,img(src = "GreenDots.png", height = 10, width = bwidth)),
       column(6,img(src = "GreenLine.png", height = 10, width = bwidth))
     ),
     fluidRow(
       column(6,sliderInput("GD_effect", "",min=-20, max=20, value=5,width=bwidth)),
       column(6,sliderInput("GS_effect", "",min=-20, max=20, value=5,width=bwidth))
     )
    ),
    wellPanel(
      h6("Probability of treatment"),
      fluidRow(
        column(6,img(src = "RedDots.png", height = 10, width = bwidth)),
        column(6,img(src = "RedLine.png", height = 10, width = bwidth))
      ),
      fluidRow(
        column(6,sliderInput("RD_prob", "", min=0, max=1, 
                             step=.05, ticks=FALSE, format="0.00",  value=.25,width=bwidth)),
        column(6,sliderInput("RS_prob", "", min=0, max=1, 
                             step=.05, ticks=FALSE, format="0.00",  value=.25,width=bwidth))
      ),
      fluidRow(
        column(6,img(src = "GreenDots.png", height = 10, width = bwidth)),
        column(6,img(src = "GreenLine.png", height = 10, width = bwidth))
      ),
      fluidRow(
        column(6,sliderInput("GD_prob", "",min=0, max=1,  
                             step=.05, ticks=FALSE, format="0.00", value=.75,width=bwidth)),
        column(6,sliderInput("GS_prob", "",min=0, max=1, 
                             step=.05, ticks=FALSE, format="0.00", value=.75,width=bwidth))
      )
    )
  ),
  
  # Show plot
  column(8,
    tabsetPanel(
      
      tabPanel("Parameters",
        fluidRow(
         column(12,plotOutput("effectPlot",height="300px",width="500px"))
        ),
        fluidRow(
          column(12,plotOutput("probPlot",height="300px",width="500px"))
        )
      ),
      
     tabPanel("Simulated Data Plot",
        fluidRow(
          column(12,plotOutput("simPlot",height="300px",width = "500px"))
        ),
        fluidRow(
          column(6,checkboxInput("trueCausal", label = "Show true average causal effect", value = FALSE)),
          column(6,checkboxInput("observed", label = "Show observed average effect", value = FALSE))
        ),
        fluidRow(
           column(12,plotOutput("randomPlot",height="300px",width = "500px"))
        )
     ),

      tabPanel("Matched Data", 
       fluidRow(
         column(12,plotOutput("matchPlot",height="300px",width = "500px"))
       ),
       fluidRow(
         column(12,checkboxInput("ACE",label="Show average causal effect",value=FALSE))
       ),
       fluidRow(
         column(12,checkboxInput("ATT",label="Show average treatment effect for treated",value=FALSE))
       ),
       fluidRow(
         column(12,checkboxInput("OBS",label="Show estimated treatment effect for treated",value=FALSE))
       ),
       fluidRow(column(12)),
       fluidRow(column(12)),
       fluidRow(
         column(12,checkboxInput("replace",label="Matching with replacement",value=TRUE))
       )
      ),

      tabPanel("Simulated Data", 
                 dataTableOutput("simTable")
      )
    )    
  )  
)))