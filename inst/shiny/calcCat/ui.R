library(shiny)

# Define UI for miles per gallon application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Calculated caternary from endpoints"),
  
  
  sidebarPanel(
    h4("Left endpoint"),
    numericInput("x0","x:",-1),
    numericInput("y0","y:",2),
    h4("Right endpoint"),
    numericInput("x1","x:",1),
    numericInput("y1","y:",2),
    selectInput("natural","Select length",
                c("Natural"="nat",
                  "Maximum"='max',
                  "Set length"="length")),
    conditionalPanel(
      condition = "input.natural=='length' ",
      sliderInput("L","Length",min=x0,max=20,value=3,step=0.1))
#       numericInput("L","Length:",4))
    ),

  
  mainPanel(
    plotOutput("catPlot")
  )
))