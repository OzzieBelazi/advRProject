library(shiny)
shinyUI(fluidPage(
  titlePanel("bikeR"),

  sidebarLayout(
    sidebarPanel(
      radioButtons("rd",
                   label="Select analysis for dataset",
                   choices=list("Modeling","Station Stats","Fun"),
                   selected="Modeling"),
      uiOutput("Box1"), # Stations
      uiOutput("Box2"), #
      uiOutput("Box3")  # Model Types
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot"),
      tableOutput('table')
    )

  )
))
