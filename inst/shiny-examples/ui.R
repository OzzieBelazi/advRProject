library(shiny)
shinyUI(fluidPage(
  titlePanel("Forecasting of stock prices and their accuracies"),

  sidebarLayout(
    sidebarPanel(
      radioButtons("rd",
                   label="Select analysis for dataset",
                   choices=list("Modeling","Exploratory Analysis","Fun"),
                   selected="Modeling"),
      uiOutput("Box1"), # Stations
      uiOutput("Box2"), # Model Types
      uiOutput("Box3")  # Something Else
    ),
    # mainPanel("Display results",
    #           tableOutput("view"))

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )

  )
))
