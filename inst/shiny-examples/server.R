library(shiny)
biz = data.frame(
  Sector = c("a", "a", "a", "a", "b", "b", "b", "b", "b", "b", "b", "b"),
  Stock = c("Infy","TCS","Wipro","TechM","SBIN","ICICI","HDFC", "Axis", "IDBI", "PSB","BOI","Bob"),
  stringsAsFactors = FALSE
)

Models = data.frame(
  model = c("arima", "k.smooth", "loess", "smooth.spline"),
  stringsAsFactors = FALSE
)

StatType = data.frame(
  type = c("Usage", "Rental"),
  stringsAsFactors = FALSE
)


shinyServer(function(input, output) {

  # output box 1 displays top level selections for plots
  output$Box1 = renderUI(
    if(is.null(input$rd) || is.null(input$rd)){
      return()
    } else if (input$rd == "Modeling"){
    selectInput("model","select a model",c(unique(Models$model),"pick one"),"pick one")
    }else if (input$rd == "Exploratory Analysis"){
      # currently selective, good to have an additional all... or put somewhere else
      selectInput("station","select a station",c(unique(data$stationStats$Station),"pick one"),"pick one")
    } else {
      # picka bike id to trace
      selectInput("bikeid","select a Bike",c(unique(input$rd),"pick one"),"pick one")
    }

  )

# select a station
  # output$Box1 = renderUI(
  #
  #     selectInput("station","select a station",c(unique(data$stationStats$Station),"pick one"),"pick one")
  #
  #   )

# view usage / rental
  output$Box2 = renderUI(
    if (is.null(input$station) || input$station == "pick one"){return()
    }else selectInput("usage",
                      "Select a hourly",
                      c(unique(biz$Stock[which(biz$Sector == input$sector)]),"pick one"),
                      "pick one")
  )

  # output$Box2 = renderUI(
  #   if (is.null(input$station) || input$station == "pick one"){return()
  #   }else selectInput("usage",
  #                     "Select a hourly",
  #                     c(unique(biz$Stock[which(biz$Sector == input$sector)]),"pick one"),
  #                     "pick one")
  # )


  # plot selected
  output$distPlot <- renderPlot({

    if( input$rd == "Modeling" & !is.null(input$model) & !input$model == "pick one"){
      ans2 = fit(data, data_type = 'hourlyRentals', fit_type = input$model)
      plot(ans2)
    } else if(1 != 1){
      return()

      map.biker(obj = ans1, data_type = 'stationStats', 'usage')


      map.biker(obj = ans1, data_type = 'stationStats', 'rentals')



    } else{ # nothing
      return()
      # x    <- faithful[, 2]  # Old Faithful Geyser data
      # # bins <- seq(min(x), max(x), length.out = input$bins + 1)
      # bins <- seq(min(x), max(x))
      # # draw the histogram with the specified number of bins
      # hist(x, breaks = bins, col = 'darkgray', border = 'white')


    }

})


  #//todo ad summary of models
  # Generate a summary of the dataset
  # output$summary <- renderPrint({
  #   dataset <- datasetInput()
  #   summary(dataset)
  # })



  # subdata1 = reactive(biz[which(biz$Sector == input$sector),])
  # subdata2 = reactive(subdata1()[which(subdata1()$Stock == input$stock),])
  #
  # output$view = renderTable({
  #   if(is.null(input$sector) || is.null(input$stock)){return()
  #   } else if (input$sector == "pick one" || input$stock == "pick one"){return()
  #
  #   } else return(subdata2())
  # })

})
