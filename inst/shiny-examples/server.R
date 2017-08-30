library(shiny)
library(dplyr)

Models = data.frame(
  model = c("arima", "loess", "smooth.spline"),
  stringsAsFactors = FALSE
)

data  = loadBikes(range = '05Jul2017-11Jul2017')

shinyServer(function(input, output) {

  # output box 1 displays top level selections for plots
  output$Box1 = renderUI(
    if(is.null(input$rd) ){
      return()
    } else if (input$rd == "Modeling"){
    selectInput("model","select a model",c(unique(Models$model),"pick one"),"pick one")
    }else if (input$rd == "Station Stats"){
      # currently selective, good to have an additional all... or put somewhere else
      selectInput("station","select a station",c(unique(as.character(data$stationStats$Station)),"pick one"),"pick one")
    } else {
      # picka bike id to trace
      selectInput("bikeid","select a Bike",c(unique(data$bikeUsage$BikeId),"pick one"),"pick one")
    }

  )


  output$Box3 = renderUI(
    if (is.null(input$model) || input$model == "pick one"){
      return()
    }else if (input$model == "loess"){
      # Sidebar with a slider input for the number of bins
      sliderInput("span",
                  "Span:",
                  min = 0.0,
                  max = 1.0,
                  value = NULL)
    }else if (input$model == "smooth.spline"){
      # Sidebar with a slider input for the number of bins
      sliderInput("spar",
                  "Smoothing Parameter:",
                  min = 0.0,
                  max = 1.0,
                  value = NULL)
    }

  )


  # plot selected
  output$distPlot <- renderPlot({

    if(!is.null(input$rd )){

      # modeling
      if(input$rd == "Modeling" & !is.null(input$model)){


        if(input$model != "pick one"){

          if(input$model == "loess"  ){
            if(input$span != 0)
              fitted = fit(data, data_type = 'hourlyRentals', fit_type = input$model, span = input$span)
            else
              fitted = fit(data, data_type = 'hourlyRentals', fit_type = input$model)

          }else if(input$model == "smooth.spline"){
            if(input$spar != 0)
              fitted = fit(data, data_type = 'hourlyRentals', fit_type = input$model, spar = input$spar)
            else
              fitted = fit(data, data_type = 'hourlyRentals', fit_type = input$model)

          } else{
            print("aaa")
            fitted = fit(data, data_type = 'hourlyRentals', fit_type = input$model)

          }
          print("printing and plotting")
          plot(fitted)
        }

      } else if(input$rd == "Station Stats" ){

        if(!is.null(input$station) & !input$station == "pick one"){

          map.biker(obj = data, data_type = 'stationJourneys', station = input$station)

          # Generate a summary of the dataset
          output$table <- renderTable({
            data$stationStats %>% filter(Station == input$station) %>%
              rename(`Avg Trip Time` = averageTripTime, `Total Trips` = TotalTrips)
          })

        }

      }


    }

})


})
