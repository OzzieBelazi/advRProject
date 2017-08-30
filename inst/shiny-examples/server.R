library(shiny)
library(dplyr)

Models = data.frame(
  model = c("arima", "loess", "smooth.spline"),
  stringsAsFactors = FALSE
)

StatType = data.frame(
  type = c("usage", "rental"),
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
      selectInput("bikeid","select a Bike",c(unique(input$rd),"pick one"),"pick one")
    }

  )

# view usage / rental
# output$Box2 = renderUI(
#   if (is.null(input$station) || input$station == "pick one"){return()
#   }else selectInput("usage",
#                     "Select a hourly",
#                     c(unique(biz$Stock[which(biz$Sector == input$sector)]),"pick one"),
#                     "pick one")
# )

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

    if( input$rd == "Modeling" & !is.null(input$model) & !input$model == "pick one"){


      fitted = if(input$model == "loess" & input$span != 0 ){

        fit(data, data_type = 'hourlyRentals', fit_type = input$model, span = input$span)

      }else if(input$model == "smooth.spline" & input$spar != 0){

        fit(data, data_type = 'hourlyRentals', fit_type = input$model, spar = input$spar)

      } else{

        fit(data, data_type = 'hourlyRentals', fit_type = input$model)

        # output$table <- renderTable({
        #   data$stationStats %>% filter(Station == input$station) %>%
        #     rename(`Avg Trip Time` = averageTripTime, `Total Trips` = TotalTrips)
        # })

      }
      print(summary(fitted$model))
      plot(fitted)




    } else if(input$rd == "Station Stats" & !is.null(input$station) & !input$station == "pick one" ){

      print(input$station)
      # map.biker(obj = data, data_type = 'stationStats', 'usage')

      # Generate a summary of the dataset
      output$table <- renderTable({
        data$stationStats %>% filter(Station == input$station) %>%
          rename(`Avg Trip Time` = averageTripTime, `Total Trips` = TotalTrips)
      })



    } else if(input$rd == "Fun"){



      map.biker(obj = data, data_type = 'stationStats', 'usage')


    }

})


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
