#' Map biker data
#'
#' @param obj Output from the \code{\link{loadBikes}} function
#' @param data_type Type of data argument, either stationJourneys (\code{stationJourneys}), stationStats (\code{stationStats})
#' @param plot_type Type of data argument, either usage (\code{usage}), rentals (\code{rentals})
#' @param ...
#' @return Nothing: just a nice plot
#' @seealso \code{\link{loadBikes}}
#' @export
#' @import ggplot2
#' @import ggmap
#' @importFrom tibble "tibble"
#' @importFrom viridis "scale_color_viridis"
#'
#' @examples
#' ans1 = loadBikes('26Jul2017-31Jul2017')
#' map.biker(obj = ans1, data_type = 'stationStats', 'usage')
#' map.biker(obj = ans1, data_type = 'stationStats', 'rentals')
map.biker = function(obj,
                data_type = c('stationJourneys',  'stationStats'),
                plot_type = c('usage', 'rentals'),...) {

  # store all optional arguments in a list
  z <- list(...)

  # Create global variables to avoid annoying CRAN notes
  pred = NumberOfRentals = BikeId = ..level.. = na.omit = averageTripTime = lon = lat = ... = NULL

  # Create a nice spatial plot from the output of loadBikes

  # Find out which data set to use
  map_dat = match.arg(data_type)
  # Find what type of plotting method
  map_arg = match.arg(plot_type)

  # Find out which bit of the data to take
  dat_choose = switch(map_dat,
                      stationJourneys = 4,
                      stationStats = 6
                      )

  # Get the data set to use
  curr_dat = obj %>% extract2(dat_choose)

  if(map_dat == 'stationStats'){

    curr_dat = inner_join(station_locations, curr_dat, by = c("Station"="Station"))

    # ......... need this for stationStats, ...
    # area = c(left = min(as.numeric(df$endLon)), bottom = min(as.numeric(df$endLat)),
    #          right = max(as.numeric(df$endLon)), top = max(as.numeric(df$endLat)))
    # map <- get_map(location = 'London', zoom = 13)

    LondonMap <- ggmap(londonMap,
                       base_layer = ggplot(aes(x = lon, y = lat),
                                           data = curr_dat))

    # Format data
    if(map_arg == 'usage') {

      LondonMap +
        geom_point(aes(x = lon, y = lat, size = averageTripTime), data = curr_dat, alpha = .4) +
        scale_size_continuous(name="Usage (mins)", breaks = waiver())

    }

    else if(map_arg == 'rentals') {


      # explode data
      df <- as.data.frame(curr_dat)
      curr_dat <- df[rep(1:nrow(df), df[,5]),-5]

      LondonMap +
        stat_density2d(aes(x = lon, y = lat,
                           fill = ..level.., alpha = ..level..),
                       bins = 5, geom = "polygon",
                       data = curr_dat) +
        scale_fill_gradient(low = "black",
                            high= "red")


    }


  } else if(map_dat == 'stationJourneys'){ # and routes

    if(is.null(z$station) ){
      print("Station not supplied, defaulting to 'Abbey Orchard Street, Westminster'")
      print("For a full list of stations please type 'View(station_locations)'")
      station =  "Abbey Orchard Street, Westminster"
    }


    curr_dat = curr_dat %>%
      filter(StartStationName == z$station & as.character(StartStationName) != as.character(EndStationName)) %>%
      arrange(desc(TotalTrips) ) %>% head(20)

    aList = list()
    for(i in 1:nrow(curr_dat)){
      aList[[i]] =
        cbind(TotalTrips = curr_dat[i,]$TotalTrips, averageTripTime = curr_dat[i,]$averageTripTime, EndStationName = curr_dat[i,]$EndStationName,
          route(from = c(as.character(curr_dat[i,]$StartStationName)),
            to = c(as.character(curr_dat[i,]$EndStationName)), mode = 'bicycling'
            ,structure = "legs")
        )

    }
    df = data.frame()
    for(i in 1:length(aList)){
      if(is.data.frame(aList[[i]]))
        df = bind_rows(df, aList[[i]])
    }
    df$StartStationName = z$station


    # # Lets trace the journey of a single bike over the course of the week
    # sample = getRoute(ans1$data, id = 1)
    # get potential routes between each station the bike has visited
    # map <- get_map(location = 'London', zoom = 14)
    ggmap(londonMap, base_layer = ggplot(aes(x = endLon, y = endLat),data = df)) +
      geom_leg(data = df,
               aes(x = startLon, y = startLat, xend = endLon, yend = endLat, colour = EndStationName),
               alpha = 3/4, size = 2  ) +
      labs(x = 'Longitude', y = 'Latitude') +
      ggtitle("Journey of a bike over the course of a week")


  }

}
