#' Map biker data
#'
#' @param obj Output from the \code{\link{loadBikes}} function
#' @param data_type Type of data argument, either stationJourneys (\code{stationJourneys}), stationStats (\code{stationStats})
#' @param plot_type Type of data argument, either usage (\code{usage}), rentals (\code{rentals})
#'
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
                plot_type = c('usage', 'rentals')) {

  # Create global variables to avoid annoying CRAN notes
  pred = NumberOfRentals = BikeId = ..level.. = na.omit = averageTripTime = lon = lat = NULL

  # Create a nice spatiol plot from the output of loadBikes

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

  } else if(map_dat == 'stationJourneys'){ # and routes
    # # subset the data top  10 ##############################
    # curr_dat = dat$stationJourneys %>% filter(TotalTrips > 10 & as.character(StartStationName) != as.character(EndStationName)) %>% arrange(desc(TotalTrips) ) %>% head(n=10)
    # # get coords
    # curr_dat = inner_join(station_locations, curr_dat, by = c("Station"="StartStationName")) %>%
    #   rename(startLon = lon, startLat = lat, startStation = Station)
    # curr_dat = inner_join(station_locations, curr_dat, by = c("Station"="EndStationName")) %>%
    #   rename(endLon = lon, endLat = lat, endStation = Station)
    # curr_dat
    #
    # route_df <- route(curr_dat$startStation[1], curr_dat$endStation[1], structure = "route")
    # qmap("London", zoom = 6) +
    #   geom_path(
    #     aes(x = lon, y = lat), colour = "red", size = 1.5,
    #     data = route_df, lineend = "round"
    #   ) ##############################
  }

  # ......... need this for stationStats, ...
  # area = c(left = min(as.numeric(df$endLon)), bottom = min(as.numeric(df$endLat)),
  #          right = max(as.numeric(df$endLon)), top = max(as.numeric(df$endLat)))

  map <- get_map(location = 'London', zoom = 13)
  LondonMap <- ggmap(map,
                     base_layer = ggplot(aes(x = lon, y = lat),
                                         data = curr_dat))

  # Format data
  if(map_arg == 'usage') {

    LondonMap +
      geom_point(aes(x = lon, y = lat, size = averageTripTime), data = curr_dat, alpha = .4) +
      scale_size_continuous(name="Usage (mins)", breaks = waiver())


  }
##########   else if(map_arg == 'routes') { ###############
#
#     "using getRoute"
#     LondonMap <- ggmap(map,
#                        base_layer = ggplot(aes(x = endLon, y = endLat),
#                                            data = sample))
#     LondonMap + geom_leg(data = sample,
#                          aes(x = startLon, y = startLat, xend = endLon, yend = endLat),
#                          alpha = 3/4, size = 2  ) +
#       labs(x = 'Longitude', y = 'Latitude')
#
#
# #
# #
# #     qmap("London", zoom = 14, maptype = 'hybrid',
# #          base_layer = ggplot(aes(x = startLon, y = startLat), data = legs_df)) +
# #       geom_leg(data = legs_df,
# #                aes(x = startLon, y = startLat, xend = endLon, yend = endLat),
# #                alpha = 3/4, size = 2  ) +
# #       labs(x = 'Longitude', y = 'Latitude')
# #
#
#   } #####################
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


}
