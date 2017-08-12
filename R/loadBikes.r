# load bikes
#
# This is a function names loadBikes
# which returns metrics on bike data from the tfl website
#
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

loadBikes <- function(type = c('Jul2017-11Jul2017','28Jun2017-04Jul2017','21Jun2017-27Jun2017')) {
  require(data.table);require(ggmap);require(dplyr)


  getBikeStation <- function(df, id){
    df[df$BikeId==id,c("StartStationName", "EndStationName")]

  }

  getRoute <- function(df, id, alt = F){
    cbind(BikeId = id, route(from = c(as.character(df$StartStation.Name)),
                             to = c(as.character(df$EndStationName)), mode = 'bicycling'
                             ,structure = "legs", alternatives = alt))
  }

  fnFormatDate <- function(d){
    as.POSIXct(d, format = "%d/%m/%Y %H:%M")
  }


  colSchema = c("integer", "integer", "integer",
                "Date", "integer", "factor", "Date",
                "integer", "factor")


  arg = match.arg(type)

  url = "http://cycling.data.tfl.gov.uk/usage-stats/55JourneyData Extract26Apr2017-02May2017.csv"
  out = url %>% fread(showProgress = F, data.table = F,
                      colClasses = colSchema) %>% na.omit %>%
    filter(Duration != 0) %>%
    rename_all(
      funs(
        stringr::str_replace_all(., ' ', '')
      )) %>%
    (function(x){
      x$StartDate = fnFormatDate(x$StartDate)
      x$EndDate = fnFormatDate(x$EndDate)
      x
    })


  # remove spaces
  # names(out) = gsub(x = names(out), pattern = ' ', replacement = '')
  # out$StartDate = fnFormatDate(out$StartDate)
  # out$EndDate = fnFormatDate(out$EndDate)

  "number of rentals per hour"
  out_HourlyRentals = out %>%
    group_by(StartDate = cut(StartDate, breaks="hour")) %>%
    rename(x=StartDate) %>%
    summarize(NumberOfRentals = n())
  out_HourlyRentals$x=as.POSIXct(out_HourlyRentals$x)

  "Trips between stations, average cycle time, total trips"
  out_StationJourneys = out %>%
    group_by(StartStationName, EndStationName) %>%
    summarize(averageTripTime = mean(Duration)/60.0, TotalTrips = n()) %>%
    mutate(Journey = paste(StartStationName,EndStationName, sep = ' - ')) %>%
    arrange(TotalTrips)

  "number of rentals, total duration, number of stations visited"
  out_BikeUsage = out %>% group_by(BikeId) %>%
    summarize(NumberOfRentals = n(), averageJourneyTime=mean(Duration)/60, totalJourneyTime = sum(Duration)/60) %>%
    arrange(NumberOfRentals)

  "station stats"
  out_StationStats = out %>%
    rename(Station=StartStationName) %>%
    group_by(Station) %>%
    summarize(averageTripTime = mean(Duration)/60.0, TotalTrips = n()) %>%
    arrange(TotalTrips)


  # > hist(bikes$NumberOfRentals)
  # > hist(bikes$averageTimeRented)
  # > hist(bikes$averageTimeRented, breaks = 50)
  # > hist(bikes$totalTimeRented, breaks = 50)
  #

  # "get top routes lon/lat"
  # routes = route(from = c(as.character(out$StartStationName)),
  #         to = c(as.character(out$EndStationName)), mode = 'bicycling'
  #         ,structure = "legs")




  # Put it all in a list and return
  out_list = list(data = out,
                  hourlyRentals = out_HourlyRentals,
                  stationJourneys = out_StationJourneys,
                  bikeUsage = out_BikeUsage,
                  stationStats = out_StationStats,
                  type = arg)
  class(out_list) = 'biker'

  return(out_list)

}
