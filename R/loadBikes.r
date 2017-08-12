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

# myfun <- function(type = "response", ...){
#   match.arg(type, choices = c("response","link","terms"))
# }


loadBikes <- function(type = '28Jun2017-04Jul2017',...) {

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

  getFiles <- function(){
    # not so automatic
    url = paste0("http://cycling.data.tfl.gov.uk/usage-stats/",csvFiles())
    df=data.frame(url = url)
    df$dates = ""

    # extract
    dateRange <- "[0-9]{2}[a-zA-Z]{3}[0-9]{2}[-][0-9]{2}[a-zA-Z]{3}[0-9]{2}"
    ranges = str_extract_all(url, dateRange) %>%  unlist %>% data.frame(stringsAsFactors = F)
    df[grepl(dateRange, df$url),]$dates = ranges$.
    dateRange <- "[0-9]{2}[a-zA-Z]{3}[0-9]{4}[-][0-9]{2}[a-zA-Z]{3}[0-9]{4}"
    ranges = str_extract_all(url, dateRange) %>%  unlist %>% data.frame(stringsAsFactors = F)
    df[grepl(dateRange, df$url),]$dates = ranges$.
    # df = ranges %>%
    #   separate(date, c("from", "to"), "-")
    # ranges %>% arrange(date)

    df
  }

  df= getFiles()

  dateRange = match.arg(type, several.ok = F, choices = df$dates)
  require(data.table);require(ggmap);require(dplyr)



  url = df %>% filter(dates == dateRange) %>% select(url)
  url = as.character(url$url)



  colSchema = c("integer", "integer", "integer",
                "Date", "integer", "factor", "Date",
                "integer", "factor")


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
    }) %>% as_tibble


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

  path = "station_locations.Rdata"
  out_station_locations <- readRDS(path)

  # Put it all in a list and return
  out_list = list(data = out,
                  hourlyRentals = out_HourlyRentals,
                  stationJourneys = out_StationJourneys,
                  bikeUsage = out_BikeUsage,
                  stationStats = out_StationStats,
                  station_locations = station_locations)
  class(out_list) = 'biker'

  return(out_list)

}
