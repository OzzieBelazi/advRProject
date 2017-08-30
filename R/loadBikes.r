#' Load in bike usage data from TFL
#'
#' @param range A list of weekly date ranges maintained by TFL.
#' @param ... date ranges corresponding to files on the tfl website
#' @return A list of \code{\link[tibble]{tibble}}s which contains hourly, a, and b values for each time series respectively.
#' @export
#' @import dplyr
#' @import tidyr
#' @import stringr
#' @importFrom data.table "fread"
#'
#'
#' @seealso \code{\link{loadBikes}}, \code{\link{fit}}
#'
#' @examples
#' ans1 = loadBikes(range = '05Jul2017-11Jul2017')

loadBikes <- function(range = c('26Jul2017-31Jul2017','19Jul2017-25Jul2017','12Jul2017-18Jul2017','05Jul2017-11Jul2017'),...) {

  BikeId = NumberOfRentals = x = station_locations = . = na.omit = StartStationName = EndStationName = StartDate = Duration = url = dates = TotalTrips = Station = NULL

  getBikeStation <- function(df, id){
    df[df$BikeId==id,c("StartStationName", "EndStationName")]

  }

  fnFormatDate <- function(d){
    as.POSIXct(d, format = "%d/%m/%Y %H:%M")
  }

  tryCatch(match.arg(range, choices = urls$dates) , error = function(c) {
    # c$message <- paste0(c$message, " (in ", range, ")")
    c$message <- paste0(" Invalid argument (range = '",range,"'). Please run validDates() for valid date ranges.")

    stop(c)
  })

  date_range = range

  url = urls[urls$dates == date_range,]$url
  url = as.character(url)

  # set column schema for faster loads
  colSchema = c("integer", "integer", "integer",
                "Date", "integer", "factor", "Date",
                "integer", "factor")

  # if selected load precache file
  if(range %in% c('05Jul2017-11Jul2017')){
    url = paste0("65JourneyDataExtract", range,".csv")
    url = system.file("extdata", url,  package='bikesR')
}

    out = fread(url, showProgress = F, data.table = F,
                      colClasses = colSchema) %>% na.omit %>%
    dplyr::filter(Duration != 0) %>%
    rename_all(
      funs(
        stringr::str_replace_all(., ' ', '')
      )) %>%
    (function(x){
      x$StartDate = fnFormatDate(x$StartDate)
      x$EndDate = fnFormatDate(x$EndDate)
      x
    }) %>% as_tibble


  #"number of rentals per hour"
  out_HourlyRentals = out %>%
    group_by(StartDate = cut(StartDate, breaks="hour")) %>%
    rename(x=StartDate) %>%
    summarize(NumberOfRentals = n())
  out_HourlyRentals$x=as.POSIXct(out_HourlyRentals$x)

  #"number of rentals per day"
  out_DailyRentals = out %>%
    group_by(StartDate = cut(StartDate, breaks="day")) %>%
    rename(x=StartDate) %>%
    summarize(NumberOfRentals = n())
  out_DailyRentals$x=as.POSIXct(out_DailyRentals$x)

  #"Trips between stations, average cycle time, total trips"
  out_StationJourneys = out %>%
    group_by(StartStationName, EndStationName) %>%
    summarize(averageTripTime = mean(Duration)/60.0, TotalTrips = n()) %>%
    mutate(Journey = paste(StartStationName,EndStationName, sep = ' - ')) %>%
    arrange(TotalTrips)

  #"number of rentals, total duration, number of stations visited"
  out_BikeUsage = out %>% group_by(BikeId) %>%
    summarize(NumberOfRentals = n(), averageJourneyTime=mean(Duration)/60, totalJourneyTime = sum(Duration)/60) %>%
    arrange(NumberOfRentals)

  #"station stats"
  out_StationStats = out %>%
    rename(Station=StartStationName) %>%
    group_by(Station) %>%
    summarize(averageTripTime = mean(Duration)/60.0, TotalTrips = n()) %>%
    arrange(TotalTrips)

  # Put it all in a list and return
  out_list = list(data = out,
                  hourlyRentals = out_HourlyRentals,
                  dailyRentals = out_DailyRentals,
                  stationJourneys = out_StationJourneys,
                  bikeUsage = out_BikeUsage,
                  stationStats = out_StationStats
                 )
  class(out_list) = 'biker'

  return(out_list)

}
