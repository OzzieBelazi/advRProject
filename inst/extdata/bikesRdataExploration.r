require(data.table);require(ggmap);require(dplyr)
"http://cycling.data.tfl.gov.uk/usage-stats/cycling-load.json"

# df.raw = read.csv(file = "http://cycling.data.tfl.gov.uk/usage-stats/55JourneyData Extract26Apr2017-02May2017.csv", header = T)
#
url = "http://cycling.data.tfl.gov.uk/usage-stats/55JourneyData Extract26Apr2017-02May2017.csv"
df.raw=fread(url, showProgress = F, data.table = F)

dat = loadBikes(type = "06Jul2016-12Jul2016")

x = fit(obj = dat, data_type = 'hourlyRentals', fit_type = 'k.smooth')
x = fit(obj = dat, data_type = 'hourlyRentals', fit_type = 'smooth.spline')
x = fit(obj = dat, data_type = 'hourlyRentals', fit_type = 'gam')
x = fit(obj = dat, data_type = 'hourlyRentals', fit_type = 'loess')


x = fit(obj = dat, data_type = 'hourlyRentals', fit_type = 'k.smooth')
plot(x)
x = fit(obj = dat, data_type = 'hourlyRentals', fit_type = 'smooth.spline')
plot(x)
x = fit(obj = dat, data_type = 'hourlyRentals', fit_type = 'gam')
plot(x)
x = fit(obj = dat, data_type = 'hourlyRentals', fit_type = 'loess')
plot(x)




################################################################# next dataset bike usage ##############################################
x = fit(obj = dat, data_type = 'hourlyRentals', fit_type = 'k.smooth')
x = fit(obj = dat, data_type = 'hourlyRentals', fit_type = 'smooth.spline')
x = fit(obj = dat, data_type = 'hourlyRentals', fit_type = 'gam')
x = fit(obj = dat, data_type = 'hourlyRentals', fit_type = 'loess')


"
Times a bike is rented from a station -
Busiest stations
Which trips are the most frequent
average amount of time a bike is rented from a station

Plots
Barplot of top 10 busiest stations
bike coverage area
stations in london



Model
Destition if a bike is rented from Station A
arules, random forest, logistic regression

BikeUsage
Bikes used the most
plot of bike journey on google maps

bike coverage area
stations in london
overlay with bike journeys (show travel area of bikes)
show longest distance by route a bike has travelled

can i get the expected distance between two opints and then compare with the times recorded

"
############################## exploring data not to be included in project analysis ###################################
explore = dat$stationJourneys
explore %>% arrange(desc(TotalTrips) )
explore %>% arrange(desc(averageTripTime) )
plot(x=explore$StartStationme, y = explore$averageTripTime)

explore = dat$bikeUsage

hist(explore$NumberOfRentals)
hist(bikes$averageTimeRented)
hist(bikes$averageTimeRented, breaks = 50)
hist(bikes$totalTimeRented, breaks = 50)

# dont understand these ones, the time is massive
explore %>% arrange(desc(averageTripTime) ) %>% filter(TotalTrips < 10)
explore %>% arrange(desc(TotalTrips) ) %>% filter(averageTripTime < 30)
explore %>% arrange(desc(averageTripTime) ) %>% filter(averageTripTime < 30)
explore %>% arrange(desc(TotalTrips) ) %>% filter(averageTripTime < 30)
# dont understand these ones, the time is massive
############################## exploring ###################################


########################################### sample routes ############################################
getRoute <- function(df){
  # split by start and end station
  # loop over each ->
  mylist=list()
  for(i in 1:nrow(df)){
    mylist[[i]] = cbind(route = paste0(df$StartStationme[i], '-', df$EndStationme[i]),
    route(from = c(as.character(df$StartStationme[i])),
    to = c(as.character(df$EndStationme[i]))
    ))

  }
  bind_rows(mylist)

}

# "get top routes lon/lat"
routes = route(from = as.character(delete$StartStationme[1]),
               to = as.character(delete$EndStationme[1]), mode = 'bicycling'
)


getRoutes <- Vectorize(route,vectorize.args=c("from","to"),SIMPLIFY=FALSE)
topRoutes =
  getRoutes(from = as.character(delete$StartStationme),to = as.character(delete$EndStationme))

topRoutes$`Triangle Car Park, Hyde Park`

routes <- Vectorize(route,vectorize.args=c("from","to"),SIMPLIFY=FALSE)

df = getRoute(delete)
head(df)
dftemp = df[df$route=='Hyde Park Corner, Hyde Park-Triangle Car Park, Hyde Park',]


ggmap("London", zoom = 14, maptype = 'terrain',
     base_layer = ggplot(aes(x = startLon, y = startLat), data = r)) +
  geom_leg(data = r,
     aes(x = startLon, y = startLat, xend = endLon, yend = endLat),
     alpha = 3/4, size = 2  ) +
  labs(x = 'Longitude', y = 'Latitude', colour = 'Route') +
  facet_wrap(~ route, ncol = 3) + theme(legend.position = 'top')


r = route(from = 'Hyde Park Corner, Hyde Park', to = 'Hyde Park, London W2 2UH')



# "get top routes lon/lat"
routes = route(from = as.character(delete$StartStationme[1:5]),
   to = as.character(delete$EndStationme[1:5])
)

routes %>% head


legs_df <- route(
  'Hyde Park Corner, Hyde Park',
  'Hyde Park, London W2 2UH',
  altertives = F
)

getRoute <- function(df, id, alt = F){
  cbind(BikeId = id, route(from = c(as.character(df$StartStationme)),
   to = c(as.character(df$EndStationme)), mode = 'bicycling'
   ,structure = "legs", altertives = alt))
}

getStationRoutes <- function(df, alt = F){
  cbind(route(from = c(as.character(df$StartStationme)),
   to = c(as.character(df$EndStationme)), mode = 'bicycling'
   ,structure = "legs", altertives = alt))
}



qmap("London", zoom = 14, maptype = 'hybrid',
     base_layer = ggplot(aes(x = startLon, y = startLat), data = legs_df)) +
  geom_leg(data = legs_df,
     aes(x = startLon, y = startLat, xend = endLon, yend = endLat),
     alpha = 3/4, size = 2  ) +
  labs(x = 'Longitude', y = 'Latitude')
########################################### sample routes ############################################



# get coords of all stations
# coords = geocode(as.character(mostUsedStations$Station))
# path = paste0("D:/UCD/R Projects/TestPackage/bikesR/stationCoords.Rdata")
# saveRDS(coords, path)
# mod2 <- readRDS(path)


"
plot station coordites and then overlay by
number of bikes rented
freq of bikes rented per day
rental duration
highest frequency of routes
"

# ggmap("London", zoom = 14, maptype = 'terrain',
#       base_layer = ggplot(aes(x = startLon, y = startLat), data = station_out)) +
#   geom_leg(data = r,
#            aes(x = lon, y = lat, xend = lon, yend = lat),
#            alpha = 3/4, size = 2  ) +
#   labs(x = 'Longitude', y = 'Latitude', colour = 'Route') +
#   facet_wrap(~ route, ncol = 3) + theme(legend.position = 'top')





library(ggmap)
library(scales)


LondonMap <- ggmap(map,
                    base_layer = ggplot(aes(x = lon, y = lat),
                                        data = station_out))
LondonMap

# explode out station_out
# dat$stationStats %>% arrange(Station)
# tmp = station_out %>% arrange(Station)
# station_locations = tmp %>% select(Station, lon, lat)
# path = paste0("D:/UCD/R Projects/TestPackage/bikesR/station_locations.Rdata")
# saveRDS(station_locations, path)
# mod2 <- readRDS(path)


############################# map of most used location#########################################

# "
# you can look at which stations have bikes rented out the longest
# the dataset is ordered by station and the mean time of the bike rented
# "
# "??????????"
# mostUsedStations = explore %>% rename(Station= StartStationName) %>% group_by(Station) %>% summarise(av = mean(averageTripTime)) %>% arrange(desc(av))
# # set area (get df again..)
# area = c(left = min(as.numeric(df$endLon)), bottom = min(as.numeric(df$endLat)),
#          right = max(as.numeric(df$endLon)), top = max(as.numeric(df$endLat)))
#
# "??????????"


station_out = dat$stationJourneys %>% group_by(StartStationName) %>% rename(Station= StartStationName) %>%
  summarise(av = mean(averageTripTime)) %>% arrange(desc(av)) %>% inner_join(., station_locations, by=c("Station"))

map <- get_map(location = 'London', zoom = 12)
# ggmap(map) +
LondonMap +
  geom_point(aes(x = lon, y = lat, size = av), data = station_out, alpha = .5) +
scale_size_continuous(name="Usage (mins)", breaks = waiver())
# c(0,25,50,75,100,125,150, 175, 200)

############################# map of busiest locations #########################################
newStationStats = inner_join(station_locations, dat$stationStats, by = c("Station"="Station"))
newStationStats$averageTripTime=NULL

# explode data
df <- as.data.frame(newStationStats)
df2 <- df[rep(1:nrow(df), df[,4]),-4]


LondonMap +
  stat_density2d(aes(x = lon, y = lat,
    fill = ..level.., alpha = ..level..),
               bins = 5, geom = "polygon",
               data = df2) +
  scale_fill_gradient(low = "black",
                      high= "red")
# +
#   facet_wrap(~ day)
############################# map of busiest locations #########################################

############################# data cleaning #########################################
#
# station_locations[station_locations$Station=='Ada Street, Hackney Central, London',]$Station = 'Ada Street, Hackney Central'
# station_locations[station_locations$Station=='Ada Street, Hackney Central',]$lon = -0.060516
# station_locations[station_locations$Station=='Ada Street, Hackney Central',]$lat = 51.535716
# station_locations[is.na(station_locations$lon),]
# "
#  1     Contact Centre, Southbury House, London    NA    NA
#  2         Fire Brigade Pier, Vauxhall, London    NA    NA Fire Brigade Pier, Vauxhall Lat/Lon 51.49369988,-0.121394101
#  3 Geraldine Street, Elephant & Castle, London    NA    NA  Geraldine St Lat/Lon, 51.49612799,-0.106000855
#  4             Grenfell Road, Avondale, London    NA    NA 51.512912, -0.214752
#  5    Hampstead Road (Cartmel), Euston, London    NA    NA 51.525847, -0.138533
#  6      Lansdowne Road, Ladbroke Grove, London    NA    NA 51.510343, -0.207612
#  7          Mechanical Workshop Penton, London    NA    NA Lat 51.531666, Long -0.109914
#  8    Plough Terrace, Clapham Junction, London    NA    NA Lat 51.462305, Long -0.175407
#  9              Poured Lines, Bankside, London    NA    NA Lat 51.506692, Long -0.103137
# 10    Simpson Street, Clapham Junction, London    NA    NA Lat 51.470847, Long -0.170703
# 11           Strata, Elephant & Castle, London    NA    NA  Lat 51.493146, Long -0.099828
# "
# station_locations[station_locations$Station=='Strata, Elephant & Castle, London',]$Station = 'Strata, Elephant & Castle'
# station_locations[station_locations$Station=='Strata, Elephant & Castle',]$lon = -0.099828
# station_locations[station_locations$Station=='Strata, Elephant & Castle',]$lat = 51.493146













# df.raw%>% head
# # remove spaces
# mes(df.raw) = gsub(x = mes(df.raw), pattern = ' ', replacement = '')
#
# df.raw %>% reme_all(df.raw, .funs = gsub(x=.,pattern = ' ', replacement = ''))
#
# head(df.raw)
#
# getBikeStation <- function(df, id){
#   df[df$BikeId==id,c("StartStationme", "EndStationme")]
#
# }
#
# getRoute <- function(df, id, alt = F){
#   cbind(BikeId = id, route(from = c(as.character(df$StartStation.me)),
#    to = c(as.character(df$EndStationme)), mode = 'bicycling'
#    ,structure = "legs", altertives = alt))
# }
#
# df.raw = dat$data
#
# bike1 = df.raw[df.raw$BikeId==1,c("StartStationme", "EndStationme")]
# legs_bike1 = route(from = c(as.character(bike1$StartStationme)),
#  to = c(as.character(bike1$EndStationme)), mode = 'bicycling'
#  ,structure = "legs", altertives = TRUE)
# legs_all = route(from = c(as.character(bike1$StartStationme)),
#      to = c(as.character(bike1$EndStationme)), mode = 'bicycling')
# # legs_df <- route(
# #   'marrs mclean science, baylor university',
# #   '220 south 3rd street, waco, tx 76701',
# #   altertives = TRUE
# # )
# legs_df
#
# # all legs
# qmap("London", zoom = 14, maptype = 'terrain',
#      base_layer = ggplot(aes(x = startLon, y = startLat), data = legs_all)) +
#   geom_leg(data = legs_bike1,
#      aes(x = startLon, y = startLat, xend = endLon, yend = endLat),
#      alpha = 3/4, size = 2  ) +
#   labs(x = 'Longitude', y = 'Latitude')
#
#
#
#
# # by alt route
# qmap("London", zoom = 14, maptype = 'hybrid',
#      base_layer = ggplot(aes(x = startLon, y = startLat), data = legs_bike1)) +
#   geom_leg(data = legs_bike1,
#      aes(x = startLon, y = startLat, xend = endLon, yend = endLat, colour = route),
#      alpha = 3/4, size = 2  ) +
#   labs(x = 'Longitude', y = 'Latitude', colour = 'Route') +
#   facet_wrap(~ route, ncol = 3) + theme(legend.position = 'top')
#
#
#
# getBikeStation <- function(df, id){
#   df[df$Bike.Id==id,c("StartStation.me", "EndStation.me")]
#
# }
#
# getRoute <- function(df, id, alt = F){
#   cbind(Bike.Id = id, route(from = c(as.character(df$StartStation.me)),
#     to = c(as.character(df$EndStation.me)), mode = 'bicycling'
#     ,structure = "legs", altertives = alt))
# }
# unique(df.raw$Bike.Id)
#
# someBikes = df.raw %>% arrange(.,by=Duration, desc) %>% unique(df.raw$Bike.Id) %>% head
# legs = list()
# for(i in 1:length(someBikes)){
#   legs[[i]] = df.raw %>% getBikeStation(id=someBikes[i]) %>% getRoute(id=someBikes[i])
#   legs[[i]]$Bike.Id=someBikes[i]
# }
#
# legs[[i]]
#
# df <- do.call("rbind", legs);df
#
# area = c(left = min(as.numeric(df$endLon)), bottom = min(as.numeric(df$endLat)),
#    right = max(as.numeric(df$endLon)), top = max(as.numeric(df$endLat)))
#
#
# df$Bike.Id=as.factor(df$Bike.Id)
# qmap("London", zoom = 13, "satellite",
#      base_layer = ggplot(aes(x = startLon, y = startLat), data = df)) +
#   geom_leg(data = df,
#      aes(x = startLon, y = startLat, xend = endLon, yend = endLat, colour = Bike.Id),
#      alpha = 3/4, size = 2  ) +
#   labs(x = 'Longitude', y = 'Latitude')
# # +
# #   facet_wrap(~ df, ncol = 3) + theme(legend.position = 'top')
#
#
# ###### fitting
# str(dat$hourlyRentals)
# mod = dat$hourlyRentals %$% ksmooth(as.POSIXct(RentalDate), NumberOfRentals)

getBikeStation <- function(df, id){
  df[df$BikeId==id,c("StartStationName", "EndStationName")]

}

getBikeStation <- function(df, id){
  df[,c("BikeId", "StartStationName", "EndStationName")]

}

getRoute <- function(df, id){
  cbind(BikeId = id, route(from = c(as.character(df$StartStationName)),
   to = c(as.character(df$EndStationName)), mode = 'bicycling'
   ,structure = "legs"))
}

"single bike"
sample = getRoute(ans1$data, id = 1)
"using getRoute"
LondonMap <- ggmap(map,
                   base_layer = ggplot(aes(x = endLon, y = endLat),
                                       data = sample))
LondonMap + geom_leg(data = sample,
         aes(x = startLon, y = startLat, xend = endLon, yend = endLat),
         alpha = 3/4, size = 2  ) +
labs(x = 'Longitude', y = 'Latitude')

"works......................................................."
"top number of rentals per bike"
sample = ans1$data %>% arrange(desc(Duration)) %>% head(20)
ids = ans1$data %>%
  group_by(BikeId) %>%
  summarise(n = n()) %>% arrange(desc(n)) %>% select(BikeId) %>% head(20)

sample2 = as.data.frame()
list.all = list()
for(id in 1:nrow(ids)){
  print(id)
  list.all[[id]] = getRoute(sample[id,], id = ids[id,])
}
sample2 = bind_rows(list.all)

"using getRoute"
LondonMap <- ggmap(map,
                   base_layer = ggplot(aes(x = endLon, y = endLat),
                                       data = sample2))
LondonMap + geom_leg(data = sample2,
                     aes(x = startLon, y = startLat, xend = endLon, yend = endLat, color = BikeId),
                     alpha = 3/4, size = 2  ) +
  labs(x = 'Longitude', y = 'Latitude')+
  scale_color_gradientn(colours = rainbow(10), breaks = seq(25, 200, by = 10))


"not using that"
sample = getBikeStation(ans1$data, 6216)
sample$StartStationName = as.character(sample$StartStationName)
sample2 = inner_join(sample, station_locations, by = c("StartStationName"="Station")) %>%
  select(StartStationName, lon, lat) %>% rename(Station = StartStationName)

map <- get_map(location = 'London', zoom = 12)
LondonMap <- ggmap(map,
                   base_layer = ggplot(aes(x = lon, y = lat),
                                       data = sample2))


LondonMap +   geom_path(data = sample2, lineend = "round")


ggmap(bikemap1) +
  geom_path(data = bike, aes(color = elevation), size = 3, lineend = "round") +
  scale_color_gradientn(colours = rainbow(7), breaks = seq(25, 200, by = 25))

"all bikes in one go"
getBikeStation <- function(df, id){
  df[,c("BikeId", "StartStationName", "EndStationName")]

}
sample = getBikeStation(ans1$data, 6216)
sample$StartStationName = as.character(sample$StartStationName)
sample2 = inner_join(sample, station_locations, by = c("StartStationName"="Station")) %>%
  select(BikeId, StartStationName, lon, lat) %>% rename(Station = StartStationName)

map <- get_map(location = 'London', zoom = 12)
LondonMap <- ggmap(map,
                   base_layer = ggplot(aes(x = lon, y = lat),
                                       data = sample2))+
  geom_path(data = bike, aes(color = elevation), size = 3, lineend = "round")

LondonMap +   geom_path(data = sample2, aes(color = BikeId), lineend = "round")


ggmap(bikemap1) +
  geom_path(data = bike, aes(color = elevation), size = 3, lineend = "round") +
  scale_color_gradientn(colours = rainbow(7), breaks = seq(25, 200, by = 25))






################################################### to be deleted ########################################################
# #####################     smooth.splines, gam
# time_grid = pretty(as.POSIXct(as.character(x$data$RentalDate)), n = 100)
# time_grid = pretty(as.POSIXct(x$data$RentalDate), n = length(x$data$RentalDate))
#
# # predict(x$model, tibble(as.numeric(time_grid)))
#
# fits =
#   if(x$fit_type == 'loess'){
#     tibble(time_grid, pred = predict(x$model, tibble(as.numeric(time_grid))))
#   }else{
#     tibble(time_grid, pred = predict(x$model, tibble(as.numeric(time_grid)))$y[,1])
#   }
#
# ls <- data.frame(x=fit.lowess$x, y=fit.lowess$y)
# fits = tibble(time_grid, pred = predict(x$model, tibble(as.numeric(time_grid)))$y[,1])
#
# predict(x$model, tibble(as.numeric(time_grid)))
#
#
# df = x$data
# df$RentalDate = as.POSIXct(df$x)
# # Filly create the plot
# ggplot(df, aes(y=NumberOfRentals, x=x)) +
#   geom_point() +
#   theme_bw() +
#   xlab('Hourly') +
#   scale_x_datetime(date_labels = '%Y-%m-%d %H', date_breaks = '3 hour', date_minor_breaks = "1 hour") +
#   ylab('Number of bike rentals') +
#   geom_line(data = fits, aes(x = time_grid, y = pred, colour = pred)) +
#   theme(legend.position = 'None', axis.text.x = element_text(angle=90, hjust = 1))
# # +
# #   scale_color_viridis()
#
# ans2 = fit(dat)
# plot(ans2)
# ans3 = fit(dat, data_type = 'hourlyRentals', fit_type = 'smooth.spline')
#
#
# plot(ans3)
# ans4 = fit(ans1, data_type = 'quarterly', fit_type = 'loess')
# plot(ans4)
# plot.biker_fit
#
#
#
# x=fitted
# time_grid = pretty(as.POSIXct(x$data$RentalDate), n = nrow(x$data))
#
# predict(fitted$model, tibble(as.numeric(time_grid)))
#
# fits = tibble(time_grid, pred = predict(x$model, tibble(as.numeric(time_grid)))$y[,1])
# df = x$data
#
# # Filly create the plot
# ggplot(df, aes(y=NumberOfRentals, x=RentalDate)) +
#   geom_point() +
#   theme_bw() +
#   xlab('Hourly') +
#   scale_x_datetime(date_labels = '%Y-%m-%d %H', date_breaks = '3 hour', date_minor_breaks = "1 hour") +
#   ylab('Number of bike rentals') +
#   geom_line(data = fits, aes(x = time_grid, y = pred, colour = pred)) +
#   theme(legend.position = 'None', axis.text.x = element_text(angle=90, hjust = 1)) +
#   scale_color_viridis()
################################################## to be deleted ######################################################
