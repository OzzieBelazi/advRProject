require(data.table);require(ggmap);require(dplyr)
"http://cycling.data.tfl.gov.uk/usage-stats/cycling-load.json"

df.raw = read.csv(file = "http://cycling.data.tfl.gov.uk/usage-stats/55JourneyData Extract26Apr2017-02May2017.csv", header = T)

url = "http://cycling.data.tfl.gov.uk/usage-stats/55JourneyData Extract26Apr2017-02May2017.csv"
df.raw=fread(url, showProgress = F, data.table = F)
df.raw%>% head
# remove spaces
names(df.raw) = gsub(x = names(df.raw), pattern = ' ', replacement = '')

df.raw %>% rename_all(df.raw, .funs = gsub(x=.,pattern = ' ', replacement = ''))

head(df.raw)

getBikeStation <- function(df, id){
  df[df$BikeId==id,c("StartStationName", "EndStationName")]

}

getRoute <- function(df, id, alt = F){
  cbind(BikeId = id, route(from = c(as.character(df$StartStation.Name)),
                           to = c(as.character(df$EndStationName)), mode = 'bicycling'
                           ,structure = "legs", alternatives = alt))
}

df.raw = dat$data

bike1 = df.raw[df.raw$BikeId==1,c("StartStationName", "EndStationName")]
legs_bike1 = route(from = c(as.character(bike1$StartStationName)),
                   to = c(as.character(bike1$EndStationName)), mode = 'bicycling'
                   ,structure = "legs", alternatives = TRUE)
legs_all = route(from = c(as.character(bike1$StartStationName)),
                 to = c(as.character(bike1$EndStationName)), mode = 'bicycling')
# legs_df <- route(
#   'marrs mclean science, baylor university',
#   '220 south 3rd street, waco, tx 76701',
#   alternatives = TRUE
# )
legs_df

# all legs
qmap("London", zoom = 14, maptype = 'terrain',
     base_layer = ggplot(aes(x = startLon, y = startLat), data = legs_all)) +
  geom_leg(data = legs_bike1,
           aes(x = startLon, y = startLat, xend = endLon, yend = endLat),
           alpha = 3/4, size = 2  ) +
  labs(x = 'Longitude', y = 'Latitude')




# by alt route
qmap("London", zoom = 14, maptype = 'hybrid',
     base_layer = ggplot(aes(x = startLon, y = startLat), data = legs_bike1)) +
  geom_leg(data = legs_bike1,
           aes(x = startLon, y = startLat, xend = endLon, yend = endLat, colour = route),
           alpha = 3/4, size = 2  ) +
  labs(x = 'Longitude', y = 'Latitude', colour = 'Route') +
  facet_wrap(~ route, ncol = 3) + theme(legend.position = 'top')



getBikeStation <- function(df, id){
  df[df$Bike.Id==id,c("StartStation.Name", "EndStation.Name")]

}

getRoute <- function(df, id, alt = F){
  cbind(Bike.Id = id, route(from = c(as.character(df$StartStation.Name)),
                            to = c(as.character(df$EndStation.Name)), mode = 'bicycling'
                            ,structure = "legs", alternatives = alt))
}
unique(df.raw$Bike.Id)

someBikes = df.raw %>% arrange(.,by=Duration, desc) %>% unique(df.raw$Bike.Id) %>% head
legs = list()
for(i in 1:length(someBikes)){
  legs[[i]] = df.raw %>% getBikeStation(id=someBikes[i]) %>% getRoute(id=someBikes[i])
  legs[[i]]$Bike.Id=someBikes[i]
}

legs[[i]]

df <- do.call("rbind", legs);df

area = c(left = min(as.numeric(df$endLon)), bottom = min(as.numeric(df$endLat)),
         right = max(as.numeric(df$endLon)), top = max(as.numeric(df$endLat)))


df$Bike.Id=as.factor(df$Bike.Id)
qmap("London", zoom = 13, "satellite",
     base_layer = ggplot(aes(x = startLon, y = startLat), data = df)) +
  geom_leg(data = df,
           aes(x = startLon, y = startLat, xend = endLon, yend = endLat, colour = Bike.Id),
           alpha = 3/4, size = 2  ) +
  labs(x = 'Longitude', y = 'Latitude')
# +
#   facet_wrap(~ df, ncol = 3) + theme(legend.position = 'top')


###### fitting
str(dat$hourlyRentals)
mod = dat$hourlyRentals %$% ksmooth(as.POSIXct(RentalDate), NumberOfRentals)
dat = loadBikes(type = 'Jul2017-11Jul2017')

x = fit(obj = dat, data_type = 'hourlyRentals', fit_type = 'k.smooth')
x = fit(obj = dat, data_type = 'hourlyRentals', fit_type = 'smooth.spline')
x = fit(obj = dat, data_type = 'hourlyRentals', fit_type = 'gam')
x = fit(obj = dat, data_type = 'hourlyRentals', fit_type = 'loess')


#####################           smooth.splines, gam
time_grid = pretty(as.POSIXct(as.character(x$data$RentalDate)), n = 100)
time_grid = pretty(as.POSIXct(x$data$RentalDate), n = length(x$data$RentalDate))

# predict(x$model, tibble(as.numeric(time_grid)))

fits =
  if(x$fit_type == 'loess'){
    tibble(time_grid, pred = predict(x$model, tibble(as.numeric(time_grid))))
  }else{
    tibble(time_grid, pred = predict(x$model, tibble(as.numeric(time_grid)))$y[,1])
  }

ls <- data.frame(x=fit.lowess$x, y=fit.lowess$y)
fits = tibble(time_grid, pred = predict(x$model, tibble(as.numeric(time_grid)))$y[,1])

predict(x$model, tibble(as.numeric(time_grid)))


df = x$data
df$RentalDate = as.POSIXct(df$x)
# Finally create the plot
ggplot(df, aes(y=NumberOfRentals, x=x)) +
  geom_point() +
  theme_bw() +
  xlab('Hourly') +
  scale_x_datetime(date_labels = '%Y-%m-%d %H', date_breaks = '3 hour', date_minor_breaks = "1 hour") +
  ylab('Number of bike rentals') +
  geom_line(data = fits, aes(x = time_grid, y = pred, colour = pred)) +
  theme(legend.position = 'None', axis.text.x = element_text(angle=90, hjust = 1))
# +
#   scale_color_viridis()

ans2 = fit(dat)
plot(ans2)
ans3 = fit(dat, data_type = 'hourlyRentals', fit_type = 'smooth.spline')


plot(ans3)
ans4 = fit(ans1, data_type = 'quarterly', fit_type = 'loess')
plot(ans4)
plot.biker_fit


# date_labels = '%Y-%m-%d %H:%M',
#


x = fit(obj = dat, data_type = 'hourlyRentals', fit_type = 'k.smooth')
plot(x)
x = fit(obj = dat, data_type = 'hourlyRentals', fit_type = 'smooth.spline')
plot(x)
x = fit(obj = dat, data_type = 'hourlyRentals', fit_type = 'gam')
plot(x)
x = fit(obj = dat, data_type = 'hourlyRentals', fit_type = 'loess')
plot(x)


# Get some predicted values based on the time_grid
if(x$fit_type == 'lm') {
  fits = tibble(time_grid, pred = predict(x$model, newdata = tibble(as.numeric(time_grid))))
} else if(x$fit_type == 'loess') {
  fits = tibble(time_grid, pred = predict(x$model, newdata = tibble(x = as.numeric(time_grid)))) %>%
    na.omit()
} else if(x$fit_type == 'smooth.spline') {
  fits = tibble(time_grid, pred = predict(x$model, tibble(as.numeric(time_grid)))$y[,1])
}





x=fitted
time_grid = pretty(as.POSIXct(x$data$RentalDate), n = nrow(x$data))

predict(fitted$model, tibble(as.numeric(time_grid)))

fits = tibble(time_grid, pred = predict(x$model, tibble(as.numeric(time_grid)))$y[,1])
df = x$data

# Finally create the plot
ggplot(df, aes(y=NumberOfRentals, x=RentalDate)) +
  geom_point() +
  theme_bw() +
  xlab('Hourly') +
  scale_x_datetime(date_labels = '%Y-%m-%d %H', date_breaks = '3 hour', date_minor_breaks = "1 hour") +
  ylab('Number of bike rentals') +
  geom_line(data = fits, aes(x = time_grid, y = pred, colour = pred)) +
  theme(legend.position = 'None', axis.text.x = element_text(angle=90, hjust = 1)) +
  scale_color_viridis()



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
Destination if a bike is rented from Station A
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

explore = dat$stationJourneys
explore %>% arrange(desc(TotalTrips) )
explore %>% arrange(desc(averageTripTime) )
plot(x=explore$StartStationName, y = explore$averageTripTime)

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

dat$data[dat$data$StartStationName == "Waterloo Place, St. James's" & dat$data$EndStationName=="Ashmole Estate, Oval",]
# average time of 76 bikes from these two spots
dat$data[dat$data$StartStationName == "Parsons Green Station, Parsons Green" &
           dat$data$EndStationName=="St. Peter's Terrace, Fulham",]

delete = explore %>% filter(as.character(StartStationName) != as.character(EndStationName) & TotalTrips > 10) %>%
  arrange(desc(TotalTrips)) %>%
  unique %>% head


df = getStationRoutes(delete)



# "get top routes lon/lat"
routes = route(from = as.character(delete$StartStationName[1]),
        to = as.character(delete$EndStationName[1]), mode = 'bicycling'
        )


getRoutes <- Vectorize(route,vectorize.args=c("from","to"),SIMPLIFY=FALSE)
topRoutes =
  getRoutes(from = as.character(delete$StartStationName),to = as.character(delete$EndStationName))

topRoutes$`Triangle Car Park, Hyde Park`

routes <- Vectorize(route,vectorize.args=c("from","to"),SIMPLIFY=FALSE)


getRoute <- function(df){

  # split by start and end station
  # loop over each ->
  mylist=list()
  for(i in 1:nrow(df)){
    mylist[[i]] = cbind(route = paste0(df$StartStationName[i], '-', df$EndStationName[i]),
          route(from = c(as.character(df$StartStationName[i])),
                to = c(as.character(df$EndStationName[i]))
                ))

  }
  bind_rows(mylist)

}

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
routes = route(from = as.character(delete$StartStationName[1:5]),
               to = as.character(delete$EndStationName[1:5])
)

routes %>% head


legs_df <- route(
  'Hyde Park Corner, Hyde Park',
  'Hyde Park, London W2 2UH',
  alternatives = F
)


aa = route(from = "Parsons Green Station, Parsons Green", to = "St. Peter's Terrace, Fulham", mode ='bicycling', structure = "legs")
sum(aa$m)
sum(aa$minutes)

getRoute <- function(df, id, alt = F){
  cbind(BikeId = id, route(from = c(as.character(df$StartStationName)),
                           to = c(as.character(df$EndStationName)), mode = 'bicycling'
                           ,structure = "legs", alternatives = alt))
}

getStationRoutes <- function(df, alt = F){
  cbind(route(from = c(as.character(df$StartStationName)),
                           to = c(as.character(df$EndStationName)), mode = 'bicycling'
                           ,structure = "legs", alternatives = alt))
}




delete = out_StationStats %>% arrange(desc(TotalTrips)) %>% head



qmap("London", zoom = 14, maptype = 'hybrid',
     base_layer = ggplot(aes(x = startLon, y = startLat), data = legs_df)) +
  geom_leg(data = legs_df,
           aes(x = startLon, y = startLat, xend = endLon, yend = endLat),
           alpha = 3/4, size = 2  ) +
  labs(x = 'Longitude', y = 'Latitude')


"
you can look at which stations have bikes rented out the longest
the dataset is ordered by station and the mean time of the bike rented
"
mostUsedStations = explore %>% rename(Station= StartStationName) %>% group_by(Station) %>% summarise(av = mean(averageTripTime)) %>% arrange(desc(av))
# set area (get df again..)
area = c(left = min(as.numeric(df$endLon)), bottom = min(as.numeric(df$endLat)),
         right = max(as.numeric(df$endLon)), top = max(as.numeric(df$endLat)))

# get coords of all stations
coords = geocode(as.character(mostUsedStations$Station))
path = paste0("D:/UCD/R Projects/TestPackage/bikesR/stationCoords.Rdata")
saveRDS(coords, path)
mod2 <- readRDS(path)

save(mod, file = "mymodel.Rdata")
load("mymodel.Rdata", envir = e <- new.env())
identical(mod, e$mod, ignore.environment = TRUE)
"
plot station coordinates and then overlay by
number of bikes rented
freq of bikes rented per day
rental duration
highest frequency of routes
"



