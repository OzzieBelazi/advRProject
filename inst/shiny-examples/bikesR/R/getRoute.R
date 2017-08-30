getRoute <-
function(df, id){
  cbind(BikeId = id, route(from = c(as.character(df$StartStationName)),
   to = c(as.character(df$EndStationName)), mode = 'bicycling'
   ,structure = "legs"))
}
