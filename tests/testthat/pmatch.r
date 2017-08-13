# # There are two ways to use the function.
# # One way is to give the choices when you call match.arg:
# myfun <- function(type = "response", ...){
#   match.arg(type, choices = c("response","link","terms"))
# }
# # The other way is to give the choices in the argument list to the function:
# myfun <- function(type = c("response","link","terms"), ...){
#   match.arg(type)
# }
#
# # Results are the same with either of the above.
# myfun()
# # [1] "response"
# myfun("li")
# # [1] "link"
# myfun("le")
# # Gives the following error message:
# #  Error: 'arg' should be one of: "response", "link", "terms"
# myfun("linki")
# # same error message as previous example
#
# # You can also use the first way with no default value
# myfun <- function(type, ...){
#   match.arg(type, choices = c("response","link","terms"))
# }
# # With no default, this fails:
# myfun()
#
# # Specify several.ok=TRUE to get the unique
# # partial match for multiple strings
# match.arg(c("li","r","t"), c("response","link","terms"), several.ok=TRUE)
# # [1] "link" "response" "terms"
#
# # Hello, world!
# #
# # This is an example function named 'hello'
# # which prints 'Hello, world!'.
# #
# # You can learn more about package authoring with RStudio at:
# #
# #   http://r-pkgs.had.co.nz/
# #
# # Some useful keyboard shortcuts for package authoring:
# #
# #   Build and Reload Package:  'Ctrl + Shift + B'
# #   Check Package:             'Ctrl + Shift + E'
# #   Test Package:              'Ctrl + Shift + T'
#
# load_bikes <- function(type = c('','')) {
#
#   getBikeStation <- function(df, id){
#     df[df$BikeId==id,c("StartStationName", "EndStationName")]
#
#   }
#
#   getRoute <- function(df, id, alt = F){
#     cbind(BikeId = id, route(from = c(as.character(df$StartStationName)),
#                               to = c(as.character(df$EndStationName)), mode = 'bicycling'
#                               ,structure = "legs", alternatives = alt))
#   }
#
#   arg = match.arg(type)
#
#   #get url dataset
#   url = "http://cycling.data.tfl.gov.uk/usage-stats/55JourneyData Extract26Apr2017-02May2017.csv"
#   d=fread(url) %>% data.frame
#   d = d %>% setNames(gsub(" ","",names(.)))
#   # names(df.raw) = gsub(x = names(df.raw), pattern = ' ', replacement = '')
#   # d %>% rename_all(~gsub(' ', '', .x))
#   # head(df.raw)
#
#   someBikes = d %>% select(Bike.Id) %>% unique %>% head
#   legs = list()
#   for(i in 1:length(someBikes)){
#     legs[[i]] = d %>% getBikeStation(id=someBikes[i]) %>% getRoute(id=someBikes[i])
#     legs[[i]]$BikeId=someBikes[i]
#   }
#
#
#
#
#
# }
