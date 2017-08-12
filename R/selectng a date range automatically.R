"
selectng a date range automatically
"


library(dplyr)
library(jsonlite)
library(stringr)
library(tidyr)



# A JSON array of primitives
json <- 'http://cycling.data.tfl.gov.uk/usage-stats/cycling-load.json'

# Simplifies into an atomic vector
d=fromJSON(json, simplifyVector = FALSE)
d$entries[1]
# extract
dateRange <- "[0-9]{2}[a-zA-Z]{3}[0-9]{2}[-][0-9]{2}[a-zA-Z]{3}[0-9]{2}"
str_subset(d$entries, dateRange)
strings = d$entries
ranges = str_extract_all(strings, dateRange) %>%  unlist %>% data.frame(stringsAsFactors = F)
names(ranges) = 'date'

dff = ranges %>%
  separate(date, c("from", "to"), "-")

fnFormatDate <- function(d){
  as.Date(d, "%d%b%y")
}

dff$fromDate = dff$from %>% fnFormatDate
dff$toDate = dff$to %>% fnFormatDate

dff

