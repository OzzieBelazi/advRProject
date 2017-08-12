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
# extract
dateRange <- "[0-9]{2}[a-zA-Z]{3}[0-9]{2}[-][0-9]{2}[a-zA-Z]{3}[0-9]{2}"
str_subset(d$entries, dateRange)
strings = d$entries
ranges = str_extract_all(strings, dateRange) %>%  unlist %>% data.frame(stringsAsFactors = F)
names(ranges) = 'date'

df = ranges %>%
  separate(date, c("from", "to"), "-")

fnFormatDate <- function(d){
  as.Date(d, "%d%b%y")
}

df$fromDate = df$from %>% fnFormatDate
df$toDate = df$to %>% fnFormatDate

df$dateRange = paste(df$from, df$to, sep = ' - ')
df$dateRange = paste(df$fromDate, df$toDate, sep = ' - ')

"
now work backwards from this to construct URL
"
df$dateRange[1]
d$entries[[1]]$url


uuu = d$entries %>% unlist %>% data.frame
names(uuu) = 'url'
uuu$url = gsub(x = uuu$url, pattern = 's3', replacement = 'http')


# extract
uuu$dates = ""
dateRange <- "[0-9]{2}[a-zA-Z]{3}[0-9]{2}[-][0-9]{2}[a-zA-Z]{3}[0-9]{2}"
ranges = str_extract_all(uuu$url, dateRange) %>%  unlist %>% data.frame(stringsAsFactors = F)
uuu[grepl(dateRange, uuu$url),]$dates = ranges$.
dateRange <- "[0-9]{2}[a-zA-Z]{3}[0-9]{4}[-][0-9]{2}[a-zA-Z]{3}[0-9]{4}"
ranges = str_extract_all(uuu$url, dateRange) %>%  unlist %>% data.frame(stringsAsFactors = F)
uuu[grepl(dateRange, uuu$url),]$dates = ranges$.

# names(ranges) = 'date'
# df = ranges %>%
#   separate(date, c("from", "to"), "-")
#
# ranges %>% arrange(date)







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







