
# validDates <- function(){
#   # not so automatic
#   url = paste0("http://cycling.data.tfl.gov.uk/usage-stats/",csvFiles())
#   df=data.frame(url = url)
#   df$dates = ""
#
#   # extract
#   dateRange <- "[0-9]{2}[a-zA-Z]{3}[0-9]{2}[-][0-9]{2}[a-zA-Z]{3}[0-9]{2}"
#   ranges = str_extract_all(url, dateRange) %>%  unlist %>% data.frame(stringsAsFactors = F)
#   df[grepl(dateRange, df$url),]$dates = ranges$.
#   dateRange <- "[0-9]{2}[a-zA-Z]{3}[0-9]{4}[-][0-9]{2}[a-zA-Z]{3}[0-9]{4}"
#   ranges = str_extract_all(url, dateRange) %>%  unlist %>% data.frame(stringsAsFactors = F)
#   df[grepl(dateRange, df$url),]$dates = ranges$.
#   # df = ranges %>%
#   #   separate(date, c("from", "to"), "-")
#   # ranges %>% arrange(date)
#
#   df
# }
