require(dplyr);require(stringr)
validDates <- function(){
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

urls = validDates()
# devtools::use_data(urls, internal = T, overwrite = T)
save(station_locations, urls, londonMap, file  = "../R/sysdata.rda")
# checking file info
file.info("../R/sysdata.rda")
file.size("../R/sysdata.rda")
# check rda to verify the best compression
tools::checkRdaFiles("../R/sysdata.rda")
# resave rda with a better compression if available
tools::resaveRdaFiles("../R/sysdata.rda")
tools::checkRdaFiles("../R/sysdata.rda")



save(station_locations, urls, londonMap, file  = "./sysdata.rda")

