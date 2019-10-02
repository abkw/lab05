# library(httr)
# library(jsonlite)
# library(lubridate)
##
## Attaching package: 'jsonlite'
##
## The following object is masked from 'package:utils':
##
##     View

# url  <- "http://api.kolada.se/v1/"
# path <- "v1/municipality"
# query <- "page=4&per_page=77"
# raw.result <- GET(url = url, path = path, query = query)
# print(names(raw.result$status_code))
# result <- fromJSON(rawToChar(raw.result$content))
# print(result)
# print(typeof(result))
