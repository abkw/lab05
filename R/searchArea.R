#' library(httr)
#' library(jsonlite)
#' library(utils)
#'
#'
#'
#' searchArea <- function(search_area) {
#'   if(typeof(search_area)!= "character") {
#'     stop("Invalid Input!")
#'   }
#'
#'   base_URL <- "http://api.kolada.se/"
#'   search_string <- URLencode(search_area, reserved = TRUE)
#'   search_path <- paste("v1/municipality/",search_string, sep = "")
#'
#'   raw_result <- GET(url = base_URL, path = search_path)
#'
#'   print(raw_result)
#'   print(fromJSON(rawToChar(raw_result$content)))
#' }
#'
#' # searchArea("stoc")
