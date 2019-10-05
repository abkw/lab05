#' library(httr)
#' library(jsonlite)
#' library(utils)
#'
#'
#' getKeyFigures <- function(search_item) {
#'   if(typeof(search_item)!= "character") {
#'     stop("Invalid Input!")
#'   }
#'
#'   base_URL <- "http://api.kolada.se/"
#'   search_string <- URLencode(search_item, reserved = TRUE)
#'   kpi_path <- paste("/v1/kpi/",search_string, sep = "")
#'
#'   raw_result <- GET(url = base_URL, path = kpi_path)
#'
#'   # print(raw_result)
#'   # print(fromJSON(rawToChar(raw_result$content)))
#' }
