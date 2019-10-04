library(httr)
library(jsonlite)
library(utils)

#' Get Key figures
#'
#' @title getKeyFigures
#'
#' @description get data from the API.
#'
#' @param search_item search string
#'
#' @return Test getKeyFigures func
#'
#' @import httr jsonlite utils
#'
#'
#' @export
#'

getKeyFigures <- function(search_item) {
  if(typeof(search_item)!= "character") {
    stop("Invalid Input!")
  }

  base_URL <- "http://api.kolada.se/"
  search_string <- URLencode(search_item, reserved = TRUE)
  kpi_path <- paste("/v1/kpi/",search_string, sep = "")

  raw_result <- GET(url = base_URL, path = kpi_path)

  # print(raw_result)
  # print(fromJSON(rawToChar(raw_result$content)))
}
