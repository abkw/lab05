library(httr)
library(jsonlite)
library(utils)
library(methods)

#' municipals RC class
#' @description This is a RC class for handle municipal data
#'
#' @field base_URL character.
#' @field all_data data.frame.
#' @field all_council_data data.frame.
#' @field all_municipal_data data.frame.
#' @field selected_municipal character.
#'
#' @import httr jsonlite utils methods
#' @return information about municipilities
#' @export
#'
municipals <- setRefClass(Class =  "municipals",

                      fields = list(
                        base_URL = "character",
                        all_data = "data.frame",
                        all_council_data = "data.frame",
                        all_municipal_data = "data.frame",
                        selected_municipal = "character"
                      ),

                      methods = list(

                        initialize = function() {
                          base_URL <<- "http://api.kolada.se/"
                          selected_municipal <<- ""
                          all_data <<- as.data.frame(getAllData())
                          all_council_data <<- as.data.frame(getAllCouncilData())
                          all_municipal_data <<- as.data.frame(getAllMunicipalData())
                        },

                        getAllData = function() {
                          search_string <- ""
                          search_path <- paste("v2/municipality",search_string, sep = "")
                          raw_result <- GET(url = base_URL, path = search_path)
                          ids <- fromJSON(rawToChar((raw_result$content)))$value$id
                          titles <- fromJSON(rawToChar((raw_result$content)))$value$title
                          types <- fromJSON(rawToChar((raw_result$content)))$value$type
                          municpal_dataframe <- data.frame("id" = ids, "title" = titles, "type" = types)
                          return(municpal_dataframe)
                        },

                        getAllCouncilData = function() {
                          council_data <- all_data[which(all_data$type == "L"),]
                          return(council_data)
                        },

                        getAllMunicipalData = function () {
                          municipal_data <- all_data[which(all_data$type == "K"),]
                          return(municipal_data)
                        },

                        getMunicipalData = function(search_name = as.character) {
                          search_string <- URLencode(search_name, reserved = TRUE)
                          search_path <- paste("v2/municipality?title=",search_string, sep = "")
                          raw_result <- GET(url = base_URL, path = search_path)
                          return(fromJSON(rawToChar((raw_result$content)))$value)
                        }

                      )
)

# item <- municipals()
# item$all_municipal_data
# item$getMunicipalData("link")
