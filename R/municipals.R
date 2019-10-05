library(httr)
library(jsonlite)
library(utils)
library(methods)
library(shiny)

#' municipals RC class
#' @description This is a RC class for handle municipal data
#'
#' @field base_URL character.
#' @field all_data data.frame.
#' @field all_council_data data.frame.
#' @field all_municipal_data data.frame.
#' @field selected_municipal character.
#'
#' @import httr utils methods shiny
#' @importFrom jsonlite fromJSON
#' @return information about municipilities
#' @export
#'
municipals <- setRefClass(Class =  "municipals",

                          fields = list(
                            base_URL = "character",
                            all_data = "data.frame",
                            all_council_data = "data.frame",
                            all_municipal_data = "data.frame",
                            selected_municipal = "character",
                            selectedValue = "data.frame"
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

                            getOuData = function(municipalId = as.character, search_text = as.character) {

                              search_string <- URLencode(search_text, reserved = TRUE)
                              search_path <- paste("v2/ou?municipality=",municipalId, "&title=", search_string, sep = "")
                              raw_result <- GET(url = base_URL, path = search_path)

                              ids <- fromJSON(rawToChar((raw_result$content)))$value$id
                              titles <- fromJSON(rawToChar((raw_result$content)))$value$title

                              ou_df <- data.frame("ID" = ids, "Institue" = titles)
                              return(ou_df)
                            },

                            getKpiData = function(search_text = as.character) {

                              search_string <- URLencode(search_text, reserved = TRUE)
                              search_path <- paste("v2/kpi?title=",search_string, sep = "")
                              raw_result <- GET(url = base_URL, path = search_path)

                              auspices <- fromJSON(rawToChar((raw_result$content)))$value$auspices
                              description <- fromJSON(rawToChar((raw_result$content)))$value$description
                              has_ou_data <- fromJSON(rawToChar((raw_result$content)))$value$has_ou_data
                              id <- fromJSON(rawToChar((raw_result$content)))$value$id
                              is_divided_by_gender <- fromJSON(rawToChar((raw_result$content)))$value$is_divided_by_gender
                              municipality_type <- fromJSON(rawToChar((raw_result$content)))$value$municipality_type
                              operating_area <- fromJSON(rawToChar((raw_result$content)))$value$operating_area
                              ou_publication_date <- fromJSON(rawToChar((raw_result$content)))$value$ou_publication_date
                              perspective <- fromJSON(rawToChar((raw_result$content)))$value$perspective
                              prel_publication_date <- fromJSON(rawToChar((raw_result$content)))$value$prel_publication_date
                              publ_period <- fromJSON(rawToChar((raw_result$content)))$value$publ_period
                              publication_date <- fromJSON(rawToChar((raw_result$content)))$value$publication_date
                              title <- fromJSON(rawToChar((raw_result$content)))$value$title

                              kpi_df <- data.frame("ID" = id,
                                                   "Divided_By_Gender" = is_divided_by_gender,
                                                   "Title" = title,
                                                   "Type" = municipality_type,
                                                   "Area" = operating_area,
                                                   "OU_Publication_Date" = ou_publication_date,
                                                   "Perspective" = perspective,
                                                   "Prel_Publication_Date" = prel_publication_date,
                                                   "Period" = publ_period,
                                                   "Publication_Date" = publication_date
                              )
                              return(kpi_df)
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

                              ids <- fromJSON(rawToChar((raw_result$content)))$value$id
                              titles <- fromJSON(rawToChar((raw_result$content)))$value$title

                              selected_municpal_dataframe <- data.frame("ID" = ids, "Name" = titles)
                              return(selected_municpal_dataframe)
                            },

                            createShiny = function() {

                              municipal_data <- c(as.vector(all_municipal_data[,"id"]))
                              names(municipal_data) <- c(as.vector(all_municipal_data[,"title"]))

                              ui <- fluidPage(

                                titlePanel("Kolanda API Connect"),

                                sidebarLayout(

                                  sidebarPanel(

                                    ###### UI for Select API ######

                                    radioButtons("radioMain",
                                                 label = h3("Data Type"),
                                                 choices = c("Search All Municipals" = "municipality", "Search All Institues" = "kpi", "Search Institues within Municipals" = "ou"),
                                                 selected = "municipality"),

                                    ###### UI elements for show municipals ######

                                    conditionalPanel(
                                      condition = "input.radioMain == 'municipality'",
                                      textInput("municipal_search",
                                                label = h3("Search Municipals by Name"),
                                                value = "Link")
                                    ),

                                    ###### UI elements for find instituts ######

                                    conditionalPanel(
                                      condition = "input.radioMain == 'kpi'",
                                      textInput("institute_search",
                                                label = h3("Search Institues by Title"),
                                                value = "skola")
                                    ),

                                    ###### UI elements for find instituts within municipals ######

                                    conditionalPanel(
                                      condition = "input.radioMain == 'ou'",
                                      selectInput("selectMunicipal",
                                                  label = h3("Select Municipality"),
                                                  choices = municipal_data,
                                                  selected = municipal_data[1]),
                                      textInput("institute_search_municipals",
                                                label = h3("Search Institues by Title"),
                                                value = "skola")
                                    )
                                  ),

                                  mainPanel(

                                    ###### UI elements for show municipal table ######

                                    conditionalPanel(
                                      condition = "input.radioMain == 'municipality'",
                                      verbatimTextOutput("show_municipal_title"),
                                      tableOutput("municipal_table_data")
                                    ),

                                    ###### UI elements for show institues table ######

                                    conditionalPanel(
                                      condition = "input.radioMain == 'kpi'",
                                      verbatimTextOutput("show_institue_title"),
                                      dataTableOutput("institute_table_data")
                                    ),

                                    ###### UI elements for show institues within Municipals ######

                                    conditionalPanel(
                                      condition = "input.radioMain == 'ou'",
                                      verbatimTextOutput("show_institue_within_municipals"),
                                      dataTableOutput("institute_table_data_for_municipals")
                                    )
                                  )

                                )
                              )

                              server <- function(input, output) {
                                ###### Server functions for Show Municipal Data ######

                                output$show_municipal_title <- renderText({
                                  paste("Search Results for - " , "'", input$municipal_search, "'")
                                })

                                output$municipal_table_data <- renderTable({
                                  getMunicipalData(input$municipal_search)
                                })

                                ###### Server functions for Show Institut Data ######

                                output$show_institue_title <- renderText({
                                  paste("Search Results for - " , "'", input$institute_search, "'")
                                })

                                output$institute_table_data <- renderDataTable({
                                  getKpiData(input$institute_search)
                                })

                                ###### Server functions for Show Institut Data within Municipals ######

                                output$show_institue_within_municipals <- renderText({
                                  paste("Search Results for - " , "'", input$institute_search_municipals, "'", "within", names(municipal_data[municipal_data == input$selectMunicipal]))
                                })

                                output$institute_table_data_for_municipals <- renderDataTable({
                                  getOuData(input$selectMunicipal, input$institute_search_municipals)
                                })

                              }

                              shinyApp(ui = ui, server = server)
                            }

                          )
)

item <- municipals$new()
#item$all_municipal_data["title"]
#item$getMunicipalData("Ale")
#item$getKpiData()
item$createShiny()
# item$createMyShiny()
#print(item$selectedValue)

