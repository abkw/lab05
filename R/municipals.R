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
                        },
                        createShiny = function(){
                        # df <- data.frame(matrix(unlist(all_municipal_data), nrow=length(all_municipal_data),byrow=T,stringsAsFactors=FALSE))
                         df <- as.data.frame(all_municipal_data)
                      print(as.numeric(df[,"id"]))
                      print(as.character(df[,"title"]))

                          # print(df)
                          # print(filter(df),id=1)
                          library(shiny)
                      # library(tidyverse)

                          # Define UI for dataset viewer app ----
                          ui <- fluidPage(

                            # App title ----
                            titlePanel("Municipal Data"),

                            # Sidebar layout with a input and output definitions ----
                            sidebarLayout(

                              # Sidebar panel for inputs ----
                              sidebarPanel(

                                # Input: Selector for choosing dataset ----
                                # radioButtons("typeInput", "Select data you want to display",
                                #              choices = c("municipality", "kpi", "ou"),
                                #              selected = "municipality"),
                                selectInput(inputId = "categoryId",
                                            label = "Select a Category:",
                                            choices = c("municipality", "kpi", "ou")),
                                selectInput(inputId = "dataId",
                                            label = "Select a Municipal:",
                                            choices = as.character(df[,"title"])), tableOutput("data"),
                                selectInput(inputId = "dataset",
                                            label = "Select Data:",
                                            choices = c("rock", "pressure", "cars")),
                                # Input: Numeric entry for number of obs to view ----
                                numericInput(inputId = "obs",
                                             label = "Number of observations to view:",
                                             value = 10)
                              ),

                              # Main panel for displaying outputs ----
                              mainPanel(

                                # Output: Verbatim text for data summary ----
                                verbatimTextOutput("summary"),

                                # Output: HTML table with requested number of observations ----
                                tableOutput("view")

                              )
                            )
                          )

                          # Define server logic to summarize and view selected dataset ----
                          server <- function(input, output) {

                            # Return the requested dataset ----
                            datasetInput <- reactive({
                              switch(input$dataset,
                                     "rock" = rock,
                                     "pressure" = pressure,
                                     "cars" = cars)
                            })

                            # Generate a summary of the dataset ----
                            output$summary <- renderPrint({
                              dataset <- datasetInput()
                              summary(dataset)
                            }
                            )

                              mainPanel(
                                DT::dataTableOutput("table")
                              )
                            # Show the first "n" observations ----
                            # output$view <- renderTable({
                            #   head(datasetInput(), n = input$obs)
                            # })

                          }

                          # Create Shiny app ----
                          shinyApp(ui = ui, server = server)
                        }

                      )
)

item <- municipals()
item$all_municipal_data["title"]
item$getMunicipalData("link")
item$createShiny()
