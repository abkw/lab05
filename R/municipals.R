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
                        ou_data = "data.frame",
                        kpi_data = "data.frame",
                        selected_municipal = "character"
                      ),

                      methods = list(

                        initialize = function() {
                          base_URL <<- "http://api.kolada.se/"
                          selected_municipal <<- ""
                          all_data <<- as.data.frame(getAllData())
                          ou_data <<- as.data.frame(getOuData())
                          kpi_data <<- as.data.frame(getKpiData())
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
                        getOuData = function() {
                          search_string <- ""
                          search_path <- paste("v2/ou",search_string, sep = "")
                          raw_result <- GET(url = base_URL, path = search_path)
                          ids <- fromJSON(rawToChar((raw_result$content)))$value$id
                          municipality <- fromJSON(rawToChar((raw_result$content)))$value$municipality
                          titles <- fromJSON(rawToChar((raw_result$content)))$value$title
                          ou_df <- data.frame("id" = ids, "municipality" = municipality, "title" = titles)
                          return(ou_df)
                        },
                        getKpiData = function() {
                          search_string <- ""
                          search_path <- paste("v2/kpi",search_string, sep = "")
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
                          kpi_df <- data.frame("id" = id,
                                               "is_divided_by_gender" = is_divided_by_gender,
                                               "title" = title,
                                               "municipality_type" = municipality_type,
                                               "operating_area" = operating_area,
                                               "ou_publication_date" = ou_publication_date,
                                               "perspective" = perspective,
                                               "prel_publication_date" = prel_publication_date,
                                               "publ_period" = publ_period,
                                               "publication_date" = publication_date
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
                          return(fromJSON(rawToChar((raw_result$content)))$value)
                        },
                        createShiny = function(){
                        # df <- data.frame(matrix(unlist(all_municipal_data), nrow=length(all_municipal_data),byrow=T,stringsAsFactors=FALSE))
                         df <- as.data.frame(all_municipal_data)
                         ou_df <- as.data.frame(ou_data)

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
                                            choices = as.character(df[,"title"])),
                                selectInput(inputId = "dataseet",
                                            label = "Select Data:",
                                            choices = c("one","two","three")),
                                dateInput(inputId = "askDate",
                                            label = "Here we ask for changes made to data in this request from 2015-02-28.",
                                            format = "yyyy-mm-dd")

                              ),


                              # Main panel for displaying outputs ----
                              mainPanel(
                                h3(textOutput("caption", container = span)),
                                # Output: Verbatim text for data summary ----
                                verbatimTextOutput("summary"),

                                # Output: HTML table with requested number of observations ----
                                tableOutput("view")

                              )
                            )
                          )

                          # Define server logic to summarize and view selected dataset ----
                      server = function(input, output, session) {
                        observe({
                          if ("municipality" %in% input$categoryId)
                            selectedChoices <- as.character(df[,"title"])
                            else if("ou" %in% input$categoryId)
                              selectedChoices <- as.character(ou_df[,"title"])
                            else
                            selectedChoices <- as.character(kpi_data[,"title"])
                            updateSelectInput(session,"dataId",choices =  selectedChoices)

                        })
                        datasetInput <- reactive({
                          switch(input$dataId,
                                 "categoryId" = categoryId,
                                 "ou" = ou,
                                 "kpi" = kpi)
                        })

                        output$view <- renderText({
                          paste("You have selected: \n", getMunicipalData(input$dataId))
                        })
                      }

                          # Create Shiny app ----
                          shinyApp(ui = ui, server = server)
                        }

                      )
)

item <- municipals$new()
item$all_municipal_data["title"]
item$getMunicipalData("Ale")
item$getKpiData()
item$createShiny()

