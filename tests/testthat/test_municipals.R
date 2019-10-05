context("municipals")

# Load Libraries

library(httr)
library(jsonlite)
library(utils)
library(methods)
library(shiny)

municipal_data <- municipals$new()

test_that("Check API URLs", {
  expect_true(class(municipal_data$base_URL) == "character")
  expect_true(municipal_data$base_URL == "http://api.kolada.se/")
})

test_that("Rejects errounous input", {
  expect_error(municipal_data$getOuData(56,"test"))
  expect_error(municipal_data$getOuData(56,448))
  expect_error(municipal_data$getOuData("56",448))

  expect_error(municipal_data$getKpiData(35))

  expect_error(municipal_data$getMunicipalData(58))
})


test_that("Class is correct", {
  expect_true(class(municipal_data)[1] == "municipals")
})

test_that("Output structure of Class parameters", {
  expect_true(class(municipal_data$all_data) == "data.frame")
  expect_true(class(municipal_data$all_council_data) == "data.frame")
  expect_true(class(municipal_data$all_municipal_data) == "data.frame")

  expect_true(class(municipal_data$getOuData("0018","test")) == "data.frame")
  expect_true(class(municipal_data$getKpiData("skola")) == "data.frame")
  expect_true(class(municipal_data$getMunicipalData("skola")) == "data.frame")
})

test_that("Test Output values", {
  expect_equal(names(municipal_data$all_data), c("id", "title", "type"))
  expect_equal(names(municipal_data$all_council_data), c("id", "title", "type"))
  expect_equal(names(municipal_data$all_municipal_data), c("id", "title", "type"))
})

