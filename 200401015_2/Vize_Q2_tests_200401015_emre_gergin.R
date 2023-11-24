library(testthat)

current_dir <- getwd()
print(current_dir)
relative_path <- file.path(current_dir, "Vize_Q1_200401015_emre_gergin.R")

source(relative_path)


# Test 2.1
test_that("Global Workspace should have 'spotify_search_artist' variable", {
  expect_true(exists("spotify_search_artist", envir = .GlobalEnv))
})

# Test 2.2
test_that("Type of 'spotify_search_artist' should be a function", {
  expect_equal(typeof(spotify_search_artist), "closure")
})

# Test 2.3
test_that("Calling spotify_search_artist with any artist name should return a list", {
  result <- spotify_search_artist("Shakira")
  expect_true(is.list(result))
})

# Test 2.4
test_that("Calling spotify_search_artist should return a list with two elements", {
  result <- spotify_search_artist("Shakira")
  expect_equal(length(result), 2)
})

# Test 2.5
test_that("First element name of the result should be 'status_code'", {
  result <- spotify_search_artist("Shakira")
  expect_equal(names(result)[1], "status_code")
})

# Test 2.6
test_that("Class of the first element should be numeric", {
  result <- spotify_search_artist("Shakira")
  expect_equal(class(result$status_code), "numeric")
})

# Test 2.7
test_that("Value of 'status_code' should be equal to 200", {
  result <- spotify_search_artist("Shakira")
  expect_equal(result$status_code, 200)
})

# Test 2.8
test_that("Second element name of the result should be 'search_results'", {
  result <- spotify_search_artist("Shakira")
  expect_equal(names(result)[2], "search_results")
})

# Test 2.9
test_that("Class of the second element should be data.frame", {
  result <- spotify_search_artist("Shakira")
  expect_true(is.data.frame(result$search_results))
})

# Test 2.10
test_that("Second element should have two columns", {
  result <- spotify_search_artist("Shakira")
  expect_equal(ncol(result$search_results), 2)
})

# Test 2.11
test_that("Column names of the second element should be c('artist', 'id')", {
  result <- spotify_search_artist("Shakira")
  expect_equal(colnames(result$search_results), c('artist', 'id'))
})

# Test 2.12
test_that("Value of 'id' column in the first row of the second element should be '22WZ7M8sxp5THdruNY3gXt'", {
  result <- spotify_search_artist("The Doors")
  expect_equal(result$search_results$id[1], "22WZ7M8sxp5THdruNY3gXt")
})

