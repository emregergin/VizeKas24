current_dir <- getwd()
print(current_dir)
relative_path <- file.path(current_dir, "Vize_Q1_200401015_emre_gergin.R")

source(relative_path)

test_that("spotify_token adlı bir değişken var", {
  expect_true(exists("spotify_token"))
})

# Test 1.2: spotify_token adlı değişkenin tipi “function” olmalı.
test_that("spotify_token adlı değişkenin tipi 'function'", {
  expect_true(is.function(spotify_token))
})
#1.3

test_that("spotify_token returns a list", {
  result <- spotify_token()
  expect_is(result, "list")
})

#1.4

test_that("spotify_token returns a list with two elements", {
  result <- spotify_token()
  expect_is(result, "list")
  expect_equal(length(result), 2)
})

#1.5

test_that("spotify_token returns a list with 'status_code' as the first element", {
  result <- spotify_token()
  expect_is(result, "list")
  expect_equal(names(result)[1], "status_code")
})

#1.6

test_that("spotify_token returns a list with 'status_code' as the first element and it's numeric", {
  result <- spotify_token()
  expect_is(result, "list")
  expect_equal(names(result)[1], "status_code")
  expect_is(result$status_code, "integer")
})

#1.7 

test_that("spotify_token returns a list with 'status_code' as the first element and it's equal to 200", {
  result <- spotify_token()
  expect_is(result, "list")
  expect_equal(names(result)[1], "status_code")
  expect_equal(result$status_code, 200)
})

#1.8

test_that("spotify_token returns a list with 'token' as the second element", {
  result <- spotify_token()
  expect_is(result, "list")
  expect_equal(names(result)[2], "token")
})

#1.9

test_that("spotify_token returns a list with 'token' as the second element and it's a character", {
  result <- spotify_token()
  expect_is(result, "list")
  expect_equal(names(result)[2], "token")
  expect_is(result$token, "character")
})

#1.10

test_that("spotify_token returns a list with 'token' as the second element and it starts with 'Bearer '", {
  result <- spotify_token()
  expect_is(result, "list")
  expect_equal(names(result)[2], "token")
  expect_true(startsWith(result$token, "Bearer "))
})

#1.11

testthat::test_that("spotify_token() çağrıldığında döndürdüğü listenin ikinci elementi character değişkeninin içinde 122 adet harf bulunmalı", {
  result <- spotify_token()
  expect_length(strsplit(result$token, "")[[1]], 122)
})