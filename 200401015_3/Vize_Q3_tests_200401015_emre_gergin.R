library(testthat)

# Test 3.1
test_that("Global Workspace’de my_artists adlı bir değişken olmalı", {
  expect_true(exists("my_artists"))
})

# Test 3.2
test_that("my_artists adlı değişkenin class’ı 'data.frame' olmalı", {
  expect_equal(class(my_artists), "data.frame")
})

# Test 3.3
test_that("my_artists adlı değişken 2 sütun bulundurmalı", {
  expect_equal(ncol(my_artists), 2)
})

# Test 3.4
test_that("my_artists adlı değişken sütun isimleri c('artist', 'id') olmalı", {
  expect_equal(colnames(my_artists), c("artist", "id"))
})
