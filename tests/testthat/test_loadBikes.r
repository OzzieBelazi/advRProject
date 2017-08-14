library(bikesR)

context("checks that loadBikes loads ok")

test_that("loadBikes produces valid data", {
  expect_output(str(loadBikes("20Sep15-03Oct15")), 'List of 7')
  expect_error(loadBikes(type = "invalid date"))
  expect_is(loadBikes("20Sep15-03Oct15"), 'biker')
})

