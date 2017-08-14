library(bikesR)

context("checks that fit loads ok")

test_that("fit produces valid data", {
  ans1 = loadBikes("20Sep15-03Oct15")
  expect_output(str(fit(ans1, data_type = 'hourlyRentals', fit_type = 'smooth.spline')), 'List of 4')
  expect_error(fit(ans1, data_type = 'invalid data type', fit_type = 'loess'))
  expect_error(fit(ans1, data_type = 'hourlyRentals', fit_type = 'invalid fit type'))
  expect_is(fit(ans1, data_type = 'hourlyRentals', fit_type = 'smooth.spline'), 'biker_fit')

})
