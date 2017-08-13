library(bikesR)

context("checks that boadBikes loads ok")

test_that("loadBikes produces valid data", {
  expect_output(str(loadBikes("20Sep15-03Oct15")), 'List of 4')
  expect_error(loadBikes(type = "aaaa"))
  # expect_is(load_clim('NH'), 'climr')
  # Here's one that fails
  #expect_output(str(load_clim('SH')), 'List of 7')
})
#
#
# loadBikes(type = "aaaa")
# loadBikes("20Sep15-03Oct15")
