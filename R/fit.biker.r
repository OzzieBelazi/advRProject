# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


# # Put it all in a list and return
# out_list = list(out = out,
#                 hourlyRentals = out_HourlyRentals,
#                 stationJourneys = out_StationJourneys,
#                 bikeUsage = out_BikeUsage,
#                 type = arg)


# todo: consider extra param for smoothing
fit <- function(obj,
                data_type = c('data', 'hourlyRentals', 'stationJourneys', "bikeUsage"),
                fit_type = c('gam', 'k.smooth', 'loess', 'smooth.spline')) {
  # Find out which data set to use
  fit_dat = match.arg(data_type)
  # Find what type of fitting method
  fit_arg = match.arg(fit_type)

  # Find out which bit of the data to take
  dat_choose = switch(fit_dat,
                      out = 1,
                      hourlyRentals = 2,
                      stationJourneys = 3,
                      bikeUsage = 4)

  # Get the data set to use
  curr_dat = obj %>% extract2(dat_choose)

  # Fit some models
  if(fit_arg == 'lm') {
    mod = curr_dat %$% lm(NumberOfRentals ~ x)
  } else if(fit_arg == 'loess') {
    mod = curr_dat %$% loess(NumberOfRentals ~ as.numeric(x))
  } else if(fit_arg == 'smooth.spline') {
    mod = curr_dat %$% smooth.spline(x, NumberOfRentals)
  } else if(fit_arg == 'k.smooth') {
    mod = curr_dat %$% ksmooth(as.POSIXct(x), NumberOfRentals, kernel = "normal")
  } else if(fit_arg == 'gam') {
    mod = curr_dat %$% smooth.spline(x, NumberOfRentals)
  }
  print(mod)

  # Output so it can be plotted
  out = list(model = mod,
             data = curr_dat,
             dat_type = fit_dat,
             fit_type = fit_arg)
  class(out) = 'biker_fit'

  invisible(out)



}
