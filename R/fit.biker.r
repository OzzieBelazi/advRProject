#' Plot biker output
#'
#' @param obj An object of class \code{biker} from \code{\link{loadBikes}}
#' @param data_type The type of data to be analysed, either yearly, quarterly, or monthly
#' @param fit_type The type of model required, either linear regression (\code{lm}), loess, or smoothing spline (\code{smooth.spline})
#'
#' @return A list of \code{\link[tibble]{tibble}}s which contains hourly, a, and b values for each time series respectively.
#' @export
#' @import tidyr
#' @import stats
#' @import mgcv
#' @import dplyr
#' @import tidyr
#' @import stringr
#' @import magrittr
#'
#'
#' @seealso \code{\link{loadBikes}}, \code{\link{plot.biker_fit}}
#' @examples
#' ans1 = loadBikes('26Jul2017-31Jul2017')
#' ans2 = fit(ans1)
#' ans3 = fit(ans1, data_type = 'hourlyRentals', fit_type = 'smooth.spline')
#' ans4 = fit(ans1, data_type = 'hourlyRentals', fit_type = 'loess')

# todo: consider extra param for smoothing
fit.biker <- function(obj,
                data_type = c('data', 'hourlyRentals', 'stationJourneys', "bikeUsage"),
                fit_type = c('gam', 'k.smooth', 'loess', 'smooth.spline')) {

  # Create global variables to avoid annoying CRAN notes
  NumberOfRentals = x = NULL

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
