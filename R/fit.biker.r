#' Fit biker output
#'
#' @param obj An object of class \code{biker} from \code{\link{loadBikes}}
#' @param data_type The type of data to be analysed, either yearly, quarterly, or monthly
#' @param fit_type The type of model required, either gam (\code{gam}), loess (\code{loess}), or smoothing spline (\code{smooth.spline})
#' @param ...
#' @return A list of \code{\link[tibble]{tibble}}s which contains hourly, a, and b values for each time series respectively.
#' @export
#' @importFrom  stats "ksmooth" "smooth.spline" "loess" "lm" "predict"
#' @import mgcv
#' @import dplyr
#' @import stringr
#' @importFrom forecast "auto.arima"
#' @importFrom magrittr "extract2" "%$%"
#'
#'
#' @seealso \code{\link{loadBikes}}, \code{\link{plot}}
#' @export
#'
#' @examples
#' ans1 = loadBikes('26Jul2017-31Jul2017')
#' ans2 = fit(ans1, data_type = 'hourlyRentals', fit_type = 'smooth.spline')
#' ans3 = fit(ans1, data_type = 'dailyRentals', fit_type = 'loess')
fit = function(obj,
               data_type = c('hourlyRentals','dailyRentals' , 'stationJourneys', 'bikeUsage'),
               fit_type = c('arima', 'loess', 'smooth.spline'), ...) {
  UseMethod('fit')
}
# todo: consider extra param for smoothing
#' @export
fit.biker <- function(obj,
                data_type = c('data', 'hourlyRentals', 'dailyRentals', 'stationJourneys', "bikeUsage"),
                fit_type = c('arima','loess', 'smooth.spline'), ...) {

  # Create global variables to avoid annoying CRAN notes
  NumberOfRentals = x = NULL
  # store all optional arguments in a list
  z <- list(...)

  # Find out which data set to use
  fit_dat = match.arg(data_type)
  # Find what type of fitting method
  fit_arg = match.arg(fit_type)

  # Find out which bit of the data to take
  dat_choose = switch(fit_dat,
                      out = 1,
                      hourlyRentals = 2,
                      dailyRentals = 3,
                      stationJourneys = 4,
                      bikeUsage = 5)

  # Get the data set to use
  curr_dat = obj %>% extract2(dat_choose)



  if(!is.null(z$spar) | !is.null(z$span) | !is.null(z$bandwidth) | !is.null(z$lambda)){
    # Fit some models
    if(fit_arg == 'loess') {
      mod = curr_dat %$% loess(NumberOfRentals ~ as.numeric(x), span = z$span)
    } else if(fit_arg == 'smooth.spline') {
      mod = curr_dat %$% smooth.spline(x, NumberOfRentals, spar = z$spar)
    # } else if(fit_arg == 'ksmooth') {
    #   mod = curr_dat %$% ksmooth(as.POSIXct(x), NumberOfRentals, kernel = "normal", bandwidth = z$bandwidth)
    } else if(fit_arg == 'arima') {
      mod = auto.arima(curr_dat$NumberOfRentals)
    }
  }else{
    # Fit some models
   if(fit_arg == 'loess') {
      mod = curr_dat %$% loess(NumberOfRentals ~ as.numeric(x))
    } else if(fit_arg == 'smooth.spline') {
      mod = curr_dat %$% smooth.spline(x, NumberOfRentals)
    # } else if(fit_arg == 'ksmooth') {
    #   mod = curr_dat %$% ksmooth(as.POSIXct(x), NumberOfRentals, kernel = "normal")
    } else if(fit_arg == 'arima') {
      mod = auto.arima(curr_dat$NumberOfRentals)
    }
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
