#' Plot biker output
#'
#' @param x Output from the \code{\link{fit}} function
#' @param time_grid An optional time grid over which to produce fitted values of the model
#' @param ... Other arguments to plot (not currently implemented)
#'
#' @return Nothing: just a nice plot
#' @seealso \code{\link{loadBikes}}, \code{\link{fit}}
#' @export
#' @import ggplot2
#' @import ggmap
#' @importFrom forecast "forecast" "geom_forecast"
#' @importFrom tibble "tibble"
#' @importFrom stats "na.omit"
#' @importFrom viridis "scale_color_viridis"
#'
#' @examples
#' ans1 = loadBikes('26Jul2017-31Jul2017')
#' ans2 = fit(ans1, data_type = 'hourlyRentals', fit_type = 'smooth.spline')
#' plot(ans2)
#' ans3 = fit(ans1, data_type = 'hourlyRentals', fit_type = 'loess')
#' plot(ans3)
#' ans4 = fit(ans1, data_type = 'dailyRentals', fit_type = 'loess')
#' plot(ans4)
#'
plot = function(x,
               time_grid, ...) {
  UseMethod('plot')
}
#' @export
plot.biker_fit = function(x, time_grid = pretty(x$data$x, n = 100), ...) {

  # Create global variables to avoid annoying CRAN notes
  pred = NumberOfRentals = BikeId = na.omit = NULL

  # Create a nice plot from the output of fit.climr

  # Get the data set out
  df = x$data


  # Get some predicted values based on the time_grid
  fits = if(x$fit_type == 'somethingelse') {
    tibble(time_grid, pred = predict(x$model, newdata = tibble(as.numeric(time_grid))))
  } else if(x$fit_type == 'loess') {
    tibble(time_grid, pred = predict(x$model, newdata = tibble(x = as.numeric(time_grid)))) %>% na.omit()
  } else if(x$fit_type == 'smooth.spline') {
    tibble(time_grid, pred = predict(x$model, tibble(as.numeric(time_grid)))$y[,1])
  } else if(x$fit_type == 'arima') {
    forecast(x$model, h = 20)
    # tibble(time_grid, pred = predict(x$model, tibble(as.numeric(time_grid))))
    # tibble(time_grid, pred = predict(x$model,type = "response"))
  }

  #todo: @ source
  # df$x = as.POSIXct(df$x)
  if(x$fit_type != 'arima'){
  # Finally create the plot
  ggplot(df, aes(y=NumberOfRentals, x=x)) +
    geom_point() +
    theme_bw() +
    xlab('Hourly') +
    scale_x_datetime(date_labels = '%Y-%m-%d %H', date_breaks = '3 hour', date_minor_breaks = "1 hour") +
    ylab('Number of bike rentals') +
    geom_line(data = fits, aes(x = time_grid, y = pred, colour = pred)) +
    theme(legend.position = 'None', axis.text.x = element_text(angle=90, hjust = 1)) +
    scale_color_viridis()
  }else
    autoplot(fits) +
    theme_bw() +
    xlab('Hourly') +
    ylab('Number of bike rentals') + geom_forecast(h=36)

}
