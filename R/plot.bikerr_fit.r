#' Plot climr output
#'
#' @param x Output from the \code{\link{fit}} function
#' @param time_grid An optional time grid over which to produce fitted values of the model
#' @param ... Other arguments to plot (not currently implemented)
#'
#' @return Nothing: just a nice plot
#' @seealso \code{\link{load_clim}}, \code{\link{fit}}
#' @export
#' @import ggplot2
#' @importFrom tibble "tibble"
#' @importFrom viridis "scale_color_viridis"
#'
#' @examples
#' ans1 = load_clim('SH')
#' ans2 = fit(ans1)
#' plot(ans2)
#' ans3 = fit(ans1, data_type = 'monthly', fit_type = 'smooth.spline')
#' plot(ans3)
#' ans4 = fit(ans1, data_type = 'quarterly', fit_type = 'loess')
#' plot(ans4)
plot.biker_fit = function(x, time_grid = pretty(x$data$x, n = 100), ...) {
require(viridis)
  # for loess
  # length(x$data$x)

  # Create global variables to avoid annoying CRAN notes
  DJF = Dec = `J-D` = Jan = SON = Year = month = pred = quarter = temp = year = NULL

  # Create a nice plot from the output of fit.climr

  # Get the data set out
  df = x$data

  # time_grid = pretty(as.POSIXct(as.character(x$data$RentalDate)), n = 100)
  # time_grid = pretty(as.POSIXct(x$data$RentalDate), n = length(x$data$RentalDate))


  # Get some predicted values based on the time_grid
  fits = if(x$fit_type == 'somethingelse') {
    tibble(time_grid, pred = predict(x$model, newdata = tibble(as.numeric(time_grid))))
  } else if(x$fit_type == 'loess') {
    tibble(time_grid, pred = predict(x$model, newdata = tibble(x = as.numeric(time_grid)))) %>% na.omit()
  } else if(x$fit_type == 'smooth.spline') {
    tibble(time_grid, pred = predict(x$model, tibble(as.numeric(time_grid)))$y[,1])
  } else if(x$fit_type == 'gam') {
    tibble(time_grid, pred = predict(x$model, tibble(as.numeric(time_grid)))$y[,1])
  }

  #todo: @ source
  # df$x = as.POSIXct(df$x)

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


}
