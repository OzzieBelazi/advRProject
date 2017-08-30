\name{ans4}
\alias{ans4}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("ans4")}
\format{
  The format is:
List of 4
 $ model   :List of 18
  ..$ coef     : Named num [1:6] 1.62618 -0.70966 -0.46891 -0.46341 -0.00334 ...
  .. ..- attr(*, "names")= chr [1:6] "ar1" "ar2" "ma1" "ma2" ...
  ..$ sigma2   : num 494781
  ..$ var.coef : num [1:6, 1:6] 1.45e-02 -1.42e-02 -1.82e-02 -6.61e-05 1.84e-02 ...
  .. ..- attr(*, "dimnames")=List of 2
  .. .. ..$ : chr [1:6] "ar1" "ar2" "ma1" "ma2" ...
  .. .. ..$ : chr [1:6] "ar1" "ar2" "ma1" "ma2" ...
  ..$ mask     : logi [1:6] TRUE TRUE TRUE TRUE TRUE TRUE
  ..$ loglik   : num -1338
  ..$ aic      : num 2690
  ..$ arma     : int [1:7] 2 3 0 0 1 0 0
  ..$ residuals: Time-Series [1:168] from 1 to 168: -701 -344 -474 -389 -510 ...
  ..$ call     : language auto.arima(y = curr_dat$NumberOfRentals, x = list(x = c(323L, 195L, 87L, 86L, 75L, 189L, 922L,  2922L, 5137L, 262| __truncated__ ...
  ..$ series   : chr "curr_dat$NumberOfRentals"
  ..$ code     : int 0
  ..$ n.cond   : int 0
  ..$ nobs     : int 168
  ..$ model    :List of 10
  .. ..$ phi  : num [1:2] 1.63 -0.71
  .. ..$ theta: num [1:3] -0.46891 -0.46341 -0.00334
  .. ..$ Delta: num(0) 
  .. ..$ Z    : num [1:4] 1 0 0 0
  .. ..$ a    : num [1:4] -1610.14 1721.69 320.16 2.29
  .. ..$ P    : num [1:4, 1:4] 0 0 0 0 0 ...
  .. ..$ T    : num [1:4, 1:4] 1.63 -0.71 0 0 1 ...
  .. ..$ V    : num [1:4, 1:4] 1 -0.46891 -0.46341 -0.00334 -0.46891 ...
  .. ..$ h    : num 0
  .. ..$ Pn   : num [1:4, 1:4] 1 -0.46891 -0.46341 -0.00334 -0.46891 ...
  ..$ bic      : num 2712
  ..$ aicc     : num 2690
  ..$ x        : Time-Series [1:168] from 1 to 168: 323 195 87 86 75 189 922 2922 5137 2629 ...
  ..$ fitted   : Time-Series [1:168] from 1 to 168: 1024 539 561 475 585 ...
  ..- attr(*, "class")= chr [1:2] "ARIMA" "Arima"
 $ data    :Classes 'tbl_df', 'tbl' and 'data.frame':	168 obs. of  2 variables:
  ..$ x              : POSIXct[1:168], format: "2017-07-05 00:00:00" "2017-07-05 01:00:00" "2017-07-05 02:00:00" ...
  ..$ NumberOfRentals: int [1:168] 323 195 87 86 75 189 922 2922 5137 2629 ...
 $ dat_type: chr "hourlyRentals"
 $ fit_type: chr "arima"
 - attr(*, "class")= chr "biker_fit"
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(ans4)
## maybe str(ans4) ; plot(ans4) ...
}
\keyword{datasets}
