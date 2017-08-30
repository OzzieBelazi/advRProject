\name{ans2}
\alias{ans2}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("ans2")}
\format{
  The format is:
List of 4
 $ model   :List of 19
  ..$ x         : num [1:168] 1.5e+09 1.5e+09 1.5e+09 1.5e+09 1.5e+09 ...
  ..$ y         : num [1:168] -173.3 46.7 279.6 539.4 836.8 ...
  ..$ w         : num [1:168] 1 1 1 1 1 1 1 1 1 1 ...
  ..$ yin       : num [1:168] 323 195 87 86 75 ...
  ..$ tol       : num 0.301
  ..$ data      :List of 3
  .. ..$ x: num [1:168] 1.5e+09 1.5e+09 1.5e+09 1.5e+09 1.5e+09 ...
  .. ..$ y: num [1:168] 323 195 87 86 75 ...
  .. ..$ w: num 1
  ..$ no.weights: logi TRUE
  ..$ lev       : num [1:168] 0.428 0.258 0.18 0.153 0.145 ...
  ..$ cv.crit   : num 913716
  ..$ pen.crit  : num 1.12e+08
  ..$ crit      : num 913716
  ..$ df        : num 24.3
  ..$ spar      : num 0.5
  ..$ ratio     : num 5.49e-07
  ..$ lambda    : num 8.78e-06
  ..$ iparms    : Named int [1:5] 1 1 500 0 NA
  .. ..- attr(*, "names")= chr [1:5] "icrit" "ispar" "iter" "" ...
  ..$ auxM      : NULL
  ..$ fit       :List of 5
  .. ..$ knot : num [1:92] 0 0 0 0 0.00599 ...
  .. ..$ nk   : num 88
  .. ..$ min  : num 1.5e+09
  .. ..$ range: num 601200
  .. ..$ coef : num [1:88] -173 -101 117 512 1150 ...
  .. ..- attr(*, "class")= chr "smooth.spline.fit"
  ..$ call      : language smooth.spline(x = x, y = NumberOfRentals, spar = z$spar)
  ..- attr(*, "class")= chr "smooth.spline"
 $ data    :Classes 'tbl_df', 'tbl' and 'data.frame':	168 obs. of  2 variables:
  ..$ x              : POSIXct[1:168], format: "2017-07-05 00:00:00" "2017-07-05 01:00:00" "2017-07-05 02:00:00" ...
  ..$ NumberOfRentals: int [1:168] 323 195 87 86 75 189 922 2922 5137 2629 ...
 $ dat_type: chr "hourlyRentals"
 $ fit_type: chr "smooth.spline"
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
data(ans2)
## maybe str(ans2) ; plot(ans2) ...
}
\keyword{datasets}
