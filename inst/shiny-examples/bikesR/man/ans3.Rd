\name{ans3}
\alias{ans3}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("ans3")}
\format{
  The format is:
List of 4
 $ model   :List of 18
  ..$ n        : int 7
  ..$ fitted   : num [1:7] 44556 44637 43738 41802 38774 ...
  ..$ residuals: Named num [1:7] 1066 -725 -651 -2091 -254 ...
  .. ..- attr(*, "names")= chr [1:7] "1" "2" "3" "4" ...
  ..$ enp      : num 2.95
  ..$ s        : num 3834
  ..$ one.delta: num 3.9
  ..$ two.delta: num 3.74
  ..$ trace.hat: num 3.03
  ..$ divisor  : num 1
  ..$ robust   : num [1:7] 1 1 1 1 1 1 1
  ..$ pars     :List of 10
  .. ..$ span       : num 5
  .. ..$ degree     : int 2
  .. ..$ normalize  : logi TRUE
  .. ..$ parametric : logi FALSE
  .. ..$ drop.square: logi FALSE
  .. ..$ surface    : chr "interpolate"
  .. ..$ cell       : num 0.2
  .. ..$ family     : chr "gaussian"
  .. ..$ trace.hat  : chr "exact"
  .. ..$ iterations : int 1
  ..$ kd       :List of 5
  .. ..$ parameter: Named int [1:7] 1 7 2 1 2 1049 849
  .. .. ..- attr(*, "names")= chr [1:7] "d" "n" "vc" "nc" ...
  .. ..$ a        : int 0
  .. ..$ xi       : num 0
  .. ..$ vert     : num [1:2] 1.5e+09 1.5e+09
  .. ..$ vval     : num [1:4] 4.45e+04 6.73e-03 2.90e+04 -6.99e-02
  ..$ call     : language loess(formula = NumberOfRentals ~ as.numeric(x), span = z$span)
  ..$ terms    :Classes 'terms', 'formula'  language NumberOfRentals ~ as.numeric(x)
  .. .. ..- attr(*, "variables")= language list(NumberOfRentals, as.numeric(x))
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "NumberOfRentals" "as.numeric(x)"
  .. .. .. .. ..$ : chr "as.numeric(x)"
  .. .. ..- attr(*, "term.labels")= chr "as.numeric(x)"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016ff9960> 
  .. .. ..- attr(*, "predvars")= language list(NumberOfRentals, as.numeric(x))
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "NumberOfRentals" "as.numeric(x)"
  ..$ xnames   : Named chr "as.numeric(x)"
  .. ..- attr(*, "names")= chr "as.numeric(x)"
  ..$ x        : num [1:7, 1] 1.5e+09 1.5e+09 1.5e+09 1.5e+09 1.5e+09 ...
  .. ..- attr(*, "dimnames")=List of 2
  .. .. ..$ : chr [1:7] "1" "2" "3" "4" ...
  .. .. ..$ : chr "as.numeric(x)"
  ..$ y        : Named num [1:7] 45622 43912 43087 39711 38520 ...
  .. ..- attr(*, "names")= chr [1:7] "1" "2" "3" "4" ...
  ..$ weights  : num [1:7] 1 1 1 1 1 1 1
  ..- attr(*, "class")= chr "loess"
 $ data    :Classes 'tbl_df', 'tbl' and 'data.frame':	7 obs. of  2 variables:
  ..$ x              : POSIXct[1:7], format: "2017-07-05" "2017-07-06" "2017-07-07" ...
  ..$ NumberOfRentals: int [1:7] 45622 43912 43087 39711 38520 40813 25725
 $ dat_type: chr "dailyRentals"
 $ fit_type: chr "loess"
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
data(ans3)
## maybe str(ans3) ; plot(ans3) ...
}
\keyword{datasets}
