\name{map}
\alias{map}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("map")}
\format{
  The format is:
 'ggmap' chr [1:1280, 1:1280] "#C8E3A1" "#C8E3A1" "#C8E3A1" "#CDCFCE" "#A3CCFF" "#A3CCFF" "#A3CCFF" ...
 - attr(*, "bb")='data.frame':	1 obs. of  4 variables:
  ..$ ll.lat: num 51.5
  ..$ ll.lon: num -0.155
  ..$ ur.lat: num 51.5
  ..$ ur.lon: num -0.1
 - attr(*, "source")= chr "google"
 - attr(*, "maptype")= chr "terrain"
 - attr(*, "zoom")= num 14
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
data(map)
## maybe str(map) ; plot(map) ...
}
\keyword{datasets}
