\name{CheckForModelListUpdates}
\alias{CheckForModelListUpdates}
\title{
Compare the model product list in rNOMADS with model products on NOMADS server
}
\description{
Check the real time and archive NOMADS available model products against the output of \code{\link{NOMADSRealTimeList}} and \code{\link{NOMADSArchiveList}}.
}
\usage{
CheckForModelListUpdates(verbose = TRUE)
}
\arguments{
  \item{verbose}{
     Display test results
}
}

\value{
    \item{model.list.check$rnomads.ncep.diff}{Models in rNOMADS but not on the real time NOMADS server}
    \item{model.list.check$ncep.rnomads.diff}{Models on real time server but not in rNOMADS}
    \item{model.list.check$rnomads.ncdc.diff}{Models in rNOMADS but not on the archive server}
    \item{model.list.check$ncdc.rnomads.diff}{Models on archive server but not in rNOMADS}
}
\references{
\url{http://nomads.ncep.noaa.gov/}
\url{http://nomads.ncdc.noaa.gov/}
}
\author{
Daniel C. Bowman \email{daniel.bowman@unc.edu}
}
\note{
I use this function to periodically check the NOMADS server against the list of model products supported by rNOMADS.
I issue periodic updates to \code{rNOMADS} when new products appear on the NOMADS server.
This function will display mismatches due to some simplifications I've made (for example, there is no GRIB interface for ``rtofs'' but since I use the same abbreviation list for GRIB and DODS, \code{CheckForModelListUpdates} will flag it).
However, if it appears that there is a model product on NOMADS that \code{rNOMADS} does not currently support, email me and I can add it.
}
\seealso{
\code{\link{NOMADSRealTimeList}}, \code{\link{NOMADSArchiveList}}
}
\examples{
\dontrun{CheckForModelListUpdates(TRUE)}
}
\keyword{ utilities }
