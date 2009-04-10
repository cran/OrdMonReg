\name{mechIng}
\alias{mechIng}
\docType{data}
\title{Mechanical engineering dataset used to illustrate ordered isotonic regression}
\description{Dataset that contains the data analyzed in Balabdaoui et al. (2009).}
\usage{data(mechIng)}
\format{
  A data frame with 1495 observations on the following 3 variables.
  \describe{
    \item{\code{x}}{Location of measurements.}
    \item{\code{g1}}{Measurements of the upper isotonic curve.}
    \item{\code{g2}}{Measurements of the lower isotonic curve.}
  }
}
\details{In Balabdaoui et al. (2009), ordered isotonic regression is illustrated using stress-strain curves from dynamical
material tests.}
\source{The data was taken from Shim and Mohr (2009).}
\references{
Balabdaoui, F., Rufibach, K., Santambrogio, F. (2009).
\emph{Least squares estimation of two ordered antitonic regression curves.}
Preprint.

Shim, J. and Mohr, D. (2009).
\emph{Using split Hopkinson pressure bars to perform large strain compression tests on polyurea at low, 
intermediate and high strain rates.}
International Journal of Impact Engineering, to appear.
}
\seealso{See the examples in \code{\link{BoundedIsoMeanTwo}} for the analysis of this data.}
\keyword{datasets}
