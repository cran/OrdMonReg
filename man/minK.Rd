\name{minK}
\alias{minK}
\alias{minK1}
\alias{minK2}
\alias{minK3}
\title{Compute projections on restriction cones in Dykstra's algorithm.}
\description{Internal functions for Dykstra's algorithm to compute bounded monotone regression estimates.}
\details{These functions are not intended to be called by the user. 
\itemize{

\item{\code{\link{minK1}}}{Compute projection of \eqn{(a, b)} on the set \eqn{\{(a, b) \ : \ a} is increasing.\}.}

\item{\code{\link{minK2}}}{Compute projection of \eqn{(a, b)} on the set \eqn{\{(a, b) \ : \ b} is increasing.\}.}

\item{\code{\link{minK3}}}{Compute projection of \eqn{(a, b)} on the set \eqn{\{(a, b) \ : \ a \le b\}}.}
}}
\references{
Balabdaoui, F., Rufibach, K., Santambrogio, F. (2009).
\emph{Least squares estimation of two ordered antitonic regression curves.}
Preprint.

Dykstra, R.L. (1983).
An Algorithm for Restricted Least Squares Regression.
\emph{J. Amer. Statist. Assoc.}, \bold{78}, 837--842.
}
\author{
Fadoua Balabdaoui \email{fadoua@ceremade.dauphine.fr}
\cr \url{http://www.ceremade.dauphine.fr/~fadoua}

Kaspar Rufibach (maintainer) \email{kaspar.rufibach@ifspm.uzh.ch}
\cr \url{http://www.biostat.uzh.ch/aboutus/people/rufibach.html}

Filippo Santambrogio \email{filippo@ceremade.dauphine.fr}
\cr \url{http://www.ceremade.dauphine.fr/~filippo}
}
\seealso{This functions are used by \code{\link{BoundedIsoMeanTwoDykstra}}.}
\keyword{regression}
\keyword{nonparametric}
