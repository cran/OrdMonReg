\name{BoundedIsoMeanTwoDykstra}
\alias{BoundedIsoMeanTwoDykstra}
\title{Compute solution to the problem of two ordered isotonic or antitonic curves}
\description{See details below.}
\usage{BoundedIsoMeanTwoDykstra(g1, w1, g2, w2, K1 = 1000, 
    delta = 10^(-8), output = TRUE)}
\arguments{
  \item{g1}{Vector in \eqn{R^n}, measurements of upper function.}
  \item{w1}{Vector in \eqn{R^n}, weights for upper function.}
  \item{g2}{Vector in \eqn{R^n}, measurements of lower function.}
  \item{w2}{Vector in \eqn{R^n}, weights for lower function.}
  \item{K1}{Upper bound on number of iterations.}
  \item{delta}{Upper bound on the error, defines stopping criterion.}
  \item{output}{Should intermediate results be output?}
}
\details{See \code{BoundedIsoMeanTwo} for a description of the problem. This function computes the estimates 
via Dykstra's (see Dykstra, 1983) cyclical projection algorithm.

The algorithm is implemented for isotonic curves.
}
\value{
  \item{g1}{The estimated function \eqn{\hat g_1^\circ}.}
  \item{g2}{The estimated function \eqn{\hat g_2^\circ}.}
  \item{L}{Value of the least squares criterion at the minimum.}
  \item{error}{Value of error (norm of difference two consecutive projections).}
  \item{k}{Number of iterations performed.}
}
\section{Warning}{Note that we have chosen a very simply stopping criterion here, namely the algorithm stops
if the norm of two consecutive projections is smaller than \eqn{\delta}. If \eqn{n} is very small, it may happen
that two consecutive projections are equal although \eqn{L} is not yet minimal (note that this typically happens
if \code{g1} = \code{g2}). If that is the case, we suggest to set \eqn{\delta < 0} and let the algorithm run
a sufficient number of iterations (specified by \code{K1}) to verify that the least squares criterion value
can not be decreased anymore.}
\references{
Balabdaoui, F., Rufibach, K., Santambrogio, F. (2009).
\emph{Least squares estimation of two ordered monotone regression curves.}
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
\seealso{The functions \code{\link{BoundedAntiMean}} and \code{\link{BoundedIsoMean}} for the problem of 
estimating \emph{one} antitonic (isotonic) regression
function bounded above and below by \emph{fixed} functions. The function \code{BoundedAntiMeanTwoDykstra} depends 
on the functions discussed in \code{\link{minK}}.}
\examples{
## examples are provided in the help file of the main function of this package:
?BoundedIsoMeanTwo
}
\keyword{regression}
\keyword{nonparametric}

