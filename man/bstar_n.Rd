\name{astar_1, bstar_n}
\alias{astar_1}
\alias{bstar_n}
\title{Computes explicitly known values of the estimates in the two ordered functions antitonic regression problem}
\description{These functions compute the values \eqn{a_1^*} and \eqn{b_n^*}, the value of the estimate of the 
upper function at \eqn{x_1} and the value of the lower estimated function at \eqn{x_n} in the two ordered
antitonic regression functions problem. These values can be computed via explicit formulas, unlike the values at
\eqn{x \in {x_2, \ldots, x_{n-1}}}, which are received via a projected subgradient algorithm. However, 
\eqn{b_n^*} enters this algorithm as an auxiliary quantity.}
\usage{
astar_1(g1, w1, g2, w2)
bstar_n(g1, w1, g2, w2)
}
\arguments{
  \item{g1}{Vector in \eqn{R^n}, measurements of upper function.}
  \item{w1}{Vector in \eqn{R^n}, weights for upper function.}
  \item{g2}{Vector in \eqn{R^n}, measurements of lower function.}
  \item{w2}{Vector in \eqn{R^n}, weights for lower function.}
}
\value{Values of \eqn{a_1^*} and \eqn{b_n^*} are returned.}
\references{
Balabdaoui, F., Rufibach, K., Santambrogio, F. (2009).
\emph{Least squares estimation of two ordered antitonic regression curves.}
Preprint.
}
\author{
Fadoua Balabdaoui \email{fadoua@ceremade.dauphine.fr}
\cr \url{http://www.ceremade.dauphine.fr/~fadoua}

Kaspar Rufibach (maintainer) \email{kaspar.rufibach@ifspm.uzh.ch}
\cr \url{http://www.biostat.uzh.ch/aboutus/people/rufibach.html}

Filippo Santambrogio \email{filippo@ceremade.dauphine.fr}
\cr \url{http://www.ceremade.dauphine.fr/~filippo}
}
\seealso{This function is used by \code{\link{BoundedAntiMeanTwo}}.}
\keyword{regression}
\keyword{nonparametric}


