\name{LSfunctional}
\alias{LSfunctional}
\title{Compute least squares criterion for two ordered antitonic regression functions}
\description{Computes the value of the least squares criterion in the problem of two ordered antitonic
regression functions.}
\usage{LSfunctional(f1, g1, w1, f2, g2, w2)}
\arguments{
  \item{f1}{Vector in \eqn{R^n}, specifies values of upper function at which criterion should be evaluated.}
  \item{g1}{Vector in \eqn{R^n}, measurements of upper function.}
  \item{w1}{Vector in \eqn{R^n}, weights for upper function.}
  \item{f2}{Vector in \eqn{R^n}, specifies values of lower function at which criterion should be evaluated.}
  \item{g2}{Vector in \eqn{R^n}, measurements of lower function.}
  \item{w2}{Vector in \eqn{R^n}, weights for lower function.}
}
\details{This function simply computes for the above vectors

\deqn{L(f_1, f_2) \ = \ \sum_{x \in X} w_1(x)(f_1(x) - g_1(x))^2 + \sum_{x \in X} w_2(x)(f_2(x) - g_2(x))^2.}
}
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


