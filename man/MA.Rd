\name{MA}
\alias{MA}
\title{Compute bounded weighted average}
\description{This function computes the bounded weighted mean for any subset of indices.}
\usage{MA(g, w, A = NA, a, b)}
\arguments{
  \item{g}{Vector in \eqn{R^n} of measurements.}
  \item{w}{Vector in \eqn{R^n} of weights.}
  \item{A}{Subset of \code{1:n}, denoting the subsets of the above vectors to compute the average with.}
  \item{a}{Vector in \eqn{R^n} that gives lower bound.}
  \item{b}{Vector in \eqn{R^n} that gives upper bound.}
}
\details{This function computes the bounded average

\deqn{MA[A] = \max\{\min\{Av[A], \min_{x \in A} b(x)\}, \max_{x \in A} a(x)\},}

see Balabdaoui et al. (2009) for details.
}
\value{The bounded weighted average is returned.}
\references{
Balabdaoui, F., Rufibach, K., Santambrogio, F. (2009).
\emph{Least squares estimation of two ordered monotone regression curves.}
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
\seealso{This function is used by \code{\link{BoundedIsoMean}}.}
\keyword{regression}
\keyword{nonparametric}
