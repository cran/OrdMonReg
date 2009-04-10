\name{Subgradient}
\alias{Subgradient}
\title{Computes a subgradient for the projected subgradient algorithm}
\description{This function computes a subgradient of the function \eqn{\Psi}.}
\usage{Subgradient(b, g1, w1, g2, w2, B, Gsi)}
\arguments{
  \item{b}{Vector in \eqn{R^{n-1}} at which subgradient should be computed.}
  \item{g1}{Vector in \eqn{R^n}, measurements of upper function.}
  \item{w1}{Vector in \eqn{R^n}, weights for upper function.}
  \item{g2}{Vector in \eqn{R^n}, measurements of lower function.}
  \item{w2}{Vector in \eqn{R^n}, weights for lower function.}
  \item{B}{Value of \eqn{b_n^*}.}
  \item{Gsi}{Matrix in \eqn{R^{n \times n}} that contains the quantities \eqn{G_{s, i}} defined
  in Balabdaoui et al. (2009).}
}
\value{The subgradient at \eqn{b}.}
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
