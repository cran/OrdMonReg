\name{OrdMonReg-package}
\alias{OrdMonReg-package}
\alias{OrdMonReg}
\docType{package}
\title{Compute least squares estimates of one bounded or two ordered antitonic regression curves}
\description{
We consider the problem of estimating two isotonic regression curves \eqn{g^\circ_1} and \eqn{g^\circ_2} under the 
constraint that \eqn{g^\circ_1 \le g^\circ_2}. Given two sets of \eqn{n} data points \eqn{y_1, \ldots, y_n} 
and \eqn{z_1, \ldots, z_n} 
that are observed at (the same) deterministic design points \eqn{x_1, \ldots, x_n}, the estimates are obtained by 
minimizing the Least Squares criterion 

\deqn{L(a, b) = \sum_{i=1}^n (y_i - a_i)^2 w_1(x_i) + \sum_{i=1}^n (z_i - b_i)^2 w_2(x_i)}

over the class of pairs of vectors \eqn{(a, b)} such that \eqn{a} and \eqn{b} are isotonic and 
\eqn{a_i \le b_i} for all \eqn{i = {1, \ldots, n}}. We offer two different approaches to compute the estimates: a 
projected subgradient algorithm where the projection is calculated using a pool-adjacent-violaters algorithm (PAVA) 
as well as Dykstra's cyclical projection algorithm..

Additionally, functions to solve the bounded isotonic regression problem described in Barlow et al. (1972, p. 57)
are provided.
} 
\details{
\tabular{ll}{
Package: \tab OrdMonReg\cr
Type: \tab Package\cr
Version: \tab 1.0.2\cr
Date: \tab 2009-10-15\cr
License: \tab GPL (>=2) \cr
}
}
\author{
Fadoua Balabdaoui \email{fadoua@ceremade.dauphine.fr}
\cr \url{http://www.ceremade.dauphine.fr/~fadoua}

Kaspar Rufibach (maintainer) \email{kaspar.rufibach@ifspm.uzh.ch}
\cr \url{http://www.biostat.uzh.ch/aboutus/people/rufibach.html}

Filippo Santambrogio \email{filippo@ceremade.dauphine.fr}
\cr \url{http://www.ceremade.dauphine.fr/~filippo}
}
\references{
Balabdaoui, F., Rufibach, K., Santambrogio, F. (2009).
\emph{Least squares estimation of two ordered monotone regression curves.}
Preprint.

Barlow, R. E., Bartholomew, D. J., Bremner, J. M., Brunk, H. D. (1972).
\emph{Statistical inference under order restrictions. The theory and application of isotonic regression}.
John Wiley and Sons, London - New York - Sydney.

Dykstra, R.L. (1983).
An Algorithm for Restricted Least Squares Regression.
\emph{J. Amer. Statist. Assoc.}, \bold{78}, 837--842.
}
\keyword{regression}
\keyword{nonparametric}
\seealso{
Other versions of bounded regression are implemented in the packages \pkg{cir}, 
\pkg{Iso}, \pkg{monreg}. The function
\code{\link{BoundedIsoMean}} is a generalization of the function \code{isoMean} in the package
\pkg{logcondens}.
}
\examples{
## examples are provided in the help files of the main functions of this package:
?BoundedAntiMean
?BoundedAntiMeanTwo
}
