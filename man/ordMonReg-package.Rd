\name{OrdMonReg-package}
\alias{OrdMonReg-package}
\alias{OrdMonReg}
\docType{package}
\title{Compute least squares estimates of one bounded or two ordered antitonic regression curves}
\description{
We consider the problem of estimating two antitonic regression curves \eqn{g_1^*} and \eqn{g_2^*} under the 
constraint that \eqn{g_1^* \ge g_2^*}. Given two sets of \eqn{n} data points \eqn{g_1(x_1), \ldots, g_1(x_n)} 
and \eqn{g_2(x_1), \ldots, g_2(x_n)} 
that are observed at (the same) deterministic points \eqn{x_1, \ldots, x_n}, the estimates are obtained by 
minimizing the Least Squares criterion 

\deqn{L(f_1, f_2) = \sum_{i=1}^n (g_1(x_i) - f_1(x_i))^2 w_1(x_i)+ \sum_{i=1}^n (g_2(x_i) - f_2(x_i))^2 w_2(x_i)}

over the class of pairs of functions \eqn{(f_1, f_2)} such that \eqn{f_1} and \eqn{f_2} are antitonic and 
\eqn{f_1(x_i) \ge f_2(x_i)} for all \eqn{i = {1, \ldots, n}}. The estimates are computed with an projected 
subgradient algorithm where the projection is calculated using a suitable version of the pool-adjacent-violaters 
algorithm (PAVA).

Additionally, functions to solve the bounded antitonic regression problem described in Barlow et al. (1972, p. 57)
are provided.
} 
\details{
\tabular{ll}{
Package: \tab OrdMonReg\cr
Type: \tab Package\cr
Version: \tab 1.0.0\cr
Date: \tab 2009-04-10\cr
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
\emph{Least squares estimation of two ordered antitonic regression curves.}
Preprint.

Barlow, R. E., Bartholomew, D. J., Bremner, J. M., Brunk, H. D. (1972).
\emph{Statistical inference under order restrictions. The theory and application of isotonic regression}.
John Wiley and Sons, London - New York - Sydney.
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
