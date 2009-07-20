\name{BoundedAntiMean, BoundedIsoMean}
\alias{BoundedIsoMean}
\alias{BoundedAntiMean}
\title{Compute least square estimate of an iso- or antitonic function, bounded below and above by fixed functions}
\description{This function computes the bounded least squares isotonic regression estimate, where the bounds
are two functions such that the estimate is above the lower and below the upper function. To find the solution, we
use the pool-adjacent-violaters algorithm for a suitable set function M, as discussed in Balabdaoui et al. (2009).
The problem was initially posed in Barlow et al. (1972), including a remark (on p. 57) that the PAVA can be used to solve
it. However, a formal proof is not given in Barlow et al. (1972). A short note detailing this proof
is available from the authors of Balabdaoui et al. (2009) on request.
}
\usage{
BoundedIsoMean(y, w, a = NA, b = NA)
BoundedAntiMean(y, w, a = NA, b = NA)}
\arguments{
  \item{y}{Vector in \eqn{R^n} of measurements.}
  \item{w}{Vector in \eqn{R^n} of weights.}
  \item{a}{Vector in \eqn{R^n} that gives lower bound.}
  \item{b}{Vector in \eqn{R^n} that gives upper bound.}
}
\details{The \emph{bounded isotonic regression problem} is given by: For \eqn{x_1 \le \ldots \le x_n} 
let \eqn{y_i, i = 1, \ldots, n} be measurements of some quantity at the \eqn{x_i}'s, with true mean function 
\eqn{g^\circ(x)}.
The goal is to estimate \eqn{g^\circ} using least squares, i.e. to minimize

\deqn{L(a) = \sum_{i=1}^n w_i(y_i - a_i)^2}

over the class of vectors \eqn{a} that are isotonic and satisfy 

\deqn{a_{L, i} \le a_i \le a_{U, i} \ \ \mathrm{for} \ \ \mathrm{all} \ \ i = 1, \ldots, n}

and two \emph{fixed} isotonic vectors \eqn{a_L} and \eqn{a_U}. 
This problem can be solved using a suitable modification of the pool-adjacent-violaters algorithm, see
Barlow et al. (1972, p. 57) and Balabdaoui et al. (2009).

The function \code{BoundedAntiMean} solves the same problem for antitonic curves, by simply invoking \code{BoundedIsoMean}
flipping some of the arguments.
}
\value{The bounded isotonic (antitonic) estimate \eqn{(\hat g^\circ)_{i=1}^n}.}
\references{
Balabdaoui, F., Rufibach, K., Santambrogio, F. (2009).
\emph{Least squares estimation of two ordered monotone regression curves.}
Preprint.

Barlow, R. E., Bartholomew, D. J., Bremner, J. M., Brunk, H. D. (1972).
\emph{Statistical inference under order restrictions. The theory and application of isotonic regression}.
John Wiley and Sons, London - New York - Sydney.
}
\author{
Fadoua Balabdaoui \email{fadoua@ceremade.dauphine.fr}
\cr \url{http://www.ceremade.dauphine.fr/~fadoua}

Kaspar Rufibach (maintainer) \email{kaspar.rufibach@ifspm.uzh.ch}
\cr \url{http://www.biostat.uzh.ch/aboutus/people/rufibach.html}

Filippo Santambrogio \email{filippo@ceremade.dauphine.fr}
\cr \url{http://www.ceremade.dauphine.fr/~filippo}
}
\seealso{The functions \code{\link{BoundedAntiMeanTwo}} and \code{\link{BoundedIsoMeanTwo}} for the problem of 
estimating \emph{two} ordered antitonic (isotonic) regression
functions. The function \code{BoundedIsoMean} depends on the function \code{\link{MA}}.
}
\examples{
## --------------------------------------------------------
## generate data
## --------------------------------------------------------
set.seed(23041977)
n <- 35
x <- 1:n / n
f0 <- - 3 * x + 5
g0 <- 1 / (x + 0.5) ^ 2 + 1 
g <- g0 + 3 * rnorm(n)

## --------------------------------------------------------
## compute estimate
## --------------------------------------------------------
g_est <- BoundedAntiMean(g, w = rep(1 / n, n), a = -rep(Inf, n), b = f0)

## --------------------------------------------------------
## plot observations and estimate
## --------------------------------------------------------
par(mar = c(4.5, 4, 3, 0.5))
plot(0, 0, type = 'n', main = "Observations, upper bound and estimate 
    for bounded antitonic regression", xlim = c(0, max(x)), ylim = 
    range(c(f0, g)), xlab = expression(x), ylab = "observations and estimate")
points(x, g, col = 1)
lines(x, g0, col = 1, lwd = 2, lty = 2)
lines(x, f0, col = 2, lwd = 2, lty = 2)
lines(x, g_est, col = 3, lwd = 2)
legend("bottomleft", c("truth", "data", "upper bound", "estimate"), 
    lty = c(1, 0, 1, 1), lwd = c(2, 1, 2, 2), pch = c(NA, 1, NA, NA), 
    col = c(1, 1:3), bty = 'n')
    
\dontrun{
## --------------------------------------------------------
## 'BoundedIsoMean' is a generalization of 'isoMean' in the 
## package 'logcondens'
## --------------------------------------------------------
library(logcondens)
n <- 50
y <- sort(runif(n, 0, 1)) ^ 2 + rnorm(n, 0, 0.2)

isoMean(y, w = rep(1 / n, n))
BoundedIsoMean(y, w = rep(1 / n, n), a = -rep(Inf, n), b = rep(Inf, n))
}    
}
\keyword{regression}
\keyword{nonparametric}

