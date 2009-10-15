\name{BoundedAntiMeanTwo, BoundedIsoMeanTwo}
\alias{BoundedAntiMeanTwo}
\alias{BoundedIsoMeanTwo}
\title{Compute solution to the problem of two ordered isotonic or antitonic curves}
\description{See details below.}
\usage{
BoundedIsoMeanTwo(g1, w1, g2, w2, K1 = 1000, K2 = 400, 
    delta = 10^(-4), errorPrec = 10, output = TRUE)
BoundedAntiMeanTwo(g1, w1, g2, w2, K1 = 1000, K2 = 400, 
    delta = 10^(-4), errorPrec = 10, output = TRUE)
}
\arguments{
  \item{g1}{Vector in \eqn{R^n}, measurements of upper function.}
  \item{w1}{Vector in \eqn{R^n}, weights for upper function.}
  \item{g2}{Vector in \eqn{R^n}, measurements of lower function.}
  \item{w2}{Vector in \eqn{R^n}, weights for lower function.}
  \item{K1}{Upper bound on number of iterations.}
  \item{K2}{Number of iterations where step length is changed from the inverse of the norm of the subgradient to 
  a \emph{diminishing} function of the norm of the subgradient.}
  \item{delta}{Upper bound on the error, defines stopping criterion.}
  \item{errorPrec}{Computation of stopping criterion is expensive. Therefore, the stopping criterion is
  only evaluated at every \code{errorPrec}-th iteration of the algorithm.}
  \item{output}{Should intermediate results be output?}
}
\details{We consider the problem of estimating two isotonic (antitonic) regression curves \eqn{g_1^\circ} and 
\eqn{g_2^\circ} under the 
constraint that \eqn{g_1^\circ \le g_2^\circ}. Given two sets of \eqn{n} data points \eqn{y_1, \ldots, y_n} 
and \eqn{z_1, \ldots, z_n} 
that are observed at (the same) deterministic design points \eqn{x_1, \ldots, x_n} with weights
\eqn{w_{1,i}} and \eqn{w_{2,i}}, respectively, the estimates are obtained by 
minimizing the Least Squares criterion 

\deqn{L_2(a, b) = \sum_{i=1}^n (y_i - a_i)^2 w_{1,i} + \sum_{i=1}^n (z_i - b_i)^2 w_{2,i}}

over the class of pairs of vectors \eqn{(a, b)} such that \eqn{a} and \eqn{b} are isotonic (antitonic) and 
\eqn{a_i \le b_i} for all \eqn{i = {1, \ldots, n}}. The estimates are computed with a projected 
subgradient algorithm where the projection is calculated using a suitable version of the pool-adjacent-violaters 
algorithm (PAVA).

The algorithm is implemented for antitonic curves in the function \code{BoundedAntiMeanTwo}.
The function \code{BoundedIsoMeanTwo} solves the same problem for isotonic curves, by simply invoking 
\code{BoundedAntiMeanTwo} and suitably flipping some of the arguments.
}
\value{
  \item{g1}{The estimated function \eqn{\hat g_1^\circ}.}
  \item{g2}{The estimated function \eqn{\hat g_2^\circ}.}
  \item{L}{Value of the least squares criterion at the minimum.}
  \item{error}{Value of error.}
  \item{k}{Number of iterations performed.}
  \item{tau}{Step length at final iteration.}
}
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
\seealso{The functions \code{\link{BoundedAntiMean}} and \code{\link{BoundedIsoMean}} for the problem of 
estimating \emph{one} antitonic (isotonic) regression
function bounded above and below by \emph{fixed} functions. The function \code{BoundedAntiMeanTwo} depends 
on the functions \code{\link{BoundedAntiMean}}, \code{\link{bstar_n}}, 
\code{\link{LSfunctional}}, and \code{\link{Subgradient}}.}
\examples{

## ========================================================
## The first example uses simulated data
## For the analysis of the mechIng dataset see below
## ========================================================

## --------------------------------------------------------
## initialization
## --------------------------------------------------------
set.seed(23041977)
n <- 100
x <- 1:n
g1 <- 1 / x^2 + 2
g1 <- g1 + 3 * rnorm(n)
g2 <- 1 / log(x+3) + 2
g2 <- g2 + 4 * rnorm(n)
w1 <- runif(n)
w1 <- w1 / sum(w1)
w2 <- runif(n)
w2 <- w2 / sum(w2)

## --------------------------------------------------------
## compute estimates
## --------------------------------------------------------
shor <- BoundedAntiMeanTwo(g1, w1, g2, w2, errorPrec = 20, 
    delta = 10^(-10))
    
## corresponding isotonic problem
shor2 <- BoundedIsoMeanTwo(-g2, w2, -g1, w1, errorPrec = 20, 
    delta = 10^(-10))
    
## the following vectors are equal
shor$g1 - -shor2$g2    
shor$g2 - -shor2$g1
        
## --------------------------------------------------------
## for comparison, compute estimates via cyclical projection
## algorithm due to Dykstra (1983) (isotonic problem)
## --------------------------------------------------------
dykstra1 <- BoundedIsoMeanTwoDykstra(-g2, w2, -g1, w1, 
    delta = 10^(-10))

## the following vectors are equal
shor2$g1 - dykstra1$g1
shor2$g2 - dykstra1$g2

## --------------------------------------------------------
## Checking of solution
## --------------------------------------------------------
# This compares the first component of shor$g1 with a^*_1:
c(shor$g1[1], astar_1(g1, w1, g2, w2))

## --------------------------------------------------------
## plot original functions and estimates
## --------------------------------------------------------
par(mfrow = c(1, 1), mar = c(4.5, 4, 3, 0.5))
plot(x, g1, col = 2, main = "Original observations and estimates in problem 
two ordered antitonic regression functions", xlim = c(0, max(x)), ylim = 
range(c(shor$g1, shor$g2, g1, g2)), xlab = expression(x), 
ylab = "measurements and estimates")
points(x, g2, col = 3)
lines(x, shor$g1 + 0.01, col = 2, type = 's', lwd = 2)
lines(x, shor$g2 - 0.01, col = 3, type = 's', lwd = 2)
legend("bottomleft", c(expression("upper estimated function g"[1]*"*"), 
    expression("lower estimated function g"[2]*"*")), lty = 1, col = 2:3, 
    lwd = 2, bty = "n")


## ========================================================
## Analysis of the mechIng dataset
## ========================================================

## --------------------------------------------------------
## input data
## --------------------------------------------------------
data(mechIng)
x <- mechIng$x
n <- length(x)
g1 <- mechIng$g1
g2 <- mechIng$g2
w1 <- rep(1, n)
w2 <- w1

## --------------------------------------------------------
## compute unordered estimates
## --------------------------------------------------------
g1_pava <- BoundedIsoMean(y = g1, w = w1, a = NA, b = NA)
g2_pava <- BoundedIsoMean(y = g2, w = w2, a = NA, b = NA)

## --------------------------------------------------------
## compute estimates via cyclical projection algorithm due to
## Dysktra (1983)
## --------------------------------------------------------
dykstra1 <- BoundedIsoMeanTwoDykstra(g1, w1, g2, w2, 
    delta = 10^-10, output = TRUE)
    
## --------------------------------------------------------
## compute smoothed versions
## --------------------------------------------------------
g1_mon <- dykstra1$g1
g2_mon <- dykstra1$g2   

kernel <- function(x, X, h, Y){
    tmp <- dnorm((x - X) / h) 
    res <- sum(Y * tmp) / sum(tmp)
    return(res)
    }
h <- 0.1 * n^(-1/5)

g1_smooth <- rep(NA, n)
g2_smooth <- g1_smooth
for (i in 1:n){
    g1_smooth[i] <- kernel(x[i], X = x, h, g1_mon)
    g2_smooth[i] <- kernel(x[i], X = x, h, g2_mon)
}
            
## --------------------------------------------------------
## plot original functions and estimates
## --------------------------------------------------------
par(mfrow = c(2, 1), oma = c(0, 0, 2, 0), mar = c(4.5, 4, 2, 0.5), 
    cex.main = 0.8, las = 1) 

plot(0, 0, type = 'n', xlim = c(0, max(x)), ylim = 
    range(c(g1, g2, g1_mon, g2_mon)), xlab = "x", ylab = 
    "measurements and estimates", main = "ordered antitonic estimates")
points(x, g1, col = grey(0.3), pch = 20, cex = 0.8)
points(x, g2, col = grey(0.6), pch = 20, cex = 0.8)
lines(x, g1_mon + 0.1, col = 2, type = 's', lwd = 3)
lines(x, g2_mon - 0.1, col = 3, type = 's', lwd = 3)
legend(0.2, 10, c(expression("upper isotonic function g"[1]*"*"), 
    expression("lower isotonic function g"[2]*"*")), lty = 1, col = 2:3, 
    lwd = 3, bty = "n")

plot(0, 0, type = 'n', xlim = c(0, max(x)), ylim = 
    range(c(g1, g2, g1_mon, g2_mon)), xlab = "x", ylab = "measurements and 
    estimates", main = "smoothed ordered antitonic estimates")
points(x, g1, col = grey(0.3), pch = 20, cex = 0.8)
points(x, g2, col = grey(0.6), pch = 20, cex = 0.8)
lines(x, g1_smooth + 0.1, col = 2, type = 's', lwd = 3)
lines(x, g2_smooth - 0.1, col = 3, type = 's', lwd = 3)
legend(0.2, 10, c(expression("lower isotonic smoothed function "*tilde(g)[1]*"*"), 
    expression("lower isotonic smoothed function "*tilde(g)[2]*"*")), 
    lty = 1, col = 2:3, lwd = 3, bty = "n")

par(cex.main = 1)
title("Original observations and estimates in mechanical engineering example", 
    line = 0, outer = TRUE)
}
\keyword{regression}
\keyword{nonparametric}

