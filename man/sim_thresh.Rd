% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sim_dat.R
\name{sim_thresh}
\alias{sim_thresh}
\alias{sim_thresh_2X}
\title{Simulation of data for example}
\usage{
sim_thresh(N = 200, th_val = 0, output = c("df", "matrix"))

sim_thresh_2X(N = 200, th_val = 0, output = c("df", "matrix"))
}
\arguments{
\item{N}{Size of sample to simulate}

\item{th_val}{The pre-specified threshold/breakpoints values.}

\item{output}{whether to return a \emph{data.frame} or \emph{matrix}}
}
\value{
A data frame with columns \emph{y}, \emph{y_true}, and various more \emph{x} columns
}
\description{
Simulates a dataset with one discontinuit) in x at value \code{th_val}. Function \code{sim_thresh_2X}
does the same but with two thresholds.
}
\details{
This simulates a simple model such as: \deqn{1.2 + 0.4 * x1 *D + 0.8 * x1 * (1-D)}
where D is an indicator function indicating whether values are below or above the threshold.
}
\examples{
data_thresh <- sim_thresh()
plot(y~x, data=data_thresh)
lines(y_true~x, data = data_thresh[order(data_thresh$x),], col=2)

## 2 X
dat_th_2X <- sim_thresh_2X()
opar <-  par()
par(mfrow=c(2,1))
plot(y~x1, data=dat_th_2X, col = dat_th_2X$regime+1)
lines(x1_reg ~ x1, data= dat_th_2X[order(dat_th_2X$x1),])

plot(y~x2, data=dat_th_2X, col = dat_th_2X$regime+1)
lines(x2_reg_low ~ x2, data= dat_th_2X[order(dat_th_2X$x2),])
lines(x2_reg_high ~ x2, data= dat_th_2X[order(dat_th_2X$x2),], col =2)
par(opar)
}
