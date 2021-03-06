% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/seglm_fit.R
\name{seglm_fit}
\alias{seglm_fit}
\alias{print.seglm_lm}
\title{Low level function to estimate a segmented/threshold regression by pre-specifying the threshold values}
\usage{
seglm_fit(X, y, th_var, nthresh = 1, th_val)

\method{print}{seglm_lm}(x, ...)
}
\arguments{
\item{X}{matrix of regressors}

\item{y}{matrix of the response}

\item{th_var}{The threshold variable, i.e. variable to segment over.}

\item{nthresh}{The number of threshold/breakpoints to estimate.}

\item{th_val}{The pre-specified threshold/breakpoints values.}

\item{x}{object of class \emph{seglm}}

\item{...}{unused}
}
\value{
An object of class "seglm" and "lm"
}
\description{
Low level function to estimate a segmented/threshold regression by pre-specifying the threshold values
}
\examples{
data_thresh <- sim_thresh()
X_inp <-  as.matrix(data_thresh[, "x", drop = FALSE])
y_inp <-  as.matrix(data_thresh[, "y"])
seglm_fit(X=X_inp, y=y_inp, th_var = X_inp, th_val = 0)
}
