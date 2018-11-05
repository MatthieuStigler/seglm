#' Low level function to estimate a segmented/threshold regression by pre-specifying the threshold values
#'
#' @param formula the usual formula for the linear model to estimate
#' @param data the data-frame containing the variables in the model.
#' @param ... further arguments passed to lower level *TODO* functions
#' @template param_th_var
#' @template param_nthresh
#' @return An object of class "seglm" and "lm"
#' @examples
#' data_thresh <- sim_thresh()
#' seglm(formula = y~x, data = data_thresh, th_var = "x", nthresh =1)
#' @export


#' @importFrom stats model.frame model.matrix model.response
seglm <- function(formula, data, th_var, nthresh = 1, ...){


  ## Extract data
  mf <- model.frame(formula, data)
  X <- model.matrix(formula, data = mf)
  Y <- model.response(mf)
  f_thVar <- formula(paste(" ~ -1 +", th_var))
  th_var_M <- model.matrix(f_thVar, data = mf)

  ## do search:
  search <- seglm_search_grid(X=X, y = Y, nthresh = nthresh, th_var=th_var_M, ...)
  # search <- seglm_search_dynprog(X=X, y = Y, nthresh = nthresh, th_var=th_var_M, ...)
  # search <- seglm_search_grid(X=X, y = Y, nthresh = nthresh, th_var=th_var_M)
  # search <- seglm_search_dynprog(X=X, y = Y, nthresh = nthresh, th_var=th_var_M)

  ## pass arguments
  res <- seglm_fit(X=X, y=Y, th_var=th_var_M, nthresh = nthresh,
            th_val = search$th)
  res
}


if(FALSE) {
  library(seglm)
  data_thresh <- sim_thresh()
  head(data_thresh)
  f <-  y ~ x

  out_1 <- seglm(formula = y ~ x, data=data_thresh, th_var = "x", nthresh = 1)
  out_1
  # regime(out_1)

  out_2 <- seglm(formula = y ~ x, data=data_thresh, th_var = "x", nthresh = 2)
  out_2
}
