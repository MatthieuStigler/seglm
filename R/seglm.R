#' High level function to estimate a segmented/threshold regression
#'
#' @param formula the usual formula for the linear model to estimate
#' @param data the data-frame containing the variables in the model.
#' @param ... further arguments passed to the lower-level \code{\link{seglm_search}} function
#' @param th_var_name the name of the threshold variable. Using *.time* will run a standard changepoint model.
#' @param th Optional. The threshold value(s), should be of length \code{nthresh}.
#' @template param_nthresh
#' @return An object of class "seglm" and "lm"
#' @examples
#' data_thresh <- sim_thresh()
#' seglm(formula = y~x, data = data_thresh, th_var_name = "x", nthresh =1)
#' @export


#' @importFrom stats model.frame model.matrix model.response
seglm <- function(formula, data, th_var_name, nthresh = 1, th=NULL, ...){


  ## Extract data
  mf <- model.frame(formula, data)
  X <- model.matrix(formula, data = mf)
  Y <- model.response(mf)
  if(th_var_name == ".time") {
    th_var_M <- matrix(seq_len(nrow(data)), ncol=1)
  } else {
    f_thVar <- formula(paste(" ~ -1 +", th_var_name))
    th_mf <- model.frame(f_thVar, data)
    th_var_M <- model.matrix(f_thVar, data = th_mf)
  }


  ## do search:
  if(!is.null(th)) {
    th_values <-  th
  } else {
    algo <- ifelse(nthresh %in% 1:2, "grid", "dynprog")
    search <- seglm_search(X=X, y = Y, nthresh = nthresh, th_var=th_var_M, algorithm = algo, ...)
    th_values <- get_th(search)
  }
  # search <- seglm_search_dynprog(X=X, y = Y, nthresh = nthresh, th_var=th_var_M, ...)
  # search <- seglm_search_grid(X=X, y = Y, nthresh = nthresh, th_var=th_var_M)
  # search <- seglm_search_dynprog(X=X, y = Y, nthresh = nthresh, th_var=th_var_M)

  ## pass arguments
  res <- seglm_fit(X=X, y=Y, th_var=th_var_M, nthresh = nthresh,
            th_val = th_values)
  res
}


if(FALSE) {
  library(seglm)
  data_thresh <- sim_thresh()
  head(data_thresh)
  f <-  y ~ x

  out_1 <- seglm(formula = y ~ x, data=data_thresh, th_var_name = "x", nthresh = 1)
  out_1
  # regime(out_1)

  out_2 <- seglm(formula = y ~ x, data=data_thresh, th_var_name = "x", nthresh = 2)
  out_2

  out_time <- seglm(formula = y ~ x, data=data_thresh, th_var_name = ".time", nthresh = 2)
}
