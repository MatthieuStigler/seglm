#' Simulation of data for example
#'
#' @param N Size of sample to simulate
#' @template param_th_val
#' @return A data frame with columns *y*, *x1* and *x2*
#' @examples
#' data_thresh <- sim_thresh()


sim_thresh <-  function(N = 200, th_val =0) {
  x1 <- rnorm(N)
  x2 <- rnorm(N)
  X <-  cbind(x1, x2)
  D <- ifelse(x1 <= th_val, 1, 0)
  y <-  1.2 + 0.4 * X *D + 0.8 * X * (1-D) + rnorm(N)
  data.frame(y=y, x1=x1, x2=x2, regime  = D)
}
