#' Simulation of data for example
#'
#' @param N Size of sample to simulate
#' @template param_th_val
#' @return A data frame with columns *y*, *x1* and *x2*
#' @examples
#' data_thresh <- sim_thresh()
#' @export

#' @importFrom stats rnorm
sim_thresh <-  function(N = 200, th_val =0, output = c("df", "matrix")) {

  output <-  match.arg(output)

  x1 <- rnorm(N)
  # x2 <- rnorm(N)
  # X <-  cbind(x1, x2)
  D <- ifelse(x1 <= th_val, 1, 0)
  y <-  1.2 + 0.4 * x1 *D + 0.8 * x1 * (1-D) + rnorm(N)
  res <- data.frame(y=y, x = x1, regime  = D)
  if(output == "matrix") res <- as.matrix(res)
  res
}
