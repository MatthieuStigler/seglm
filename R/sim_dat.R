#' Simulation of data for example
#'
#' @param N Size of sample to simulate
#' @param output whether to return a *data.frame* or *matrix*
#' @template param_th_val
#' @return A data frame with columns *y*, *x1* and *x2*
#' @examples
#' data_thresh <- sim_thresh()
#' plot(y~x, data=data_thresh)
#' lines(y_true~x, data = data_thresh[order(data_thresh$x),], col=2)
#'
#' ## 2 X
#' dat_th_2X <- sim_thresh_2X()
#' opar <-  par()
#' par(mfrow=c(2,1))
#' plot(y~x1, data=dat_th_2X, col = dat_th_2X$regime+1)
#' lines(x1_reg ~ x1, data= dat_th_2X[order(dat_th_2X$x1),])
#'
#' plot(y~x2, data=dat_th_2X, col = dat_th_2X$regime+1)
#' lines(x2_reg_low ~ x2, data= dat_th_2X[order(dat_th_2X$x2),])
#' lines(x2_reg_high ~ x2, data= dat_th_2X[order(dat_th_2X$x2),], col =2)
#' par(opar)

#' @export

#' @importFrom stats rnorm
sim_thresh <-  function(N = 200, th_val =0, output = c("df", "matrix")) {

  output <-  match.arg(output)

  x1 <- rnorm(N)
  # x2 <- rnorm(N)
  # X <-  cbind(x1, x2)
  D <- ifelse(x1 <= th_val, 1, 0)
  y_true <-  1.2 + 0.4 * x1 *D + 0.8 * x1 * (1-D)
  y <-  y_true + rnorm(N)
  res <- data.frame(y=y, y_true = y_true, x = x1, regime  = D)
  if(output == "matrix") res <- as.matrix(res)
  res
}

#'@rdname sim_thresh
#' @export
sim_thresh_2X <-  function(N = 200, th_val =0, output = c("df", "matrix")) {

  output <-  match.arg(output)

  x1 <- rnorm(N)
  x2 <- rnorm(N)
  # X <-  cbind(x1, x2)
  D <- ifelse(x1 <= th_val, 1, 0)
  y_true <-  1.2 + 0.4 * x1 *D + 0.8 * x1 * (1-D) + 1.1 * x2 * D + 0.9 * x2 * (1-D)
  pred_x1 <- 1.2 + 0.4 * x1 *D + 0.8 * x1 * (1-D)
  pred_x2_low <- 1.2 +1.1 * x2
  pred_x2_high <- 1.2 +0.9 * x2
  y <-  y_true + rnorm(N)
  res <- data.frame(y=y, y_true = y_true, x1 = x1, x2=x2, regime  = D,
                    x1_reg = pred_x1,
                    x2_reg_low = pred_x2_low,
                    x2_reg_high = pred_x2_high)
  if(output == "matrix") res <- as.matrix(res)
  res
}
