#' Low level function to estimate a segmented/threshold regression by pre-specifying the threshold values
#'
#' @param X matrix of regressors
#' @param y matrix of the response
#' @template param_th_var
#' @template param_th_val
#' @template param_nthresh
#' @return An object of class "seglm" and "lm"
#' @examples
#' data_thresh <- sim_thresh()
#' X_inp <-  as.matrix(data_thresh[, "x", drop = FALSE])
#' y_inp <-  as.matrix(data_thresh[, "y"])
#' seglm_fit(X=X_inp, y=y_inp, th_var = X_inp, th_val = 0)
#' @export

seglm_fit <- function(X, y, th_var, nthresh = 1, th_val){

  if(length(th_val)!=nthresh) stop("arg 'th_val' should be of same length as arg 'nthresh")
  th_var <-  as.numeric(th_var)

  # X_dat <- prep_X(X=X, th_val=th_val, th_var= th_var, nthresh= nthresh)
  X_dat <- prep_Xany(X=X, th_val=th_val, th_var= th_var, nthresh= nthresh, clean_names = TRUE)
  regimes <- cut(th_var, breaks = c(-Inf, th_val, Inf),
                 labels = 1:(nthresh +1))

  ## rename
  # X_cols <-  colnames(X)
  # new_cols <- paste(rep(X_cols, times = nthresh),
  #                   rep(c("_L", if(nthresh ==2) "_M" else NULL, "_H"), each = length(X_cols)),
  #                   sep = "")
  Xy_dat <-  cbind(y=y, X_dat)
  colnames(Xy_dat)[1] <-  "y"


  ## estimate
  res <-  lm(y~. - 1, data = as.data.frame(Xy_dat))
  res$th_val <-  th_val
  res$nthresh <- nthresh
  res$regime <-  regime
  class(res) <-  c("seglm_lm", "lm")
  res
}

coef.seglm_lm <-  function(x, by_reg = FALSE) {
  res <- x$coefficient
  if(by_reg) {
    res <- matrix(res, ncol = x$nthresh+1, byrow=TRUE)
    rownames(res) <- unique(gsub("^`|_L`?$|_M`?$|_H`?$|_seg[0-9]{1,2}`?$", "",  names(x$coefficient)))
    colnames(res) <-  paste("seg", 1:(x$nthresh+1), sep="")
  }
  res
}

#' @param x  object of class *seglm*
#' @param ... unused
#' @rdname seglm_fit
#' @export
print.seglm_lm <-  function(x, ...) {
  cat("Coefs:\n")
  print(coef.seglm_lm(x, by_reg = TRUE))
  cat("\nThreshold:", x$th_val, "\n")
}



# plot.segLM <- function(x,type=1,var=2,...){
#   if(type==1){
#     gr <- x$thGrid
#     plot(x=gr$th, y=gr$SSR, ylab="SSR", xlab="th", type="l")
#     points(x$th, deviance(x), col=2, cex=3)
#     points(x$optim["par"], x$optim["SSR"], col=3, cex=3)
#     points(x$optim2["par"], x$optim2["SSR"], col=4, cex=3)
#   } else if(type==2){
#     mod <- x$model_original
#     preds <- data.frame(mod[,var], predict(x))[order(mod[,var]),]
#     plot(mod[,1]~mod[,var], xlab=colnames(mod)[var])
#     lines(preds)
#   }
# }

if(FALSE){

  # library(strucchange)
  library(tidyverse)
  lag <-  stats:::lag


  ## prepare data
  X <- freeny.x
  Xint <- cbind(int = 1, X)
  Y <- freeny.y
  th <- freeny.x[, "price index", drop=FALSE]

  # dupli
  sort(th)[5:7]
  sort(th)[duplicated(sort(th))]
  order(th)
  Xint[order(th),]


  ## search
  seg_gr_1 <- seglm_search_grid(X=Xint, y= Y, th_var = th)
  seg_gr_2 <- seglm_search_grid(Xint, Y, th, nthresh = 2, trace = TRUE)
  seg_dyn_1 <- seglm_search_dynprog(Xint, Y, th, nthresh=1)
  seg_dyn_2 <- seglm_search_dynprog(Xint, Y, th, nthresh=2)




  ## check out: 1 nthresh
  seg_gr_1
  seg_dyn_1
  seglm_fit(X=X, y=Y, th_val = seg_gr_1$th, th_var = th) %>%  deviance

  ## check out: 1 nthresh
  seg_gr_2
  seg_dyn_2
  seglm_fit(X, Y, th_val = seg_gr_2$th, nthresh = 2, th_var = th) %>%  deviance
  seglm_fit(X, Y, th_val = seg_dyn_2$th, nthresh = 2, th_var = th) %>%  deviance

  ###
  out_1 <- seglm_fit(X, y = Y, th_val = seg_gr_1$th, nthresh = 1, th_var = th)
  out_2 <- seglm_fit(X, y = Y, th_val = seg_gr_2$th, nthresh = 2, th_var = th)

  out_1
  out_2

  summary(out_1)
  summary(out_2)

  coef(x=out_1, by_reg=TRUE)
  coef(out_2, by_reg=TRUE)

  #
  example(breakpoints, echo=FALSE)
  bp.seat <- breakpoints(y ~ ylag1 + ylag12, data = seatbelt, h = 0.1, breaks = 5)
  breakpoints(bp.seat, breaks=2)
  bp.seat
  ## methods
  a <- seglm.fit(X=Xint, y=Y, thVar="price index", trim=0.15)
  a$th
  as_data_frame(a$thGrid)
  b <- seglm_search_dynprog(X=X, y = Y, thVar = th, nthresh=1)
  # b$breakpoint$extract.breaks(b$breakpoint$RSS.table, breaks= 1)
  b$th
  b$RSS.table
  summary(b)
  seglm_search_dynprog(X=freeny.x, y = freeny.y, thVar = freeny.x[, "price index", drop=FALSE],
                     nthresh=1)

  ## quick lm check
  library(broom)
  freeny %>%
    mutate(regime = price.index<=a$th) %>%
    as_tibble %>%
    nest(-regime) %>%
    mutate(reg = map(data, ~lm(.) %>% tidy)) %>%
    unnest(reg) %>%
    select(regime, term, estimate) %>%
    spread(term, estimate) %>%
    print(digits=4)

  a$coefficients %>%
    matrix(nrow=2, byrow=TRUE)

  a$th
  plot(a)
  plot(a, type=2, var=3)
  deviance(a)
  a$thGrid[which.min(a$thGrid$SSR),]
  crossprod(residuals(a))

  cbind(freeny.x[,"price index"], a$model[,c(3,7)])

  ## join=TRUE
  aa <- seglm.fit(X=cbind(1, freeny.x[,2, drop=FALSE]),
                   y=freeny.y, thVar="price index", trim=0.15, join=TRUE)
  plot(aa, type=2, var=3)


  ## time series example:
  b <- seglm.fit(X=freeny.x[, "price index", drop=FALSE],
                  y=freeny.y, thVar=1:nrow(freeny), trim=0.15)
  b$th
  plot(b)
  library(strucchange)
  lm(y~.-1, data=freeny)
  lm(freeny.y~freeny.x-1)
  fr <- as.matrix(freeny.y)
  breakpoints(fr~freeny.x, h=0.2)

  br <- breakpoints(y~price.index-1,data=freeny, h=0.15, breaks=2)
  br$breakpoints

  ## use breakpoints for
  X <- freeny.x[, "price index", drop=FALSE]
  y <- freeny.y
  thVar <- X
  Xy <- cbind(X,y)[order(X),]
  br <- breakpoints(y~X, data=as.data.frame(Xy), breaks=2, h = 0.15)
  br$breakpoints
  Xy[27,]
  a$th

  ##



}
