#' @rdname seglm_search
#' @export
#' @import dplyr
seglm_search_tree <- function(X, y, th_var, nthresh=1,
                              trim=0.15){

  n_min <- round(length(y)*trim)

  ##Initial regression
  reg <- lm.fit(X,y)
  res <- residuals(reg)
  dat_th_res <- as.data.frame(cbind(th_var, res))

  ## get the tree
  tree <- rpart::rpart(res~., data=dat_th_res,
                       control=rpart.control(maxdepth=nthresh,
                                             minsplit=n_min*2,
                                             minbucket=n_min,
                                             cp=0))
  ths <- sort(as.data.frame(tree$splits)[,"index"])
  if(length(ths)!=nthresh){
    warning("rpart only estimated ", length(ths), " thresholds, sorry!")
    nthresh <- length(ths)
  }

  # compute SSR
  SSR <-  SSR_XY_th(X, y, th_val =  ths, th_var = th_var, nthresh = nthresh)

  ## export results
  res <- list()
  # res$index <- br_points
  res$th_val <- ths
  res$SSR <-  SSR
  class(res) <- c("seglm_search", "list")
  res
}


if(FALSE){
  library(seglm)
  data_thresh <- sim_thresh()
  X_inp <-  as.matrix(data_thresh[, "x", drop = FALSE])
  X_const <-  cbind(1, X_inp)
  y_inp <-  as.matrix(data_thresh[, "y"])

  ## 1 th
  seglm:::seglm_search_tree(X=X_const, y=y_inp, th_var = X_inp)
  seglm_search_grid(X=X_const, y=y_inp, th_var = X_inp)

  ## 2ths
  seglm:::seglm_search_tree(X=X_const, y=y_inp, th_var = X_inp, nthresh=2)
  seglm_search_grid(X=X_const, y=y_inp, th_var = X_inp, nthresh = 2)
  seglm_search_dynprog(X=X_const, y=y_inp, th_var = X_inp, nthresh = 2)

  ## 4 ths
  seglm_search_tree(X=X_const, y=y_inp, th_var = X_inp, nthresh=4)
  seglm_search_dynprog(X=X_const, y=y_inp, th_var = X_inp, nthresh = 4)

  ## alter: residuals then grid
  res <- residuals(lm.fit(X_const, y_inp))
  seglm_search_grid(X=matrix(1, nrow=nrow(data_thresh)),
                    y=res, th_var = X_inp)
}
