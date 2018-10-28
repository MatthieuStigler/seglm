
#' @importFrom stats model.matrix

prep_X <-  function(X, th_val, th_var, nthresh) {
  if(nthresh == 1) {
    indic <-  ifelse(th_var <= th_val, 1, 0)
    X_L <- X * indic
    X_H <- X * (1- indic)
    res <-  cbind(X_L, X_H)
  } else {
    indic_L <-  ifelse(th_var <= th_val[1], 1, 0)
    indic_H <-  ifelse(th_var >  th_val[2], 1, 0)
    X_L <- X * indic_L
    X_M <- X * (1- indic_L - indic_H)
    X_H <- X * indic_H
    res <-  cbind(X_L, X_M, X_H)
  }
  res
}

prep_Xany <-  function(X, th_val, th_var, nthresh = length(th_val),
                       clean_names = FALSE) {

  seg <- cut(th_var, breaks = c(-Inf, th_val, Inf),
             labels = 1:(nthresh +1))
  XX <-  model.matrix( ~ 0+ . : seg,
                       data = as.data.frame(X))
  if(clean_names) {
    colnames(XX) <- gsub(":seg", "_seg", colnames(XX))
  }
  XX
}


get_mat_dim <- function(th_var, nthresh=1, trim=0.15){

  th_var_unique <-  sort(unique(th_var))


  n_unique <-  length(th_var_unique)
  n_min <- ceiling(n_unique * trim)

  ##
  n_unique <-  25
  n_min <-  5

  num_unique <-  1:n_unique
  lapply(num_unique, function(x)  (min(x+n_min, n_unique) : n_unique))



  M <- matrix(NA, n_unique, n_unique)
  colnames(M) <-  rownames(M) <- th_var_unique


  M[1:5, 1:5]


}
