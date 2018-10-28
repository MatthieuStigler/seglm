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