
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
  data <- as.data.frame(X)
  if("seg"%in%colnames(data)) stop("data cannot contain reserved variable name `seg")
  data$seg <- seg
  XX <-  model.matrix( ~ 0+ . : seg, data = data)
  XX <- XX[, -grep("^seg", colnames(XX))]
  if(clean_names) {
    colnames(XX) <- gsub(":seg", "_seg", colnames(XX))
    colnames(XX) <- gsub("`?\\(Intercept\\)`?_seg", "Intercept_seg", colnames(XX))
  }
  XX
}

SSR_XY <-  function(X, y) {
  c(crossprod(lm.fit(X, y)$residuals))
}


SSR_XY_th <-  function(X, y, th_val, th_var, nthresh) {
  X_dat <- prep_Xany(X, th_val=th_val, th_var= th_var, nthresh= nthresh)
  SSR_XY(X_dat, y)
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

getPerms <- function(x) {
  if (length(x) == 1) {
    res <- x
  }
  else {
    res <- matrix(nrow = 0, ncol = length(x))
    for (i in seq_along(x)) {
      res <- rbind(res, cbind(x[i], Recall(x[-i])))
    }
  }
  res2 <- as.data.frame(res)
  colnames(res2) <-  paste("break", 1:length(x), sep="")
  res2
}
