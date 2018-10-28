#' Low level function to search the thresholds
#'
#' @param X matrix of regressors
#' @param y matrix of the response
#' @param th_var the threshold variable.
#' @template param_nthresh
#' @template param_trim
#' @return An object of class "segreg_search" and "list"
#' @examples
#' add(1, 1)




segReg_search_dynprog <- function(X, y, th_var, th, nthresh=1, trim=0.15){

  require(strucchange)

  ##
  N <- nrow(X)
  K <- ncol(X)
  if( (N * (1-2*trim))/(nthresh+1) < K) warning ("N too small")

  y <- as.matrix(y)

  ##
  th_var_order <- order(th_var)
  th_var_ordered <- th_var[th_var_order]
  # X_ordrd <- X[th_var_order,]
  # y_ordrd <- y[th_var_order,, drop=FALSE]


  # trim arg
  th_var_s <- sort(unique(th_var))
  n_th <- length(th_var_s)
  # if(n_th!= N) warning("Identical values in th_var, sorting is not unique")
  down <- ceiling(trim*n_th)
  up <- floor(n_th*(1-trim))
  allin<- (down-K+1):(up+K-1)
  allin <- (down):(up)
  n_in <- length(allin)
  if(n_in/nthresh < K) warning("Data too small/trim too large")

  ## alter
  if(any(grepl(pattern = " ", colnames(X)))) {
    colnames(X) <- gsub(" ", "_", colnames(X))
  }
  Xy <- (data.frame(y=y, X)[th_var_order,])#[allin,]

  ## formu
  formu <-  reformulate(colnames(X), response="y")
  n_min <- max(ceiling(trim*n_th), K+1)

  # fixInNamespace(breakpoints.formula, "strucchange")
  br_first <- breakpoints(formula = formu, data=Xy, breaks=nthresh, h= NULL)
  br <- breakpoints(br_first, breaks=nthresh)
  br_points <- br$breakpoints
  if(all(is.na(br_points))) warning("br is NA")

  RSS.table <- as.data.frame(br_first$RSS.table) %>% as_tibble()
  RSS.table$is_min <- if(nthresh == 1) {
    RSS.table$index == br_points
  } else if(nthresh == 2) {
    RSS.table$break1 == br_points[1] & RSS.table$break2 == br_points[2] | RSS.table$break1 == br_points[2] & RSS.table$break2 == br_points[1]
  }

  if(nthresh == 1) {
    RSS.table <-  RSS.table %>%
      mutate(th = th_var[th_var_order[index]])
  } else if (nthresh == 2) {
    RSS.table <-  RSS.table %>%
      mutate(th1 = th_var[th_var_order[break1]],
             th2 = th_var[th_var_order[break2]])

  }
  #   mutate(th = th_var[th_var_order[index]]) %>%
  #   select(th, index, break.RSS)
  # RSS.table$th <-

  ## export results
  res <- list()
  res$index <- br$breakpoints
  res$th <- th_var_ordered[br$breakpoints]
  res$RSS.table <- RSS.table
  # res$breakpoint <- br
  res$SSR <-  NA
  class(res) <- c("segreg_search", "list")
  res
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



segReg_search_grid <- function(X, y, th_var, th, nthresh=1,
                               trim=0.15,
                               iter = TRUE, trace = FALSE, max.iter = 3,
                               return_details = FALSE){


  thVar <-  as.vector(th_var)

  ##
  N <- nrow(X)
  K <- ncol(X)
  if( (N * (1-2*trim))/(nthresh+1) < K) warning ("N to small")

  y <- as.matrix(y)

  # trim arg
  thVar_s <- sort(unique(thVar))
  n_th <- length(thVar_s)

  n_min <-  if(trim <1)  ceiling(trim * N) else trim


  # down <- ceiling(trim*n_th)
  # up <- floor(n_th*(1-trim))
  # allin <- (down):(up)
  #
  # n_in <- length(allin)
  # if(n_in/nthresh < K) warning("Data too small/trim too large")

  SSR_XY <-  SSR_1value <-  function(X, y) {
    c(crossprod(lm.fit(X, y)$residuals))
  }

  SSR_1value <-  function(X, y, th_val, th_var, nthresh) {
    X_dat <- prep_X(X, th_val=th_val, th_var= th_var, nthresh= nthresh)
    SSR_XY(X_dat, y)
  }

  condi_step <-  function(df, n_row_best = NULL, th_best = NULL) {

    if(is.null(n_row_best)) n_row_best <-  which.min(df$SSR)
    if(is.null(th_best)) th_best <-  df[n_row_best, "thresh"]

    ## define feasible points
    if(n_row_best - n_min > n_min) {
      df$step_2 <- df$n_row %in% n_min : (n_row_best - n_min )
    }
    if(n_row_best + n_min < n_th - n_min) {
      df$step_2 <- df$step_2 | df$n_row %in% (n_row_best + n_min) : ( n_th - n_min)
    }

    df

    for(i in 1:n_th) {
      if(df[i, "step_2"]) {

        df[i, "SSR_2"] <-  SSR_1value(X=X, y=y, th_val = sort(c(th_best, df[i,]$thresh)),
                                      th_var = thVar, nthresh = 2)
      } else {
        df[i, "SSR_2"] <-  NA
      }
    }
    df
  }

  SSR_df <- data.frame(n_row = 1:n_th,
                       thresh = thVar_s,
                       trim = 1:n_th %in% n_min: (n_th - n_min +1),
                       SSR = NA)


  for(i in 1:n_th) {
    if(SSR_df[i, "trim"]) {
      SSR_df[i, "SSR"] <-  SSR_1value(X=X, y=y, th_val = SSR_df[i,]$thresh, th_var = thVar, nthresh = 1)
    }
  }

  SSR_df


  ## select best
  SSR_best <-  SSR_df[which.min(SSR_df$SSR),]


  if(nthresh == 2) {
    if(trace) cat("Best for nthresh=1:", SSR_best$th, "\n")
    old_best <- SSR_best
    SSR_df <-  condi_step(df=SSR_df, n_row_best = old_best$n_row)
    new_best <- SSR_df[which.min(SSR_df$SSR_2),]
    th_best <- sort(c(old_best$thresh, new_best$thresh))
    if(trace) cat("Best for nthresh=2, given th = ", old_best$thresh, ":", new_best$thresh, "\n")

    for(i in 1:max.iter) {
      old_best <- new_best
      SSR_df <-  condi_step(df=SSR_df, n_row_best = old_best$n_row)
      new_best <- SSR_df[which.min(SSR_df$SSR_2),]
      if(all(sort(c(old_best$thresh, new_best$thresh))==sort(th_best))) {
        th_best <- sort(c(old_best$thresh, new_best$thresh))
        if(trace) cat("Best for nthresh=2, iter", i, "given th = ", old_best$thresh, ":", new_best$thresh,
                      ". Converged, exit iteration, \n")
        break()
      }
      th_best <- sort(c(old_best$thresh, new_best$thresh))
      if(trace) cat("Best for nthresh=2, iter", i, "given th = ", old_best$thresh, ":", new_best$thresh, "\n")
    }

    # th_best
    SSR_best <-  SSR_df[which.min(SSR_df$SSR_2),"SSR_2"]
    # th_best <- sort(c(SSR_best$thresh, SSR_best2$thresh))
    # SSR_best <- SSR_best2$SSR_2
  } else {
    th_best <- SSR_best$thresh
    SSR_best <-  SSR_best$SSR
  }

  ## export results
  res <- list()
  res$th <- th_best
  res$SSR <-  SSR_best
  if(return_details) res$details <- SSR_df
  class(res) <- c("segreg_search", "list")
  res
}


print.segreg_search <-  function(x) {
  cat(paste("th:", x$th), "\n")
  cat(paste("SSR:", x$SSR), "\n")
}

