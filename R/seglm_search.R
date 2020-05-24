#' Low level function to search the thresholds
#'
#' @param X matrix of regressors
#' @param y matrix of the response
#' @param th_var the threshold variable.
#' @template param_nthresh
#' @template param_trim
#' @param algorithm The type of algorithm used.
#' @param ... Further arguments passed to the underlying breakpoints function
#' @return An object of class "seglm_search" and "list".
#' This contains mainly the slot \code{th} with the threshold estimates, and the slot \code{SSR} with the resulting SSR.
#' @examples
#' data_thresh <- sim_thresh()
#' X_inp <-  as.matrix(data_thresh[, "x", drop = FALSE])
#' y_inp <-  as.matrix(data_thresh[, "y"])
#' seglm_search_dynprog(X=X_inp, y=y_inp, th_var = X_inp)
#' seglm_search_grid(X=X_inp, y=y_inp, th_var = X_inp)
#' @export

seglm_search <- function(X, y, th_var, nthresh=1, trim=0.15, algorithm = c("grid", "dynprog"), ...){
  algorithm <-  match.arg(algorithm)

  if(algorithm=="grid" & nthresh>2) stop("Algorithm `grid` works only for nthresh %in% 1,2")
  if(algorithm=="grid") {
    res <- seglm_search_grid(X=X, y = y, nthresh = nthresh, th_var=th_var, trim=trim, ...)
  } else {
    res <- seglm_search_dynprog(X=X, y = y, nthresh = nthresh, th_var=th_var, trim=trim, ...)
  }
  res
}

#' @rdname seglm_search
#' @param RSS.table Whether to return the RSS.table, contiaining the underlying RSS computations.
#' @importFrom strucchange breakpoints
#' @importFrom stats reformulate lm lm.fit
#' @export
seglm_search_dynprog <- function(X, y, th_var, nthresh=1, trim=0.15, RSS.table = FALSE, ...){

  if(!requireNamespace("strucchange", quietly = TRUE)) {
    stop("Package 'strucchange' needed for this function to work. Please install it.",
         call. = FALSE)
  }

  #
  y <- as.matrix(y)
  if(!is.matrix(X)) stop("X should be a matrix")
  if(is.null(colnames(X))) colnames(X) <- paste("x", 1:ncol(X), sep="_")


  ##
  N <- nrow(X)
  K <- ncol(X)
  if( (N * (1-2*trim))/(nthresh+1) < K) warning ("N too small")




  ##
  th_var_order <- order(th_var)
  th_var_ordered <- th_var[th_var_order]
  X_ordrd <- X[th_var_order,, drop=FALSE]
  y_ordrd <- y[th_var_order,, drop=FALSE]


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
  # if(any(grepl(pattern = " ", colnames(X)))) {
  #   colnames(X) <- gsub(" ", "_", colnames(X))
  # }
  # Xy <- (data.frame(y=y, X)[th_var_order,])#[allin,]
  # colnames(Xy) <- c("y", colnames(X))
  #
  # # ## formu
  # formu <-  reformulate(colnames(X), response="y")
  # n_min <- max(ceiling(trim*n_th), K+1)

  # fixInNamespace(breakpoints.formula, "strucchange")
  # br_first <- strucchange::breakpoints(formula = formu, data=Xy, breaks=nthresh, h= NULL)
  # br <- strucchange::breakpoints(br_first, breaks=nthresh)
  br <- breakpoints.manual(X=X_ordrd, y=y_ordrd, breaks=nthresh, h= NULL)

  br_points <- br$breakpoints
  if(all(is.na(br_points))) warning("br is NA")

  if(RSS.table) {
    RSS.table <- as.data.frame(br$RSS.table)
    if(nthresh ==1)   colnames(RSS.table) <- c("break1", "RSS1")
    row_good <- RSS.table %>%
      mutate(n_row = 1:n()) %>%
      semi_join(getPerms(br_points), by = paste("break", 1:nthresh, sep=""))
    RSS.table <- RSS.table %>%
      mutate(is_min = 1:n() %in% row_good$n_row)
    # if(nthresh == 1) {
    #   RSS.table <-  RSS.table %>%
    #     mutate(th = th_var[th_var_order[.data$index]])
    # } else if (nthresh == 2) {
    #   RSS.table <-  RSS.table %>%
    #     mutate(th1 = th_var[th_var_order[.data$break1]],
    #            th2 = th_var[th_var_order[.data$break2]])

  } else {
    RSS.table <-  NULL
  }

  # compute SSR (ideally would just retrieve from the grid...)
  th <- th_var_ordered[br_points]
  SSR <-  SSR_XY_th(X, y, th_val =  th, th_var = th_var, nthresh = nthresh)

  ## export results
  res <- list()
  res$index <- br_points
  res$th_val <- th
  res$RSS.table <- RSS.table
  res$SSR <-  SSR
  class(res) <- c("seglm_search", "list")
  res
}


#' @rdname seglm_search
#' @param iter,max.iter,trace,return_details arguments to set the number of iterations, as well return_details
#' @param n_points number of points in the grid. By default (NULL) takes all values, excluding the trimming.
#' @export
#' @import dplyr
seglm_search_grid <- function(X, y, th_var, nthresh=1,
                              trim=0.15,
                              n_points = NULL,
                              iter = TRUE, trace = FALSE, max.iter = 3,
                              return_details = FALSE){

  thVar <-  as.vector(th_var)
  if(nthresh>2) stop("Works only for nthresh %in% 1,2")

  ##
  if(!is.matrix(X)){
    X <- as.matrix(X)
    warning("Converting X to matrix")
  }
  N <- nrow(X)
  K <- ncol(X)
  if( (N * (1-2*trim))/(nthresh+1) < K) warning ("N too small")

  y <- as.matrix(y)

  # Various args trim arg
  thVar_s <- sort(unique(thVar))
  n_th <- length(thVar_s)
  n_min <-  if(trim <1)  ceiling(trim * N) else trim
  n_grid <- if(is.null(n_points)) n_th else n_points # number of points to search over


  # down <- ceiling(trim*n_th)
  # up <- floor(n_th*(1-trim))
  # allin <- (down):(up)
  #
  # n_in <- length(allin)
  # if(n_in/nthresh < K) warning("Data too small/trim too large")

  SSR_XY <-  function(X, y) {
    c(crossprod(lm.fit(X, y)$residuals))
  }

  SSR_1value <-  function(X, y, th_val, th_var, nthresh) {
    X_dat <- prep_X(X, th_val=th_val, th_var= th_var, nthresh= nthresh)
    SSR_XY(X_dat, y)
  }



  ## funciton for conditional step
  condi_step <-  function(df, n_row_best = NULL, th_best = NULL) {

    if(is.null(n_row_best)) n_row_best <-  which.min(df$SSR)
    if(is.null(th_best)) th_best <-  df[n_row_best, "thresh"]

    ## define feasible points
    df$step_2 <-  FALSE
    if(n_row_best - n_min > n_min) {
      df$step_2 <- df$n_row %in% n_min : (n_row_best - n_min )
    }
    if(n_row_best + n_min < n_th - n_min) {
      df$step_2 <- df$step_2 | df$n_row %in% (n_row_best + n_min) : ( n_th - n_min)
    }

    df

    for(i in seq(1, n_th, length.out = n_grid)) {
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


  for(i in seq(1, n_th, length.out = n_grid)) {
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

    SSR_best <-  SSR_df[which.min(SSR_df$SSR_2),"SSR_2"]
    # th_best <- sort(c(SSR_best$thresh, SSR_best2$thresh))
    # SSR_best <- SSR_best2$SSR_2
  } else {
    th_best <- SSR_best$thresh
    SSR_best <-  SSR_best$SSR
  }

  ## export results
  res <- list()
  res$th_val <- th_best
  res$SSR <-  SSR_best
  if(return_details) res$details <- SSR_df
  class(res) <- c("seglm_search", "list")
  res
}



#' @param x  object of class *seglm_search*
#' @rdname seglm_search
#' @export
print.seglm_search <-  function(x, ...) {
  cat(paste("th:", x$th), "\n")
  cat(paste("SSR:", x$SSR), "\n")
}

#' @param object  object of class *seglm_search*
#' @rdname seglm_search
#' @export
deviance.seglm_search <-  function(object, ...) {
  object$SSR
}
