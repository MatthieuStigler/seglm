breakpoints.manual <- function(X, y, h = 0.15, breaks = NULL, hpc = c("none", "foreach"), ...)
{


  n <- nrow(X)
  k <- ncol(X)
  intercept_only <- isTRUE(all.equal(as.vector(X), rep(1L, n)))
  if(is.null(h)) h <- k + 1
  if(h < 1) h <- floor(n*h)
  if(h <= k)
    stop("minimum segment size must be greater than the number of regressors")
  if(h > floor(n/2))
    stop("minimum segment size must be smaller than half the number of observations")
  if(is.null(breaks)) {
    breaks <- ceiling(n/h) - 2
  } else {
    if(breaks > ceiling(n/h) - 2) {
      breaks0 <- breaks
      breaks <- ceiling(n/h) - 2
      warning(sprintf("requested number of breaks = %i too large, changed to %i", breaks0, breaks))
    }
  }

  hpc <- match.arg(hpc)
  if(hpc == "foreach" && !requireNamespace("foreach")) {
    warning("High perfomance computing (hpc) support with 'foreach' package is not available, foreach is not installed.")
    hpc <- "none"
  }

  ## compute ith row of the RSS diagonal matrix, i.e,
  ## the recursive residuals for segments starting at i = 1:(n-h+1)

  RSSi <- function(i)
  {
    ssr <- if(intercept_only) {
      (y[i:n] - cumsum(y[i:n])/(1L:(n-i+1L)))[-1L] * sqrt(1L + 1L/(1L:(n-i)))
    } else {
      strucchange::recresid(X[i:n,,drop = FALSE],y[i:n])
    }
    c(rep(NA, k), cumsum(ssr^2))
  }

  ## employ HPC support if available/selected
  RSS.triang <- if(hpc == "none") sapply(1:(n-h+1), RSSi) else foreach::foreach(i = 1:(n-h+1)) %dopar% RSSi(i)

  ## function to extract the RSS(i,j) from RSS.triang
  RSS <- function(i,j) RSS.triang[[i]][j - i + 1]

  ## compute optimal previous partner if observation i is the mth break
  ## store results together with RSSs in RSS.table

  ## breaks = 1

  index <- h:(n-h)
  break.RSS <- sapply(index, function(i) RSS(1,i))

  RSS.table <- cbind(index, break.RSS)
  rownames(RSS.table) <- as.character(index)

  ## breaks >= 2

  extend.RSS.table <- function(RSS.table, breaks)
  {
    if((breaks*2) > ncol(RSS.table)) {
      for(m in (ncol(RSS.table)/2 + 1):breaks)
      {
        my.index <- (m*h):(n-h)
        my.RSS.table <- RSS.table[,c((m-1)*2 - 1, (m-1)*2)]
        my.RSS.table <- cbind(my.RSS.table, NA, NA)
        for(i in my.index)
        {
          pot.index <- ((m-1)*h):(i - h)
          break.RSS <- sapply(pot.index, function(j) my.RSS.table[as.character(j), 2] + RSS(j+1,i))
          opt <- which.min(break.RSS)
          my.RSS.table[as.character(i), 3:4] <- c(pot.index[opt], break.RSS[opt])
        }
        RSS.table <- cbind(RSS.table, my.RSS.table[,3:4])
      }
      colnames(RSS.table) <- as.vector(rbind(paste("break", 1:breaks, sep = ""),
                                             paste("RSS", 1:breaks, sep = "")))
    }
    return(RSS.table)
  }

  RSS.table <- extend.RSS.table(RSS.table, breaks)

  ## extract optimal breaks

  extract.breaks <- function(RSS.table, breaks)
  {
    if((breaks*2) > ncol(RSS.table)) stop("compute RSS.table with enough breaks before")
    index <- RSS.table[, 1, drop = TRUE]
    break.RSS <- sapply(index, function(i) RSS.table[as.character(i),breaks*2] + RSS(i + 1, n))
    opt <- index[which.min(break.RSS)]
    if(breaks > 1) {
      for(i in ((breaks:2)*2 - 1))
        opt <- c(RSS.table[as.character(opt[1]),i], opt)
    }
    names(opt) <- NULL
    return(opt)
  }

  opt <- extract.breaks(RSS.table, breaks)

  RVAL <- list(breakpoints = opt,
               RSS.table = RSS.table,
               RSS.triang = RSS.triang,
               RSS = RSS,
               extract.breaks = extract.breaks,
               extend.RSS.table = extend.RSS.table,
               nobs = n,
               nreg = k,
               y = y,
               X = X,
               call = match.call())
  class(RVAL) <- c("breakpointsfull", "breakpoints")
  RVAL$breakpoints <- strucchange:::breakpoints.breakpointsfull(RVAL, breaks = breaks)$breakpoints
  return(RVAL)
}
