
## one formulation can be using contunuous regression
## for 1X, same th_var (see Greene 7.2.4, 7.2.5):
##   - only one intercept
##   - recenter x_H at threshold
## this should be equivalent to pkg segmented
## what about 2 X?
##   - is it meaningful recenter X2 at th (x1)? TO THINK!!

prep_Xany_conti <-  function(X, th_val, th_var, nthresh = length(th_val),
                       clean_names = FALSE) {

  # seg <- cut(th_var, breaks = c(-Inf, th_val, Inf),
  #            labels = 1:(nthresh +1))
  piece.formula <- function(var.name, th_val) {
    formula.sign <- rep(" - ", length(th_val))
    formula.sign[th_val < 0] <- " + "
    paste(var.name, "+",
          paste("I(pmax(", var.name, formula.sign, abs(th_val), ", 0))",
                collapse = " + ", sep=""))
  }
  formu <- formula(paste(" ~", piece.formula(colnames(X), th_val)))
  XX <-  model.matrix(formu, data = as.data.frame(X))
  if(clean_names) {
    colnames(XX) <- gsub(":seg", "_seg", colnames(XX))
    colnames(XX) <- gsub("`?\\(Intercept\\)`?_seg", "Intercept_seg", colnames(XX))
  }
  XX
}

df <- sim_thresh()
X <- prep_Xany_conti(X=as.matrix(df[, "x", drop = FALSE]), th_var=as.matrix(df$x), th_val = 0)
nrow(X)

length(df$y)
lm(df$y ~ -1 + X)


## alternative 2
library(fda)

basisobj <- create.polygonal.basis(seq(0,1,0.1))

plot(basisobj)
