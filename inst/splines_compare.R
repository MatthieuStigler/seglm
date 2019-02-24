library(tidyverse)
library(seglm)
df <- sim_thresh(N=500) %>%
  as_tibble

df_M <-  cbind(1, df$x)
class(df_M)

model <- cbnd()

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

prep_Xany_continuous <-  function(X, th_val, th_var, nthresh = length(th_val),
                       clean_names = FALSE) {

  seg <- cut(th_var, breaks = c(-Inf, th_val, Inf),
             labels = 1:(nthresh +1))
  XX <-  model.matrix( ~ 0+ . : seg,
                       data = as.data.frame(X))
  if(clean_names) {
    colnames(XX) <- gsub(":seg", "_seg", colnames(XX))
    colnames(XX) <- gsub("`?\\(Intercept\\)`?_seg", "Intercept_seg", colnames(XX))
  }
  XX
}

prep_Xany_continuous(df_M)

prep_X_spline <-  function(X, th_val, th_var, nthresh) {
  if(nthresh == 1) {
    indic <-  ifelse(th_var <= th_val, 1, 0)
    X_L <- X #* indic
    plus_part <-  function(x) {
      x[x<0] <- 0
      x
    }
    X_H <- (X-th_val) * (1- indic)
    X_H <- plus_part(X-th_val)
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


X2 <- prep_X(df_M, 0, th_var = df_M[, 2], nthresh=1)
X2_s <- prep_X_spline(df_M[, 2, drop=FALSE], 0, th_var = df_M[, 2], nthresh=1)
X2_s_bs <- bs(df_M[, 2, drop=FALSE], knots = 0, degree=1, intercept = FALSE)
X2_s_bs_int <- bs(df_M[, 2, drop=FALSE], knots = 0, degree=1, intercept = TRUE)
X2_s_ns <- ns(df_M[, 2, drop=FALSE], knots = 0, intercept = TRUE)

head(X2_s)
head(X2_s_bs)
head(X2_s_bs_int)
head(X2_s_ns)

reg1 <- lm(df$y ~ -1+ X2)
reg2 <- lm(df$y ~  X2_s)
reg2_bs <- lm(df$y ~  X2_s_bs)
reg2_ns <- lm(df$y ~  -1+X2_s_ns)
reg2_bs_int <- lm(df$y ~  -1+ X2_s_bs_int)

reg2
reg2_bs
reg2_bs_int
reg2_ns

# same prediction!!
df_res <-  df %>%
  mutate(pred1 = predict(reg1),
         pred2 = predict(reg2),
         pred3 = predict(reg2_bs),
         pred4 = predict(reg2_bs_int),
         pred_ns = predict(reg2_ns))

df_res

ggplot(aes(x=x, y=y), data = df_res) +
  geom_point() +
  geom_line(aes(y = pred1), colour = 2) +
  geom_line(aes(y = pred2), colour = 3) +
  geom_line(aes(y = pred3), colour = 4)
