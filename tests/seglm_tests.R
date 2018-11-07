library(seglm)

set.seed(123)
data_thresh <- sim_thresh()
head(data_thresh)
f <-  y ~ x

out_1 <- seglm(formula = y ~ x, data=data_thresh, th_var_name = "x", nthresh = 1)
out_2 <- seglm(formula = y ~ x, data=data_thresh, th_var_name = "x", nthresh = 2)
# out_3 <- seglm(formula = y ~ x, data=data_thresh, th_var_name = "x", nthresh = 3)

out_time <- seglm(formula = y ~ x, data=data_thresh, th_var_name = ".time", nthresh = 2)


## together
out_list <-  list(out_1 = out_1,
                  out_2 = out_2,
                  out_time = out_time)

a <-  lapply(out_list, print)
lapply(out_list, coef)
lapply(out_list, summary)
sapply(out_list, deviance)
sapply(out_list, AIC)


