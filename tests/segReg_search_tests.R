library(seglm)

set.seed(123)
data_thresh <- sim_thresh()
X <-  as.matrix(data_thresh[, "x", drop = FALSE])
y <-  as.matrix(data_thresh[, "y"])

## 1 th
seg_dyn_1 <- seglm_search_dynprog(X=X, y=y, th_var = X)
seg_grd_1 <- seglm_search_grid(X=X, y=y, th_var = X)

seg_dyn_1_tr <- seglm_search_dynprog(X=X, y=y, th_var = X, trim = 0.1)
seg_grd_1_tr <- seglm_search_grid(X=X, y=y, th_var = X, trim = 0.1)

seg_dyn_1
seg_grd_1

seg_dyn_1_tr
seg_grd_1_tr

## 2 th
seg_dyn_2 <- seglm_search_dynprog(X=X, y=y, th_var = X, nthresh = 2)
seg_grd_2 <- seglm_search_grid(X=X, y=y, th_var = X, nthresh = 2)

seg_dyn_2
seg_grd_2

## all in list
seg_search_all <-  list(seg_dyn_1 = seg_dyn_1,
                        seg_dyn_2 = seg_dyn_2,
                        seg_dyn_1_tr = seg_dyn_1_tr,
                        seg_grd_1 = seg_grd_1,
                        seg_grd_2 = seg_grd_2,
                        seg_grd_1_tr = seg_grd_1_tr)


sapply(seg_search_all, deviance)

## trace
seg_grid_2 <- seglm_search_grid(X=X, y=y, th_var = X, nthresh = 2, trace = TRUE)

## fit
seg_dyn_1_fit <- seglm_fit(X=X, y=y, th_val = seg_dyn_1$th, th_var = X)
seg_grd_1_fit <- seglm_fit(X=X, y=y, th_val = seg_grd_1$th, th_var = X)

seg_dyn_2_fit <- seglm_fit(X=X, y=y, th_val = seg_dyn_2$th, th_var = X, nthresh = 2)
seg_grd_2_fit <- seglm_fit(X=X, y=y, th_val = seg_grd_2$th, th_var = X, nthresh = 2)

seg_grd_1_fit

seg_fit_all <-  list(seg_dyn_1_fit = seg_dyn_1_fit,
                     seg_dyn_2_fit = seg_dyn_2_fit,
                     seg_grd_1_fit = seg_grd_1_fit,
                     seg_grd_2_fit = seg_grd_2_fit)


sapply(seg_fit_all, print)
sapply(seg_fit_all, deviance)
sapply(seg_fit_all, coef)
sapply(seg_fit_all, function(x) coef(summary(x)))
