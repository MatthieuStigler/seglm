library(segmentedReg)

set.seed(123)
data_thresh <- sim_thresh()
X <-  as.matrix(data_thresh[, "x", drop = FALSE])
y <-  as.matrix(data_thresh[, "y"])

## 1 th
seg_dyn_1 <- segReg_search_dynprog(X=X, y=y, th_var = X)
seg_grd_1 <- segReg_search_grid(X=X, y=y, th_var = X)

seg_dyn_1_tr <- segReg_search_dynprog(X=X, y=y, th_var = X, trim = 0.1)
seg_grd_1_tr <- segReg_search_grid(X=X, y=y, th_var = X, trim = 0.1)

seg_dyn_1
seg_grd_1

seg_dyn_1_tr
seg_grd_1_tr

## 2 th
seg_dyn_2 <- segReg_search_dynprog(X=X, y=y, th_var = X, nthresh = 2)
seg_grid_2 <- segReg_search_grid(X=X, y=y, th_var = X, nthresh = 2)

seg_dyn_2
seg_grid_2
