context("test-seglm_search")

set.seed(123)
data_thresh <- sim_thresh()
X <-  as.matrix(data_thresh[, "x", drop = FALSE])
y <-  as.matrix(data_thresh[, "y"])


seg_dyn_1 <- seglm_search_dynprog(X=X, y=y, th_var = X)
seg_grd_1 <- seglm_search_grid(X=X, y=y, th_var = X)

seg_dyn_2 <- seglm_search_dynprog(X=X, y=y, th_var = X, nthresh=2)
seg_grd_2 <- seglm_search_grid(X=X, y=y, th_var = X, nthresh=2)


test_that("same th", {
  expect_equal(seg_dyn_1$th, seg_grd_1$th)
  expect_equal(deviance(seg_dyn_1), deviance(seg_grd_1))
  expect_equal(seg_dyn_2$th, seg_grd_2$th)
})
