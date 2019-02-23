


#### Simulate
N <-  300
x1 <- rnorm(N)
th_val <-  c(-0.5, 0.5)
D <- as.integer(as.character(cut(x1, c(-Inf, th_val, Inf), include.lowest=TRUE, labels = c(1,2, 3))))

y <-  1.2 + 0.4 * x1 * (D==1) + 0.8 * x1 * (D==2) +  0.2 * x1 * (D==2)+ rnorm(N)
res <- data.frame(y=y, x = x1, regime  = D)
res

library(seglm)

seg_1 <- seglm(y~x, data=res, th_var_name = "x")
seg_2 <- seglm(y~x, data=res, th_var_name = "x", nthresh=2)
seg_4 <- seglm(y~x, data=res, th_var_name = "x", nthresh=4)
seg_5 <- seglm(y~x, data=res, th_var_name = "x", nthresh=5)

library("RcppDynProg")
Rc_dyn_1 <- solve_for_partition(res$x, res$y, penalty = 5.997758)
Rc_dyn_4 <- solve_for_partition(res$x, res$y, penalty = 5.5)
Rc_dyn_5 <- solve_for_partition(res$x, res$y, penalty = 5)


## 1 th
getTh(seg_1)
