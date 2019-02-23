


#### Simulate
N <-  300
set.seed(123)
x1 <- rnorm(N)
th_val <-  c(-0.5, 0.5)
D <- as.integer(as.character(cut(x1, c(-Inf, th_val, Inf), include.lowest=TRUE, labels = c(1,2, 3))))

y <-  1.2 + 0.4 * x1 * (D==1) + 0.8 * x1 * (D==2) +  0.2 * x1 * (D==2)+ rnorm(N)
res <- data.frame(y=y, x = x1, regime  = D)
res

library(seglm)

seg_1 <- seglm(y~x, data=res, th_var_name = "x")
seg_2 <- seglm(y~x, data=res, th_var_name = "x", nthresh=2)
seg_3 <- seglm(y~x, data=res, th_var_name = "x", nthresh=3)
seg_4 <- seglm(y~x, data=res, th_var_name = "x", nthresh=4)
seg_5 <- seglm(y~x, data=res, th_var_name = "x", nthresh=5)

seglm_sol = list(th1 = seg_1,
                 th2 = seg_1,
                 th3 = seg_3,
                 th4 = seg_4,
                 th5 = seg_5)

library("RcppDynProg")
Rc_dyn_1 <- solve_for_partition(res$x, res$y, penalty = 5.997759)
Rc_dyn_1b <- solve_for_partition(res$x, res$y, penalty = 4)
# Rc_dyn_4 <- solve_for_partition(res$x, res$y, penalty = 4)
Rc_dyn_4 <- solve_for_partition(res$x, res$y, penalty = 3.49)

Rc_dyn_1

Rc_dyn = list(th1 = Rc_dyn_1,
              # th2 = seg_1,
              # th3 = seg_3,
              th4 = Rc_dyn_4)


get_th.data.frame <- function(x) {
  n_th <- max(x$group)
  if(n_th==1) n_th <-  Inf
  x_sub <- subset(x, what=="right" & group!=n_th)
  x_sub$x
}

## 1 th
get_th(seg_4)
get_th(Rc_dyn_4)


## re-estimate
seg_fin_seg <- seglm(y~x, data=res, th_var_name = "x", th = get_th(seg_4), nthresh=4)
seg_fin_rc_dyn <- seglm(y~x, data=res, th_var_name = "x", th = get_th(Rc_dyn_4), nthresh=4)

deviance(seg_fin_seg)
deviance(seg_fin_rc_dyn)
