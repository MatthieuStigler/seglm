## from: https://www.rdocumentation.org/packages/RcppDynProg/versions/0.1.0/vignettes/Segmentation.Rmd
## https://www.rdocumentation.org/packages/RcppDynProg/versions/0.1.0
library("RcppDynProg")

plot <- requireNamespace("ggplot2", quietly = TRUE)
if(plot) {
  library("ggplot2")
}

set.seed(2018)
g <- 50
d <- data.frame(
  x = 1:(3*g)) # ordered in x
d$y_ideal <- c(rep(0, g), rep(1, g), rep(-1, g))
d$y_observed <- d$y_ideal + rnorm(length(d$y_ideal))


y_permuted <- d$y_ideal[sample.int(nrow(d), nrow(d), replace = FALSE)]


solve_with_penalty <- function(ycol, penalty) {
  n <- length(ycol)
  indices = seq_len(n)
  x <- const_costs(ycol, 1+numeric(n), 1, indices)
  x <- x + penalty
  solve_interval_partition(x, n)
}

lb <- 1
ub <- 10
while(length(solve_with_penalty(y_permuted, ub))>2) {
  ub <- ub*2
}
while(TRUE) {
  mid <- ceiling((ub+lb)/2)
  if(mid>=ub) {
    break
  }
  si <- solve_with_penalty(y_permuted, mid)
  if(length(si)<=2) {
    ub <- mid
  } else {
    lb <- mid
  }
}
print(ub)


soln <- solve_with_penalty(d$y_observed, ub)
print(soln)


d$group <- as.character(findInterval(d$x, soln))
group_means <- tapply(d$y_observed, d$group, mean)
d$group_mean <- group_means[d$group]

print(sum((d$y_observed - d$y_ideal)^2))

print(sum((d$group_mean - d$y_ideal)^2))

if(plot) {
  plt2 <- ggplot(data= d, aes(x = x)) +
    geom_line(aes(y = y_ideal), linetype=2) +
    geom_point(aes(y = y_observed, color = group)) +
    geom_line(aes(y = group_mean, color = group)) +
    ylab("y") +
    ggtitle("RcppDynProg piecewise constant estimate",
            subtitle = "dots: observed values, segments: observed group means, dashed line: unobserved true values") +
    theme(legend.position = "none")
  print(plt2)
}

## simpler
x_cuts <- solve_for_partition(d$x, d$y_observed, penalty = 1)
x_cuts_p5 <- solve_for_partition(d$x, d$y_observed, penalty = 5)


library(seglm)
segreg <- seglm(y_observed~x, data=d, th_var_name  ="x", nthresh=2)
segreg2 <- seglm(y_observed~1, data=d, th_var_name  ="x", nthresh=2)
segreg_nt5 <- seglm(y_observed~1, data=d, th_var_name  ="x", nthresh=5)
segreg2
d2 <- d
d2$pred <- predict(segreg2)


print(soln)
segreg2

ggplot(aes(x=x, y = pred), data =d2) +
  geom_line(aes(, color = group)) +
  geom_line(aes(y = y_ideal), linetype=2) +
  geom_point(aes(y = y_observed, color = group))


plt2 <- ggplot(data= d, aes(x = x)) +
  geom_line(aes(y = y_ideal), linetype=2) +
  geom_point(aes(y = y_observed, color = group)) +
  geom_line(aes(y = group_mean, color = group)) +
  ylab("y") +
  ggtitle("RcppDynProg piecewise constant estimate",
          subtitle = "dots: observed values, segments: observed group means, dashed line: unobserved true values") +
  theme(legend.position = "none")

