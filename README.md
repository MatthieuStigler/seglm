
<!-- README.md is generated from README.Rmd. Please edit that file -->
seglm
=====

The goal of seglm is to do a *segmented* regression. Segmented regression, also called *threshold* regression, estimates a piecewise linear regression. That is, it estimates a linear regression over multiples segments/regime. Location of the breapoints (aka thresholds) separating each segment is estimated simultaneously.

When the threshold variable is time, the model reduces to the well-known structural break/changepoint model.

Installation
------------

todo

``` r
install.packages("seglm")
```

Example
-------

The main function is `seglm()`, which uses the standard formula - data style, and specific arguments `th_var_name` and `nthresh`, indicating wich is the threshold variable, as well as how many threshols/breaks are to be estimated.

``` r
library(seglm)

## data randomly generated: true breakpoint is 0 
data_thresh <- sim_thresh()
seglm(formula = y~x, data = data_thresh, th_var = "x", nthresh =1)
#> Coefs:
#>               seg1      seg2
#> Intercept 1.099105 0.3848674
#> x         1.316637 0.9534091
#> 
#> Threshold: 0.4280042
```
