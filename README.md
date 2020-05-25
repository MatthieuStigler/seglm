
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
<!-- badges: end -->

# seglm

The goal of seglm is to do a *segmented* regression. Segmented
regression, also called *threshold* regression, estimates a piecewise
linear regression. That is, it estimates a linear regression over
multiples segments/regime. Location of the breapoints (aka thresholds)
separating each segment is estimated simultaneously.

When the threshold variable is time, the model reduces to the well-known
structural break/changepoint model.

## Installation

``` r
devtools::install_github("MatthieuStigler/seglm")
```

## Example

The main function is `seglm()`, which uses the standard formula - data
style, and specific arguments `th_var_name` and `nthresh`, indicating
wich is the threshold variable, as well as how many thresholds/breaks
are to be estimated.

``` r
library(seglm)

## data randomly generated: true breakpoint is 0 
data_thresh <- sim_thresh()
seglm_lm(formula = y~x, data = data_thresh, th_var = "x", nthresh =1)
#> Coefs:
#>                 seg1      seg2
#> Intercept 0.71983097 1.4209476
#> x         0.08262137 0.4669618
#> 
#> Threshold: -0.6502997
```
