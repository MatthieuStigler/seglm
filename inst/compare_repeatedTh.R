library(seglm)
library(strucchange)


## MC
N <-  500
x <- rnorm(N)
mean(x>0.5)
y <-  0.1 + ifelse(x<0.5, 0.7, 0.9)*x + rnorm(N)

df <- data.frame(x=x, y=y)

##
seglm_lm(y~x, data = df, th_var_name = "x")
seglm_lm(y~x, data = df, th_var_name = "x", th = 0.5)



