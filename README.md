# RobSpatialReg
Robust Spatial Mixture Regression

```
dat_simu1 = simulateData_outlier(n=c(40,40,20), beta=c(1.5,1), coordinate = c(1,1, -1,-1))
data1 = data.frame(dat_simu1$mat)
xy1 = dat_simu1$xy
cl1 = dat_simu1$cl

# 1:black, 2:red, 3:green, 4:blue, 5:lightblue
plot(data1, col=cl1, pch=16)
plot(xy1, col=cl1, pch=16)

library(RobMixReg)
library(robustbase)

#----------------------
RobSpa.res = RobSpaReg(formula = as.formula("y~x"), data1, nit=20, nc=2, rlr_method="ltsReg", Cdn=xy1, lamb=5)
RobSpa.res@inds_in
RobSpa.res@indout
RobSpa.res@ctleclusters[1:40]
RobSpa.res@ctleclusters[41:80]
RobSpa.res@ctleclusters[81:100]
cl[1:40]
cl[41:80]
cl[81:100]
```
