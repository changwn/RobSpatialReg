#-------------------------------------
dat_simu1 = simulateData_outlier(n=c(40,40,20), beta=c(1.5,1.1), coordinate = c(1,1, -1,-1))
data = data.frame(dat_simu1$mat)
xy = dat_simu1$xy
cl = dat_simu1$cl

# 1:black, 2:red, 3:green, 4:blue, 5:lightblue
plot(data, col=cl, pch=16)
plot(xy, col=cl, pch=16)

RobSpa.res = RobSpaReg(formula = as.formula("y~x"), data, nit=20, nc=2, rlr_method="ltsReg", Cdn=xy, lamb=5)
RobSpa.res@inds_in
RobSpa.res@indout
RobSpa.res@ctleclusters[1:40]
RobSpa.res@ctleclusters[41:80]
RobSpa.res@ctleclusters[81:100]

#-------------------------------------

setwd('C:/Users/wnchang/Documents/F/PhD_Research/2020_12_02_spatial/spatial_regression')

dat_simu1 = simulateData_outlier(n=c(40,40,20), beta=c(1.5,1), coordinate = c(1,1, -1,-1))
data1 = data.frame(dat_simu1$mat)
xy1 = dat_simu1$xy
cl1 = dat_simu1$cl

# 1:black, 2:red, 3:green, 4:blue, 5:lightblue
plot(data1, col=cl1, pch=16)
plot(xy1, col=cl1, pch=16)

# library(RobMixReg)
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

#----------------------
RobSpa.res4 = RobSpaReg(formula = as.formula("y~x"), data, nit=20, nc=2, rlr_method="ltsReg", Cdn=xy, lamb=0)
RobSpa.res4@inds_in
RobSpa.res4@indout
RobSpa.res4@ctleclusters[1:40]
RobSpa.res4@ctleclusters[41:80]
RobSpa.res4@ctleclusters[81:100]
cl[1:40]
cl[41:80]
cl[81:100]

###########################
dat_simu2 = simulateData_outlier(n=c(40,40,20), beta=c(1.5,-1.3), coordinate = c(1,1, -1,-1))
data2 = data.frame(dat_simu2$mat)
xy2 = dat_simu$xy
cl2 = dat_simu$cl

# 1:black, 2:red, 3:green, 4:blue, 5:lightblue
plot(data2, col=cl2, pch=16)
plot(xy2, col=cl2, pch=16)

#----------------------
RobSpa.res2 = RobSpaReg(formula = as.formula("y~x"), data2, nit=20, nc=2, rlr_method="ltsReg", Cdn=xy2, lamb=5)
RobSpa.res2@inds_in
RobSpa.res2@indout
RobSpa.res2@ctleclusters[1:40]
RobSpa.res2@ctleclusters[41:80]
RobSpa.res2@ctleclusters[81:100]
cl2[1:40]
cl2[41:80]
cl2[81:100]

#----------------------
RobSpa.res3 = RobSpaReg(formula = as.formula("y~x"), data2, nit=20, nc=2, rlr_method="ltsReg", Cdn=xy2, lamb=0)
RobSpa.res3@inds_in
RobSpa.res3@indout
RobSpa.res3@ctleclusters[1:40]
RobSpa.res3@ctleclusters[41:80]
RobSpa.res3@ctleclusters[81:100]
cl2[1:40]
cl2[41:80]
cl2[81:100]

# save(dat_simu1, dat_simu2, file='dataset_simu.RData')

###########################
dat_simu3 = simulateData_outlier(n=c(40,40,20), beta=c(3.0,-0.3), coordinate = c(1,1, -1,-1))
data3 = data.frame(dat_simu3$mat)
xy3 = dat_simu$xy
cl3 = dat_simu$cl

# 1:black, 2:red, 3:green, 4:blue, 5:lightblue
plot(data3, col=cl3, pch=16)
plot(xy3, col=cl3, pch=16)

#----------------------
RobSpa.res5 = RobSpaReg(formula = as.formula("y~x"), data3, nit=20, nc=2, rlr_method="ltsReg", Cdn=xy3, lamb=5)
RobSpa.res5@inds_in
RobSpa.res5@indout
RobSpa.res5@ctleclusters[1:40]
RobSpa.res5@ctleclusters[41:80]
RobSpa.res5@ctleclusters[81:100]
cl2[1:40]
cl2[41:80]
cl2[81:100]

#----------------------
RobSpa.res6 = RobSpaReg(formula = as.formula("y~x"), data3, nit=20, nc=2, rlr_method="ltsReg", Cdn=xy3, lamb=0)
RobSpa.res6@inds_in
RobSpa.res6@indout
RobSpa.res6@ctleclusters[1:40]
RobSpa.res6@ctleclusters[41:80]
RobSpa.res6@ctleclusters[81:100]
cl2[1:40]
cl2[41:80]
cl2[81:100]
