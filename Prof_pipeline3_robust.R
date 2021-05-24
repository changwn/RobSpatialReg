


dat_simu = simulateData_outlier(n=c(40,40,20), beta=c(1.5,1.3), coordinate = c(1,1, -1,-1))
data = data.frame(dat_simu$mat)
xy = dat_simu$xy
cl = dat_simu$cl

# 1:black, 2:red, 3:green, 4:blue, 5:lightblue
plot(data, col=cl, pch=16)
plot(xy, col=cl, pch=16)

# library(RobMixReg)
library(robustbase)

#----------------------
RobSpa.res = RobSpaReg(formula = as.formula("y~x"), data, nit=20, nc=2, rlr_method="ltsReg", Cdn=xy, lamb=5)
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


