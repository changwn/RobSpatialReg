
# library(RobMixReg)
library(robustbase)
source('./Prof_pipeline3_roubst_lib.R')
source('./expr_simu_lib')

library(tictoc)

simu_list = list() # outside wrapper -> Rdata
set.seed(1234)

#-----sigma level--------
group_sig1 = list()
group_sig2 = list()
group_sig3 = list()
for(i in 1:100){
  dat_simu1 = simulateData_outlier_sig(n=c(40,40,20), beta=c(1.5,1.0), coordinate = c(1,1, -1,-1), sigma=0.05)
  group_sig1[[i]] = dat_simu1
  
  dat_simu1 = simulateData_outlier_sig(n=c(40,40,20), beta=c(1.5,1.0), coordinate = c(1,1, -1,-1), sigma=0.1)
  group_sig2[[i]] = dat_simu1
  
  dat_simu1 = simulateData_outlier_sig(n=c(40,40,20), beta=c(1.5,1.0), coordinate = c(1,1, -1,-1), sigma=0.2)
  group_sig3[[i]] = dat_simu1
}
simu_sig = list(group_sig1, group_sig2, group_sig3)
names(simu_sig) = c('sig=0.1', 'sig=0.2', 'sig=0.5')

#-----N--------
group_1 = list()
group_2 = list()
group_3 = list()
for(i in 1:100){
  dat_simu1 = simulateData_outlier_n(n=c(40,40,20), beta=c(1.5,1.0), coordinate = c(1,1, -1,-1))
  group_1[[i]] = dat_simu1
  
  dat_simu1 = simulateData_outlier_n(n=c(80,80,40), beta=c(1.5,1.0), coordinate = c(1,1, -1,-1))
  group_2[[i]] = dat_simu1
  
  dat_simu1 = simulateData_outlier_n(n=c(160,160,80), beta=c(1.5,1.0), coordinate = c(1,1, -1,-1))
  group_3[[i]] = dat_simu1
}
simu_n = list(group_1, group_2, group_3)
names(simu_n) = c('n=100', 'n=500', 'n=1000')


#----K-----
group_1 = list()
group_2 = list()
group_3 = list()
for(i in 1:100){
  dat_simu1 = simulateData_outlier_k(k=2)
  group_1[[i]] = dat_simu1
  
  dat_simu1 = simulateData_outlier_k(k=3)
  group_2[[i]] = dat_simu1
  
  dat_simu1 = simulateData_outlier_k(k=4)
  group_3[[i]] = dat_simu1
}
simu_k = list(group_1, group_2, group_3)
names(simu_k) = c('k=2', 'k=3', 'k=4')


#----Balance-----
group_1 = list()
group_2 = list()
group_3 = list()
for(i in 1:100){
  dat_simu1 = simulateData_outlier_bal(n=c(40,40,20), beta=c(1.5,1.0), coordinate = c(1,1, -1,-1), sigma=0.05)
  group_1[[i]] = dat_simu1
  
  dat_simu1 = simulateData_outlier_bal(n=c(30,50,20), beta=c(1.5,1.0), coordinate = c(1,1, -1,-1), sigma=0.05)
  group_2[[i]] = dat_simu1
  
  dat_simu1 = simulateData_outlier_bal(n=c(20,60,20), beta=c(1.5,1.0), coordinate = c(1,1, -1,-1), sigma=0.05)
  group_3[[i]] = dat_simu1
}
simu_bal = list(group_1, group_2, group_3)
names(simu_bal) = c('40_40', '30_50', '20_60')


# library(RobMixReg)
# library(robustbase)
# dat_simu1 = simu_bal[[3]][[100]]
# data = data.frame(dat_simu1$mat)
# xy = dat_simu1$xy
# cl = dat_simu1$cl
# plot(data, col=cl, pch=16)
# plot(xy, col=cl, pch=16)
# tic()
# RobSpa.res = RobSpaReg(formula = as.formula("y~x"), data, nit=20, nc=2, rlr_method="ltsReg", Cdn=xy, lamb=5)
# toc()
# RobSpa.res@inds_in
# RobSpa.res@indout
# RobSpa.res@ctleclusters[1:40]
# RobSpa.res@ctleclusters[41:80]
# RobSpa.res@ctleclusters[81:100]







