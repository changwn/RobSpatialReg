
# library(RobMixReg)
library(robustbase)
source('./Prof_pipeline3_roubst_lib.R')
source('./expr_simu_lib')

library(tictoc)

# simu_list = list() # outside wrapper -> Rdata
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


#-----beta (coefficient)--------
group_1 = list()
group_2 = list()
group_3 = list()
for(i in 1:100){
  dat_simu1 = simulateData_outlier_sig(n=c(40,40,20), beta=c(1.5,1.0), coordinate = c(1,1, -1,-1), sigma=0.05)
  group_1[[i]] = dat_simu1
  
  dat_simu1 = simulateData_outlier_sig(n=c(40,40,20), beta=c(1.5,0.1), coordinate = c(1,1, -1,-1), sigma=0.05)
  group_2[[i]] = dat_simu1
  
  dat_simu1 = simulateData_outlier_sig(n=c(40,40,20), beta=c(1.5,-1.2), coordinate = c(1,1, -1,-1), sigma=0.05)
  group_3[[i]] = dat_simu1
}
simu_beta = list(group_1, group_2, group_3)
names(simu_beta) = c('beta1', 'beta2', 'beta3')

#-----reg-outlier--------only 2 group
group_1 = list()
group_2 = list()
for(i in 1:100){
  dat_simu1 = simulateData_outlier_regOut(n=c(40,40,20), beta=c(1.5,1.0), coordinate = c(1,1, -1,-1), sigma=0.05) #20%
  group_1[[i]] = dat_simu1
  
  dat_simu1 = simulateData_outlier_regOut(n=c(45,45,10), beta=c(1.5,1.0), coordinate = c(1,1, -1,-1), sigma=0.05) #10%
  group_2[[i]] = dat_simu1
}
simu_regOut = list(group_1, group_2)
names(simu_regOut) = c('20%', '10%')

#-----spa-outlier--------only 2 group
group_1 = list()
group_2 = list()
for(i in 1:100){
  dat_simu1 = simulateData_outlier_spaOut(n_spa=10 ) #10 = 5+5
  group_1[[i]] = dat_simu1
  
  dat_simu1 = simulateData_outlier_spaOut(n_spa=20) #20 = 10+10
  group_2[[i]] = dat_simu1
}
simu_spaOut = list(group_1, group_2)
names(simu_spaOut) = c('10%', '20%')

#-----shape-square--------only 2 group
group_1 = list()
group_2 = list()
for(i in 1:100){
  dat_simu1 = simulateData_outlier_shape(sh='gaus') 
  group_1[[i]] = dat_simu1
  
  dat_simu1 = simulateData_outlier_shape(sh='unif') 
  group_2[[i]] = dat_simu1
}
simu_shape = list(group_1, group_2)
names(simu_shape) = c('gaus', 'unif')

#-----spa-center-location--------only 2 group
group_1 = list()
group_2 = list()
for(i in 1:100){
  dat_simu1 = simulateData_outlier_sig(n=c(40,40,20), beta=c(1.5,1.0), coordinate = c(1,1, -1,-1), sigma=0.05)
  group_1[[i]] = dat_simu1
  
  dat_simu1 = simulateData_outlier_sig(n=c(40,40,20), beta=c(1.5,1.0), coordinate = c(0.5,0, -0.5,0), sigma=0.05)
  group_2[[i]] = dat_simu1

}
simu_spaLoca = list(group_1, group_2)
names(simu_spaLoca) = c('diagnal', 'sideBySide')

#-----spa-bal--------only 2 group
group_1 = list()
group_2 = list()
for(i in 1:100){
  dat_simu1 = simulateData_outlier_spaRadius(n=c(40,40,20), beta=c(1.5,1.0), coordinate = c(1,1, -1,-1), sigma=0.05, rad=c(0.1,0.1))
  group_1[[i]] = dat_simu1
  
  dat_simu1 = simulateData_outlier_spaRadius(n=c(40,40,20), beta=c(1.5,1.0), coordinate = c(1,1, -1,-1), sigma=0.05, rad=c(0.1,0.5))
  group_2[[i]] = dat_simu1
  
}
simu_spaBal = list(group_1, group_2)
names(simu_spaBal) = c('bal', 'imbal')



#----------------------------------------------------------
#### outside wrapper -> Rdata
simu_listAndList = list(simu_sig, simu_n, simu_k, simu_bal, simu_beta,
                          simu_regOut, simu_spaOut,
                          simu_shape, simu_spaLoca, simu_spaBal)
names(simu_listAndList) = c('simu_sig', 'simu_n', 'simu_k', 'simu_bal', 'simu_beta',
                            'simu_regOut', 'simu_spaOut',
                            'simu_shape', 'simu_spaLoca', 'simu_spaBal')
getwd()
setwd('C:/Users/wnchang/Documents/F/PhD_Research/2020_12_02_spatial/spatial_regression/expr_dataset')
save(simu_listAndList, file='simu_ListInList.RData')
#----------------------------------------------------------


# # library(RobMixReg)
# # library(robustbase)
# dat_simu1 = simu_spaBal[[2]][[99]]
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




