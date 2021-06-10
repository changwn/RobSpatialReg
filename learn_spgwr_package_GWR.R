library(spgwr)
# res = ggwr(formula= as.formula("y~x"), data = data1, coords=xy1, bandwidth, gweight = gwr.Gauss,
#            adapt = NULL, fit.points, family = gaussian, longlat = NULL, type =
#              c("working", "deviance", "pearson", "response"))
bw <- ggwr.sel(as.formula("y~x"), data=data1, coords=xy1,  adapt = FALSE,
               family=poisson(), verbose = TRUE, longlat = NULL, RMSE=FALSE,
               tol=.Machine$double.eps^0.25)
res = ggwr(formula= as.formula("y~x"), data = data1, coords=xy1, bandwidth=bw, gweight = gwr.Gauss,
           adapt = NULL, family = gaussian, longlat = NULL, type =
             c("working", "deviance", "pearson", "response"))
res$lm$coefficients
res$lm$prior.weights
res$lm