
library(here)
setwd(here('pirates/'))
library(funk)
library(tidyverse)
library(rethinking)


load(file = 'results/portfolio/merged_predictor_master.Rdata')

### define whether full dataset is used for div estimates, or reduced to 5 catches (sN = TRUE)
# sn = TRUE
# if(sn == TRUE){
# 	response.pred$simpson.div<-response.pred$simpson.div_sN
# 	response.pred$beta.bray<-response.pred$beta.bray_sN
# 	response.pred$beta.jac<-response.pred$beta.jac_sN
# }


response.pred$depth<-NULL
response.pred$simpson.div_sN<-NULL
response.pred$richness_sN<-NULL
response.pred$beta.bray_sN<-NULL
response.pred$beta.jac_sN<-NULL

### model of annual mean CPUE, with SZ-level predictors

head(response.pred)
focal<-scaler(response.pred, ID = c('cpue_pred', 'SZ', 'year', 'year.SZ'), cats = FALSE)
uniques(focal$SZ)

cors<-focal %>% ungroup() %>% select(-SZ, -year.SZ, -year) %>% na.omit()
pairs2(cors)
dim(cors)


### Annual mean CPUE - does the strategy or capacity of fishing vessel affect catch?

m.cpue <- map2stan(
	alist(
	    cpue_pred ~ dgamma2( mu , scale ) ,
	    log(mu) <- a + ar[SZ] + ar2[year] + 
	    			   bA*days + 
	    			   bB*diesel +
	    			   bC*boatpower +
	    			   bD*boatsize +
	    			   bE*fishing.area.km2 +
	    			   bF*median.latitude + 
	    			   bG*median.longitude ,#+
	    			   # bH*beta.bray +
	    			   # bI*simpson.div +
	    			   # bJ*simpson.div*beta.bray,
	    a ~ dnorm(0, 10),
	    c(ar)[SZ] ~  dnorm(0,sigmar1), 
	    c(ar2)[year] ~  dnorm(0,sigmar2),
	   c(bA, bB, bC, bD, bE, bF, bG) ~ dnorm(0, 10),
	    c(sigmar1, sigmar2) ~ dcauchy( 0 , 2),
	    scale ~ dexp(2)
), data=focal, iter=3000, chains=3)

# m.cpue <- map2stan(
# 	alist(
# 	    cpue_diff ~ dnorm( mu , sigma ) ,
# 	    mu <- a + ar[SZ] + ar2[year] + 
# 	    			   bA*days + 
# 	    			   bB*diesel +
# 	    			   bC*boatpower +
# 	    			   bD*boatsize +
# 	    			   bE*fishing.area.km2 +
# 	    			   bF*median.latitude + 
# 	    			   bG*median.longitude +
# 	    			   bH*beta.bray +
# 	    			   bI*simpson.div,
# 	    a ~ dnorm(0, 10),
# 	    c(ar)[SZ] ~  dnorm(0,sigmar1), 
# 	    c(ar2)[year] ~  dnorm(0,sigmar2),
# 	   c(bA, bB, bC, bD, bE, bF, bG, bH, bI) ~ dnorm(0, 10),
# 	    c(sigma, sigmar1, sigmar2) ~ dcauchy( 0 , 2)
# ), data=focal, iter=3000, chains=3)

precis(m.cpue)
# postcheck(m.cpue)

uniques(focal$SZ)


save(m.cpue, focal, response.pred, file = 'results/catch-drivers/cpue_sz_model.Rdata')
