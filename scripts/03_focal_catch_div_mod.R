
library(here)
setwd(here('pirates/'))
library(funk)
library(tidyverse)
library(rethinking)


load(file = 'results/portfolio/merged_predictor_master.Rdata')
summary(response.pred)
response.pred$days<-NULL
response.pred$diesel<-NULL
response.pred$depth<-NULL
response.pred$fishing.area.km2<-NULL
response.pred$median.latitude<-NULL
response.pred$median.longitude<-NULL
# response.pred$boatdum<-ifelse(response.pred$boatsize > 9, 1, 0)

### define whether full dataset is used for div estimates, or reduced to 5 catches (sN = TRUE)
sn = TRUE
if(sn == TRUE){
	response.pred$simpson.div<-response.pred$simpson.div_sN
	response.pred$beta.bray<-response.pred$beta.bray_sN
	response.pred$beta.jac<-response.pred$beta.jac_sN
}


### model of annual mean CPUE, with SZ-level predictors
focal<-scaler(response.pred, ID = c('cpue_pred', 'SZ', 'year', 'year.SZ'), cats = FALSE)

cors<-focal %>% ungroup() %>% select(-SZ, -year.SZ, -year) %>% na.omit()
pairs2(cors)
dim(cors)


### Annual mean CPUE - does the strategy or capacity of fishing vessel affect catch?

m.cpue <- map2stan(
	alist(
	    cpue_pred ~ dgamma2( mu , scale ) ,
	    log(mu) <- a + ar[SZ] + ar2[year] + 
	    			   bD*simpson.div +
	    			   bE*beta.bray +
	    			   bF*simpson.div*beta.bray,
	    a ~ dnorm(4.5, 5),
	    c(ar)[SZ] ~  dnorm(0,sigmar1), 
	    c(ar2)[year] ~  dnorm(0,sigmar2),
  	    c(bD, bE, bF) ~ dnorm(0, 5),
	    c(sigmar1, sigmar2) ~ dcauchy( 0 , 2),
	    scale ~ dexp(2)
), data=focal, iter=3000, chains=3)

precis(m.cpue)
# postcheck(m.cpue)

uniques(focal$SZ)


save(m.cpue, focal, response.pred, file = 'results/catch-drivers/cpue_sz_model_div_interaction.Rdata')
