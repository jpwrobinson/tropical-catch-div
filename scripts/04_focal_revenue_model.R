
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
response.pred$boatcat<-cut(response.pred$boatsize, breaks=c(1, 7,11, 15), labels=c('Small', 'Medium', 'Large'))


### define whether full dataset is used for div estimates, or reduced to 5 catches (sN = TRUE)
sn = TRUE
if(sn == TRUE){
	response.pred$simpson.div<-response.pred$simpson.div_sN
	response.pred$beta.bray<-response.pred$beta.bray_sN
	response.pred$beta.jac<-response.pred$beta.jac_sN
}

response.pred$income_perpredkg<-response.pred$income_perday/response.pred$cpue_pred

### model of annual mean CPUE, with SZ-level predictors
focal<-scaler(response.pred, ID = c('income_perday', 'SZ', 'year', 'year.SZ'), cats = TRUE)

cors<-focal %>% ungroup() %>% select(-SZ, -year.SZ, -year) %>% na.omit()
pairs2(cors)



ggplot(response.pred, aes(simpson.div, income, col=boatsize)) + geom_point()

ggplot(response.pred[!is.na(response.pred$simpson.div),],
	 aes(simpson.div, income_perday, col=cpue_pred, size=boatsize)) + geom_point()

ggplot(response.pred[!is.na(response.pred$simpson.div),],
	 aes(simpson.div, income_perkg, col=cpue_pred, size=boatsize)) + geom_point() 

ggplot(response.pred[!is.na(response.pred$simpson.div),], 
			aes(simpson.div, income_perday, col=boatsize, size=boatsize)) + geom_point() + 
			scale_size_continuous(range = c(1,4), breaks=c(3,7.5, 12)) #+ scale_y_log10()

ggplot(response.pred[!is.na(response.pred$beta.bray),], 
			aes(beta.bray, income_perday, col=boatsize, size=boatsize)) + geom_point() + 
			scale_size_continuous(range = c(1,4), breaks=c(3,7.5, 12)) #+ scale_y_log10()




### Annual mean CPUE - does the strategy or capacity of fishing vessel affect catch?

m.simp <- map2stan(
	alist(
	    income_perday ~ dgamma2( mu , scale ) ,
	    log(mu) <- a + ar[SZ] + ar2[year] + 
	    			   bD*simpson.div +
	    			   bE*simpson.div*Small.Medium.dummy +
	    			   bF*simpson.div*Small.Large.dummy,
	    a ~ dnorm(8, 5),
	    c(ar)[SZ] ~  dnorm(0,sigmar1), 
	    c(ar2)[year] ~  dnorm(0,sigmar2),
  	    c(bD, bE, bF) ~ dnorm(0, 5),
	    c(sigmar1, sigmar2) ~ dcauchy( 0 , 2),
	    scale ~ dexp(2)
), data=focal, iter=3000, chains=3)

precis(m.simp)

m.beta <- map2stan(
	alist(
	    income_perday ~ dgamma2( mu , scale ) ,
	    log(mu) <- a + ar[SZ] + ar2[year] + 
	    			   bD*beta_bray +
	    			   bE*beta_bray*Small.Medium.dummy +
	    			   bF*beta_bray*Small.Large.dummy,
	    a ~ dnorm(8, 5),
	    c(ar)[SZ] ~  dnorm(0,sigmar1), 
	    c(ar2)[year] ~  dnorm(0,sigmar2),
  	    c(bD, bE, bF) ~ dnorm(0, 5),
	    c(sigmar1, sigmar2) ~ dcauchy( 0 , 2),
	    scale ~ dexp(2)
), data=focal, iter=3000, chains=3)

precis(m.beta)
# postcheck(m.cpue)

m.div <- map2stan(
	alist(
	    income_perday ~ dgamma2( mu , scale ) ,
	    log(mu) <- a + ar[SZ] + ar2[year] + 
	    			   bD*simpson.div +
	    			   bE*beta_bray +
	    			   bF*cpue_pred,
	    a ~ dnorm(8, 5),
	    c(ar)[SZ] ~  dnorm(0,sigmar1), 
	    c(ar2)[year] ~  dnorm(0,sigmar2),
  	    c(bD, bE, bF) ~ dnorm(0, 5),
	    c(sigmar1, sigmar2) ~ dcauchy( 0 , 2),
	    scale ~ dexp(2)
), data=focal, iter=3000, chains=3)

precis(m.div)

save(m.simp,m.beta, m.div, focal, response.pred, file = 'results/catch-drivers/cpue_sz_model_div_incomes.Rdata')
