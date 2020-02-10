
library(here)
setwd(here('pirates/'))
library(funk)
library(tidyverse)
library(rethinking)
theme_set(theme_sleek())

load(file = 'results/diversity_fulldataset.Rdata')

df <- catch

## cap outliers to 95% max.
one_col<-df$catch
a<-as.vector(round(quantile(one_col, c(0.95), na.rm = T),2))
one_col[which(one_col > a)]<-a
df$catch<-one_col
df<-as.data.frame(df)
df$SZ<-factor(df$SZ)


focal<-funk::scaler(df %>% select(catch, SZ, time, boatsize, simpson.diversity, div), ID = c('catch', 'SZ'), cats = TRUE)
focal$cpue<-focal$catch
focal$catch<-NULL
focal$time.raw<-df$DATE.ym

### Raw CPUE across diversity classes
m.cpue <- map2stan(
	alist(
	    cpue ~ dgamma2( mu , scale ) ,
	    log(mu) <- a + ar[SZ] + 
	    			   bA*time +
	    			   bB*simpson.diversity +
	    			   bC*time*simpson.diversity + 
	    			   bD*boatsize ,
	    a ~ dnorm(5.09, 10),
	    c(ar)[SZ] ~  dnorm(0,sigmar1), 
	   c(bA, bB, bC, bD) ~ dnorm(0, 10),
	    c(sigmar1) ~ dcauchy( 0 , 2),
	    scale ~ dexp(2)
), data=focal, iter=3000, chains=3)

save(df, focal, m.cpue, file='results/bayesian/cpue_div_timeseries.Rdata')

