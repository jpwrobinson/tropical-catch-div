
library(here)
setwd(here('pirates/'))
library(funk)
library(vegan)
library(tidyverse)
library(mgcv)
library(gratia)
library(rethinking)
theme_set(theme_sleek())

load(file = 'results/portfolio/diversity_fulldataset_sampleN.Rdata')


df <- catch


## add categorial diversity level
## 0 - 0.25 is low; 0.25 - 0.75 is medium, 0.75-1 is high
df$div<-cut(df$simpson.diversity, 
	breaks = quantile(df$simpson.diversity, probs=c(0, 0.25, 0.75, 1)), 
	labels=c('Low', 'Medium', 'High'))
df$div[df$simpson.diversity == 0]<-'Low'

df$time <- scale(df$DATE.ym)
df$dmi.scaled <- scale(df$dmi)
df$enso.scaled <- scale(df$benso)
df$month.scaled <- scale(df$month)
df$boatsize.scaled <- scale(df$boatsize)

## cap outliers to 95% max.
one_col<-df$catch
a<-as.vector(round(quantile(one_col, c(0.95), na.rm = T),2))
one_col[which(one_col > a)]<-a
df$catch<-one_col
df<-as.data.frame(df)


df$SZ<-factor(df$SZ)
mgam<-mgcv::gam(catch ~ 
              s(time, by = div, bs='cr', k = -1) + 
              s(boatsize.scaled, bs='cr', k = -1) + 
              s(enso.scaled, bs = 'cr', k = -1) +  
              s(dmi.scaled, bs ='cr', k = -1) + 
              s(month.scaled, bs = 'cc', k = -1) +
              s(SZ, bs = "re"), 
              data = df, family='Gamma'(link='log'))

summary(mgam)

pdf(file = paste0('figures/models/fullfleet_diversity.pdf'), height= 7, width=12)
print(draw(mgam))
print(appraise(mgam))
dev.off()


## setup predictor df for temporal smooths
time.pred<-expand.grid(time=seq(min(df$time), max(df$time),length.out=100), 
				boatsize.scaled=0,
				dmi.scaled=0, enso.scaled=0, month.scaled=0, SZ = '330', div = unique(df$div))
time.pred$time.raw<-seq(min(df$DATE.ym), max(df$DATE.ym),length.out=100)

pred<-predict(mgam, newdata=time.pred, type='response', exclude = 's(time,SZ)', se.fit=T)
time.pred$fit<-pred$fit
time.pred$se<-pred$se.fit*2

## get predicted covariate effects
enso.pred<-evaluate_smooth(mgam, "enso.scaled")
enso.pred$enso.raw<-seq(min(df$benso), max(df$benso),length.out=100)

dmi.pred<-evaluate_smooth(mgam, "dmi.scaled")
dmi.pred$dmi.raw<-seq(min(df$dmi), max(df$dmi),length.out=100)

month.pred<-evaluate_smooth(mgam, "month.scaled")
month.pred$month.raw<-seq(min(df$month), max(df$month),length.out=100)

## save observed data
observed<-df


results<-list(time.pred, enso.pred, dmi.pred, month.pred, observed)

ggplot(time.pred, aes(time, fit, col=div)) + geom_line() #+ geom_ribbon(aes(ymin = fit - se, ymax = fit+se, group=div),col='transparent', alpha=0.2)


## observed
df %>% group_by(year, div) %>% summarise(se = funk::se(catch), cpue = mean(catch)) %>% ggplot() +
		geom_line(aes(year, cpue, col=div)) + geom_pointrange(aes(year, cpue,ymin=cpue-2*se, ymax = cpue+2*se, col=div))

df %>% group_by(DATE.ym, year, div) %>% summarise(cpue = mean(catch)) %>% ggplot() +
		geom_jitter(aes(year, cpue, col=div)) + facet_wrap(~div) 


focal<-scaler(df %>% select(catch, SZ, time, boatsize, simpson.diversity, div), ID = c('catch', 'SZ'), cats = TRUE)
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
	    			   # bB*time*Low.Medium.dummy + 
	    			   # bC*time*Low.High.dummy + 
	    			   bD*boatsize ,
	    a ~ dnorm(5.09, 10),
	    c(ar)[SZ] ~  dnorm(0,sigmar1), 
	   c(bA, bB, bC, bD) ~ dnorm(0, 10),
	    c(sigmar1) ~ dcauchy( 0 , 2),
	    scale ~ dexp(2)
), data=focal, iter=3000, chains=3)

pairs2(extract.samples(m.cpue)[-2])



save(df, focal, m.cpue, file='results/cpue_div_timeseries.Rdata')