# library(here) 
library(dplyr) 
library(funk) 
library(scales)
library(tidyr)
library(itsadug)
library(mgcv)
library(ggplot2)
library(tidyr)
library(here)
library(gratia)
# library(ggfortify)

setwd(here('tropical-ssf-div/'))

## Data load
load(file='data/TBD.Rdata')

t$log10catch<-scale(log10(t$cpue))
t$SZ<-factor(t$SZ)

## clean species names
t$Species <- gsub('\\ ', '', tolower(t$Species))
t$Species <- gsub('\\.', '', t$Species)

## fit models 

run=FALSE
# run=TRUE
if(run == TRUE){
       source('scripts/gam_func.R')
       redsnapper<-gamit_full(t, species = 'redsnapper', plot=T, save=T)
       grouper<-gamit_full(t, species = 'grouper', plot=T, save=T)
       lethrinidsp<-gamit_full(t, species = 'lethrinidsp', plot=T, save=T)
       job<-gamit_full(t, species = 'job', plot=T, save=T)
       sphyraenidsp<-gamit_full(t, species = 'sphyraenidsp', plot=T, save=T)
       pelagics<-gamit_full(t, species = 'pelagics', plot=T, save=T)
       caranxsp<-gamit_full(t, species = 'caranxsp', plot=T, save=T)
       sharks<-gamit_full(t, species = 'sharks', plot=T, save=T)
       otherspecies<-gamit_full(t, species = 'otherspecies', plot=T, save=T)
}

#### TOTAL CPUE 
t<-whaler %>% group_by(SZ, unique.id, DATE.ym, year,  month, dmi, benso, DAYS.FISH) %>% 
              summarise(catch= sum(biomass.kg)) %>%
              mutate(catch = catch / DAYS.FISH) %>%
               droplevels() %>%
              group_by(SZ, DATE.ym, year,  dmi, benso, month) %>%
              summarise(cpue = mean(catch)) 

t <- t %>% mutate(id = paste(SZ, DATE.ym, sep = '-'))
t$log10catch<-scale(log10(t$cpue))
t$SZ<-factor(t$SZ)


### fit models ###
df<-t
df$time <- scale(df$DATE.ym)
df$dmi.scaled <- scale(df$dmi)
df$enso.scaled <- scale(df$benso)
df$month.scaled <- scale(df$month)

mgam<-mgcv::gam(cpue ~ 
              s(time, bs='cr') + 
              s(enso.scaled, bs = 'cr') +  
              s(dmi.scaled, bs ='cr') + 
              s(month.scaled, bs = 'cc') +
              s(SZ, bs = "re"), 
              data = df, family='Gamma'(link='log'))

## setup predictor df for temporal smooths
time.pred<-expand.grid(time=seq(min(df$time), max(df$time),length.out=100), 
                            dmi.scaled=0, enso.scaled=0, month.scaled=0, SZ = unique(df$SZ)[2])
time.pred$time.raw<-seq(min(df$DATE.ym), max(df$DATE.ym),length.out=100)

pred<-predict(mgam, newdata=time.pred, type='response', exclude = 's(SZ)', se.fit=T)
time.pred$fit<-pred$fit
time.pred$se<-pred$se.fit*2
time.pred$species<-'Total'

## get predicted covariate effects
enso.pred<-evaluate_smooth(mgam, "enso.scaled")
enso.pred$enso.raw<-seq(min(df$benso), max(df$benso),length.out=100)
enso.pred$species<-'Total'

dmi.pred<-evaluate_smooth(mgam, "dmi.scaled")
dmi.pred$dmi.raw<-seq(min(df$dmi), max(df$dmi),length.out=100)
dmi.pred$species<-'Total'

month.pred<-evaluate_smooth(mgam, "month.scaled")
month.pred$month.raw<-seq(min(df$month), max(df$month),length.out=100)
month.pred$species<-'Total'

observed<-df
observed$species = 'Total'

results<-list(time.pred, enso.pred, dmi.pred, month.pred, observed)
save(results, mgam, file='data/results/cpue_gam_total.Rdata')
