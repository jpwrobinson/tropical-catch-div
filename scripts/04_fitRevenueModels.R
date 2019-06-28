
## Fit varying intercept models w/ student-t variance parameters to 
# Seychelles fisheries revenue data

library(here)
setwd(here('tropical-catch-div/'))
library(funk)
library(tidyverse)
library(tidybayes)
library(rstan)
theme_set(theme_sleek())

th<-theme(axis.text.y=element_text(size=12, colour='black'),
        axis.title.y=element_text(size=12, colour='black'),
        axis.title.x=element_text(size=12, colour='black'),
                axis.text.x=element_text(angle = 0, size=12, colour='black'),
                axis.line = element_line(size=0),
                panel.border = element_rect(colour='black'),
                plot.margin = unit(c(5.5, 2.5, 0, 1.5), "pt"))

obs<- read.csv('data/scaled_revenue_dataset.csv')

## read data
focal <- read.csv('data/scaled_revenue_dataset.csv') %>% 
  arrange(SZ, year) %>% 
  mutate(yrID = as.numeric(as.factor(year)),
         szID = as.numeric(as.factor(SZ))) %>% 
  select(yrID, szID, div = simpson.diversity, cpue, 
         rev = revenue,
         revDay = revenue_per_fisher_perday,
         revDayKg  = revenue_per_fisher_perday_perkg) 

## truncate outliers
day_outliers<-quantile(focal$revDay, c(0.01, 0.99))
kg_outliers<-quantile(focal$revDayKg, c(0.01, 0.99))

focal$revDay[focal$revDay < day_outliers[1]]<-day_outliers[1]
focal$revDay[focal$revDay > day_outliers[2]]<-day_outliers[2]

focal$revDayKg[focal$revDayKg < kg_outliers[1]]<-kg_outliers[1]
focal$revDayKg[focal$revDayKg > kg_outliers[2]]<-kg_outliers[2]


## centre response for comparison
focal<- focal %>% mutate(
         divZ = as.numeric(scale(div)),
         revDayZ = as.numeric(scale(revDay)),
         revDayKgZ = as.numeric(scale(revDayKg)))


options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)


## Fit student-t model with stan
studTMod <- stan_model("scripts/stan/varyIntSlope_studT_rand.stan")

# Prep input data 
N <- nrow(focal) #sample size
nYr <- length(unique(focal$yrID)) #number of yrs 
nSz <- length(unique(focal$szID)) #number of sz groups
yrID <- focal$yrID
szID <- focal$szID

# ## Fit student-t models to each response in sequence

revDayMod <- sampling(studTMod, data=list(N=N, J=nYr, K=nSz, fisher_id = szID, 
                                       yr_id = yrID, x1=focal[, "divZ"], 
                                       y = focal[ ,"revDayZ"]),
                   iter = 3000, chains = 4, cores = 4,
                   control = list(adapt_delta = 0.9, max_treedepth = 20))
revDayKgMod <- sampling(studTMod, data=list(N=N, J=nYr, K=nSz, fisher_id = szID, 
                                          yr_id = yrID, x1=focal[, "divZ"], 
                                          y = focal[ ,"revDayKgZ"]),
                      iter = 3000, chains = 4, cores = 4,
                      control = list(adapt_delta = 0.9, max_treedepth = 20))

modOutList <- list(revDayMod, revDayKgMod)
names(modOutList) <- c("revDay", "revDayKg")
respSeq <- list("revDay", "revDayKg")

saveRDS(revDayMod, file="results/rstan/revDay_parEstMCMC.rds")
saveRDS(revDayKgMod, file="results/rstan/revDayKg_parEstMCMC.rds")