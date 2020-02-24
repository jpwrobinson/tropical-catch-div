# tropical-ssf-div
This repository contains R code for the GAM and Bayesian models constructed in  Robinson, Robinson, Gerry, Govinden, Freshwater and Graham, **Diversification insulates fisher catch and revenue in heavily exploited tropical fisheries. (2020) *Science Advances.***

[https://advances.sciencemag.org/content/6/8/eaaz0587](https://advances.sciencemag.org/content/6/8/eaaz0587)


The following R packages were used to analyse data:

```
install.packages(c("tidyverse", "rethinking", "here", "mgcv", "itsadug", "gratia", 'rstan', 'tidybayes'))
```

as well as a personal package with a function for centering and standardizing covariates ([github.com/jpwrobinson/funk](https://github.com/jpwrobinson/funk)).

**data**

* `cpue_1990-2016_total`: total CPUE for full fleet, 1990-2016
* `focal_fleet`: catch and effort for focal fleet, 2006-2016
* `cpue_species_1990-2016`: CPUE estimates by species group for full fleet, 1990-2016
* `revenue_scaled`: revenue estimates (total, perday, perday_perkg) with catch diversity for focal fleet
* `diversity_fulldataset`: catch diversity estimates for full fleet, 1990-2016

**scripts**

* `01_cpue_gams.R`: GAM models to fit temporal CPUE trends
* `02_focal_catch_mod.R`: Predictors of CPUE for focal fleet
* `03_catch_div_allboats.R`: Temporal CPUE trends by low/high catch diversity
* `04_fitRevenueModels.R`: Fitting revenue models
* `gam_func.R`: generalized function for fitting GAMs to CPUE data
* `varyIntSlope_studT_rand.stan`: Stan model structure for revenue patterns

**results/cpue-gam**

*General additive models underlying CPUE trends from 1990-2016. Rdata files contain GAM model structures and fitted datasets*. 

* `gam_caranxsp.Rdata`
* `gam_grouper.Rdata`
* `gam_job.Rdata`
* `gam_lethrinidsp.Rdata`
* `gam_otherspecies.Rdata`
* `gam_pelagics.Rdata`
* `gam_redsnapper.Rdata`
* `gam_sharks.Rdata`
* `gam_sphyraenidsp.Rdata`
* `gam_total.Rdata`

**results/bayesian**

*Bayesian models underlying diversification effects. Rdata/rds contain model structures fit in rethinking and rstan packages.*

* `cpue_sz_model.Rdata`: focal fleet, catch predictors
* `cpue_div_timeseries.Rdata`: full fleet, CPUE trends by catch diversity
* `revDay_parEstMCMC.rds`: focal fleet, revenue per day
* `revDayKg_parEstMCMC.rds`: focal fleet, revenue per day per kg

