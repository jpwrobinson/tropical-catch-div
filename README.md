# tropical-ssf-div
This repository contains R code for the GAM and Bayesian models accompanying  Robinson, Robinson, Gerry, Govinden, Freshwater and Graham, **Diversification insulates fisher catch and revenue in heavily exploited tropical fisheries. (2020) *Science Advances.***


The following R packages were used to analyse data:

```
install.packages(c("tidyverse", "rethinking", "here", "mgcv", "itsadug", "gratia"))
```

as well as a personal package with a function for centering and standardizing covariates ([github.com/jpwrobinson/funk](https://github.com/jpwrobinson/funk)).

**data**

* `cpue_1990-2016_total.Rdata`
* `focal_fleet.Rdata`
* `cpue_species_1990-2016.Rdata`
* `scaled_revenue_dataset.csv`
* `diversity_fulldataset.Rdata`

**scripts**

* `01_cpue_gams.R`
* `02_focal_catch_mod.R`	
* `03_catch_div_allboats.R`
* `04_fitRevenueModels.R`
* `gam_func.R`
* `varyIntSlope_studT_rand.stan`
* `varyIntSlope_studT_rand.rds`

**results/cpue-gam**

*General additive models underlying CPUE trends from 1990-2016*

* `gam_caranxsp.Rdata`
* `gam_grouper.Rdata`
* `gam_job.Rdata`
* `gam_lethrinidsp.Rdata`
* `gam_otherspecies.Rdata`
* `gam_pelagics.Rdata`
* `gam_redsnapper.Rdata`
* `gam_sharks.Rdata`
* `gam_sphyraenidsp.Rdata`
* ``gam_total.Rdata`

**results/bayesian**

*Bayesian models underlying diversification effects*

* `revDay_parEstMCMC.rds`
* `revDayKg_parEstMCMC.rds`
* 

