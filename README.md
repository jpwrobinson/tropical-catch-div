# tropical-ssf-div
This repository contains R code for the Bayesian models accompanying  Robinson, Robinson, Gerry, Govinden, Freshwater and Graham, **Diversification insulates fisher catch and revenue in heavily exploited tropical fisheries. (2020) *Science Advances.***


The following R packages were used to analyse data.

```
install.packages(c("tidyverse", "rethinking", "here"))
```

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
