###### baseline comparison code

## Author: Michael Wögerer, (IIASA)

## Aim: Comparison of trends between models after merge from: https://accelerator.iiasa.ac.at/
## Notes: Magnet 2019 changed to 2020 for easier comparison, be transparent about that
## V1 of this script, if you find bugs, feel free to fix them or mail me via wogerer@iiasa.ac.at


rm(list=ls())


###### SOME SETTINGS, YOU MAY CHANGE THESE TO YOUR NEEDS
##############################################################################################################################
##############################################################################################################################

comp.file <- "inputs/MW_26022025.csv" # set location of the accelerator merge file!!!
output.label <- "_full" #all outputs will end end with that label (except plots to avoid duplicates)
output.folder <- "27022025_full" #subfolder name in outputs and plots folder for versioning

create.plots <- TRUE #use with caution, setting this true will take LOTS of time by creating many plots, might lower user experience
create.heatmap <- TRUE #create heatmap to show how many models reported on set of items and variables
create.reporting.map <- TRUE #create reporting map to show how which models reported on set of items and variables
create.modelVSaglink <- TRUE #quite fast. compares trends between models and aglink for all time series where data in both is present
create.model.diverging.trend.ranking <- TRUE #quite fast. compares trends between models and reports them ranked by max trend divergence
  only.opposing.signs <- TRUE #This will cause reporting to only include time series where trends between lowest and highest model have opposite signs
  include.scaled.sheets <- FALSE #ranking based on difference divided by intercept. differences in smaller regions more highlighted
  include.level.sheets <- TRUE #also provides sheets with level differences in 2020
EU.only <- FALSE #Will restrict the results to be based on European regions (countries as well as regions)

#OUTPUT FILTERING
critical.witzke <- FALSE #based on excel by peter witzke, dependent on sheet name and start row.. might fail with updates
critical.file <- "baseline_critical_outliers20241202.xlsx" #name of the file from witzke

#if vectors are kept empty, results will be based on all reportings, otherwise restricted to named ones.
var.select <- c()
item.select <- c()

##############################################################################################################################
##############################################################################################################################

##### DONE WITH SETTINGS, YOU MAY RUN THAT STUFF NOW


source("codes/cleaning_filtering.R")

if(create.heatmap){
  source("codes/reporting_heatmap.R")
  source("codes/reporting_heatmap_regioncount.R")
}

if(create.reporting.map){
  source("codes/inclusion_map.R")
}

if(any(create.plots, create.modelVSaglink, create.model.diverging.trend.ranking)){
  source("codes/trend_calculations.R")
}


if(create.plots){
  source("codes/trendplots.R")
}

if(create.modelVSaglink){
  source("codes/aglink_trendcomparison.R")
}

if(create.model.diverging.trend.ranking){
  source("codes/diverging_trend_ranking.R")
}

