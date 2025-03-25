###### baseline comparison code

## Author: Michael Wögerer, (IIASA)

## Aim: Comparison of trends between models after merge from: https://accelerator.iiasa.ac.at/
## Notes: Magnet 2019 changed to 2020 for easier comparison, be transparent about that
## V1 of this script, if you find bugs, feel free to fix them or mail me via wogerer@iiasa.ac.at
rm(list=ls())

source("codes/user_settings.R")


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

