###### SOME SETTINGS, YOU MAY CHANGE THESE TO YOUR NEEDS
##############################################################################################################################
##############################################################################################################################

comp.file <- "inputs/model_merge.csv" # set location of the accelerator merge file!!!
output.label <- "" #all outputs will end end with that label (except plots to avoid duplicates)
output.folder <- "output" #subfolder name in outputs and plots folder for versioning

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
critical.witzke <- TRUE #based on excel by peter witzke, dependent on sheet name and start row.. might fail with updates
critical.file <- "outliers/baseline_critical_outliers20241202.xlsx" #name of the file from witzke

#if vectors are kept empty, results will be based on all reportings, otherwise restricted to named ones.
var.select <- c("area","emis", "prod", "yild", "xprp", "cons")
item.select <- c()

##############################################################################################################################
##############################################################################################################################

##### DONE WITH SETTINGS, YOU MAY RUN THAT STUFF NOW