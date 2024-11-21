###### baseline comparison code

## Author: Michael Wögerer, (IIASA)

## Aim: Comparison of trends between models after merge from: https://accelerator.iiasa.ac.at/
## Notes: Magnet 2019 changed to 2020 for easier comparison, be transparent about that
## V1 of this script, if you find bugs, feel free to fix them or mail me via wogerer@iiasa.ac.at


rm(list=ls())



###### SOME SETTINGS, YOU MAY CHANGE THESE TO YOUR NEEDS
##############################################################################################################################
##############################################################################################################################

comp.file <- "inputs/Ipsita_Merge_29102024.csv" # set location of the accelerator merge file!!!
output.label <- "_postSEVILLA" #all outputs will end end with that label (except plots to avoid duplicates)
output.folder <- "nov21_old_merge" #subfolder name in outputs and plots folder for versioning

create.plots <- TRUE #use with caution, setting this true will take LOTS of time by creating many plots, might lower user experience
create.modelVSaglink <- TRUE #quite fast. compares trends between models and aglink for all time series where data in both is present
create.model.diverging.trend.ranking <- TRUE #quite fast. compares trends between models and reports them ranked by max trend divergence
  only.opposing.signs <- TRUE #This will cause reporting to only include time series where trends between lowest and highest model have opposite signs
  include.scaled.sheets <- FALSE #ranking based on difference divided by intercept. differences in smaller regions more highlighted
  include.level.sheets <- TRUE #also provides sheets with level differences in 2020
EU.only <- TRUE #Will restrict the results to be based on European regions (countries as well as regions)

#OUTPUT FILTERING
critical.witzke <- TRUE #based on excel by peter witzke, dependent on sheet name and start row.. might fail with updates
critical.file <- "baseline_critical_outliers20241109.xlsx" #name of the file from witzke

#if vectors are kept empty, results will be based on all reportings, otherwise restricted to named ones.
var.select <- c("area","emis", "prod", "yild", "xprc", "cons")
item.select <- c()

##############################################################################################################################
##############################################################################################################################

##### DONE WITH SETTINGS, YOU MAY RUN THAT STUFF NOW


### load libraries
library(dplyr)
library(tidyr)
library(broom)
library(openxlsx)
library(ggplot2)

dir.create(paste0(output.folder))
dir.create(paste0(output.folder,"/outputs"))
dir.create(paste0(output.folder,"/plots"))

### read data
comp.dat <- read.csv(comp.file)


#######
##### filtering
if(critical.witzke){
  to.filter <- openxlsx::read.xlsx(paste0("inputs/",critical.file), sheet = "important_items", startRow = 7)
  colnames(to.filter) <- c("combi","region", "item","importance", "exclusion", "treshold")
  to.filter$treshold <- as.numeric(to.filter$treshold)
  to.filter <- to.filter[!is.na(to.filter$treshold),]

  comp.dat <- comp.dat %>% mutate(to.select=paste0(toupper(region),toupper(item))) %>% filter(to.select%in%to.filter$combi) %>% dplyr::select(-to.select)

  if(length(var.select)!=0){
    comp.dat <- comp.dat %>%
      filter(variable%in%var.select)
  }

} else {



  if(EU.only){
    EU.select <- c("eue", "eur", "aut", "bgr", "blx", "cyp", "cze", "deu", "dnk", "esp", "est", "fin", "fra", "gbr", "grc", "hrv", "hun", "irl", "ita", "ltu", "lva", "mlt", "nld", "pol", "prt", "rou", "svk", "svn", "swe", "alb", "bel", "bih", "mkd", "mne", "srb", "lux")
    comp.dat <- comp.dat %>%
      filter(region%in%EU.select)
  }
  if(length(var.select)!=0){
    comp.dat <- comp.dat %>%
      filter(variable%in%var.select)
  }
  if(length(item.select)!=0){
    comp.dat <- comp.dat %>%
      filter(item%in%item.select)
  }

}


### paste some tables for quick info
table(comp.dat$model, comp.dat$scenario)
table(comp.dat$item,comp.dat$model)
table(comp.dat$variable, comp.dat$model)
table(comp.dat$region, comp.dat$model)

#### avoid that some models have capitalized letters in reporting
comp.dat <- comp.dat %>% mutate(variable=tolower(variable), item=tolower(item), region=tolower(region))

### compare scenario names, as all model report different, (agmemod has 2, assuming baseline is correct)
comp.dat <- comp.dat %>% filter(scenario!="module_bb")
comp.dat <- comp.dat %>% mutate(scenario="baseline")
table(comp.dat$model,comp.dat$scenario)

### selection of comparison items
comp.dat <- comp.dat %>% filter(nchar(item)%in%c(3,4))
check.table <- table(comp.dat$item,comp.dat$model)


#change Magnet to 2020 for easier comparison, be transparent about that
comp.dat <- comp.dat %>% mutate(year=ifelse(model=="magnet"&year==2019, 2020, year))
table(comp.dat$year, comp.dat$model)

# Ensure value is numeric
comp.dat <- comp.dat %>%
  mutate(value = as.numeric(value))



### calculting linear model before and after 2020
before2020_trend <- comp.dat %>% filter(year <= 2020)
after2020_trend <-  comp.dat %>% filter(year >= 2020)

before2020_trend <- before2020_trend %>% na.omit() %>%
  group_by(model, scenario, region, variable, item) %>%
  do(tidy(lm(value ~ year, data = .))) %>%
  filter(term %in% c("(Intercept)", "year")) %>%
  select(model, scenario, region, variable, item, term, estimate) %>%
  spread(term, estimate, fill = NA) %>%
  rename(intercept_before2020 = `(Intercept)`, trend_before_2020 = year)


after2020_trend <- after2020_trend %>% na.omit() %>%
  group_by(model, scenario, region, variable, item) %>%
  do(tidy(lm(value ~ year, data = .))) %>%
  filter(term %in% c("(Intercept)", "year")) %>%
  select(model, scenario, region, variable, item, term, estimate) %>%
  spread(term, estimate, fill = NA) %>%
  rename(intercept_after2020 = `(Intercept)`, trend_after_2020 = year)


##################  plotting all the trends
if(create.plots){
variables <- unique(comp.dat$variable)
items <- unique(comp.dat$item)
regions <- unique(comp.dat$region)

model_colors <- c(
  "aglink" = "#228B22",    # blue
  "agmemod" = "#B22222",   # orange
  "capri" = "#ff7f0e",     # green
  "faodata" = "#32CD32",   # red
  "globiom" = "#9467bd",   # purple
  "image" = "#1f77b4",     # brown
  "magnet" = "#888888"     # pink
)

# vv <- "area"
# ii <- "agr"
# rr <- "bgr"



  for(ii in items){
    temp.i <- comp.dat %>% ungroup() %>% filter(item==ii)
    if(nrow(temp.i)==0){next}
    for(rr in regions){
      temp.r <- temp.i %>% ungroup() %>% filter(region==rr)
      if(nrow(temp.r)==0){next}
      pdf(paste0(output.folder,"/plots/",ii,"_",rr, "_trend_comparison.pdf"), width = 7, height = 4)
      for(vv in variables){
        temp.v <- temp.r %>% ungroup() %>% filter(variable==vv)
        if(nrow(temp.v)==0){next}
        curr.unit <- temp.v$unit[1]

      ### temp.plotdata
      plot_data <- temp.v %>%
        left_join(before2020_trend, by = c("model", "scenario", "region", "variable", "item")) %>%
        left_join(after2020_trend, by = c("model", "scenario", "region", "variable", "item")) %>%
        mutate(
          predicted2020 = ifelse(
            year <= 2020,
            intercept_before2020 + trend_before_2020 * year,
            intercept_after2020 + trend_after_2020 * year
          ),
          predictedpast2020 = ifelse(
            year < 2020,
            intercept_before2020 + trend_before_2020 * year,
            intercept_after2020 + trend_after_2020 * year
          )
        )

      if(nrow(plot_data)==0){next}

      plot_data_before_2020 <- plot_data %>% filter(year <= 2020)
      plot_data_after_2020 <- plot_data %>% filter(year >= 2020)

      # Plot the data with separate trend lines per model
      ppp <- ggplot(plot_data, aes(x = year, y = value, color = model)) +
        geom_point(alpha = 0.6) +  # Original data points
        geom_line(data = plot_data_before_2020, aes(x = year, y = predicted2020, color = model), linewidth = 1) +  # Trend line before 2020
        geom_line(data = plot_data_after_2020, aes(x = year, y = predictedpast2020, color = model), linetype = "dashed", linewidth = 1) +  # Trend line after 2020
        labs(title = paste0("Trend Lines by model for ", rr, "-",vv, "-",ii),
             x = "year",
             y = curr.unit) +
        expand_limits(y = 0) +
        xlim(2000,2050) +
        theme_classic() +
        scale_color_manual(values = model_colors) +
        theme(legend.position = "right") +
        geom_vline(xintercept = 2020, color = "grey", size = .6)

      print(ppp)
      }
      dev.off()
  }
}

}




if(create.modelVSaglink){
########### PART ABOUT printing shares of AgLINK alignment into csv


###### how to identify outlier cases vs aglink:
trend_consistency <- after2020_trend %>% na.omit() %>%
  left_join(after2020_trend %>% filter(model=="aglink") %>% ungroup %>% dplyr::select(region, variable, item, trend_after_2020) %>%
              rename("aglink_trend"="trend_after_2020")) %>%
  mutate(same.as.aglink=ifelse(trend_after_2020/abs(trend_after_2020)==aglink_trend/abs(aglink_trend),1,0)) %>% group_by(model)

general <- trend_consistency %>% na.omit() %>%
  dplyr::summarise(similar.to.aglink=sum(same.as.aglink, na.rm = TRUE), total=n(), share=similar.to.aglink/total)

variable <- trend_consistency %>% na.omit() %>% group_by(model, variable) %>%
  dplyr::summarise(similar.to.aglink=sum(same.as.aglink, na.rm = TRUE), total=n(), share=similar.to.aglink/total)

item <- trend_consistency %>% na.omit() %>% group_by(model, item) %>%
  dplyr::summarise(similar.to.aglink=sum(same.as.aglink, na.rm = TRUE), total=n(), share=similar.to.aglink/total)

region <- trend_consistency %>% na.omit() %>% group_by(model, region) %>%
  dplyr::summarise(similar.to.aglink=sum(same.as.aglink, na.rm = TRUE), total=n(), share=similar.to.aglink/total)

variable.item <- trend_consistency %>% na.omit() %>% group_by(model, variable, item) %>%
  dplyr::summarise(similar.to.aglink=sum(same.as.aglink, na.rm = TRUE), total=n(), share=similar.to.aglink/total)

variable.region <- trend_consistency %>% na.omit() %>% group_by(model, variable, region) %>%
  dplyr::summarise(similar.to.aglink=sum(same.as.aglink, na.rm = TRUE), total=n(), share=similar.to.aglink/total)

region.item <- trend_consistency %>% na.omit() %>% group_by(model, region, item) %>%
  dplyr::summarise(similar.to.aglink=sum(same.as.aglink, na.rm = TRUE), total=n(), share=similar.to.aglink/total)

variable.item.region <- trend_consistency %>% na.omit() %>% group_by(model, variable, item, region) %>%
  dplyr::summarise(similar.to.aglink=sum(same.as.aglink, na.rm = TRUE), total=n(), share=similar.to.aglink/total)




models <- unique(comp.dat$model)
mm <- "magnet"
for(mm in models){

  # Create a new workbook
  wb <- createWorkbook()

  addWorksheet(wb, paste0("general"))
  writeData(wb, paste0("general"), general)


  temp <- variable %>% filter(model==mm)
  addWorksheet(wb, paste0(mm, "_variable"))
  writeData(wb, paste0(mm, "_variable"), temp)

  temp <- item %>% filter(model==mm)
  addWorksheet(wb, paste0(mm, "_item"))
  writeData(wb, paste0(mm, "_item"), temp)

  temp <- region %>% filter(model==mm)
  addWorksheet(wb, paste0(mm, "_region"))
  writeData(wb, paste0(mm, "_region"), temp)

  temp <- variable.item %>% filter(model==mm)
  addWorksheet(wb, paste0(mm, "_variable.item"))
  writeData(wb, paste0(mm, "_variable.item"), temp)

  temp <- variable.region %>% filter(model==mm)
  addWorksheet(wb, paste0(mm, "_variable.region"))
  writeData(wb, paste0(mm, "_variable.region"), temp)

  temp <- region.item %>% filter(model==mm)
  addWorksheet(wb, paste0(mm, "_region.item"))
  writeData(wb, paste0(mm, "_region.item"), temp)

  temp <- variable.item.region %>% filter(model==mm)
  addWorksheet(wb, paste0(mm, "_variable.item.region"))
  writeData(wb, paste0(mm, "_variable.item.region"), temp)


  # Save the workbook to a file
  saveWorkbook(wb, paste0(output.folder,"/outputs/",mm,"_comparison",output.label,".xlsx"), overwrite = TRUE)

}
}


############ identifying outliers, sorting by highest diff in min vs. max past 2020

if(create.model.diverging.trend.ranking){

outliers <- after2020_trend %>% mutate(intercept=2000*trend_after_2020+intercept_after2020) %>% group_by(scenario, region, variable, item) %>% filter(model!="faodata") %>%
  mutate(diff=max(trend_after_2020)-min(trend_after_2020),
         opp.sign=ifelse(max(trend_after_2020)/abs(max(trend_after_2020))==min(trend_after_2020)/abs(min(trend_after_2020)), 0, 1),
         min.model=ifelse(trend_after_2020==min(trend_after_2020),1,0),
         max.model=ifelse(trend_after_2020==max(trend_after_2020),1,0),
         min.model.lvl=ifelse(intercept==min(intercept),1,0),
         max.model.lvl=ifelse(intercept==max(intercept),1,0),
         diff.level=max(intercept)-min(intercept),
         diff.scaled=diff/intercept
  )

if(only.opposing.signs){
  outliers <- outliers %>% filter(opp.sign==1)
}


all.vars <- unique(outliers$variable)

#vv <- "area"
# Create a new workbook
wb <- createWorkbook()

for(vv in all.vars){
  addWorksheet(wb, paste0(vv, "_trend"))
  outliers.temp <- outliers %>% filter(variable %in% vv)

  outliers.temp <- outliers.temp %>% mutate(min.model=ifelse(min.model==1,model,NA), max.model=ifelse(max.model==1,model,NA))

  res.temp <- outliers.temp %>% group_by(region, item) %>% summarise(diff=mean(diff, na.rm = T), oppo.sign=mean(opp.sign))
  names.temp.min <- outliers.temp %>% ungroup() %>% dplyr::select(region, item, min.model) %>% na.omit()
  names.temp.max <- outliers.temp %>% ungroup() %>% dplyr::select(region, item, max.model) %>% na.omit()


  temp <- res.temp %>% left_join(names.temp.min) %>% left_join(names.temp.max) %>% filter(diff!=0) %>% arrange(desc(diff))



  writeData(wb, paste0(vv, "_trend"), temp)


  if(include.scaled.sheets){
  addWorksheet(wb, paste0(vv, "_trend.scaled"))

  outliers.temp <- outliers %>% filter(variable %in% vv)

  outliers.temp <- outliers.temp %>% mutate(min.model.lvl=ifelse(min.model.lvl==1,model,NA), max.model.lvl=ifelse(max.model.lvl==1,model,NA))

  res.temp <- outliers.temp %>% group_by(region, item) %>% summarise(diff=mean(diff.scaled, na.rm = T))
  names.temp.min <- outliers.temp %>% ungroup() %>% dplyr::select(region, item, min.model.lvl) %>% na.omit()
  names.temp.max <- outliers.temp %>% ungroup() %>% dplyr::select(region, item, max.model.lvl) %>% na.omit()


  temp <- res.temp %>% left_join(names.temp.min) %>% left_join(names.temp.max) %>% filter(diff!=0) %>% arrange(desc(diff))
  writeData(wb, paste0(vv, "_trend.scaled"), temp)
  }




  if(include.level.sheets){
  addWorksheet(wb, paste0(vv, "_level"))

  outliers.temp <- outliers %>% filter(variable %in% vv)

  outliers.temp <- outliers.temp %>% mutate(min.model.lvl=ifelse(min.model.lvl==1,model,NA), max.model.lvl=ifelse(max.model.lvl==1,model,NA))

  res.temp <- outliers.temp %>% group_by(region, item) %>% summarise(diff=mean(diff.level, na.rm = T))
  names.temp.min <- outliers.temp %>% ungroup() %>% dplyr::select(region, item, min.model.lvl) %>% na.omit()
  names.temp.max <- outliers.temp %>% ungroup() %>% dplyr::select(region, item, max.model.lvl) %>% na.omit()


  temp <- res.temp %>% left_join(names.temp.min) %>% left_join(names.temp.max) %>% filter(diff!=0) %>% arrange(desc(diff))
  writeData(wb, paste0(vv, "_level"), temp)
}
}
saveWorkbook(wb, paste0(output.folder,"/outputs/outliers_analysis",output.label,".xlsx"), overwrite = TRUE)
}

