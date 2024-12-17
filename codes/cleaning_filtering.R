### load libraries
library(dplyr)
library(tidyr)
library(broom)
library(openxlsx)
library(ggplot2)

dir.create(paste0(output.folder))
dir.create(paste0(output.folder,"/outputs"))
dir.create(paste0(output.folder,"/plots"))
dir.create(paste0(output.folder,"/plots/heatmaps"))
non.country <- c("ame", "anz", "eue", "eur", "fsu", "men", "oam", "oas", "osa", "sas", "sea", "ssa", "wld", "blx", "gcm")

### read data
comp.dat <- read.csv(comp.file)

#### avoid that some models have capitalized letters in reporting
comp.dat <- comp.dat %>% mutate(variable=tolower(variable), item=tolower(item), region=tolower(region))

### compare scenario names, as all model report different, (agmemod has 2, assuming baseline is correct)
comp.dat <- comp.dat %>% filter(scenario!="module_bb")
comp.dat <- comp.dat %>% mutate(scenario="baseline")


### selection of comparison items
comp.dat <- comp.dat %>% filter(nchar(item)%in%c(3,4))

#change Magnet to 2020 for easier comparison, be transparent about that
comp.dat <- comp.dat %>% mutate(year=ifelse(model=="magnet"&year==2019, 2020, year))

# Ensure value is numeric
comp.dat <- comp.dat %>%
  mutate(value = as.numeric(value))


if(length(var.select)!=0){
  comp.dat <- comp.dat %>%
    filter(variable%in%var.select)
}

if(length(item.select)!=0){
  comp.dat <- comp.dat %>%
    filter(item%in%item.select)
}


comp.dat.unfiltered <- comp.dat


##### filtering
if(critical.witzke){
  to.filter <- openxlsx::read.xlsx(paste0("inputs/",critical.file), sheet = "important_items_dec", startRow = 6)
  colnames(to.filter) <- c("region", "item",paste0("var", seq(1,8,1)),"treshold")
  to.filter$treshold <- as.numeric(to.filter$treshold)
  to.filter <- to.filter[!is.na(to.filter$treshold),]
  to.filter$combi <- paste0(toupper(to.filter$region),toupper(to.filter$item))

  comp.dat <- comp.dat %>% mutate(to.select=paste0(toupper(region),toupper(item))) %>% filter(to.select%in%to.filter$combi) %>% dplyr::select(-to.select)

} else {


  if(EU.only){
    EU.select <- c("eue", "eur", "aut", "bgr", "blx", "cyp", "cze", "deu", "dnk", "esp", "est", "fin", "fra", "gbr", "grc", "hrv", "hun", "irl", "ita", "ltu", "lva", "mlt", "nld", "pol", "prt", "rou", "svk", "svn", "swe", "alb", "bel", "bih", "mkd", "mne", "srb", "lux")
    comp.dat <- comp.dat %>%
      filter(region%in%EU.select)
  }

}


