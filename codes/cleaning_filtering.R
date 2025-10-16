### load libraries
library(dplyr)
library(tidyr)
library(broom)
library(openxlsx)
library(ggplot2)
library(stringr)


dir.create(paste0(output.folder))
dir.create(paste0(output.folder,"/outputs"))
dir.create(paste0(output.folder,"/plots"))
dir.create(paste0(output.folder,"/plots/heatmaps"))
non.country <- c("ame", "anz", "eue", "eur", "fsu", "men", "oam", "oas", "osa", "sas", "sea", "ssa", "wld", "nam", "gcm")

### read data
comp.dat <- read.csv(comp.file)

#### avoid that some models have capitalized letters in reporting
comp.dat <- comp.dat %>% mutate(variable=tolower(variable), item=tolower(item), region=tolower(region), scenario=tolower(scenario), model=tolower(model))

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

##### remove inf
comp.dat <- comp.dat[!is.infinite(comp.dat$value),]
comp.dat <- comp.dat[!is.nan(comp.dat$value),]
comp.dat <- comp.dat[!is.na(comp.dat$value),]


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

##### filtering
if(jonathan.figures){
  to.filter <- openxlsx::read.xlsx(paste0("inputs/outliers/variable_figure_reporting.xlsx"), sheet = "variable-item-selection", startRow = 1)
  colnames(to.filter) <- c("var", "make","deliverable","items")

  reg.filter <- openxlsx::read.xlsx(paste0("inputs/outliers/variable_figure_reporting.xlsx"), sheet = "country-selection", startRow = 0)
reg.filter <- c(colnames(reg.filter), t(reg.filter))


  to.filter <- to.filter %>% filter(make=="yes")
  to.filter <- to.filter %>%
    mutate(items = str_split(items, "\\s*;\\s*")) %>%  # split on ";"
    unnest(items)

  to.filter$combi <- paste0(
    toupper(gsub("[^[:alnum:]]+", "", to.filter$var)),
    toupper(gsub("[^[:alnum:]]+", "", to.filter$items))
  )

  to.filter.neat <- to.filter %>% filter(nchar(items)>0, nchar(var)>0) %>% na.omit()
  to.filter.2 <- to.filter %>% filter(is.na(items))


  comp.dat.neat <- comp.dat %>% mutate(to.select=paste0(toupper(variable),toupper(item))) %>% filter(to.select%in%to.filter.neat$combi, region%in%tolower(reg.filter), variable%in%tolower(unique(to.filter$var))) %>% dplyr::select(-to.select)
  comp.dat.2 <- comp.dat %>% filter(region%in%tolower(reg.filter), variable%in%tolower(unique(to.filter.2$var)))

  comp.dat <- comp.dat.neat %>% bind_rows(comp.dat.2)

}



# Variable labels (AgMIP template)
variable_labels <- c(
  "area" = "Area harvested",
  "arir" = "Area irrigated share",
  "eco2" = "CO2 emissions",
  "emis" = "Total GHG emissions",
  "enh3" = "N2O emissions",
  "land" = "Land use",
  "soc" = "Soil organic carbon",
  "vegc" = "Vegetation carbon",
  "watr" = "Water use",
  "frtin" = "Fertiliser input nitrogen",
  "frtip" = "Fertiliser input phosphorus",
  "frton" = "Fertiliser output nitrogen",
  "frtop" = "Fertiliser output phosphorus",
  "nbal" = "Nitrogen balance",
  "pbal" = "Phosphorus balance",
  "pest" = "Pesticide use",
  "pesth" = "Pesticide use intensity",
  "frtn" = "Feed nitrogen content",
  "frtp" = "Feed phosphorus content",
  "bfp" = "Biofuel production",
  "cdi" = "Calories dietary intake",
  "fbi" = "Food balance index",
  "calo" = "Caloric supply",
  "cons_dm" = "Consumption (dry matter)",
  "expo_dm" = "Exports (dry matter)",
  "food_dm" = "Food use (dry matter)",
  "impo_dm" = "Imports (dry matter)",
  "nett_dm" = "Net trade (dry matter)",
  "prod_dm" = "Production (dry matter)",
  "xprp_idx" = "Producer price index",
  "cali" = "Calorie availability",
  "empl_abs" = "Employment (absolute)",
  "fwst_hh" = "Food waste households",
  "fwst_proc" = "Food waste processing",
  "fwst_prod" = "Food waste production",
  "proti" = "Protein intake",
  "proto" = "Protein output",
  "tfps" = "Total factor productivity",
  "vaad" = "Value added",
  "wagt" = "Wages total",
  "xprc" = "Consumer price",
  "xprc_dm" = "Consumer price (dry matter)",
  "xprp" = "Producer price"
)

# Item labels (AgMIP template)
item_labels <- c(
  "agr" = "Agriculture total",
  "cgr" = "Coarse grains",
  "crp" = "Crops total",
  "vfn" = "Vegetables & fruits",
  "wht" = "Wheat",
  "tot" = "Total",
  "for" = "Forestry",
  "grs" = "Grass",
  "lsp" = "Livestock products total",
  "dry" = "Dairy",
  "nrm" = "Non-ruminants",
  "ocr" = "Other crops",
  "osd" = "Oilseeds",
  "pfb" = "Pulses & fibre crops",
  "ric" = "Rice",
  "rum" = "Ruminants",
  "sgc" = "Sugar crops",
  "fsh" = "Fish",
  "oap" = "Other animal products",
  "elec" = "Electricity",
  "frt" = "Fruits",
  "ecp" = "Energy crops"
)

# Region labels (AgMIP template)
region_labels <- c(
  "deu" = "Germany",
  "eue" = "European Union",
  "fra" = "France",
  "wld" = "World"
)




