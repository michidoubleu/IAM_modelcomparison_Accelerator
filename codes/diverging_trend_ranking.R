
outliers <- after2020_trend %>% mutate(intercept=2000*trend_after_2020+intercept_after2020) %>% group_by(scenario, region, variable, item) %>% filter(model!="fao") %>%
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