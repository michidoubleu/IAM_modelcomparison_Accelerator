
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