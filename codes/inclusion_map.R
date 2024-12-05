#### creates inclusion map


heat.comp <- comp.dat %>% filter(model!="fao") %>% mutate(itemreg=paste0(region,"-",item)) %>% filter(year==2020)


pdf(paste0(output.folder,"/plots/heatmaps/model_reporting_map.pdf"), width = 14, height = 5)

for(rrr in unique(heat.comp$region)){

  temp.heat_complete <- heat.comp %>% filter(region==rrr)

  # Step 1: Create a complete grid of all combinations
  all_combinations <- expand.grid(
    model = c("aglink","agmemod", "capri", "globiom", "image", "magnet"),
    region = unique(temp.heat_complete$region),
    variable = unique(temp.heat_complete$variable),
    itemreg = unique(temp.heat_complete$itemreg)
  )

  # Step 2: Identify missing combinations
  temp.heat_complete <- all_combinations %>%
    left_join(temp.heat_complete, by = c("model", "variable", "region", "itemreg")) %>%
    mutate(missing = ifelse(is.na(value), "Missing", "Reported"))



  # Step 3: Plot the heatmap
  pp <- ggplot(temp.heat_complete, aes(x = model, y = variable, fill = missing)) +
    geom_tile(color = "white") + # Create heatmap tiles
    geom_text(aes(label = ifelse(missing == "Missing", "X", "")), color = "black") + # Add "X" for missing
    scale_fill_manual(values = c("Reported" = "lightblue", "Missing" = "red"), name = "Status") +
    labs(title = paste0("Model Reporting Status for Region: ",toupper(rrr)),
         x = "Model",
         y = "Variable") +
    theme_minimal() +
    facet_grid(~itemreg)+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.4))

  print(pp)

}

dev.off()