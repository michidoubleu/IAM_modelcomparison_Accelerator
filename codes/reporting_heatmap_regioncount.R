#### creates inclusion map
heat.comp <- comp.dat.unfiltered %>% filter(model!="fao", item%in%unique(comp.dat$item)) %>% mutate(itemreg=paste0(region,"-",item)) %>% filter(year==2020) %>% group_by(model, item, variable) %>% summarise(value=n())


# Step 1: Create a complete grid of all combinations
all_combinations <- expand.grid(
  model = c("aglink","agmemod", "capri", "globiom", "image", "magnet"),
  variable = unique(heat.comp$variable),
  item = unique(heat.comp$item)
)

# Step 2: Identify missing combinations
temp.heat_complete <- all_combinations %>%
  left_join(heat.comp, by = c("model", "variable", "item")) %>%
  mutate(missing = ifelse(is.na(value), "Missing", value))


ggplot(temp.heat_complete, aes(x = model, y = variable, fill = value)) +
  geom_tile(color = "white") + # Create heatmap tiles
  geom_text(aes(label = ifelse(missing == "Missing", "X", value)), color = "black") + # Add "X" for missing
  scale_fill_steps(
    low = "red", high = "green", n.breaks = 9, # Specify color range and number of steps
    name = "Number of regions/countries"
  ) +
  labs(title = paste0("Model reporting status in number of regions/countries per item"),
       x = "Model",
       y = "Variable") +
  theme_minimal() +
  facet_wrap(~item, scales = "free") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4))

ggsave(paste0(output.folder,"/plots/heatmaps/heatmap_regionnumber_all.pdf"), last_plot(), width = 12, height = 9)









heat.comp <- comp.dat.unfiltered %>% filter(model!="fao", item%in%unique(comp.dat$item), region%in%non.country) %>% mutate(itemreg=paste0(region,"-",item)) %>% filter(year==2020) %>% group_by(model, item, variable) %>% summarise(value=n())


# Step 1: Create a complete grid of all combinations
all_combinations <- expand.grid(
  model = c("aglink","agmemod", "capri", "globiom", "image", "magnet"),
  variable = unique(heat.comp$variable),
  item = unique(heat.comp$item)
)

# Step 2: Identify missing combinations
temp.heat_complete <- all_combinations %>%
  left_join(heat.comp, by = c("model", "variable", "item")) %>%
  mutate(missing = ifelse(is.na(value), "Missing", value))


ggplot(temp.heat_complete, aes(x = model, y = variable, fill = value)) +
  geom_tile(color = "white") + # Create heatmap tiles
  geom_text(aes(label = ifelse(missing == "Missing", "X", value)), color = "black") + # Add "X" for missing
  scale_fill_steps(
    low = "red", high = "green", n.breaks = 9, # Specify color range and number of steps
    name = "Number of regions"
  ) +
  labs(title = paste0("Model reporting status in number of regions per item"),
       x = "Model",
       y = "Variable") +
  theme_minimal() +
  facet_wrap(~item, scales = "free") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4))

ggsave(paste0(output.folder,"/plots/heatmaps/heatmap_regionnumber_region.pdf"), last_plot(), width = 12, height = 9)




heat.comp <- comp.dat.unfiltered %>% filter(model!="fao", item%in%unique(comp.dat$item), !region%in%non.country) %>% mutate(itemreg=paste0(region,"-",item)) %>% filter(year==2020) %>% group_by(model, item, variable) %>% summarise(value=n())


# Step 1: Create a complete grid of all combinations
all_combinations <- expand.grid(
  model = c("aglink","agmemod", "capri", "globiom", "image", "magnet"),
  variable = unique(heat.comp$variable),
  item = unique(heat.comp$item)
)

# Step 2: Identify missing combinations
temp.heat_complete <- all_combinations %>%
  left_join(heat.comp, by = c("model", "variable", "item")) %>%
  mutate(missing = ifelse(is.na(value), "Missing", value))

ggplot(temp.heat_complete, aes(x = model, y = variable, fill = value)) +
  geom_tile(color = "white") + # Create heatmap tiles
  geom_text(aes(label = ifelse(missing == "Missing", "X", value)), color = "black") + # Add "X" for missing
  scale_fill_steps(
    low = "red", high = "green", n.breaks = 9, # Specify color range and number of steps
    name = "Number of countries"
  ) +
  labs(title = paste0("Model reporting status in number of countries per item"),
       x = "Model",
       y = "Variable") +
  theme_minimal() +
  facet_wrap(~item, scales = "free") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4))

ggsave(paste0(output.folder,"/plots/heatmaps/heatmap_regionnumber_countries.pdf"), last_plot(), width = 12, height = 9)

