#### creates heatmap

# Define colors for integers 1 to 5
fill_colors <- c("1" = "yellow", "2" = "gold", "3" = "orange", "4" = "darkorange", "5" = "red")
text_colors <- c("1" = "black", "2" = "black", "3" = "black", "4" = "white", "5" = "white")

regions.sel <- tolower(c("USA", "BRA", "FSU", "EUR", "MEN", "SSA", "CHN", "IND", "SEA", "OAS", "EUE"))

## wld region
heat <- comp.dat %>% filter(model!="fao")
temp.heat.wld <- heat %>% filter(region=="wld") %>% mutate(itemreg=paste0(region,"-",item)) %>% filter(year==2020) %>% group_by(itemreg,variable) %>% summarise(number=n())
temp.heat.reg <- heat %>% filter(region%in%regions.sel) %>% mutate(itemreg=paste0(region,"-",item)) %>% filter(year==2020) %>% group_by(itemreg,variable) %>% summarise(number=n())
temp.heat.country <- heat %>% filter(!region%in%c(regions.sel,"wld")) %>% mutate(itemreg=paste0(region,"-",item)) %>% filter(year==2020) %>% group_by(itemreg,variable) %>% summarise(number=n())

# Create the heatmap with discrete colors and text colors
ggplot(temp.heat.wld, aes(x = variable, y = itemreg, fill = as.factor(number))) +
  geom_tile(color = "white") + # Adds grid lines between tiles
  geom_text(aes(label = number, color = as.factor(number))) + # Adds text labels with color
  scale_fill_manual(values = fill_colors, name = "Number") + # Discrete fill colors
  scale_color_manual(values = text_colors, guide = "none") + # Discrete text colors
  labs(title = "Heatmap of Variables by Item Region",
       x = "Variable",
       y = "Item Region") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(paste0(output.folder,"/plots/heatmaps/heatmap_wld.pdf"), last_plot(), width = 6, height = 5)


# Create the heatmap with discrete colors and text colors
ggplot(temp.heat.reg, aes(x = variable, y = itemreg, fill = as.factor(number))) +
  geom_tile(color = "white") + # Adds grid lines between tiles
  geom_text(aes(label = number, color = as.factor(number))) + # Adds text labels with color
  scale_fill_manual(values = fill_colors, name = "Number") + # Discrete fill colors
  scale_color_manual(values = text_colors, guide = "none") + # Discrete text colors
  labs(title = "Heatmap of Variables by Item Region",
       x = "Variable",
       y = "Item Region") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(paste0(output.folder,"/plots/heatmaps/heatmap_region.pdf"), last_plot(), width = 6, height = 12)

# Create the heatmap with discrete colors and text colors
ggplot(temp.heat.country, aes(x = variable, y = itemreg, fill = as.factor(number))) +
  geom_tile(color = "white") + # Adds grid lines between tiles
  geom_text(aes(label = number, color = as.factor(number))) + # Adds text labels with color
  scale_fill_manual(values = fill_colors, name = "Number") + # Discrete fill colors
  scale_color_manual(values = text_colors, guide = "none") + # Discrete text colors
  labs(title = "Heatmap of Variables by Item Region",
       x = "Variable",
       y = "Item Region") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(paste0(output.folder,"/plots/heatmaps/heatmap_country.pdf"), last_plot(), width = 6, height = 12)