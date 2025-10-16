# Define unique values
variables <- unique(comp.dat$variable)
items <- unique(comp.dat$item)
regions <- unique(comp.dat$region)

# Define model colors
model_colors <- c(
  "aglink" = "#228B22",
  "agmemod" = "#B22222",
  "capri" = "#ff7f0e",
  "fao" = "#32CD32",
  "globiom" = "#9467bd",
  "image" = "#1f77b4",
  "magnet" = "#888888"
)

# Loop by region -> item -> variable
for (rr in regions) {
  temp.r <- comp.dat %>% ungroup() %>% filter(region == rr)
  if (nrow(temp.r) == 0) next

  # Create region folder
  region_dir <- file.path(output.folder, "plots", rr)
  if (!dir.exists(region_dir)) dir.create(region_dir, recursive = TRUE)

  for (ii in items) {
    temp.i <- temp.r %>% filter(item == ii)
    if (nrow(temp.i) == 0) next

    # Create item subfolder inside region folder
    item_dir <- file.path(region_dir, ii)
    if (!dir.exists(item_dir)) dir.create(item_dir, recursive = TRUE)

    for (vv in variables) {
      temp.v <- temp.i %>% filter(variable == vv)
      if (nrow(temp.v) == 0) next

      curr.unit <- temp.v$unit[1]

      # Prepare data for plotting
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

      if (nrow(plot_data) == 0) next

      plot_data_before_2020 <- plot_data %>% filter(year <= 2020)
      plot_data_after_2020 <- plot_data %>% filter(year >= 2020)

      vv_label <- ifelse(is.na(variable_labels[[vv]]),vv,variable_labels[[vv]])
      ii_label <- ifelse(is.na(item_labels[[ii]]),ii,item_labels[[ii]])
      rr_label <- ifelse(is.na(region_labels[[rr]]),rr,region_labels[[rr]])



      # Create plot
      ppp <- ggplot(plot_data, aes(x = year, y = value, color = model)) +
        geom_point(alpha = 0.6) +
        geom_line(data = plot_data_before_2020, aes(y = predicted2020), linewidth = 1) +
        geom_line(data = plot_data_after_2020, aes(y = predictedpast2020), linetype = "dashed", linewidth = 1) +
        labs(
          title = paste0("Trend Lines by Model for ", rr_label, " - ", ii_label, " - ", vv_label),
          x = "Year",
          y = curr.unit
        ) +
        expand_limits(y = 0) +
        xlim(2000, 2050) +
        theme_classic() +
        scale_color_manual(values = model_colors) +
        theme(legend.position = "bottom") +
        geom_vline(xintercept = 2020, color = "grey", size = 0.6)

      # Save plot for this variable
      output_file <- file.path(item_dir, paste0(vv, "_trend_comparison.png"))
      png(output_file, width = 8, height = 4, units = "in", res = 300)
      print(ppp)
      dev.off()
    }
  }
}
