### trend plots

variables <- unique(comp.dat$variable)
items <- unique(comp.dat$item)
regions <- unique(comp.dat$region)

model_colors <- c(
  "aglink" = "#228B22",    # blue
  "agmemod" = "#B22222",   # orange
  "capri" = "#ff7f0e",     # green
  "fao" = "#32CD32",   # red
  "globiom" = "#9467bd",   # purple
  "image" = "#1f77b4",     # brown
  "magnet" = "#888888"     # pink
)

vv <- "prod"
ii <- "osd"
rr <- "wld"



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
