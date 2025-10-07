
# Importing packages ------------------------------------------------------

pacman::p_load(
  fable,
  forecast,
  broom,
  dplyr,
  tidyr,
  ggplot2,
  patchwork,
  scales,
  purrr
)
source("R/functions.R")

# Importing data ----------------------------------------------------------

list2env(readRDS("data/all_dfs.RDS"), envir = .GlobalEnv)

# Some initial cleaning ---------------------------------------------------

df1 <- kenya_ah2 |> 
  select(date, contains("incidence"))

df2 <- ah2 |> 
  select(date, county, contains("incidence"))

# Running the models ------------------------------------------------------

results_combined <- tslmFunction(df1, max_lag = 6, combined = TRUE)
results_individual <- tslmFunction(df1, max_lag = 6, combined = FALSE)

fp_df <- results_individual$result_df |>
  filter(variable != "(Intercept)") |>
  rbind(results_combined$result_df |>
          filter(variable != "(Intercept)")) |>
  arrange(lag)

fwrite(results_combined$result_df, "tables/combined_tslm_results.csv", row.names = F)
fwrite(fp_df, "tables/tslm_results.csv", row.names = F)


# Metrics for national level models ---------------------------------------

combined_metrics <- map_dfr(results_combined$models, accuracy) |> 
  mutate(lag = paste("lag", 0:6), model = "Combined animal incidence") |> 
  select(lag, model, RMSE, MAE, MAPE) 
all_tslm_metrics <- map_dfr(results_individual$models, accuracy) |> 
  mutate(lag = paste("lag", 0:6), model = "Individual animal incidence") |> 
  select(lag, model, RMSE, MAE, MAPE) |> 
  rbind(combined_metrics) |> 
  arrange(lag)
fwrite(all_tslm_metrics, "tables/all_tslm_metrics.csv", row.names = F)

# Running the combined animal incidence models for each county ----------

county_cb <- df2 %>%
  split(.$county) |> 
  imap(\(x, y){
    x <- x |> 
      select(-county)
    tslmFunction(x, max_lag = 6, combined = TRUE)
  }) |> 
  imap(\(x,y){
    x <- x$result_df |> 
      mutate(county = y, `Model type` = "Combined") |> 
      select(county, lag, `Model type`, everything())
  }) |> 
  rbindlist()

# Running the individual animal incidence models for each county ----------

county_individual <- df2 %>%
  split(.$county) |>
  imap(\(x, y) {
    x <- x |>
      select(-county)
    tslmFunction(x, max_lag = 6, combined = F)
  }) |>
  imap(\(x, y) {
    x <- x$result_df |>
      mutate(county = y, `Model type` = "Individual") |>
      select(county, lag, `Model type`, everything())
  }) |>
  rbindlist()

county_level_tslm <- rbind(county_cb, county_individual) |>
  arrange(county)

fwrite(county_level_tslm,
       "tables/county_level_tslm.csv",
       row.names = F)

# Plotting the forest plots -----------------------------------------------

individual_fps <- imap(fp_df %>%
                         split(.$lag), \(x, y) {
                           x |>
                             ggplot(aes(x = variable, y = estimate)) +
                             geom_point(aes(color = variable), size = 3) +
                             geom_errorbar(
                               aes(
                                 ymin = conf.low,
                                 ymax = conf.high,
                                 color = variable
                               ),
                               width = 0.2,
                               linewidth = .7
                             ) +
                             geom_hline(
                               yintercept = 0,
                               linetype = 2,
                               linewidth = .7,
                               color = "#009E73"
                             ) +
                             coord_flip() +
                             scale_color_manual(
                               values = c(
                                 "Camel incidence" = "#0072B2",
                                 "Sheep incidence" = "#0072B2",
                                 "Cattle incidence" = "#0072B2",
                                 "Goat Incidence" = "#0072B2",
                                 "Total animal incidence" = "#E69F00"
                               )
                             ) +
                             labs(x = "Species", y = "Estimate") +
                             theme_minimal() +
                             theme(
                               axis.text.y = element_text(size = 10, color = 'black'),
                               axis.title.x = element_text(size = 12, color = 'black'),
                               axis.title.y = element_text(size = 12),
                               plot.title = element_text(size = 16, hjust = 0.5),
                               legend.position = "none"
                             ) +
                             ggtitle(paste("Lag", y))
                           
                         }) |> 
  setNames(c(paste("Lag", 0:6)))

imap(individual_fps, \(x, y){
  ggsave(
    plot = x,
    filename = paste0("plots/tslm forest plots/", y, ".png"),
    dpi = 5e2,
    width = 2 * 5,
    height = 1 * 5,
    units = "in",
    bg = NULL
  )
})

individual_fp <- wrap_plots(individual_fps, axes = "collect")

ggsave(
  plot = individual_fp,
  filename = "plots/tslm_forest_plots.png",
  dpi = 5e2,
  width = 3 * 5,
  height = 2 * 5,
  units = "in",
  bg = NULL
)

# Plotting the model diagnostics ------------------------------------------

combined_diags <- map(results_combined$models, ~gg_tsresiduals(.)) |> 
  setNames(c(paste("lag_", 0:6)))

imap(combined_diags, \(x, y){
  ggsave(
    plot = x,
    filename = paste0("plots/combined tslm diagnostics/", y, ".png"),
    dpi = 5e2,
    width = 3 * 5,
    height = 2 * 5,
    units = "in",
    bg = NULL
  )
})

individual_diags <- map(results_individual$models, ~gg_tsresiduals(.)) |> 
  setNames(c(paste("lag_", 0:6)))

imap(individual_diags, \(x, y){
  ggsave(
    plot = x,
    filename = paste0("plots/individual tslm diagnostics/", y, ".png"),
    dpi = 5e2,
    width = 3 * 5,
    height = 2 * 5,
    units = "in",
    bg = NULL
  )
})

labels = map(paste("lag", 0:6), \(x) {
  ggplot() +
    annotate(
      "text",
      x = 0,
      y = 0.5,
      label = x,
      size = 6,
      hjust = 0.5,
      vjust = 0.5
    ) +
    coord_fixed(ratio = 1, xlim = c(-0.5, 0.5), ylim = c(-0.5, 0.5)) +
    theme_void() +
    theme(plot.margin = margin(0, 0, 0, 0, "pt"))
}) |> 
  setNames(paste("Lag", 0:6))

plot_list <- map2(labels, combined_diags, \(label, diag_plots) {
  list(label, wrap_plots(diag_plots, nrow = 2, ncol = 3))
}) %>%
  flatten()

combined_diag <- wrap_plots(plot_list, ncol = 2) +
  plot_layout(widths = c(0.1, 1))

ggsave(
  plot = combined_diag,
  filename = "plots/combined_diag.png",
  dpi = 5e2,
  width = 7 * 5,
  height = 7 * 5,
  units = "in",
  bg = NULL
)

# Which model performed the best ------------------------------------------

# Which model performed the best in the combined?
temp_cb <- results_combined$result_df |> 
  summarise(
    AIC = first(AIC),
    r_squared = first(r_squared),
    adj_r_squared = first(adj_r_squared),
    .by = lag
  ) |> 
  arrange(AIC)

# Which model performed the best in the individual?
temp_cb <- results_individual$result_df |> 
  summarise(
    AIC = first(AIC),
    r_squared = first(r_squared),
    adj_r_squared = first(adj_r_squared),
    .by = lag
  ) |> 
  arrange(AIC)

