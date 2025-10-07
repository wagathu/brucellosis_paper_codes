
# Importing packages ------------------------------------------------------

pacman::p_load(
  data.table,
  tidyr,
  ggplot2,
  dplyr,
  scales,
  patchwork
)
source("R/functions.R")

# Importing data ----------------------------------------------------------

dfs <- readRDS("data/all_dfs.RDS")
list2env(dfs, envir = .GlobalEnv)

# Calculating the Pearson's correlation coefficient -----------------------

df <- kenya_ah2 |>
  select(date, contains("incidence")) |>
  arrange(date)

lags = 0:6

results <- map(lags, ~ corFunction(df, lag = .))
names(results) <- paste("lag", 0:6)

plots_with_avg <- map2(results, names(results), function(result, lag_name) {
  result$plot +
    labs(caption = paste0(
      "Average absolute correlation = ",
      round(result$avg_abs_cor, 3)
    ))
})
cor_plots <- wrap_plots(plots_with_avg)

ggsave(
  plot = cor_plots,
  filename = "plots/cor_plots.png",
  dpi = 5e2,
  width = 3 * 5,
  height = 3 * 5,
  units = "in",
  bg = NULL
)
