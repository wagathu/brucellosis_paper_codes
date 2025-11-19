# Importing packages ------------------------------------------------------

pacman::p_load(
  data.table,
  dplyr,
  ggplot2,
  forecast,
  fable,
  fabletools,
  tseries,
  xts,
  tsibble,
  stringr,
  feasts,
  ggalt,
  lubridate,
  purrr,
  tidyr
)
source("R/functions.R")

# Importing data ----------------------------------------------------------

list2env(readRDS("data/all_dfs.RDS"), envir = .GlobalEnv)
a_ts <- fread("data/a_ts.csv")

# Some initial cleaning ---------------------------------------------------

d <- a_ts |>
  select(date, contains("incidence")) |>
  rowwise() |>
  mutate(
    animal_incidence = sum(
      catt_incidence,
      cam_incidence,
      goat_incidence,
      shp_incidence
    )
  ) |> 
  mutate(
    date = yearmonth(date),
    trend_number = row_number()
  ) |> 
  select(date, hum_incidence, animal_incidence) |> 
  as_tsibble(index = date) |> 
  filter(date <= (yearmonth("2023 Dec")))

d1 <- d |> 
  filter(date <= (yearmonth("2022 Dec")))

# Initial plot of the overall trend ---------------------------------------

hum_ts <- d |> 
  ggplot(aes(x = date, y = hum_incidence)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  theme(
    axis.title = element_text(color = "black"),
    axis.text = element_text(color = "black")
  ) +
  labs(x = "Date", y = "Human incidence")

ggsave(
  plot = hum_ts,
  filename = "plots/hum_ts.png",
  dpi = 5e2,
  width = 2 * 5,
  height = 1 * 5,
  units = "in",
  bg = NULL
)

animal_ts <- d |> 
  ggplot(aes(x = date, y = animal_incidence)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  theme(
    axis.title = element_text(color = "black"),
    axis.text = element_text(color = "black")
  ) +
  labs(x = "Date", y = "Animal incidence")

ggsave(
  plot = animal_ts,
  filename = "plots/animal_ts.png",
  dpi = 5e2,
  width = 2 * 5,
  height = 1 * 5,
  units = "in",
  bg = NULL
)


# Monthly seasonality -----------------------------------------------------

d |> 
  mutate(month = as.integer(month(date)), year = as.factor(year(date))) |> 
  ggplot(aes(x = month, y = hum_incidence, colour = year)) +
  geom_xspline() +
  theme_minimal() +
  scale_x_continuous(
    labels = month.abb,
    breaks = 1:12
  )
d |> 
  mutate(month = as.integer(month(date)), year = as.factor(year(date))) |> 
  ggplot(aes(x = month, y = animal_incidence, colour = year)) +
  geom_xspline() +
  theme_minimal() +
  scale_x_continuous(
    labels = month.abb,
    breaks = 1:12
  )

# Decomposing the time series ---------------------------------------------

df_stl  <- d |> 
  model(STL(hum_incidence))

components(df_stl) |>
  as_tsibble() |>
  ggplot(aes(x = date)) +
  geom_xspline(aes(y = hum_incidence, colour = "Human Incidence")) +
  geom_xspline(aes(y = trend, color = 'Trend')) +
  labs(y = "Incidence",
       title = "Human Brucellois incidence") +
  theme_minimal() +
  theme(
    axis.title = element_text(color = 'black'),
    axis.text = element_text(color = 'black'),
    plot.title = element_text(color = 'black', hjust = .5),
    legend.position = 'bottom'
  ) +
  scale_color_manual(values = c("Human Incidence" = "gray", "Trend" = "#FE7501")) +
  labs(color = 'Series:')

components(df_stl) |>
  as_tsibble() |>
  ggplot(aes(x = date)) +
  geom_xspline(aes(y = hum_incidence, colour = "Human Incidence")) +
  geom_xspline(aes(y = season_year, color = 'Season')) +
  labs(y = "Incidence",
       title = "Human Brucellois incidence") +
  theme_minimal() +
  theme(
    axis.title = element_text(color = 'black'),
    axis.text = element_text(color = 'black'),
    plot.title = element_text(color = 'black', hjust = .5),
    legend.position = 'bottom'
  ) +
  scale_color_manual(values = c("Human Incidence" = "gray", "Season" = "#FE7501")) +
  labs(color = 'Series:')

components(df_stl) |>
  autoplot() +
  theme_minimal()

# Testing for stationarity of the model -----------------------------------

# Null hypothesis: The series has a unit root (is non-stationary)
# Alternative hypothesis: The series is stationary

adf.test(d$hum_incidence)

# Preparing data for time series model -------------------------------------------------------

# Splitting data into 80% training and 20% testing
nrow(d1)
nrow_80 <- floor(.8 * nrow(d1))
train <- d1[1:nrow_80,]
test <- d1[(nrow_80 + 1):nrow(d1),]

# The ACF plot ------------------------------------------------------------

acf(train$hum_incidence)
pacf(train$hum_incidence)

ccf_plot <- d |>
  CCF(hum_incidence, animal_incidence, lag_max = 12) |>
  autoplot() +
  theme_minimal()

ggsave(
  plot = ccf_plot,
  filename = "plots/ccf_plot.png",
  dpi = 5e2,
  width = 2 * 5,
  height = 1 * 5,
  units = "in",
  bg = NULL
)

# Training the model without the covariates -------------------------------

set.seed(123)
model_human <- train |>
  as.data.frame() %>%
  as_tsibble(index = date) |>
  model(ARIMA(hum_incidence, ic = "aic", stepwise = T))

fitted_hum <- augment(model_human) |> 
  select(-.model)

hum_hilo <- hilo(forecast::forecast(model_human, h = nrow(test)))

fc_hum <-  hilo(forecast::forecast(model_human, h = nrow(test))) |>
  as.data.frame() %>%
  mutate(
    lower_80 = `80%`$lower,
    upper_80 = `80%`$upper,
    lower_95 = `95%`$lower,
    upper_95 = `95%`$upper,
    actual_hum_incidence = test$hum_incidence,
    .mean = round(.mean, 3)
  ) |>
  select(date, forecast_hum_incidence  = .mean, actual_hum_incidence, lower_80, upper_80, lower_95, upper_95, )

human_only_plot <- ggplot() +
  geom_ribbon(data = fc_hum, aes(date, ymin = lower_95, ymax = upper_95, fill = "95% CI"), alpha = 0.1) +
  geom_line(data  = d1, aes(x = date, y = hum_incidence, colour = "Observed human incidence"), linewidth = .7) +
  geom_vline(xintercept = as.Date(yearmonth("2021 Mar")), linetype = 2) + 
  geom_line(data  = fitted_hum, aes(x = date, y = .fitted, color = "Fitted human incidence"), linewidth = .7) +
  geom_ribbon(data = fc_hum, aes(date, ymin = lower_80, ymax = upper_80, fill = "80% CI"), alpha = 0.4) +
  geom_line(data = fc_hum, aes(date, forecast_hum_incidence, color = "Predicted human incidence"))+
  scale_color_manual(
    values = c(
      "Fitted human incidence" = "blue",
      "Observed human incidence" = "black",
      'Predicted human incidence' = 'red'
    )
  ) +
  scale_fill_manual(
    values = c("95% CI" = "blue", "80% CI" = "blue"),
    guide = guide_legend(override.aes = list(alpha = c(0.25, 1)))
  ) +
  labs(col = NULL, fill = NULL) +
  theme_minimal() +
  theme(
    axis.text = element_text(color = "black", size = 15),
    axis.title = element_text(color = "black", size = 16),
    axis.ticks = element_line(color = "black"),
    legend.position = "bottom",
    plot.title = element_text(color = "black", hjust = .5),
    legend.text = element_text(color = "black", size = 15),
    legend.title = element_text(color = "black", size = 16),
    legend.key.size = unit(1.5, "lines")
  ) +
  labs(x = "Date", y = "Incidence")
  
ggsave(
  plot = human_only_plot,
  filename = "plots/human_only_plot.png",
  dpi = 5e2,
  width = 2 * 5,
  height = 1 * 5,
  units = "in",
  bg = NULL
)


# Fitting the model with animal incidence as exogenus variable ------------

results <- tsFunction(train, test, max_lag = 6)
metrics <- map_dfr(results, ~calculate_metrics(.$forecast))
fwrite(metrics, "tables/time_series_lags.csv", row.names = F)
# The best model is lag1 from the Time series linear models

# Plotting for the Best model at lag 1
best_train_model_plot <- ggplot() +
  geom_ribbon(data = results[[2]]$forecast, aes(date, ymin = lower_95, ymax = upper_95, fill = "95% CI"), alpha = 0.1) +
  geom_line(data  = d1, aes(x = date, y = hum_incidence, colour = "Observed human incidence"), linewidth = .7) +
  geom_vline(xintercept = as.Date(yearmonth("2021 Mar")), linetype = 2) + 
  geom_line(data  = results[[2]]$fitted, aes(x = date, y = .fitted, color = "Fitted human incidence"), linewidth = .7) +
  geom_ribbon(data = results[[2]]$forecast, aes(date, ymin = lower_80, ymax = upper_80, fill = "80% CI"), alpha = 0.4) +
  geom_line(data = results[[2]]$forecast, aes(date, forecast_hum_incidence, color = "Predicted human incidence"))+
  scale_color_manual(
    values = c(
      "Fitted human incidence" = "blue",
      "Observed human incidence" = "black",
      'Predicted human incidence' = 'red'
    )
  ) +
  scale_fill_manual(
    values = c("95% CI" = "blue", "80% CI" = "blue"),
    guide = guide_legend(override.aes = list(alpha = c(0.25, 1)))
  ) +
  labs(col = NULL, fill = NULL) +
  theme_minimal() +
  theme(
    axis.text = element_text(color = "black", size = 15),
    axis.title = element_text(color = "black", size = 16),
    axis.ticks = element_line(color = "black"),
    legend.position = "bottom",
    plot.title = element_text(color = "black", hjust = .5),
    legend.text = element_text(color = "black", size = 15),
    legend.title = element_text(color = "black", size = 16),
    legend.key.size = unit(1.5, "lines")
  ) +
  labs(x = "Date", y = "Incidence")

ggsave(
  plot = best_train_model_plot,
  filename = "plots/best_train_model_plot.png",
  dpi = 5e2,
  width = 2 * 5,
  height = 1 * 5,
  units = "in",
  bg = NULL
)
ggsave(
  plot = wrap_plots(human_only_plot, best_train_model_plot, nrow = 2) & 
    plot_annotation(
      title = "",
      tag_levels = list(c("(a)", "(b)"))
    ) ,
  filename = "plots/train_models_plot.png",
  dpi = 5e2, 
  width = 3 * 5,
  height = 2 * 5,
  units = "in",
  bg = NULL
)

# Comparing the model with no animal incidence and that with --------------

model_humans <- fc_hum |>
  summarise(
    MAE = mean(abs(
      actual_hum_incidence - forecast_hum_incidence
    )),
    RMSE = sqrt(mean((actual_hum_incidence - forecast_hum_incidence)^2
    )),
    MAPE = mean(abs((actual_hum_incidence - forecast_hum_incidence) / actual_hum_incidence
    )) * 100
  ) |>
  mutate(model = "Model with no exogenus variable") |>
  select(model, everything())
model_animal <- calculate_metrics(results[[2]]$forecast) |>
  select(-lag) |>
  mutate(model = "Model with exogenus variable") |>
  select(model, everything())

model_metrics_results <- rbind(model_humans, model_animal)
fwrite(model_metrics_results, "tables/time_series_comparison.csv", row.names = F)

# Retraining the model at lag 1 -------------------------------------------

test2 <- d |>
  filter(date > yearmonth("2022-12-01"))

full_model <- d |> 
  filter(date <= yearmonth("2022-12-01")) |> 
  as_tsibble(index = date) |> 
  model(ARIMA(hum_incidence, ic = "aic", stepwise = T))

fitted_full <- augment(full_model) |> 
  select(-.model)

full_hilo <- hilo(forecast::forecast(full_model, new_data = test2 |> select(-hum_incidence)))

fc_full <-  hilo(forecast::forecast(full_model,  new_data = test2)) |>
  as.data.frame() %>%
  mutate(
    lower_80 = `80%`$lower,
    upper_80 = `80%`$upper,
    lower_95 = `95%`$lower,
    upper_95 = `95%`$upper,
    actual_hum_incidence = test2$hum_incidence,
    .mean = round(.mean, 3)
  ) |>
  select(date, forecast_hum_incidence  = .mean, actual_hum_incidence, lower_80, upper_80, lower_95, upper_95, )

full_model_plot <- ggplot() +
  geom_ribbon(data = fc_full, aes(date, ymin = lower_95, ymax = upper_95, fill = "95% CI"), alpha = 0.1) +
  geom_line(data  = d, aes(x = date, y = hum_incidence, colour = "Observed human incidence"), linewidth = .7) +
  geom_vline(xintercept = as.Date(yearmonth("2022 Dec")), linetype = 2) + 
  geom_line(data  = fitted_full, aes(x = date, y = .fitted, color = "Predicted human incidence"), linewidth = .7) +
  geom_ribbon(data = fc_full, aes(date, ymin = lower_80, ymax = upper_80, fill = "80% CI"), alpha = 0.4) +
  geom_line(data = fc_full, aes(date, forecast_hum_incidence, color = "Forecasted human incidence"))+
  scale_color_manual(
    values = c(
      "Predicted human incidence" = "blue",
      "Observed human incidence" = "black",
      'Forecasted human incidence' = 'red'
    )
  ) +
  scale_fill_manual(
    values = c("95% CI" = "blue", "80% CI" = "blue"),
    guide = guide_legend(override.aes = list(alpha = c(0.25, 1)))
  ) +
  labs(col = NULL, fill = NULL) +
  theme_minimal() +
  theme(
    axis.text = element_text(color = "black", size = 15),
    axis.title = element_text(color = "black", size = 16),
    axis.ticks = element_line(color = "black"),
    legend.position = "bottom",
    plot.title = element_text(color = "black", hjust = .5),
    legend.text = element_text(color = "black", size = 15),
    legend.title = element_text(color = "black", size = 16),
    legend.key.size = unit(1.5, "lines")
  ) +
  labs(x = "Date", y = "Incidence")

ggsave( 
  plot = full_model_plot,
  filename = "plots/full_model_plot.png",
  dpi = 5e2,
  width = 2.5 * 5,
  height = 1 * 5,
  units = "in",
  bg = NULL
)

# Model diagnostics for the full model of time series ---------------------

ggsave(
  plot = gg_tsresiduals(full_model),
  filename = "plots/full_model_diagnostics.png",
  dpi = 5e2,
  width = 3 * 5,
  height = 2 * 5,
  units = "in",
  bg = NULL
)


























