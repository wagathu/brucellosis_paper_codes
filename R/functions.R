# Functions for drawing maps
mapFunction <- \(df, column, title = NULL, levels = NULL, fill = NULL) {
  if (is.null(fill)) {
    fill <- "Animal incidence"
  }
  map <- maps_df |>
    ggplot() +
    geom_sf(aes(fill = get(column)), show.legend = T, color = "black") +
    scale_fill_manual(
      values = c("white", brewer.pal(5, "YlOrRd")),
      drop = F,
      na.value = "white",
      labels = function(x) {
        sapply(x, function(single_x) {
          if (single_x == "0") return("0")
          parts <- strsplit(single_x, "-")[[1]]
          if (length(parts) == 2) {
            formatted_parts <- sapply(parts, function(p) {
              num <- as.numeric(p)
              format(num, big.mark = ",", nsmall = 2, digits = 2)
            })
            paste(formatted_parts, collapse = " – ")
          } else {
            single_x
          }
        })
      }
    ) +
    theme_void() +
    facet_wrap( ~ year, nrow = 1) +
    theme(
      plot.title = element_text(
        color = "black",
        hjust = .5,
        size = 30,
        vjust = .8
      ),
      legend.position = "bottom",
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 12, colour = "black"),
      legend.key.size = unit(0.7, "cm"),
      strip.text = element_text(colour = "black", size = 26, vjust = -.1)
    ) +
    ggtitle(title) +
    labs(fill = fill)
  return(map)
}

# Function for correlation testing
corFunction <- function(df, lag = 0) {
  if (lag > 0) {
    df_processed <- df %>%
      as_tibble() %>%
      mutate(across(
        c(
          catt_incidence,
          cam_incidence,
          goat_incidence,
          shp_incidence
        ),
        ~ lag(., n = lag)
      )) %>%
      na.omit()
  } else {
    df_processed <- df %>%
      as_tibble()
  }
  cor_matrix <- df_processed %>%
    select(-date) %>%
    setNames(c("Human", "Cattle", "Camel", "Goat", "Sheep")) %>%
    cor()
  subtitle_text <- if (lag == 0) {
    "Correlation between human incidence \nand animal incidences"
  } else {
    paste0("Correlation between\nhuman incidence \nand animal incidences at lag ",
           lag)
  }
  
  plot <- cor_matrix %>%
    ggcorrplot::ggcorrplot(type = "upper",
                           lab = TRUE,
                           lab_size = 6) +
    theme_light() +
    labs(title = paste("Lag", lag), x = NULL, y = NULL) +
    guides(fill = "none") +
    theme(
      strip.background = element_rect(fill = "white", colour = "grey"),
      strip.text = element_text(color = "black", size = 12),
      axis.title = element_text(colour = "black"),
      axis.text = element_text(color = "black"),
      axis.ticks = element_line(color = "black", linewidth = 1),
      plot.title = element_text(
        color = "black",
        hjust = 0.5,
        size = 14
      ),
      plot.subtitle = element_text(
        color = "black",
        hjust = 0.5,
        size = 14
      ),
      plot.caption = element_text(
        color = "black",
        hjust = 0.5,
        size = 12
      ),
      axis.title.y = element_text(color = "black", size = 10),
      legend.position = "right",
      legend.text = element_text(color = "black")
    )
  human_animal_cors <- cor_matrix["Human", c("Cattle", "Camel", "Goat", "Sheep")]
  avg_abs_cor <- mean(abs(human_animal_cors))
  
  return(list(plot = plot, data = cor_matrix, avg_abs_cor = avg_abs_cor))
}

# Running TSLM models
tslmFunction <- function(df = df1, max_lag = 6, combined = FALSE, ...) {
  suppressMessages({
    result_df <- tibble()
    models <- list()
    for (lag_value in 0:max_lag) {
      df_lagged <- df |>
        arrange(date) |> 
        mutate(date = yearmonth(date)) |> 
        as_tsibble(index = date) %>%
        mutate(across(contains("incidence"), ~fifelse(is.nan(.), 0, .))) |> 
        mutate(across(contains("incidence"), ~ difference(.))) %>%
        na.omit() %>%
        mutate(across(
          c(catt_incidence, cam_incidence, goat_incidence, shp_incidence),
          ~ lag(., n = lag_value)
        )) |>
        na.omit() %>%
        mutate(animal_incidence = catt_incidence + cam_incidence + goat_incidence + shp_incidence)
      
      if (combined) {
        mod <- df_lagged |>
          as_tsibble() |>
          model(TSLM(hum_incidence ~ animal_incidence)) 
      } else {
        mod <- df_lagged |>
          as_tsibble() |>
          model(TSLM(
            hum_incidence ~ cam_incidence + shp_incidence + catt_incidence + goat_incidence
          ))
      }
      
      mod_results <- tidy(mod) %>%
        dplyr::select(-.model) %>%
        as_tibble() %>%
        mutate(
          conf.low = estimate - 1.645 * std.error,
          conf.high = estimate + 1.645 * std.error,
          term = case_when(
            term == "goat_incidence" ~ "Goat Incidence",
            term == "catt_incidence" ~ "Cattle incidence",
            term == "shp_incidence" ~ "Sheep incidence",
            term == "cam_incidence" ~ "Camel incidence",
            term == "animal_incidence" ~ "Total animal incidence",
            TRUE ~ as.character(term)
          ),
          variable = term,
          lag = lag_value
        ) |> 
        dplyr::select(variable, estimate, std.error, statistic, p.value, conf.low, conf.high, lag)
      
      adj_r_squared <- glance(mod) %>%
        dplyr::select(r_squared, AIC, adj_r_squared)
      
      mod_results <- bind_cols(mod_results, adj_r_squared) |>
        mutate(across(
          c(estimate, std.error, statistic, p.value, conf.low, conf.high, adj_r_squared),
          ~ round(., 3)
        )) |>
        mutate(significance = ifelse(conf.low * conf.high > 0, "Significant", "Not Significant"))
      
      result_df <- bind_rows(result_df, mod_results)
      models[[paste0("lag_", lag_value)]] <- mod
    }
  })
  models <- setNames(models, paste0("model", 0:max_lag))
  return(
    list(
      result_df = result_df,
      models = models
    )
  )
}

# Function for residuals plotting
gg_tsdisplay <- function (data, y = NULL, plot_type = c("auto", "partial", "season", 
                                                        "histogram", "scatter", "spectrum"), lag_max = NULL) 
{
  if (n_keys(data) > 1) {
    abort("The data provided to contains more than one time series. Please filter a single time series to use `gg_tsdisplay()`")
  }
  require("grid")
  y <- feasts:::guess_plot_var(data, !!enquo(y))
  plot_type <- match.arg(plot_type)
  if (plot_type == "auto") {
    period <- get_frequencies(NULL, data, .auto = "all")
    if (all(period <= 1)) {
      plot_type <- if (any(is.na(data[[rlang::as_name(y)]]))) 
        "partial"
      else "spectrum"
    }
    else {
      plot_type <- "season"
    }
  }
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(2, 
                                                               2)))
  # Add theme_minimal() to p1
  p1 <- ggplot(data, aes(x = !!index(data), y = !!y)) + 
    geom_line() + 
    geom_point() + 
    theme_minimal()
  
  # Add theme_minimal() to p2
  p2 <- autoplot(ACF(data, !!y, lag_max = lag_max)) + 
    theme_minimal()
  
  if (plot_type == "partial") {
    p3 <- autoplot(PACF(data, !!y, lag_max = lag_max)) + 
      theme_minimal()
    p2_yrange <- ggplot2::layer_scales(p2)$y$range$range
    p3_yrange <- ggplot2::layer_scales(p3)$y$range$range
    yrange <- range(c(p2_yrange, p3_yrange))
    p2 <- p2 + ylim(yrange)
    p3 <- p3 + ylim(yrange)
  }
  else if (plot_type == "season") {
    p3 <- gg_season(data, !!y) + theme_minimal()
  }
  else if (plot_type == "histogram") {
    p3 <- ggplot(data, aes(x = !!y)) + 
      geom_histogram(bins = min(500, grDevices::nclass.FD(na.exclude(data[[rlang::as_name(y)]])))) + 
      ggplot2::geom_rug() + 
      theme_minimal()
  }
  else if (plot_type == "scatter") {
    p3 <- data %>% 
      mutate(`:=`(!!paste0(rlang::as_name(y), "_lag"), lag(!!y, 1))) %>% 
      .[complete.cases(.), ] %>% 
      ggplot(aes(y = !!y, x = !!sym(paste0(rlang::as_name(y), "_lag")))) + 
      geom_point() + 
      xlab(expression(Y[t - 1])) + 
      ylab(expression(Y[t])) + 
      theme_minimal()
  }
  else if (plot_type == "spectrum") {
    spec <- safely(stats::spec.ar)(eval_tidy(y, data), plot = FALSE)
    p3 <- if (is.null(spec[["result"]])) {
      if (spec$error$message == "missing values in object") {
        warn("Spectrum plot could not be shown as the data contains missing values. Consider using a different `plot_type`.")
      }
      else {
        warn(sprintf("Spectrum plot could not be shown as an error occurred: %s", 
                     spec$error$message))
      }
      ggplot() + 
        ggplot2::labs(x = "frequency", y = "spectrum") + 
        theme_minimal()
    }
    else {
      spec[["result"]] %>% {
        tibble(spectrum = drop(.$spec), frequency = .$freq)
      } %>% 
        ggplot(aes(x = !!sym("frequency"), y = !!sym("spectrum"))) + 
        geom_line() + 
        ggplot2::scale_y_log10() + 
        theme_minimal()
    }
  }
  structure(list(p1, p2, p3), class = c("gg_tsensemble", "gg"))
}

# Function for residuals plotting
gg_tsresiduals <- function (data, type = "innovation", ...) 
{
  if (!fabletools::is_mable(data)) {
    abort("gg_tsresiduals() must be used with a mable containing only one model.")
  }
  data <- stats::residuals(data, type = type)
  if (n_keys(data) > 1) {
    abort("gg_tsresiduals() must be used with a mable containing only one model.")
  }
  out <- gg_tsdisplay(data, !!sym(".resid"), plot_type = "histogram", 
                      ...)
  out[[1]] <- out[[1]] + ggplot2::ylab(sub("([[:alpha:]])(.+)", 
                                           "residuals", type, perl = TRUE)) +
    theme(
      axis.text = element_text(color = "black", size = 20),
      axis.title = element_text(color = "black", size = 17)
    )
  
  out
}

# Function for time series model
tsFunction <- function(train, test, max_lag = 6) {
  results <- list()
  for (lag_value in 0:max_lag) {
    temp <- train |>
      mutate(animal_incidence = lag(animal_incidence, n = lag_value)) |>
      na.omit()
    
    test_tsibble <- test |>
      select(-hum_incidence) |>
      as_tsibble()
    
    # Fit model
    model <- temp |>
      as.data.frame() |>
      as_tsibble() |>
      model(ARIMA(
        hum_incidence ~ animal_incidence,
        ic = "aic",
        stepwise = TRUE
      ))
    fitted_hum <- augment(model) |>
      select(-.model)
    
    forecast_obj <- forecast::forecast(model, new_data = test_tsibble)
    hum_hilo <- hilo(forecast_obj)
    
    # Process forecast results
    fc_hum <- hum_hilo |>
      as.data.frame() |>
      mutate(
        lower_80 = `80%`$lower,
        upper_80 = `80%`$upper,
        lower_95 = `95%`$lower,
        upper_95 = `95%`$upper,
        actual_hum_incidence = test$hum_incidence,
        forecast_hum_incidence = round(.mean, 3),
        lag = lag_value
      ) |>
      select(
        date,
        lag,
        forecast_hum_incidence,
        actual_hum_incidence,
        lower_80,
        upper_80,
        lower_95,
        upper_95
      )
    results[[lag_value + 1]] <- list(
      lag = lag_value,
      model = model,
      fitted = fitted_hum,
      forecast = fc_hum
    )
  }
  return(results)
}

# Accuracy metrics
calculate_metrics <- function(forecast) {
  forecast_data <- forecast
  forecast_data |>
    summarise(
      lag = first(lag),
      MAE = mean(abs(actual_hum_incidence - forecast_hum_incidence)),
      RMSE = sqrt(mean((actual_hum_incidence - forecast_hum_incidence)^2)),
      MAPE = mean(abs((actual_hum_incidence - forecast_hum_incidence) / actual_hum_incidence)) * 100
    )
}
