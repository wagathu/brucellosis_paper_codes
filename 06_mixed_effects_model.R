
# Importing packages ------------------------------------------------------

pacman::p_load(
  data.table,
  ggplot2,
  lme4,
  glmmTMB,
  dplyr,
  tidyr,
  scales,
  patchwork,
  recipes
)

# Importing data ----------------------------------------------------------

list2env(readRDS("data/all_dfs.RDS"), envir = .GlobalEnv)

# Preparing data for the model --------------------------------------------

df_m <- ah2 |>
  select(year, date, county, contains("cases"), pop) |>
  mutate(hum_cases = as.integer(round(hum_cases)),
         countyID = as.numeric(as.factor(county))) |>
  mutate(timeID = as.numeric(as.factor(date)), .by = county) 

# Modelling ---------------------------------------------------------------

mixedModelFunction <- \(df, max_lag = 6, combined = F) {
  result_df <- tibble()
  models <- list()
  for (lag_value in 0:max_lag) {
    df1 <- df |>
      arrange(county, date) |>
      mutate(across(
        c(catt_cases, cam_cases, goat_cases, shp_cases),
        ~ lag(., n = lag_value)
      ), .by = county) |>
      na.omit() %>%
      mutate(animal_cases = catt_cases + cam_cases + goat_cases + shp_cases) |>
      select(-c(year, date, county))
    if (combined) {
      df_lagged <- df1 |>
        mutate(timeID2 = timeID) |>
        select(hum_cases, animal_cases, timeID, timeID2, countyID, pop) |>
        recipe(hum_cases + timeID + countyID + pop ~ animal_cases) |>
        step_center(all_double_predictors()) |>
        step_scale(all_double_predictors()) |>
        prep() |>
        juice()
      mod <- glmmTMB(
        hum_cases ~ animal_cases + timeID +  (1 + timeID | countyID),
        family = "poisson",
        data = df_lagged,
        offset = log(pop)
      )
    } else {
      df_lagged <- df1 |>
        mutate(timeID2 = timeID) |>
        select(-animal_cases) |>
        recipe(hum_cases + timeID + countyID + pop ~ catt_cases + cam_cases + goat_cases + shp_cases) |>
        step_center(all_double_predictors()) |>
        step_scale(all_double_predictors()) |>
        prep() |>
        juice()
      mod <- glmmTMB(
        hum_cases ~
          catt_cases +
          cam_cases +
          goat_cases +
          shp_cases +
          timeID +
          (1 + timeID | countyID),
        family = "nbinom1",
        data = df_lagged,
        offset = log(pop)
      )
    }
  }
  result_df <- tidy(mod, conf.int = T, conf.level = 0.90, exponentiate = T) |> 
    filter(is.na(group)) |> 
    filter(!term %in% c("timeID", "(Intercept)")) |> 
    select(-c(effect, component, group))
}
