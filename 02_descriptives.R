
# Importing packages ------------------------------------------------------

pacman::p_load(
  data.table,
  readxl,
  dplyr,
  stringr,
  stringi,
  purrr,
  sf,
  lubridate,
  scales
)

# Importing data ----------------------------------------------------------

dfs <- readRDS("data/all_dfs.RDS")
list2env(dfs, envir = .GlobalEnv)

# Descriptive statistics at country level ---------------------------------

# Cases descriptives
table1a <- kenya_yah |>
  summarise(across(contains("cases"), ~ sum(., na.rm = T)), .by = c(diagnosis)) |>
  pivot_longer(cols = -1, names_to = "Species") |>
  mutate(total_numeric = sum(value), .by = Species) |>
  mutate(`Total cases` = comma(total_numeric)) |>
  pivot_wider(
    values_from = value,
    names_from = diagnosis,
    values_fn = list(value = ~ .x)
  ) |>
  mutate(across(
    c(`Clinically confirmed`, `Lab confirmed`, `Post Mortem`),
    ~ paste0(comma(.x), " (", round(.x / total_numeric * 100, 2), "%)")
  ), .keep = "unused") |> 
  mutate(
    `Average cases` = comma(as.numeric(str_remove_all(`Total cases`, ","))/9),
    Species = case_when(
      Species ==  "hum_cases" ~   "Human",
      Species =="catt_cases"  ~  "Cattle", 
      Species == "goat_cases" ~  "Goat", 
      Species == "cam_cases"  ~ "Camel", 
      Species == "shp_cases" ~  "Sheep"
    )
  ) |> 
  select(Species, `Total cases`, `Average cases`, everything())

# Incidence descriptives
table1b <- kenya_ah |> 
  summarise(
    across(contains("incidence"), ~sum(., na.rm = T)),
    .by = c(year)
  ) |> 
  pivot_longer(cols = -1, names_to = 'Species') |> 
  summarise(
    `Mean incidence rate` = format(round(mean(value, na.rm = TRUE), 2), big.mark = ","),
    `Minimum incidence rate` = (round(min(value, na.rm = TRUE), 2)),
    `Median incidence rate` = (round(median(value, na.rm = TRUE), 2)),
    `Maximum incidence rate` = (round(max(value, na.rm = TRUE), 2)),
    `SD of incidence rate` = (round(sd(value, na.rm = TRUE), 2)),
    .by = Species
  ) |> 
  mutate(
    Species = case_when(
      Species ==  "hum_incidence" ~   "Human",
      Species =="catt_incidence"  ~  "Cattle", 
      Species == "goat_incidence" ~  "Goat", 
      Species == "cam_incidence"  ~ "Camel", 
      Species == "shp_incidence" ~  "Sheep"
    )
  ) 

table1 <- merge(table1a, table1b, by = 'Species', sort = F)
fwrite(table1, "tables/table1.csv", row.names = F)

# Descriptive statistics at county level ----------------------------------

table2a <- yah |>
  summarise(across(contains("cases"), ~ sum(., na.rm = T)), .by = c(county, diagnosis)) |>
  pivot_longer(cols = -c(1:2), names_to = "Species") |>
  mutate(total_numeric = sum(value, na.rm = T), .by = c(county, Species)) |>
  mutate(`Total cases` = comma(total_numeric)) |>
  pivot_wider(
    values_from = value,
    names_from = diagnosis,
    values_fn = list(value = ~ .x),
    values_fill = 0
  ) |>
  mutate(across(
    c(`Clinically confirmed`, `Lab confirmed`, `Post Mortem`),
    ~ paste0(comma(.x), " (", round(.x / total_numeric * 100, 2), "%)")
  ), .keep = "unused") |> 
  mutate(across(
    c(`Clinically confirmed`, `Lab confirmed`, `Post Mortem`),
    ~ str_replace(., 'NaN', '0')
  ), .keep = "unused") |> 
  mutate(
    `Average cases` = comma(as.numeric(str_remove_all(`Total cases`, ","))/9),
    Species = case_when(
      Species ==  "hum_cases" ~   "Human",
      Species =="catt_cases"  ~  "Cattle", 
      Species == "goat_cases" ~  "Goat", 
      Species == "cam_cases"  ~ "Camel", 
      Species == "shp_cases" ~  "Sheep"
    )
  )  |> 
  select(Species, `Total cases`, `Average cases`, everything())

table2b <- ah |> 
  summarise(
    across(contains("incidence"), ~sum(., na.rm = T)),
    .by = c(county, year)
  ) |> 
  pivot_longer(cols = -c(1, 2), names_to = 'Species') |> 
  summarise(
    `Mean incidence rate` = format(round(mean(value, na.rm = TRUE), 2), big.mark = ","),
    `Minimum incidence rate` = (round(min(value, na.rm = TRUE), 2)),
    `Median incidence rate` = (round(median(value, na.rm = TRUE), 2)),
    `Maximum incidence rate` = (round(max(value, na.rm = TRUE), 2)),
    `SD of incidence rate` = (round(sd(value, na.rm = TRUE), 2)),
    .by = c(county, Species)
  ) |> 
  mutate(
    Species = case_when(
      Species ==  "hum_incidence" ~   "Human",
      Species =="catt_incidence"  ~  "Cattle", 
      Species == "goat_incidence" ~  "Goat", 
      Species == "cam_incidence"  ~ "Camel", 
      Species == "shp_incidence" ~  "Sheep"
    )
  ) 

table2 <- merge(table2a, table2b, by = c('county', 'Species'), sort = F)
fwrite(table2, "tables/table2.csv", row.names = F)

