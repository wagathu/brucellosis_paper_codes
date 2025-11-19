
# Importing packages ------------------------------------------------------

pacman::p_load(
  dplyr,
  tidyr,
  ggplot2,
  scales,
  patchwork,
  RColorBrewer,
  data.table,
  sf,
  stringr,
  purrr
)
source("R/functions.R")

# Importing data ----------------------------------------------------------

dfs <- readRDS("data/all_dfs.RDS")
list2env(dfs, envir = .GlobalEnv)
humanPop <- fread("data/population_county_2014_2021.csv")
animalPop <- fread("data/animalPop.csv")
county_shp <- st_read("shapefiles/gadm41_KEN_1.shp", query = "select NAME_1 as county from gadm41_KEN_1", quiet = T)

# Preparing data for plots ------------------------------------------------

smoothed_ah <- ah |>
  summarise(across(contains("cases"), ~ sum(., na.rm = T)), .by = c(year, date)) |> 
  merge(animalPop |>
          summarise(across(contains("pop"), ~ sum(.)), .by = year), , by = "year") |> 
  merge(humanPop |>
          summarise(across(contains("pop"), ~ sum(.)), .by = year), , by = "year") |> 
  mutate(
    hum_incidence = hum_cases / pop * 1e6,
    catt_incidence = catt_cases / catt_pop * 1e6,
    cam_incidence = cam_cases / cam_pop * 1e6,
    goat_incidence = goat_cases / goat_pop * 1e6,
    shp_incidence = shp_cases / sheep_pop * 1e6
  ) |> 
  arrange(date) |> 
  mutate(across(contains("incidence"), ~ zoo::rollmean(., k = 4, fill = NA, align = "right"), .names = "smoothed_{.col}")) |> 
  na.omit()

animal_sm_df <- smoothed_ah |>
  select(date, contains("incidence"), -hum_incidence, -smoothed_hum_incidence) |>
  pivot_longer(-date, names_to = "variable", values_to = "value") |>
  mutate(
    species = str_extract(variable, "catt|cam|goat|shp"),
    type = if_else(str_starts(variable, "smoothed"), "Smoothed", "Original"),
    species = case_when(
      species == "catt" ~ "Cattle",
      species == "cam" ~ "Camels",
      species == "goat" ~ "Goats",
      species == "shp" ~ "Sheep",
    )
  ) 

maps_df <- yah |>
  select(year, county, diagnosis, contains("incidence")) |>
  summarise(across(contains("incidence"), ~ sum(., na.rm = T)), .by = c(year, county)) |>
  mutate(
    county = case_when(
      county == "Elgeyo Marakwet" ~ "Elgeyo-Marakwet",
      county == "Tharaka Nithi" ~ "Tharaka-Nithi",
      TRUE ~ county
    ),
    human_cat = {
      breaks <- quantile(hum_incidence, na.rm = TRUE)
      labels <- paste(round(breaks[-length(breaks)], 2), round(breaks[-1], 2), sep = "-")
      cut_result <- cut(
        hum_incidence,
        breaks = breaks,
        labels = labels,
        include.lowest = TRUE
      )
      result <- as.character(cut_result)
      result[is.na(result)] <- "0"
      factor(result, levels = c("0", labels))
    }
  ) |>
  pivot_longer(cols = contains("incidence") &
                 !starts_with("hum_")) |>
  mutate(value = fifelse(value == 0, NA, value), cat_value = {
    breaks <- quantile(value, na.rm = TRUE)
    labels <- paste(round(breaks[-length(breaks)], 2), round(breaks[-1], 2), sep = "-")
    cut_result <- cut(
      value,
      breaks = breaks,
      labels = labels,
      include.lowest = TRUE
    )
    result <- as.character(cut_result)
    result[is.na(result)] <- "0"
    factor(result, levels = c("0", labels))
  }) |>
  pivot_wider(names_from = name,
              values_from = c(value, cat_value)) |> merge(data.table(county_shp), by = "county") |>
  st_as_sf()
  
# Plotting for the smootvalues_to = # Plotting for the smoothed values ----------------------------------------

human_sm <- smoothed_ah |> 
  ggplot(aes(x = date, y = smoothed_hum_incidence)) +
  geom_line(col = "black") +
  geom_point(aes(y = hum_incidence), color = "red") +
  scale_y_continuous(labels = comma) +
  theme_light() +
  theme(
    strip.background = element_rect(fill = "white", colour = "grey"),
    strip.text = element_text(color = "black", size = 17, face = "bold"),
    axis.title = element_text(colour = "black", size = 17),
    axis.text = element_text(color = "black", size = 16),
    axis.ticks = element_line(color = "black", linewidth = 1),
    plot.title = element_text(color = "black", hjust = 0.5, size = 16),
    legend.position = "bottom",
    legend.text = element_text(color = "black", size = 16),
    legend.key.size = unit(3, "lines")
  ) +
  ylab("Incidence") +
  xlab("Year") 

# For animals
animal_sm <- animal_sm_df |>
  ggplot(aes(x = date, y = value, colour = species)) +
  geom_point(data = . %>% filter(type == "Original"), alpha = 0.6) +
  geom_line(data = . %>% filter(type == "Smoothed"), linewidth = 1) +
  theme_light() +
  theme(
    strip.background = element_rect(fill = "white", colour = "grey"),
    strip.text = element_text(color = "black", size = 17, face = "bold"),
    axis.title = element_text(colour = "black", size = 17),
    axis.text = element_text(color = "black", size = 16),
    axis.ticks = element_line(color = "black", linewidth = 1),
    plot.title = element_text(color = "black", hjust = 0.5, size = 16),
    legend.position = "bottom",
    legend.title = element_text(color = "black", size = 17),
    legend.text = element_text(color = "black", size = 16),
    legend.key.size = unit(2.5, "lines"),
    legend.key.height = unit(10, "lines")
  ) +
  guides(colour = guide_legend(override.aes = list(linewidth = 1.5))) +
  guides(colour = guide_legend(override.aes = list(size = 2))) +
  ylab("Incidence") +
  xlab("Year") +
  labs(col = "Species")

smoothed_plot <- human_sm / animal_sm & plot_annotation(title = "", tag_levels = list(c("(a)", "(b)")))
ggsave(
  plot = smoothed_plot,
  filename = "plots/smoothed_plot.png",
  dpi = 1e3,
  width = 2 * 5,
  height = 2 * 5,
  units = "in",
  bg = 'white'
)

# The maps ----------------------------------------------------------------

levels <- levels(maps_df$catt_incidence_range)
cate_catt <- length(levels(maps_df$catt_incidence_range))


columns <- c(
  "Humans" = "human_cat",
  "Cattle" = "cat_value_catt_incidence", 
  "Goats" = "cat_value_goat_incidence",
  "Camels" = "cat_value_cam_incidence",
  "Sheep" = "cat_value_shp_incidence"
)

maps <- imap(columns, \(column, title) {
  fill <- if (column == "human_cat") "Human incidence" else NULL
  mapFunction(maps_df, column = column, fill = fill, title = title)
})

all_plots <-
  wrap_plots(
    maps,
    guides = "collect",
    ncol = 1
  ) &
  theme(
    plot.caption = element_text(size = 16, colour = "black"),
    legend.position = "right",
    legend.key.size = unit(3, "lines"),
    legend.key.height = unit(3, "lines"),
    legend.spacing.x = unit(0.5, "lines"),
    legend.spacing.y = unit(2, "lines"),
    legend.title = element_text(color = "black", size = 26),
    legend.text = element_text(color = "black", size = 26),
    legend.box.spacing = unit(0.2, "cm"),   
    legend.margin = margin(l = -20),         
    plot.margin = margin(r = 5) 
  )

ggsave(
  plot = all_plots,
  filename = "plots/all_plots.png",
  dpi = 7e2,
  width = 8 * 5,
  height = 5 * 5,
  units = "in",
  bg = 'white'
)
