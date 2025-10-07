
# Importing packages ------------------------------------------------------

pacman::p_load(
  readxl,
  dplyr,
  tidyr,
  stringr
)

# Importing data ----------------------------------------------------------

p09 <- read_excel("data/Projections.xlsx", sheet = "2009")
p19 <- read_excel("data/Projections.xlsx", sheet = "2019")

# Cleaning population data ------------------------------------------------


# 2009
colnames(p09) <- p09[1,] #Make the 1st row the header
p09 <- p09[-1, ]  #Remove the 1st row

p09_clean <- p09 |>
  filter(
    !County %in% c(
      "Kenya",
      "Central",
      "Coast",
      "Eastern",
      "Western",
      "N. Eastern",
      "Nyanza",
      "R. Valley"
    )
  ) |> 
  distinct() |>
  dplyr::select(County, "2014", "2015") 

# 2019
p19_clean <- p19 |>
  dplyr::select(County, "2020", "2021", "2022", "2023", "2024", "2025") |>
  setNames(c("County", "pop2020", "pop2021", "pop2022", "pop2023", "pop2024", "pop2025")) |> 
  na.omit() |> 
  mutate(across(2:7, ~ as.numeric(str_remove(., ","))),
         across(2:7, ~ . * 1000))

p19_clean2 <- p19_clean |> 
  filter(County != "Kenya") |> 
  pivot_longer(
    cols = -County,
    names_to = "Year",
    values_to = "pop"
  ) |>
  arrange()


p19_clean2_kenya <- p19_clean |> 
  filter(County == "Kenya") |> 
  pivot_longer(
    cols = -County,
    names_to = "Year",
    values_to = "pop"
  ) |>
  arrange() |> 
  dplyr::select(County,Year, pop) |> 
  mutate(Year = str_replace(Year, "pop", "") |> str_squish() |> as.numeric())

write.csv(p19_clean2_kenya, "kenya_pop_2020_2025.csv")

# Growth Rate
p19_growth <- p19_clean2 |> 
  group_by(County) |> 
  arrange(County, Year) |> 
  mutate(growth_rate = (pop - lag(pop)) / lag(pop) * 100) |> 
  group_by(County) |>
  summarise(growth_rate = mean(growth_rate, na.rm = T) |> round(2)) 
write.csv(p19_growth, "data/growthrate.csv")

# County Population for 20222-2023
pop_22_23 <- p19_clean2 %>%
  setNames(c(str_to_lower(colnames(.)))) |> 
  mutate(year = str_remove(year, '[A-Za-z]+') |> 
           str_squish() |> 
           as.numeric()) |> 
  filter(year %in% c(2022, 2023))

# 2019 Population
pop <- rKenyaCensus::V1_T2.2 |> 
  dplyr::select(County, pop2019 = Total) |> 
  filter(!County %in% c("Total")) |>
  arrange(County) |> 
  mutate(County = case_when(
    County == "Elgeyo/Marakwet" ~ "Elgeyo Marakwet",
    County == "Nairobi City" ~ "Nairobi",
    County == "Taita/Taveta" ~ "Taita Taveta",
    County == "Tharaka-Nithi" ~ "Tharaka Nithi",
    TRUE ~ County
  )) |> 
  merge(p19_growth, by = "County") |> 
  mutate(
    pop2018 = round(pop2019 * (1 - (growth_rate / 100))),
    pop2017 = round(pop2018 * (1 - (growth_rate / 100))),
    pop2016 = round(pop2017 * (1 - (growth_rate / 100))),
    pop2015 = round(pop2016 * (1 - (growth_rate / 100))),
    pop2014 = round(pop2015 * (1 - (growth_rate / 100)))
  ) |> 
  merge(p19_clean |> dplyr::select(1:4), by = "County") |> 
  dplyr::select(County,
                pop2014,
                pop2015,
                pop2016,
                pop2017,
                pop2018,
                pop2019,
                pop2020,
                pop2021
  ) |>
  pivot_longer(
    cols = -County,
    values_to = "pop"
  ) |> 
  mutate(year = case_when(
    name == "pop2014" ~ 2014,
    name == "pop2015" ~ 2015,
    name == "pop2016" ~ 2016,
    name == "pop2017" ~ 2017,
    name == "pop2018" ~ 2018,
    name == "pop2019" ~ 2019,
    name == "pop2020" ~ 2020,
    name == "pop2021" ~ 2021
  )) |> 
  dplyr::select(county = County, year, pop) |> 
  rbind(pop_22_23 |>  filter(year != 2023))
write.csv(pop, "data/population_county_2014_2021.csv", row.names = F)
