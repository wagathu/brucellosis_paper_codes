

# Importing packages ------------------------------------------------------

pacman::p_load(data.table,
               dplyr,
               ggplot2,
               forecast,
               fable,
               fabletools,
               tseries,
               xts,
               stringr,
               readxl,
               tidyr,
               lubridate
               )

# Importing data ----------------------------------------------------------

list2env(readRDS("data/all_dfs.RDS"), envir = .GlobalEnv)
animalData4 <- read_excel("data/animal_brucellosis_2023.xlsx")
animalPop1 <- fread("data/animalPop.csv")
humanPop1 <- fread("data/county_yearly_population.csv")
h2 <- fread("data/h2.csv")

# Some initial cleaning ---------------------------------------------------

animalPop <- animalPop1 |>
  select(-year) |>
  summarise(across(where(is.integer), ~ first(.)), .by = county) |>
  mutate(year = 2023)

humanPop <- humanPop1 %>%
  setNames(c(str_to_lower(names(.)))) |>
  select(county, pop = pop2023) |>
  mutate(year = 2023)

# Cleaning the 2023 data --------------------------------------------------

#' All the initial cleaning of the above data was done in the
#' `01_data_preparation.R` file, but it didn't include data for
#' the year 2023, which, we require in this time series, so as
#' to test how well our time series model works
#'
#' This is why the cleaning of this data is found in this
#' file otherwise it would have been in `01_data_preparation.R`

a4 <- animalData4 |>
  dplyr::select(
    county = County,
    subcounty = `Sub-County`,
    ward = Ward,
    lat = Latitude,
    lng = Longitude,
    month = Month,
    year = Year,
    disease_condition =  Disease,
    diagnosis = `Nature of Diagnosis`,
    species = `Species Affected`,
    no_sick = `Number Sick`
  ) |>
  filter(!is.na(month) & county != "") |>
  mutate(
    species = str_to_title(species) |>
      str_trim(),
    species = case_when(
      species %in% c("Bobine") ~ "Bovine",
      species %in% c("Bufallo") ~ "Buffalo",
      species %in% c("Cani", "Canine(Jackal)", "Cannine", "Dog", "Dogs") ~ "Canine",
      species %in% c("Goat") ~ "Goats",
      species %in% c("", "Nd") ~ NA_character_,
      TRUE ~ species
    ),
    species = case_when(
      species == "Bovine & Canine" ~ "Canine",
      species == "Caprine/Ovine" ~ "Caprine",
      species == "Caprine+Donkey" ~ "Caprine",
      species == "Dogs/Donkey" ~ "Dogs",
      species == "Canine, Feline" ~ "Dogs",
      species == "Dogs, Cats" ~ "Dogs",
      TRUE ~ species
    ),
    species = case_when(
      species == "Camelidae" ~ "Camel",
      species == "Goats" ~ "Caprine",
      species == "Cats" ~ "Feline",
      species == "Dogs" ~ "Canine",
      species == "Sheep" ~ "Ovine",
      species == "Pig" ~ "Porcine",
      species == "Donkey" ~ "Equine",
      species == "Cattle" ~ "Bovine",
      species == "Pigs" ~ "Porcine",
      species == "White Tailed Mongoose" ~ "Mongoose",
      TRUE ~ species
    ),
    species = case_when(
      species == "Camel" ~ "Camels",
      species == "Bovine" ~ "cattle",
      species == "Equine" ~ "Donkeys",
      species == "Caprine" ~ "Goats",
      species == "Porcine" ~ "Pigs",
      species == "Ovine" ~ "Sheep",
      TRUE ~ species
    )
  ) |>
  mutate(
    county = str_to_title(county) |>
      str_squish() |>
      str_trim(),
    subcounty = str_to_sentence(subcounty) |>
      str_squish() |>
      str_trim(),
    
    ward = str_to_sentence(ward) |>
      str_squish() |>
      str_trim(),
    
    date = my(paste(month, year)),
    
    disease_condition = fifelse(
      str_detect(disease_condition, "(?i)brucellosis|brucella"),
      "Brucellosis",
      disease_condition
    ),
    
    diagnosis = str_to_title(diagnosis) |>
      str_squish() |>
      str_trim(),
    
    diagnosis = case_when(
      diagnosis %in% c(
        "Clinical Signs/Lab",
        "Lab",
        "Lab Diagnosis",
        "Laboratory",
        "Pm &Lb"
      ) ~ "Lab confirmed",
      diagnosis %in% c("Pm", "Postmortem", "Post Mortem") ~ "Post Mortem",
      TRUE ~ "Clinically confirmed"
    )
  ) |>
  mutate(
    county = case_when(
      county == "Elgeiyo Marakwet" ~ "Elgeyo Marakwet",
      county == "Elgeyo/Marakwet" ~ "Elgeyo Marakwet",
      county == "Kaimbu" ~ "Kiambu",
      county == "Karatina" ~ "Nyeri",
      county == "Murang?" ~ "Murang'a",
      county == "Murang�" ~ "Murang'a",
      county == "Mwingi" ~ "Kitui",
      county == "Sotik" ~ "Bomet",
      county == "Tharaka-Nithi" ~ "Tharaka Nithi",
      county == "Trans-Nzoia" ~ "Trans Nzoia",
      county == "Pokot" ~ "West Pokot",
      TRUE ~ county
    )
  ) |>
  summarise(
    no_sick = sum(no_sick, na.rm = TRUE),
    .by = c(county, year, date, species, disease_condition, diagnosis)
  ) |>
  filter(
    species %in% c("Camels", "cattle", "Goats", "Sheep") &
      disease_condition == "Brucellosis"
  ) |>
  select(-disease_condition) |>
  pivot_wider(names_from = species, values_from = no_sick) |>
  setNames(
    c(
      "county",
      "year",
      "date",
      "diagnosis",
      "catt_cases",
      "cam_cases",
      "goat_cases",
      "shp_cases"
    )
  ) |>
  merge(animalPop, by = c("county", "year"))

# Cleaning human data for 2023 --------------------------------------------

h2.1 <-   h2 |>
  mutate(
    county = str_remove(county, " County") |>
      str_squish() |>
      str_trim() %>%
      ifelse(. == "Muranga", "Murang'a", .),
    
    date = my(periodname),
    year = year(date)
  ) |>
  select(-rift_valley_fever, -anthrax, -periodname) |>
  setnames(
    old = c('brucellosis', 'brucella'),
    new = c("Clinically confirmed", "Lab confirmed")
  ) |>
  pivot_longer(
    cols = c("Clinically confirmed", "Lab confirmed"),
    names_to = "diagnosis",
    values_to = "hum_cases"
  ) |>
  select(county, year, date, everything()) |>
  merge(humanPop, by = c("county", "year"))

# Merging the two data sets -----------------------------------------------

a_all <- merge(h2.1,
               a4,
               by = c("county", "year", "date", "diagnosis"),
               all = T) |>
  mutate(
    hum_incidence = hum_cases / pop * 1e6,
    catt_incidence = catt_cases / catt_pop * 1e6,
    cam_incidence = cam_cases / cam_pop * 1e6,
    goat_incidence = goat_cases / goat_pop * 1e6,
    shp_incidence = shp_cases / sheep_pop * 1e6
  ) |>
  select(!contains("incidence")) |>
  select(!contains("pop")) |>
  summarise(across(contains("cases"), ~ sum(., na.rm = T)), .by = c("date", "year")) |>
  merge(animalPop |>
          summarise(across(contains("pop"), ~ sum(., na.rm = T)), .by = c("year")), by = c("year")) |>
  merge(humanPop |>
          summarise(across(contains("pop"), ~ sum(., na.rm = T)), .by = c("year")), by = "year") %>%
  mutate(hum_cases  = fifelse(hum_cases > 900000, round(mean(.$hum_cases [.$hum_cases < 900000])), hum_cases)) |>
  mutate(
    hum_incidence = hum_cases / pop * 1e6,
    catt_incidence = catt_cases / catt_pop * 1e6,
    cam_incidence = cam_cases / cam_pop * 1e6,
    goat_incidence = goat_cases / goat_pop * 1e6,
    shp_incidence = shp_cases / sheep_pop * 1e6
  )

a_ts <- rbind(kenya_ah2, a_all) |>
  arrange(date)
fwrite(a_ts, "data/a_ts.csv", row.names = F)
