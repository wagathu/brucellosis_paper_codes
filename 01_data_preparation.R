
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
  rKenyaCensus,
  tidyr
)

# Importing data ----------------------------------------------------------

county_shp <- st_read("shapefiles/gadm41_KEN_1.shp", query = "select NAME_1 as county from gadm41_KEN_1", quiet = T)
animalData1 <- fread("data/animal_data1.csv", check.names = T)
animalData2 <- fread("data/july-dec_animal.xlsx.csv", check.names = T)
animalData3 <- read_excel("data/animal_brucellosis_2022.xlsx")
humanData1 <- fread("data/Zoonosis_2014_2021_county.csv")
humanData2 <- fread("data/county_brucella_2022_2023.csv")
humanPop <- fread("data/population_county_2014_2021.csv")

# Cleaning animal population ----------------------------------------------

animalPop <- rKenyaCensus::V4_T2.24 |> 
  filter(AdminArea != "County") |> 
  select(
    county = County, 
    subcounty = SubCounty, 
    contains("cattle"),
    Goats,
    Sheep,
    Camels
    ) |> 
  filter(county != "xxx") |>
  mutate(
    county = str_to_title(county),
    subcounty = str_to_title(subcounty)
  ) |> 
  ungroup() |> 
  rowwise(  ) |> 
  mutate(
    cattle = sum(ExoticCattle_Dairy, ExoticCattle_Beef, IndigenousCattle, na.rm = T),
    county = case_when(
      county == "Elgeyo/Marakwet" ~ "Elgeyo Marakwet",
      county == "Nairobi City" ~ "Nairobi",
      county == "Taita/Taveta" ~ "Taita Taveta",
      county == "Tharaka-Nithi" ~ "Tharaka Nithi",
      TRUE ~ county
    )
  ) |> 
  select(county, subcounty, cattle, goats = Goats, sheep = Sheep, camels = Camels) |> 
  ungroup() |> 
  summarise(
    catt_pop = sum(cattle, na.rm = TRUE),
    goat_pop = sum(goats, na.rm = TRUE),
    sheep_pop = sum(sheep, na.rm = TRUE),
    cam_pop = sum(camels, na.rm = TRUE),
    .by = c(county)
  ) |> 
  pivot_longer(-county, names_to = "species", values_to = "pop") |>
  cross_join(tibble(year = c(2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022))) |> 
  pivot_wider(
    names_from = species,
    values_from = pop
  )

fwrite(animalPop, "data/animalPop.csv", row.names = F)

# Cleaning animal data -----------------------------------------------------------

a1 <- animalData1 |>
  dplyr::select(
    county = County,
    subcounty = SubCounty,
    ward = Ward,
    lat = lat,
    lng = long,
    ward = Ward,
    month = Month,
    year,
    disease_condition =  Disease.Condition,
    diagnosis = `Nature.of.diagnosis..clinical.lab.pm.`,
    species = Species,
    no_sick = `No..sick`
  )

a2 <- animalData2 |>
  dplyr::select(
    county = County,
    subcounty = `Sub.County`,
    ward = Ward,
    lat = Latitude,
    lng = Longitude,
    ward = Ward,
    month = Month,
    year = year.1,
    disease_condition =  `Disease.Condition`,
    diagnosis = `Nature.of.Diagnosis`,
    species = `Species.Affected`,
    no_sick = `No..sick`
  )

a3 <- animalData3 |>
  dplyr::select(
    county = County,
    subcounty = `Sub-County`,
    ward = Ward,
    lat = Latitude,
    lng = Longitude,
    ward = Ward,
    month = Month,
    year = `year...10`,
    disease_condition =  `Disease/Condition`,
    diagnosis = `Nature of Diagnosis`,
    species = `Species Affected`,
    no_sick = `Number Sick`
  )

a <- rbind(a1, a2, a3) |> 
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
      species == "Canine, Feline" ~"Dogs",
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
  pivot_wider(
    names_from = species,
    values_from = no_sick
  ) |> 
  setNames(c("county", "year", "date", "diagnosis", "catt_cases", "cam_cases", "goat_cases", "shp_cases")) |> 
  merge(animalPop, by = c("county", "year"))

# Cleaning human data -----------------------------------------------------

h1 <- humanData1 |> 
  dplyr::select(
    periodname,
    county = organisationunitname,
    brucellosis = Brucellosis,
    rift_valley_fever = `MOH 705A Rev 2020_ Rift valley fever`,
    anthrax = `MOH 705A Rev 2020_ Suspected anthrax`,
    brucella = `MOH 706_Brucella` 
  )

h2 <- humanData2 |> 
  dplyr::select(
    periodname,
    county = organisationunitname,
    brucellosis = Brucellosis,
    rift_valley_fever = `MOH 705A Rev 2020_ Rift valley fever`,
    anthrax = `MOH 705A Rev 2020_ Suspected anthrax`,
    brucella = `MOH 706_Brucella` 
  ) |> 
  rbind(
    data.table(
      periodname = c("April 2022", "May 2022", "June 2022", "July 2022", "November 2022"),
      county = "Lamu County",
      brucellosis = NA,
      rift_valley_fever = NA,
      anthrax = NA,
      brucella = NA
    )
  )
fwrite(h2, "data/h2.csv", row.names = F)

h <- rbind(h1, h2) |>
  mutate(
    county = str_remove(county, " County") |>
      str_squish() |>
      str_trim() %>%
      ifelse(. == "Muranga", "Murang'a",.),
    
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

# Merging the whole data --------------------------------------------------

setdiff(h$county, a$county)
setdiff(a$diagnosis, h$diagnosis)

ah <- merge(h,
            a,
            by = c("county", "year", "date", "diagnosis"),
            all = T) |>
  mutate(
    catt_cases = ifelse(catt_cases >= 69, round(mean(catt_cases, na.rm = T)), catt_cases),
    goat_cases = ifelse(goat_cases > 28, round(mean(goat_cases, na.rm = T)), goat_cases),
    shp_cases = ifelse(shp_cases > 12, round(mean(shp_cases, na.rm = T)), shp_cases)
  ) |>
  mutate(
    hum_incidence = hum_cases / pop * 1e6,
    catt_incidence = catt_cases / catt_pop * 1e6,
    cam_incidence = cam_cases / cam_pop * 1e6,
    goat_incidence = goat_cases / goat_pop * 1e6,
    shp_incidence = shp_cases / sheep_pop * 1e6
  )
fwrite(ah, "data/df_incidence2.1.csv", row.names = F)

ah2 <- ah |> 
  select(!contains("incidence")) |> 
  select( !contains("pop")) |> 
  summarise(across(contains("cases"), ~sum(., na.rm = T)), .by = c("county", "date", "year")) |> 
  merge(animalPop, by = c("county", "year")) |> 
  merge(humanPop, by = c("county", "year")) |> 
  mutate(
    hum_incidence = hum_cases / pop * 1e6,
    catt_incidence = catt_cases / catt_pop * 1e6,
    cam_incidence = cam_cases / cam_pop * 1e6,
    goat_incidence = goat_cases / goat_pop * 1e6,
    shp_incidence = shp_cases / sheep_pop * 1e6
  )


# Obtaining monthly data at Kenya level -----------------------------------

kenya_ah <- ah |> 
  select(!contains("incidence")) |> 
  select( !contains("pop")) |> 
  summarise(across(contains("cases"), ~sum(., na.rm = T)), .by = c("date", "year", "diagnosis")) |> 
  merge(animalPop |>
          summarise(across(contains("pop"), ~ sum(., na.rm = T)), .by = c("year")), by = c("year")) |>
  merge(humanPop |>
          summarise(across(contains("pop"), ~ sum(., na.rm = T)), .by = c("year")), by = "year") |>
  mutate(
    hum_incidence = hum_cases / pop * 1e6,
    catt_incidence = catt_cases / catt_pop * 1e6,
    cam_incidence = cam_cases / cam_pop * 1e6,
    goat_incidence = goat_cases / goat_pop * 1e6,
    shp_incidence = shp_cases / sheep_pop * 1e6
  )

fwrite(kenya_ah, "data/kenya_monthly_incidence.csv", row.names = F)

kenya_ah2 <- ah |> 
  select(!contains("incidence")) |> 
  select( !contains("pop")) |> 
  summarise(across(contains("cases"), ~sum(., na.rm = T)), .by = c("date", "year")) |> 
  merge(animalPop |>
          summarise(across(contains("pop"), ~ sum(., na.rm = T)), .by = c("year")), by = c("year")) |>
  merge(humanPop |>
          summarise(across(contains("pop"), ~ sum(., na.rm = T)), .by = c("year")), by = "year") |>
  mutate(
    hum_incidence = hum_cases / pop * 1e6,
    catt_incidence = catt_cases / catt_pop * 1e6,
    cam_incidence = cam_cases / cam_pop * 1e6,
    goat_incidence = goat_cases / goat_pop * 1e6,
    shp_incidence = shp_cases / sheep_pop * 1e6
  )

fwrite(kenya_ah2, "data/kenya_monthly_incidence2.csv", row.names = F)

# Obtaining yearly data at county level ---------------------------------------------------

yah <- ah |> 
  select(!contains("incidence")) |> 
  select( !contains("pop")) |> 
  summarise(across(contains("cases"), ~sum(., na.rm = T)), .by = c("county", "year", "diagnosis")) |> 
  merge(animalPop, by = c("county", "year")) |> 
  merge(humanPop, by = c("county", "year")) |> 
  mutate(
    hum_incidence = hum_cases / pop * 1e6,
    catt_incidence = catt_cases / catt_pop * 1e6,
    cam_incidence = cam_cases / cam_pop * 1e6,
    goat_incidence = goat_cases / goat_pop * 1e6,
    shp_incidence = shp_cases / sheep_pop * 1e6
  )
fwrite(yah, "data/county_yearly_incidence.csv", row.names = F)

yah2 <- ah |> 
  select(!contains("incidence")) |> 
  select( !contains("pop")) |> 
  summarise(across(contains("cases"), ~sum(., na.rm = T)), .by = c("county", "year")) |> 
  merge(animalPop, by = c("county", "year")) |> 
  merge(humanPop, by = c("county", "year")) |> 
  mutate(
    hum_incidence = hum_cases / pop * 1e6,
    catt_incidence = catt_cases / catt_pop * 1e6,
    cam_incidence = cam_cases / cam_pop * 1e6,
    goat_incidence = goat_cases / goat_pop * 1e6,
    shp_incidence = shp_cases / sheep_pop * 1e6
  )
fwrite(yah2, "data/county_yearly_incidence2.csv", row.names = F)

# Obtaining yearly data at Kenya level ---------------------------------------------------

kenya_yah <- ah |>
  select(!contains("incidence")) |>
  select(!contains("pop")) |>
  summarise(across(contains("cases"), ~ sum(., na.rm = T)), .by = c("year", "diagnosis")) |>
  merge(animalPop |>
          summarise(across(contains("pop"), ~ sum(., na.rm = T)), .by = c("year")), by = c("year")) |>
  merge(humanPop |>
          summarise(across(contains("pop"), ~ sum(., na.rm = T)), .by = c("year")), by = "year") |>
  mutate(
    hum_incidence = hum_cases / pop * 1e6,
    catt_incidence = catt_cases / catt_pop * 1e6,
    cam_incidence = cam_cases / cam_pop * 1e6,
    goat_incidence = goat_cases / goat_pop * 1e6,
    shp_incidence = shp_cases / sheep_pop * 1e6
  )
fwrite(kenya_yah, "data/kenya_yearly_incidence.csv", row.names = F)

kenya_yah2 <- ah |>
  select(!contains("incidence")) |>
  select(!contains("pop")) |>
  summarise(across(contains("cases"), ~ sum(., na.rm = T)), .by = c("year")) |>
  merge(animalPop |>
          summarise(across(contains("pop"), ~ sum(., na.rm = T)), .by = c("year")), by = c("year")) |>
  merge(humanPop |>
          summarise(across(contains("pop"), ~ sum(., na.rm = T)), .by = c("year")), by = "year") |>
  mutate(
    hum_incidence = hum_cases / pop * 1e6,
    catt_incidence = catt_cases / catt_pop * 1e6,
    cam_incidence = cam_cases / cam_pop * 1e6,
    goat_incidence = goat_cases / goat_pop * 1e6,
    shp_incidence = shp_cases / sheep_pop * 1e6
  )
fwrite(kenya_yah2, "data/kenya_yearly_incidence2.csv", row.names = F)

# Saving RDS --------------------------------------------------------------

dfs <- list(
  ah,
  ah2,
  kenya_ah,
  kenya_ah2,
  yah,
  yah2,
  kenya_yah,
  kenya_yah2
) |> 
  setNames(c(
    'ah',
    'ah2',
    'kenya_ah',
    'kenya_ah2',
    'yah',
    'yah2',
    'kenya_yah',
    "kenya_yah2"
  ))
saveRDS(dfs, "data/all_dfs.RDS")
