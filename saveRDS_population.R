
# Load libraries
library(sf)
library(cartogram)
library(ggplot2)
library(dplyr)
library(gganimate)
library(viridis)
library(svglite)
library(rmapshaper)

# adatok és térképadatok betöltése
population_data <- read.csv('adat/WPP2024_Demographic_Indicators_Medium.csv')
world_sf <- st_read('adat/TM_WORLD_BORDERS_SIMPL-0.3.shp')


# országkód oszlop nevek átnevezése
world_sf <- world_sf %>% rename(CountryCode = ISO3)
population_data <- population_data %>% rename(CountryCode = ISO3_code)

# térkép adatok manipulációja
world_sf_simplified <- ms_simplify(world_sf, keep = 0.1, keep_shapes = TRUE)
world_sf_valid <- st_make_valid(world_sf_simplified)
world_sf_transformed <- st_transform(world_sf_valid, crs = 3857)

# évek kinyerése adatokból
years <- sort(unique(population_data$Time))

# mappa RDS fileoknak
map_dir <- "saveRDS_population/"
if (!dir.exists(map_dir)) {
  dir.create(map_dir)
}

# végig megyünk minden éven
for (year in years) {
  map_file <- paste0(map_dir, "map_growth_rate", year, ".rds")
  
  if (!file.exists(map_file)) {
    pop_data_year <- population_data %>% filter(Time == year)
    world_data_year <- world_sf_transformed %>% left_join(pop_data_year, by = "CountryCode")
    
    # NA kiszürése
    world_data_year <- world_data_year %>% filter(!is.na(PopGrowthRate))
    
    # az adott évre az adat létrehozása, év hozzáadása
    map_year <- cartogram_cont(world_data_year, weight = "PopGrowthRate", itermax = 5)
    map_year$Year <- as.numeric(year) # Ensure Year is numeric
    
    # mentés
    saveRDS(map_year, map_file)
  }
}





map_files <- list.files(map_dir, pattern = "\\.rds$", full.names = TRUE)
map_list <- lapply(map_files, readRDS)

# Combine the data
map_combined <- do.call(rbind, map_list)

map_combined$Year <- as.numeric(map_combined$Year)

# Convert PopGrowthRate to numeric
map_combined$TPopulation1Jan <- as.numeric(map_combined$TPopulation1Jan)

# népesség kategóriák létrehozása
map_combined$PopulationCategory <- cut(map_combined$TPopulation1Jan,
                                          breaks = c(2000, 5000, 10000, 20000, 50000, 70000, 100000, 400000, 800000, Inf),
                                          labels = c("0-2M", "2M-5M", "5M-10M", "10M-20M", "20M-70M", "70M-100M", 
                                                     "100M-400M", "400M-800M", "800M-"),
                                          right = FALSE, include.lowest = TRUE)

# Save the combined data to a single RDS file
saveRDS(map_combined, file = "map_combined_Population.rds")