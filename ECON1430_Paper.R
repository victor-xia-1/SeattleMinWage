library(httr)
library(sf)
library(tigris)
library(tidycensus)
library(r5r)
library(sf)
library(data.table)
library(ggplot2)
library(dplyr)
library(osmdata)
library(readxl)
library(stringr)
library(giscoR)
library(ggplot2)
library(readr)
library(purrr)
library(stargazer)
library(xtable)
setwd("/Users/VictorXia/ECON1430")
folder_path = "/Users/VictorXia/ECON1430"
rm()
#lodes <- read.csv("wa_od_main_JT00_2013.csv") #Upload the data (HARDCODE)
# Victor Xia April 2025
# NOTES: This script outputs the regression table for cross-border commuting elasticity of nonresidents 
# in response to the change in log minimum wage ratio between Seattle and outlying regions after the 2015 treatment.
# ChatGPT was used to assist in the creation of this code.
#READING IN LODES FILES
#KEEP TRACK OF COMMUTE TOTALS


LODES_filename <- paste0("wa_od_main_JT00_\\d{4}\\.csv")
LODES_filename_2 <- paste0(".*","wa_od_main_JT00_(\\d{4})\\.csv")
file_list <- list.files(path = folder_path, pattern = LODES_filename, full.names = TRUE)
years <- sub(LODES_filename_2, "\\1", basename(file_list))
commute_totals <- c()

#for (i in seq_along(file_list))
for (i in seq_along(file_list)) {
  data <- read.csv(file_list[i])
  yr <- paste0("lodes_", years[i])
  assign(yr, data)
  cat("Read file:", file_list[i], "into variable:", yr, "\n")
}

#READ IN SEATTLE SHAPEFILE
catchment_path <- paste0(folder_path, "/Catchment") #Catchment .shp file location
catch_alt <- st_read(catchment_path)

#GET MSA BLOCKS
target_city = "Seattle"
msa_shapefile <- core_based_statistical_areas(cb = TRUE, year = 2020) # last year of analysis window
msa_geoID = msa_shapefile$GEOID[grep(target_city, msa_shapefile$NAME)]#Find the GeoID of target MSA (e.g. Dallas = "19100")
msa <- msa_shapefile %>% filter(GEOID == msa_geoID)# transform the coordinate reference system (CRS) of blocks to match that of the MSA shapefile
blocks <- blocks(state = 53, year = 2020)#Washington is 53
blocks <- st_transform(blocks, 4269) #I manually retrieved the EPSG code for the MSA shapefile

#FILTER MSA BLOCKS INTO SEATTLE
msa_blks <- st_intersection(blocks, msa)
msa_blks$numgeoid <- as.numeric(msa_blks$GEOID20)
msa_blks <- st_transform(msa_blks, crs = st_crs(catch_alt))
# selects only blocks that are within our catchment (SEATTLE SHAPEFILE)
catch_blks <- st_intersection(msa_blks, catch_alt)
# converts them to a dataframe instead of a spatial object
indest <- data.frame(catch_blks)
indest$GEOID20 <- as.numeric(indest$GEOID20)

#JOINING ALL DATASETS BY YEAR
datasets <- map2(file_list, years, ~read.csv(.x) %>% select(h_geocode, w_geocode, S000, SE01) %>% filter(w_geocode %in% indest$GEOID20) %>% filter(! h_geocode %in% indest$GEOID20) %>% mutate(year = .y))
combined_data <- bind_rows(datasets)
min_wage_data <- read.csv("MinWageRatio.csv")
min_wage_data <- min_wage_data%>%mutate_at("year", as.character) 
final_data <- combined_data %>%
  left_join(min_wage_data, by = c("year"))
min_wage_data <- min_wage_data %>% mutate_at("year", as.integer)

# Step 1: Add tract-level geocodes to final_data
final_data <- final_data %>%
  mutate(
    h_tract = substr(as.character(h_geocode), 1, 11),
    w_tract = substr(as.character(w_geocode), 1, 11)
  )

# Step 2: Compute counts by tract pair
tract_counts <- final_data %>%
  group_by(h_tract, w_tract) %>%
  summarise(tract_pair_count = n(), .groups = "drop")

# Step 3: Merge back into final_data
final_data <- final_data %>%
  left_join(tract_counts, by = c("h_tract", "w_tract"))

# Only keep groups with at all 5 unique years of data (201) and at least 1 worker earning less than $1250 a month
filtered_data <- final_data %>%
  group_by(h_tract, w_tract) %>%
  filter(n_distinct(year) > 8) %>%
  ungroup()

lm_minwage = lm(log(S000) ~ log(MinWageRatio) + year, data = filtered_data) #Create a linear regression with two variables
summary(lm_minwage) #Review the results
stargazer(lm_minwage)




# #Query LODES for workers with SEATTLE AS DESTINATION GEOCODE
# for (year in years) {
#   # lodes dataframe name
#   df_name <- paste0("lodes_", year)
# 
#   # Get the data for the current year
#   lodes_data <- get(df_name)
# 
#   # Filter the data for individuals who WORK IN SEATTLE but LIVE OUTSIDE SEATTLE
#   filtered_data <- lodes_data %>%
#     filter(w_geocode %in% indest$GEOID20) %>% filter(! h_geocode %in% indest$GEOID20)
# 
#   # Optionally, assign the filtered data back to a variable
#   assign(paste0("in.lodes_", year), filtered_data)
# 
#   # Optionally, print a message for each filtered dataset
#   cat("Filtered data for year:", year, "into variable:", paste0("in.lodes_", year), "\n")
#   print(paste0("# OBSERVATIONS COMMUTING INTO SEATTLE: ", count(filtered_data)))
#   commute_totals = append(commute_totals, c(as.numeric(count(filtered_data))))
# }

# STEP 3: Append in lat/lon information to in.lodes
# Select only the desired columns from msa_blks
geom_msa <- msa_blks %>%
  select(numgeoid, INTPTLAT20, INTPTLON20)

blocks$numgeoid <- as.numeric(blocks$GEOID20)

for (year in years) {
  # lodes dataframe name
  df_name <- paste0("in.lodes_", year)
  lodes_data <- get(df_name)
  # perform the join
  
  joined_data <- lodes_data %>%
    left_join(geom_msa, by = c("h_geocode" = "numgeoid")) %>%
    rename(origin_lat = INTPTLAT20, origin_lon = INTPTLON20)
  
  # assign the data back to a df
  assign(paste0("in.lodes_", year), joined_data)
  
  # print a message for each filtered dataset
  cat("Joined data for year:", year, "into variable:", paste0("in.lodes_", year), "\n")
}


for (year in years) {
  # lodes dataframe name
  df_name <- paste0("in.lodes_", year)
  lodes_data <- get(df_name)
  # perform the join
  joined_data <- lodes_data %>%
    left_join(geom_msa, by = c("w_geocode" = "numgeoid")) %>%
    rename(dest_lat = INTPTLAT20, dest_lon = INTPTLON20)
  
  # assign the data back to a df
  assign(paste0("in.lodes_", year), joined_data)
  
  # print a message for each filtered dataset
  cat("Joined data for year:", year, "into variable:", paste0("in.lodes_", year), "\n")
}

## OK OK OK. we have to get rid of nulls.... if we don't, later code falls apart !
for (year in years) {
  # lodes dataframe name
  df_name <- paste0("in.lodes_", year)
  lodes_data <- get(df_name)
  # perform the join
  filtered_data <- lodes_data %>%
    filter(!is.na(origin_lat))
  
  # assign the data back to a df
  assign(paste0("in.lodes_", year), filtered_data)
  
  # Optionally, print a message for each filtered dataset
  cat("Filtered data for year:", year, "into variable:", paste0("in.lodes_", year), "\n")
}


# Step 4: Isolate the origins and destinations, and then obtain geographic dist. information 
for (year in years) {
  # lodes dataframe name
  df_name <- paste0("in.lodes_", year)
  lodes_data <- get(df_name)
  
  # do the selection 
  selected_data <- lodes_data %>%
    select(h_geocode, origin_lat, origin_lon) %>%
    rename(id = h_geocode, lat = origin_lat, lon = origin_lon)
  selected_data$id <- as.character(selected_data$id)
  selected_data$lat <- as.numeric(selected_data$lat)
  selected_data$lon <- as.numeric(selected_data$lon)
  
  df_filtered <- selected_data %>%
    filter(!is.na(lat) & !is.na(lon))
  
  df <- st_as_sf(df_filtered, coords = c("lon","lat"), crs = st_crs(msa_blks))
  
  # assign the data back to a df
  assign(paste0("origins_", year), df)
  
  # Optionally, print a message for each filtered dataset
  cat("Selected, filtered, and spatialized data for year:", year, "into variable:", paste0("origins_", year), "\n")
}


for (year in years) {
  # lodes dataframe name
  df_name <- paste0("in.lodes_", year)
  lodes_data <- get(df_name)
  # do the selection 
  # w_geocode: workplace census block code
  selected_data <- lodes_data %>%
    select(w_geocode, dest_lat, dest_lon) %>%
    rename(id = w_geocode, lat = dest_lat, lon = dest_lon)
  selected_data$id <- as.character(selected_data$id)
  selected_data$lat <- as.numeric(selected_data$lat)
  selected_data$lon <- as.numeric(selected_data$lon)
  
  df_filtered <- selected_data %>%
    filter(!is.na(lat) & !is.na(lon))
  
  df <- st_as_sf(df_filtered, coords = c("lon","lat"))
  
  # assign the data back to a df
  assign(paste0("destinations_", year), df)
  
  # Optionally, print a message for each filtered dataset
  cat("Selected, filtered, and spatialized data for year:", year, "into variable:", paste0("destinations_", year), "\n")
}

#PLOT SEATTLE AND WORKERS IN IT
 
plot(msa_blks %>%  filter(!is.na(GEOID20)) %>%
       filter(startsWith(GEOID20, "530330031")) %>%
       select(GEOID20), reset = FALSE)

# in.lodes_2013 <- in.lodes_2013%>%mutate_at( 
#   "w_geocode", as.character) 
# in.lodes_2013$coords <- st_as_sf(in.lodes_2013, coords = c("dest_lon", "dest_lat"), crs = 4269)

plot(destinations_2013 %>% filter(startsWith(id, "530330031")) %>% 
       select(geometry) %>% slice_sample(n = 10), reset = FALSE, add = TRUE, pch = 16, col = "black")

totals = data.frame(years, commute_totals)
p <- ggplot(totals, aes(years, commute_totals)) + 
  geom_point() + geom_line(aes(group = 1)) 
p + labs(title = "Count of Nonresidents Working in Seattle (Commuters)", x = "Year", y = "Count") 

