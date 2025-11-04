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
library(gt)
setwd("/Users/VictorXia/ECON1430")
folder_path = "/Users/VictorXia/ECON1430"
rm()
#lodes <- read.csv("wa_od_main_JT00_2013.csv") #Upload the data (HARDCODE)

#READING IN LODES FILES
#KEEP TRACK OF COMMUTE TOTALS


LODES_filename <- paste0("wa_wac_S000_JT00_\\d{4}\\.csv")
LODES_filename_2 <- paste0(".*","wa_wac_S000_JT00_(\\d{4})\\.csv")
file_list <- list.files(path = folder_path, pattern = LODES_filename, full.names = TRUE)
years <- sub(LODES_filename_2, "\\1", basename(file_list))
commute_totals <- c()
means <- c(0)
st_devs <- c(0)
ranges <- c(0)

#for (i in seq_along(file_list))
for (i in seq_along(file_list)) {
  data <- read.csv(file_list[i])
  yr <- paste0("lodes_wac_", years[i])
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

#Query LODES for workers with SEATTLE AS DESTINATION GEOCODE
for (year in years) {
  # lodes dataframe name
  df_name <- paste0("lodes_wac_", year)

  # Get the data for the current year
  lodes_data <- get(df_name)

  # Filter the data
  filtered_data <- lodes_data %>%
    filter(w_geocode %in% indest$GEOID20) %>% filter(CNS18 > 0)

  # Optionally, assign the filtered data back to a variable
  assign(paste0("food_lodes_", year), filtered_data)

  # Optionally, print a message for each filtered dataset
  cat("Filtered data for year:", year, "into variable:", paste0("in.lodes_", year), "\n")
  print(paste0("# OBSERVATIONS OF WORKPLACES IN SEATTLE: ", count(filtered_data)))
  commute_totals = append(commute_totals, c(sum(filtered_data$CNS18)))
  if (year != "2013") {
    current_year_data <- get(paste0("food_lodes_", year)) %>% 
      select(w_geocode, CNS18) %>%
      rename(CNS18_current = CNS18)
    
    last_year_data <- get(paste0("food_lodes_", as.character(as.numeric(year) - 1))) %>% 
      select(w_geocode, CNS18) %>%
      rename(CNS18_last = CNS18)
    
    # Perform a full join to retain all blocks
    joined <- full_join(current_year_data, last_year_data, by = "w_geocode") %>%
      mutate(
        delta = CNS18_current - CNS18_last,
        year = year
      )
    
    assign(paste0("delta_lodes_", year), joined)
    means = append(means, c(mean(joined$delta, na.rm = TRUE)))
    st_devs = append(st_devs, c(sd(joined$delta, na.rm = TRUE)))
    ranges = append(ranges, c(paste0("[", as.character(range(joined$delta, na.rm = TRUE)[1]), ",", as.character(range(joined$delta, na.rm = TRUE)[2]),"]")))
    
  }
}
  for (year in years[2:8]) {
    # lodes dataframe name
    df_name <- paste0("delta_lodes_", year)
    lodes_data <- get(df_name)
    # perform the join
    joined_data <- lodes_data %>%
      left_join(geom_msa, by = c("w_geocode" = "numgeoid"))
    # assign the data back to a df
    assign(paste0("delta_lodes_", year), joined_data)
    
    # print a message for each filtered dataset
    cat("Joined data for year:", year, "into variable:", paste0("delta_lodes_", year), "\n")
  }
  
  for (year in years[2:8]) {
    # lodes dataframe name
    df_name <- paste0("delta_lodes_", year)
    lodes_data <- get(df_name)
    # do the selection 
    # w_geocode: workplace census block code
    selected_data <- lodes_data %>%
      select(w_geocode, work_lat, work_lon, delta) %>%
    rename(id = w_geocode, lat = work_lat, lon = work_lon)
    selected_data$id <- as.character(selected_data$id)
    selected_data$lat <- as.numeric(selected_data$lat)
    selected_data$lon <- as.numeric(selected_data$lon)
    selected_data$delta <- as.numeric(selected_data$delta)
    
    df_filtered <- selected_data %>%
      filter(!is.na(lat) & !is.na(lon) &!is.na(delta))
    
    df <- st_as_sf(df_filtered, coords = c("lon","lat"), agr = "aggregate")
    
    # assign the data back to a df
    assign(paste0("delta_lodes_spatialized", year), df)
    
    # Optionally, print a message for each filtered dataset
  }




totals = data.frame(years, commute_totals, means, st_devs, ranges)


totals %>%
  gt() %>%
  
  # 1. Header
  tab_header(
    title    = md("*Table 3: Average Annual Change in Food-Service Jobs by Census-Block*")
  ) %>% cols_label(
    years = "Year",
    means  = "Mean Change",
    st_devs    = "Standard Deviation",
    ranges = "Range",
    commute_totals = "Job Count within Treatment Zone"
  ) %>%
  
  # 2. Font: try Times New Roman, fallback to Georgia/serif
  opt_table_font(
    font = list(
      "Times New Roman",
      "Georgia",
      "serif"
    )
  ) %>%
  
  # 4. Bold column labels
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) %>%
  
  # 5. Numeric formatting for all numeric columns
  fmt_number(
    columns  = where(is.numeric),
    decimals = 2
  ) %>%
  
  # 6. Tighten spacing & borders
  tab_options(
    table.font.size           = px(12),
    heading.title.font.size   = px(14),
    heading.subtitle.font.size= px(12),
    data_row.padding          = px(5),
    table_body.border.top.color    = "transparent",
    table_body.border.bottom.color = "transparent"
  )


p <- ggplot(totals, aes(years, means)) + 
  geom_point() + geom_line(aes(group = 1)) 
p + labs(title = "Year-on-year Differences in Food-Service Workers by Census-Block", x = "Year", y = "Additional Workers") 

