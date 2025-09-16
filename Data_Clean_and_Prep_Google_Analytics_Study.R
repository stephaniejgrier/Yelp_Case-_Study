#install.packages("scales")
#install.packages("leaflet")
#install.packages("tigris")
#install.packages("shiny")
#install.packages("janitor")
#install.packages("rsconnect")
#install.packages("htmltools")

#run each session to use these tools
library(rsconnect)
library(readr)
library(janitor)
library(zipcodeR)
library(sf)
library(maps)
library(dplyr)
library(purrr)
library(tidycensus)
library(tidyr)
library(usethis)
library(jsonlite)
library(tidyverse)
library(scales) # comma(), number()
library(forcats)usethis::use_github(private = TRUE)   # use FALSE if you want it public
library(reshape2)
library(stringr)
library(leaflet)
library(tigris)      # ZCTA shapes
library(DataExplorer)
library(shiny)
library(htmltools)
library(reticulate)
library(ggplot2)     
options(tigris_use_cache = TRUE, tigris_class = "sf")
#-------------------------------------------------------------------------------
# replace absolute paths for sharing on github
#-------------------------------------------------------------------------------
# Top of Data_Clean_and_Prep_Google_Analytics_Study.R
if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
library(here)
here::i_am("Data_Clean_and_Prep_Google_Analytics_Study.R")  # pins project root

data_path      <- function(...) here::here("data", "raw", ...)
processed_path <- function(...) here::here("data", "processed", ...)
output_path    <- function(...) here::here("outputs", ...)

dir.create(processed_path(), recursive = TRUE, showWarnings = FALSE)
dir.create(output_path(),    recursive = TRUE, showWarnings = FALSE)


#-------------------------------------------------------------------------------
#Set the working directory***** Not needed for github repo****
#-------------------------------------------------------------------------------
#setwd("C:/Users/sgrie/Documents/Yelp_Case _Study")
#-------------------------------------------------------------------------------
#Read in the Yelp data (json file)
#-------------------------------------------------------------------------------
# Create a connection to the JSON file
#This is required since the json file is line separated and very large
connection <- file("archive (17)/yelp_academic_dataset_business.json", "r")
# Read the JSON data using stream_in()
yelp_data <- jsonlite::stream_in(connection)
# Close the connection
close(connection)
# Convert the list to a data frame
#view the structure and a bit of each column
yelp_business_df <- as.data.frame(yelp_data)
str(yelp_business_df)
#-------------------------------------------------------------------------------
# Filter for restaurant. Handles case variations and missing values
#-------------------------------------------------------------------------------
restaurant_df <- yelp_business_df %>% 
  filter(grepl("Restaurants", categories, ignore.case = TRUE) & !is.na(categories))

# Show how many restaurants were found
cat("Number of restaurants found:", nrow(restaurant_df), "\n")

# Check a few examples of the filtered results
head(restaurant_df$categories, 5)
#-------------------------------------------------------------------------------
#    Filter by Fast Food Brands
#-------------------------------------------------------------------------------
# Define the list of QSR restaurants
QSR_restaurants <- c(
  "Arby's", "Auntie Anne's", "Baskin-Robbins", "Bojangles'", "Boston Market", 
  "Burger King", "Carl's Jr", "Checkers", "Rally's", "Chick-fil-A", "Chipotle", 
  "Church's Chicken", "Culver's", "Dairy Queen", "Del Taco", "Domino's", 
  "Dunkin'", "El Pollo Loco", "Firehouse Subs", "Five Guys", "Hardee's", 
  "In-N-Out Burger", "Jack in the Box", "Jason's Deli", "Jersey Mike's", 
  "Jimmy John's", "KFC", "Little Caesars", "Marco's", "McAlister's Deli", 
  "McDonald's", "Moe's", "Panda Express", "Panera Bread", "Papa John's", 
  "Papa Murphy's", "Pizza Hut", "Popeyes", "Qdoba", "Raising Cane's", 
  "Sonic Drive-In", "Starbucks", "Steak 'n' Shake", "Subway", "Taco Bell", 
  "Krispy Kreme", "Wendy's", "Whataburger", "White Castle", "Wingstop", "Zaxby's"
)

# Create a regex pattern that matches any of these restaurant names
# This handles variations in punctuation and spacing
QSR_pattern <- paste(QSR_restaurants, collapse = "|")

# Filter the data set for these specific QSRs
QSR_restaurants_df <- yelp_business_df %>%
  filter(grepl(QSR_pattern, name, ignore.case = TRUE) & !is.na(name))

# Check results
cat("Number of QSR restaurants found (loose matching):", nrow(QSR_restaurants_df), "\n")

head(QSR_restaurants_df$name, 10)

# Count by QSR (to see which QSRs are most common in the data)
QSR_counts <- QSR_restaurants_df %>%
  mutate(QSR_name = sapply(name, function(x) {
    matches <- sapply(QSR_restaurants, function(QSR) grepl(QSR, x, ignore.case = TRUE))
    if(any(matches)) QSR_restaurants[matches][1] else "Other"
  })) %>%
  dplyr::count(QSR_name) %>%
  arrange(desc(n))

print(QSR_counts)
#-------------------------------------------------------------------------------
# Determine the 11 Metro areas conatined in the Yelp Data by filtering the city
# name column in the yelp_business_df data frame
#-------------------------------------------------------------------------------
unique_cities <- yelp_business_df %>% distinct(city) %>% pull(city)
#Expected 11 names. Got 1416. I will add a conditional statement
frequent_cities <- yelp_business_df %>%
  count(city) %>%
  filter(n > 100) %>%
  pull(city)
#Expected 11 names. Got 196. Increase frequency requirement
More_frequent_cities <- yelp_business_df %>%
  count(city) %>%
  filter(n > 950) %>%
  pull(city)
#Expected 11 names. Got 24. Need a new approach
#-------------------------------------------------------------------------------
# Determine the 11 Metro areas conatined in the Yelp Data by used clustering 
# and averaging of latitudes and longitudes to find and map the centroids 
#-------------------------------------------------------------------------------
# Remove any rows with missing coordinates
clean_df <- yelp_business_df %>% 
  filter(!is.na(latitude) & !is.na(longitude))

# Group by city/state and calculate metro area centroids
metro_centroids_by_city <- clean_df %>%
  group_by(city, state) %>% #column names
  dplyr::summarise(
    avg_latitude = mean(latitude),
    avg_longitude = mean(longitude),
    business_count = dplyr::n(),
    .groups = 'drop'
  ) %>%
  mutate(metro_name = paste(city, state, sep = ", "))

print("Metro areas by city/state:")
print(metro_centroids_by_city)

# Get US map data
us_map <- map_data("state")

# Plot results on US map
ggplot() +
  geom_polygon(data = us_map, aes(x = long, y = lat, group = group), 
               fill = "white", color = "gray") +
  geom_point(data = metro_centroids_by_city, aes(x = avg_longitude, y = avg_latitude, size = business_count), 
             color = "blue", alpha = 0.7) +
  geom_text(data = metro_centroids_by_city, 
            aes(x = avg_longitude, y = avg_latitude, label = metro_name),
            nudge_y = 0.5, size = 3, check_overlap = TRUE) +
  coord_fixed(1.3) +
  theme_void() +
  labs(title = "Metropolitan Areas - Grouped by City/State",
       subtitle = "Point size represents number of businesses",
       size = "Business Count") +
  scale_size_continuous(range = c(2, 8))
#-------------------------------------------------------------------------------
# Remove Outliers Edmonton, AB since it is not in the US
#                  Bennington, VT since it is outside all 11 metro areas
#-------------------------------------------------------------------------------
# Remove AB and VT from clean_df
clean_df <- clean_df %>%
  filter(!state %in% c("AB", "VT"))
# Check the results
cat("Remaining states in dataset:\n")
print(sort(unique(clean_df$state)))
cat("\nNumber of businesses after removing AB and VT:", nrow(clean_df), "\n")
#----------------------------------------------------------
#  Get Census Data
#----------------------------------------------------------

#----------------------------------------------------------
# Setup Census API Key (one-time setup)
#----------------------------------------------------------
#setup_census_api <- function(api_key) {
#  census_api_key(api_key, install = TRUE)
#  message("Census API key installed. Restart R session for changes to take effect.")

# Run once: setup_census_api("dcea899293b345bf3a621e5a1b560b6adbfabb9f")

# Define Metro areas and their display names 
# Metro areas defined by Metro zip code data downloaded from data.mongabay.com
metro_files <- c(
  PA = "philadelphia_zip",
  NV = "reno_zip", 
  AZ = "tucson_zip",
  TN = "nashville_zip",
  MO = "st_louis_zip",
  FL = "tampa_zip",
  ID = "boise_zip",
  CA = "santa_barbara_zip",
  IN = "indianapolis_zip",
  LA = "new_orleans_zip"
)

metro_names <- c(
  PA = "Philadelphia",
  NV = "Reno", 
  AZ = "Tucson",
  TN = "Nashville",
  MO = "St Louis",
  FL = "Tampa",
  ID = "Boise",
  CA = "Santa Barbara",
  IN = "Indianapolis",
  LA = "New Orleans"
)
# ---- Ensure the 10 metro ZIP CSVs are in data/raw/ (copy them if found elsewhere) ----
# expects: metro_files already defined, e.g.
# metro_files <- c(PA="philadelphia_zip", NV="reno_zip", AZ="tucson_zip", ...)

# 1) Make sure target folder exists
dir.create(here::here("data","raw"), recursive = TRUE, showWarnings = FALSE)

# 2) What I expect to exist (with exact names)
expected_paths <- here::here("data","raw", paste0(metro_files, ".csv"))
names(expected_paths) <- names(metro_files)

# 3) If any are missing, try to find & copy them from my computer
missing <- expected_paths[!file.exists(expected_paths)]
if (length(missing)) {
  message("Looking for missing ZIP CSVs under Documents and project root...")
  
  # Search roots (add more roots if needed)
  search_roots <- c(
    normalizePath("~/Documents", mustWork = FALSE),
    normalizePath(here::here(),   mustWork = FALSE)
  )
  
  # Pattern matches any of the 10 expected file names (case-insensitive)
  pat <- paste0("(", paste0(metro_files, collapse="|"), ")\\.csv$")
  
  # Find candidates
  all_candidates <- unique(unlist(lapply(search_roots, function(root) {
    if (!nzchar(root) || !dir.exists(root)) return(character(0))
    list.files(root, pattern = pat, ignore.case = TRUE, recursive = TRUE, full.names = TRUE)
  })))
  
  # Map lowercase base name -> path for easy matching
  base_lc <- tolower(tools::file_path_sans_ext(basename(all_candidates)))
  names(all_candidates) <- base_lc
  
  # Try to copy each missing file into data/raw with the exact expected name
  copied <- character(0)
  still_missing <- character(0)
  for (mf in unname(metro_files)) {
    target <- here::here("data","raw", paste0(mf, ".csv"))
    if (!file.exists(target)) {
      key <- tolower(mf)
      if (!is.na(all_candidates[key])) {
        ok <- file.copy(all_candidates[key], target, overwrite = TRUE)
        if (ok) copied <- c(copied, target) else still_missing <- c(still_missing, target)
      } else {
        still_missing <- c(still_missing, target)
      }
    }
  }
  
  if (length(copied)) {
    message("Copied into data/raw/:")
    cat("  -", paste(relative_path <- fs::path_rel(copied, here::here()), collapse = "\n  - "), "\n")
  }
  if (length(still_missing)) {
    message("Still missing (place these manually into data/raw/ with exact names):")
    cat("  -", paste(fs::path_rel(still_missing, here::here()), collapse = "\n  - "), "\n")
  }
}

# 4) Final check (will be TRUE if everything is now in place)
message("ZIP file presence check:")
print(file.exists(expected_paths))


# ---------------------------
# Step 1. Build lookup objects
# ---------------------------
metro_zipcodes_formatted <- list()
metro_zipcode_lookup <- data.frame()

for (metro in names(metro_files)) {
  file <- data_path(paste0(metro_files[metro], ".csv"))
  
  if (!file.exists(file)) {
    warning("File not found: ", file)
    next
  }
  
  zip_data <- readr::read_csv(file, show_col_types = FALSE)  
  formatted_zips <- sprintf("%05d", unique(zip_data[[1]]))  
  
  # Save into list + dataframe
  metro_zipcodes_formatted[[metro]] <- formatted_zips
  metro_zipcode_lookup <- rbind(
    metro_zipcode_lookup,
    data.frame(metro_area = metro, zip_code = formatted_zips, stringsAsFactors = FALSE)
  )
}

# ---------------------------
# Step 2. Function to get ACS data
# ---------------------------
get_metro_population_data <- function(year = 2017) {
  tryCatch({
    raw_data <- get_acs(
      geography = "zcta",
      variables = c(total_pop = "B01001_001"),
      year = year,
      output = "wide",
      geometry = FALSE
    )
    
    # Clean population data
    raw_data %>%
      transmute(
        zip_code   = as.character(GEOID),
        population = total_popE
      ) %>%
      filter(!is.na(population), population > 0)
    
  }, error = function(e) {
    message("Error fetching census data: ", e$message)
    return(NULL)
  })
}

# ---------------------------
# Step 3. Get ACS data & join
# ---------------------------
all_zcta_population_data <- get_metro_population_data(2017)

metro_population_data <- all_zcta_population_data %>%
  inner_join(metro_zipcode_lookup, by = "zip_code") %>%
  mutate(city_name = metro_names[metro_area])

# ---------------------------
# Step 4. Summarize
# ---------------------------
metro_summary <- metro_population_data %>%
  group_by(metro_area, city_name) %>%
  summarise(
    zip_count       = n(),
    total_population = sum(population, na.rm = TRUE),
       .groups = "drop"
  ) %>%
  arrange(desc(total_population))

print("Metro Area Population Summary:")
print(metro_summary)
#-------------------------------------------------------------------------------
# Join the QSR_restaurants_df and metro_population_data
#-------------------------------------------------------------------------------
#Change the name of postal_code in filtered Yelp to zip_code for common key
colnames(QSR_restaurants_df)[colnames(QSR_restaurants_df) == "postal_code"] <- "zip_code"

final_data <- left_join(metro_population_data, QSR_restaurants_df, by = "zip_code")
final_data_cleaned <- final_data[!is.na(final_data$name), ]
#-------------------------------------------------------------------------------
# Add a new metric. Customer Satisfaction and add it to the data frame
#-------------------------------------------------------------------------------
customer_satisfaction <- (final_data_cleaned$stars - 2) * final_data_cleaned$review_count
final_data_cleaned$customer_satisfaction<- customer_satisfaction
print(head(final_data_cleaned))
#-------------------------------------------------------------------------------
# Categorize QSRs by food type and add a column to final_data
#-------------------------------------------------------------------------------
#Define food types
burger_names <- c("McDonald's", "Burger King", "Wendy's", "Five Guys", "In-N-Out", "Shake Shack")
chicken_names <- c("KFC", "Popeyes", "Chick-fil-A", "Church's Chicken")
pizza_names <- c("Pizza Hut", "Domino's", "Papa John's", "Little Caesars")
sandwich_names <- c("Subway", "Jimmy John's", "Quiznos")
global_names <- c("Taco Bell", "Chipotle", "Panda Express")
snacks_names <- c("Starbucks", "Dunkin'", "Baskin-Robbins")
# Function to Categorize each QSR by Food Type
categorize_food_type <- function(restaurant_name) {
  # Create patterns for matching (handles variations in names)
  burger_pattern <- paste(burger_names, collapse = "|")
  chicken_pattern <- paste(chicken_names, collapse = "|")
  sandwich_pattern <- paste(sandwich_names, collapse = "|")
  global_pattern <- paste(global_names, collapse = "|")
  pizza_pattern <- paste(pizza_names, collapse = "|")
  snacks_pattern <- paste(snacks_names, collapse = "|")
  
  case_when(
    grepl(burger_pattern, restaurant_name, ignore.case = TRUE) ~ "Burger",
    grepl(chicken_pattern, restaurant_name, ignore.case = TRUE) ~ "Chicken", 
    grepl(sandwich_pattern, restaurant_name, ignore.case = TRUE) ~ "Sandwich",
    grepl(global_pattern, restaurant_name, ignore.case = TRUE) ~ "Global",
    grepl(pizza_pattern, restaurant_name, ignore.case = TRUE) ~ "Pizza",
    grepl(snacks_pattern, restaurant_name, ignore.case = TRUE) ~ "Snacks",
    TRUE ~ "Other"
  )
}
final_data_cleaned <- final_data_cleaned %>%
  mutate(food_type = categorize_food_type(name))
print(head(final_data_cleaned))
# --- 0) Normalize keys & types ---
final_data_cleaned <- final_data_cleaned %>%
  mutate(
    zip_code  = str_pad(as.character(zip_code), 5, pad = "0"),
    city_name = as.character(city_name),
    population = suppressWarnings(as.numeric(population))
  )

# --- 1) ZIP-level population (one row per ZIP per city_name) ---
zip_pop <- final_data_cleaned %>%
  group_by(city_name, zip_code) %>%
  summarise(population_zip = max(population, na.rm = TRUE), .groups = "drop") %>%
  mutate(population_zip = ifelse(is.finite(population_zip), population_zip, NA_real_))

# --- 2) ZIP-level QSR counts (use business_id if available) ---
if ("business_id" %in% names(final_data_cleaned)) {
  zip_qsr <- final_data_cleaned %>%
    distinct(business_id, city_name, zip_code) %>%
    count(city_name, zip_code, name = "Total_QSRs")
} else {
  zip_qsr <- final_data_cleaned %>%
    count(city_name, zip_code, name = "Total_QSRs")
}

# Merge ZIP metrics
zip_level <- full_join(zip_pop, zip_qsr, by = c("city_name","zip_code")) %>%
  mutate(Total_QSRs = coalesce(Total_QSRs, 0L))

# --- 3) CITY totals (population & QSRs) built from ZIP-level data ---
city_totals <- zip_level %>%
  group_by(city_name) %>%
  summarise(
    Total_Population_City = sum(population_zip, na.rm = TRUE),
    Total_QSRs_City       = sum(Total_QSRs,     na.rm = TRUE),
    .groups = "drop"
  )

# --- 4) Attach to every row in final_data_cleaned ---
final_data_cleaned <- final_data_cleaned %>%
  left_join(zip_level,   by = c("city_name","zip_code")) %>%   # adds: population_zip, Total_QSRs (ZIP)
  left_join(city_totals, by = "city_name")                      # adds: Total_Population_City (+ Total_QSRs_City)

# Optional city-level density fields (handy for Tableau/R)
final_data_cleaned <- final_data_cleaned %>%
  mutate(
    residents_per_qsr_city        = ifelse(Total_QSRs_City > 0, Total_Population_City / Total_QSRs_City, NA_real_),
    scaled_residents_per_qsr_city = residents_per_qsr_city / 1000
  )

# --- Quick checks (optional) ---
final_data_cleaned %>% select(city_name, zip_code, Total_QSRs, Total_Population_City) %>% head()
 final_data_cleaned %>% distinct(city_name) %>% nrow()
 print(colnames(final_data_cleaned))
#-------------------------------------------------------------------------------
# Data Prep for Tableau Food Type Viz
#-------------------------------------------------------------------------------
viz_data <- final_data_cleaned %>%
  group_by(city_name, metro_area, food_type) %>%
  summarise(
    residents_by_qsr = sum(population, na.rm = TRUE) / n_distinct(business_id),  # proxy: pop per restaurant
    avg_satisfaction = mean(customer_satisfaction, na.rm = TRUE),
    qsr_count = n_distinct(business_id)
  ) %>%
  ungroup()
 readr::write_csv(viz_data, processed_path("viz_data.csv"))
#-------------------------------------------------------------------------------
#Data Prep for Tableau Maps Blue ocean vs Ideal
#-------------------------------------------------------------------------------
#df for Main map
# ---- 0) Setup ----
city_order <- c(
  "Philadelphia","Tampa","Indianapolis","St Louis","Nashville",
  "Tucson","New Orleans","Boise","Reno","Santa Barbara"
)

# ---- 1) Standardize keys & restrict to the 10 metros ----
f <- final_data_cleaned %>%
  mutate(
    zip_code  = ifelse(is.na(zip_code), NA_character_,
                       str_pad(as.character(zip_code), 5, pad = "0")),
    city_name = city_name |> as.character() |> str_squish(),
    population = suppressWarnings(as.numeric(population)),
    customer_satisfaction = suppressWarnings(as.numeric(customer_satisfaction)),
    review_count = if ("review_count" %in% names(.))
      suppressWarnings(as.numeric(review_count)) else NA_real_
  ) %>%
  filter(city_name %in% city_order)

# ---- 2) ZIP-level metrics (population per ZIP + QSR count per ZIP) ----
# Count distinct businesses per ZIP if business_id exists; else count rows.
if ("business_id" %in% names(f)) {
  zip_level <- f %>%
    distinct(business_id, city_name, zip_code, .keep_all = TRUE) %>%
    group_by(city_name, zip_code) %>%
    summarise(
      population_zip = max(population, na.rm = TRUE),
      Total_QSRs     = n(),
      .groups = "drop"
    )
} else {
  zip_level <- f %>%
    group_by(city_name, zip_code) %>%
    summarise(
      population_zip = max(population, na.rm = TRUE),
      Total_QSRs     = n(),
      .groups = "drop"
    )
}

# ---- 3) City-level totals (population & QSRs) ----
city_totals <- zip_level %>%
  group_by(city_name) %>%
  summarise(
    Total_Population_City = sum(population_zip, na.rm = TRUE),
    Total_QSRs_City       = sum(Total_QSRs,     na.rm = TRUE),
    .groups = "drop"
  )

# ---- 4) City-level satisfaction (simple mean; flip switch for weighted) ----
USE_WEIGHTED_SAT <- FALSE
city_sat <- if (USE_WEIGHTED_SAT && "review_count" %in% names(f)) {
  f %>%
    group_by(city_name) %>%
    summarise(
      avg_customer_satisfaction = {
        w   <- dplyr::coalesce(review_count, 0)
        num <- sum(customer_satisfaction * w, na.rm = TRUE)
        den <- sum(w, na.rm = TRUE)
        ifelse(den > 0, num / den, NA_real_)
      },
      .groups = "drop"
    )
} else {
  f %>%
    group_by(city_name) %>%
    summarise(
      avg_customer_satisfaction = mean(customer_satisfaction, na.rm = TRUE),
      .groups = "drop"
    )
}

# ---- 5) City summary + derived metrics ----
city_summary <- city_totals %>%
  left_join(city_sat, by = "city_name") %>%
  mutate(
    residents_per_qsr_city        = ifelse(Total_QSRs_City > 0,
                                           Total_Population_City / Total_QSRs_City, NA_real_),
    scaled_residents_per_qsr_city = residents_per_qsr_city / 1000,
    IC_score = scaled_residents_per_qsr_city - avg_customer_satisfaction,
    city_name = factor(city_name, levels = city_order)
  )

# ---- 6) Canonical city-center coordinates (no Edmonton) ----
pin_data <- tibble::tribble(
  ~City,           ~Longitude,  ~Latitude,
  "Philadelphia",  -75.1652,     39.9526,
  "Tampa",         -82.4572,     27.9506,
  "Indianapolis",  -86.1581,     39.7684,
  "St Louis",      -90.1994,     38.6270,
  "Nashville",     -86.7816,     36.1627,
  "Tucson",        -110.9747,    32.2226,
  "New Orleans",   -90.0715,     29.9511,
  "Boise",         -116.2146,    43.6187,
  "Reno",          -119.8138,    39.5296,
  "Santa Barbara", -119.6982,    34.4208
)

# ---- 7) Final dataset for the main map (pins + city metrics) ----
metro_areas_for_map <- pin_data %>%
  left_join(city_summary %>% rename(City = city_name), by = "City")

# ---- 8) Optional checks & export ----
# Any cities missing from the join?
# anti_join(city_summary %>% rename(City = city_name), pin_data, by = "City")

# Write a CSV for Tableau (pins + metrics)
readr::write_csv(metro_areas_for_map, processed_path("metro_areas_for_map.csv"))

# I now have columns:
# City, Latitude, Longitude,
# Total_Population_City, Total_QSRs_City,
# residents_per_qsr_city, scaled_residents_per_qsr_city,
# avg_customer_satisfaction, IC_score
#df for City maps
# 1. Summarize per ZIP
map_data_prepped <- final_data_cleaned %>%
  group_by(zip_code, city_name) %>%
  summarise(
    population = max(as.numeric(population), na.rm = TRUE),
    Total_QSRs = max(Total_QSRs, na.rm = TRUE),   # <- reuse the stored count
    customer_satisfaction = mean(as.numeric(customer_satisfaction), na.rm = TRUE),
    .groups = "drop"
  )
print(colnames(metro_areas_for_map))
#-------------------------------------------------------------------------------
# R Visualizations 
#-------------------------------------------------------------------------------
#Blue Ocean vs Ideal Condition zips
#-------------------------------------------------------------------------
city_order <- c("Philadelphia","Tampa","Indianapolis","St Louis","Nashville",
                "Tucson","New Orleans","Boise","Reno","Santa Barbara")
POP_THRESH     <- 2500
SAT_THRESH     <- 15
DENSITY_CUTOFF <- 2500

# ----------------- 0) Clean inputs -----------------
f <- final_data_cleaned %>%
  mutate(
    zip_code  = ifelse(is.na(zip_code), NA_character_, str_pad(as.character(zip_code), 5, pad = "0")),
    city_name = city_name |> as.character() |> str_squish(),
    population = suppressWarnings(as.numeric(population)),
    customer_satisfaction = suppressWarnings(as.numeric(customer_satisfaction))
  ) %>%
  filter(city_name %in% city_order)

# ----------------- 1) Build ZIP BASE with city_name (from metro_area) -----------------
# Map metro_area -> city_name from existing data (1 city per metro_area)
metro_city_map <- final_data_cleaned %>%
  transmute(metro_area = as.character(metro_area),
            city_name  = as.character(city_name)) %>%
  distinct()

zip_base <- metro_zipcode_lookup %>%
  mutate(
    metro_area = as.character(metro_area),
    zip_code   = str_pad(as.character(zip_code), 5, pad = "0")
  ) %>%
  left_join(metro_city_map, by = "metro_area")

# Attach population since I have a ZCTA table
if (exists("all_zcta_population_data")) {
  zip_base <- zip_base %>%
    left_join(
      all_zcta_population_data %>%
        transmute(zip_code = str_pad(as.character(zip_code), 5, pad = "0"),
                  population = as.numeric(population)),
      by = "zip_code"
    )
} else {
  # fallback: use population in final_data_cleaned
  zip_base <- zip_base %>%
    left_join(
      f %>%
        group_by(zip_code) %>%
        summarise(population = max(population, na.rm = TRUE), .groups = "drop"),
      by = "zip_code"
    )
}

# quick diagnostic: any metro_area not mapped to a city_name?
# anti_join(metro_zipcode_lookup %>% mutate(metro_area=as.character(metro_area)),
#           metro_city_map, by="metro_area")

# ----------------- 2) QSR counts per ZIP (keep zeros!) -----------------
qsr_counts <- f %>%
  { if ("business_id" %in% names(.))
    distinct(., business_id, city_name, zip_code)
    else
      select(., city_name, zip_code)
  } %>%
  count(city_name, zip_code, name = "qsr_count")

zip_level <- zip_base %>%
  left_join(qsr_counts, by = c("city_name","zip_code")) %>%
  mutate(Total_QSRs = coalesce(qsr_count, 0L)) %>%   # <- zeros preserved
  select(metro_area, city_name, zip_code, population, Total_QSRs)

# optional ZIP-level satisfaction (NA is fine for true zero-QSR ZIPs)
zip_sat <- f %>%
  group_by(city_name, zip_code) %>%
  summarise(customer_satisfaction = mean(customer_satisfaction, na.rm = TRUE), .groups = "drop")

map_data_prepped <- zip_level %>%
  left_join(zip_sat, by = c("city_name","zip_code")) %>%
  mutate(
    residents_per_qsr = if_else(!is.na(Total_QSRs) & Total_QSRs > 0,
                                population / Total_QSRs, Inf)
  )

write_csv(map_data_prepped, "map_data_prepped.csv")

# ----------------- 3) Opportunities chart (ZIP counts per city) -----------------
metrics_by_city <- map_data_prepped %>%
  filter(city_name %in% city_order) %>%
  group_by(city_name) %>%
  summarise(
    `No Competition`   = sum((Total_QSRs == 0) & (population > POP_THRESH), na.rm = TRUE),
    `Ideal Conditions` = sum((customer_satisfaction > SAT_THRESH) & (residents_per_qsr < DENSITY_CUTOFF), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(c(`No Competition`, `Ideal Conditions`),
               names_to = "Scenario", values_to = "count") %>%
  rename(City = city_name)

# order by Ideal Conditions
order_tbl <- metrics_by_city %>% filter(Scenario == "Ideal Conditions") %>%
  select(City, order_key = count)

metrics_plot <- metrics_by_city %>%
  left_join(order_tbl, by = "City") %>%
  mutate(City = forcats::fct_reorder(City, order_key, .desc = FALSE, .na_rm = TRUE)) %>%
  arrange(order_key, City)  # optional: stable tie-break for plotting

ggplot(metrics_plot, aes(x = City, y = count, fill = Scenario)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  labs(
    title = "Comparison of Market Opportunities by Metro Area",
    x = "Metro Area",
    y = "Number of ZIP Codes",
    fill = "Scenario"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#-------------------------------------------------------------------------------
#   Ideal Conditions Graph
#-------------------------------------------------------------------------------

ideal_graph_data<-readr::read_csv(data_path("Sheet_9_data.csv"))

# ---- Load exported CSV ----
df_raw <- readr::read_csv(data_path("Sheet_9_data.csv"), show_col_types = FALSE) |>
  janitor::clean_names()

# Robust column picks (handles slight header variations)
pick <- function(nms, candidates) {
  hit <- intersect(candidates, nms); stopifnot(length(hit) >= 1); hit[1]
}
nm_city <- pick(names(df_raw), c("city","city_name"))
nm_rpq  <- pick(names(df_raw), c("scaled_residents_qsr","scaled_resident_qsr",
                                 "scaled_residents_qsr_","scaled_residents_qsr_50000"))
nm_csat <- pick(names(df_raw), c("avg_customer_satisfaction","avg_customer_sat",
                                 "customer_satisfaction"))

df <- df_raw |>
  transmute(
    city = .data[[nm_city]],
    rpq  = as.numeric(.data[[nm_rpq]]),   # already precomputed in the file
    csat = as.numeric(.data[[nm_csat]])
  ) |>
  mutate(city = factor(city, levels = unique(city)))  # keep CSV order

# ---- Right axis fixed to 70 ----
left_top <- max(df$rpq, na.rm = TRUE)  # I let the left axis top equal the max RPQ
k <- left_top / 70                     # scale CSAT to overlay on the left scale

# ---- Colors (RPQ blue, CSAT red) ----
col_res  <- "#3f77a9"
col_csat <- "#e45756"

# ---- Plot (areas + lines + points) ----
ggplot(df, aes(x = city, group = 1)) +
  # RPQ (left axis)
  geom_area(aes(y = rpq, fill = "Residents/QSR"), alpha = 0.25) +
  geom_line(aes(y = rpq, color = "Residents/QSR"), linewidth = 1) +
  geom_point(aes(y = rpq, color = "Residents/QSR"), size = 2) +
  
  # CSAT (plotted on left via *k*, labeled on right)
  geom_area(aes(y = csat * k, fill = "Customer satisfaction"), alpha = 0.25) +
  geom_line(aes(y = csat * k, color = "Customer satisfaction"), linewidth = 1) +
  geom_point(aes(y = csat * k, color = "Customer satisfaction"), size = 2) +
  
  scale_y_continuous(
    name = "Scaled residents/QSR",
    sec.axis = sec_axis(~ . / k, name = "Avg. Customer satisfaction (max 70)")
  ) +
  coord_cartesian(ylim = c(0, left_top)) +
  
  scale_fill_manual(
    values = c("Residents/QSR" = col_res, "Customer satisfaction" = col_csat),
    guide = "none"   
  ) +
  scale_color_manual(
    name   = NULL,
    values = c("Residents/QSR" = col_res, "Customer satisfaction" = col_csat)
  ) +
  
  labs(title = "City", x = NULL) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x       = element_text(angle = 35, hjust = 1),
    panel.grid.minor  = element_blank()
  )
