# ==============================================================================
# SCRIPT 1: DOWNLOAD AND FILTER TANZANIA REGIONS (GADM)
# ==============================================================================

# Install packages if you don't have them
# install.packages(c("geodata", "sf", "dplyr"))

library(geodata)
library(sf)
library(dplyr)

# 1. Define where to save the shapefiles
tz_shape_dir <- "/Users/jacobninantharrakan/Desktop/R/SKIN_CANCER/IARC/file_in/tanzania/regions"

# Create the directory if it doesn't exist
if (!dir.exists(tz_shape_dir)) {
  dir.create(tz_shape_dir, recursive = TRUE)
}

# 2. Download Tanzania level 1 boundaries (Regions) from GADM
print("Downloading Tanzania shapefile from GADM...")
tz_map_raw <- gadm(country = "TZA", level = 1, path = tz_shape_dir)

# 3. Convert to an sf object for easy filtering
tz_sf <- st_as_sf(tz_map_raw)

# 4. Define the 17 regions you want to keep
target_tz_regions <- c(
  "Arusha", "Dar es Salaam", "Dodoma", "Geita", "Kagera", 
  "Katavi", "Kigoma", "Manyara", "Mara", "Morogoro", 
  "Mwanza", "Pwani", "Shinyanga", "Simiyu", "Singida", 
  "Tabora", "Tanga"
)

# 5. Filter the shapefile
tz_filtered <- tz_sf %>% 
  filter(NAME_1 %in% target_tz_regions)

# Print a check to make sure we got them all
print(paste("Regions captured:", nrow(tz_filtered)))
print(tz_filtered$NAME_1)

# 6. Save the filtered shapefile
filtered_shape_path <- file.path(tz_shape_dir, "Tanzania_Filtered_Regions.shp")
st_write(tz_filtered, filtered_shape_path, delete_layer = TRUE)

print("Success! Filtered Tanzania shapefile is saved and ready.")




# ==============================================================================
# SCRIPT 2: DOWNLOAD AND FILTER MALAWI DISTRICTS (GADM) - CORRECTED
# ==============================================================================

library(geodata)
library(sf)
library(dplyr)

# 1. Define where to save the shapefiles
mw_shape_dir <- "/Users/jacobninantharrakan/Desktop/R/SKIN_CANCER/IARC/file_in/malawi/regions"

# Create the directory if it doesn't exist
if (!dir.exists(mw_shape_dir)) {
  dir.create(mw_shape_dir, recursive = TRUE)
}

# 2. Download Malawi level 1 boundaries (Districts) from GADM
print("Downloading Malawi shapefile from GADM...")
mw_map_raw <- gadm(country = "MWI", level = 1, path = mw_shape_dir)

# 3. Convert to an sf object for easy filtering
mw_sf <- st_as_sf(mw_map_raw)

# 4. Define the 12 core districts you want to keep
target_mw_districts <- c(
  "Balaka", "Blantyre", "Lilongwe", "Machinga", "Mangochi", 
  "Mchinji", "Mulanje", "Nkhotakota", "Ntchisi", "Phalombe", 
  "Salima", "Zomba"
)

# 5. Filter the shapefile using NAME_1
mw_filtered <- mw_sf %>% 
  filter(NAME_1 %in% target_mw_districts)

# Print a check to make sure we got them all
print(paste("Districts captured:", nrow(mw_filtered)))
print(mw_filtered$NAME_1)

# 6. Save the filtered shapefile
filtered_shape_path <- file.path(mw_shape_dir, "Malawi_Filtered_Districts.shp")
st_write(mw_filtered, filtered_shape_path, delete_layer = TRUE)

print("Success! Filtered Malawi shapefile is saved and ready.")
