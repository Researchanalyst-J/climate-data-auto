# ==============================================================================
# SCRIPT 3: TANZANIA HOURLY UV EXTRACTION (PROCESS & TOSS)
# ==============================================================================

# Install any missing packages first: 
# install.packages(c("terra", "exactextractr", "sf", "dplyr", "tidyr", "lubridate"))

library(terra)
library(exactextractr)
library(sf)
library(dplyr)
library(tidyr)
library(lubridate)

# 1. Define your exact IARC directory paths
nc_dir <- "/Users/jacobninantharrakan/Desktop/R/SKIN_CANCER/IARC/file_in/tanzania"
shape_path <- "/Users/jacobninantharrakan/Desktop/R/SKIN_CANCER/IARC/file_in/tanzania/regions/Tanzania_Filtered_Regions.shp"
out_csv <- "/Users/jacobninantharrakan/Desktop/R/SKIN_CANCER/IARC/file_out/Tanzania_Hourly_UV_Master.csv"

# 2. Load the Tanzania 17-Region shapefile
print("Loading Tanzania shapefile...")
tz_regions <- st_read(shape_path)

# 3. Get the list of the 29 NetCDF files
nc_files <- list.files(nc_dir, pattern = "\\.nc$", full.names = TRUE)
print(paste("Found", length(nc_files), "NetCDF files to process."))

# 4. Process and Toss Loop
for (i in seq_along(nc_files)) {
  current_file <- nc_files[i]
  print(paste("Processing file", i, "of", length(nc_files), ":", basename(current_file)))
  
  # A. Load the NetCDF file as a raster grid
  uv_raster <- rast(current_file)
  
  # B. Extract UTC time and convert to Tanzania Local Time (East Africa Time: UTC+3)
  utc_times <- time(uv_raster)
  local_times <- with_tz(utc_times, tzone = "Africa/Dar_es_Salaam")
  
  # Format as 24-hour string (e.g., "1985-01-01 06:00:00")
  time_strings <- format(local_times, "%Y-%m-%d %H:%M:%S")
  
  # Rename the raster layers to their exact local time so we don't lose track
  names(uv_raster) <- time_strings
  
  # C. Extract the regional average UV dose for EVERY hour in the file
  print("   -> Running spatial extraction (this takes a moment)...")
  extracted_data <- exact_extract(uv_raster, tz_regions, 'mean', progress = FALSE)
  
  # D. Format the data to match our clean blueprint (Wide to Long format)
  extracted_data$Region <- tz_regions$NAME_1
  extracted_data$Country <- "Tanzania"
  
  clean_data <- extracted_data %>%
    pivot_longer(
      cols = starts_with("mean."),
      names_to = "Datetime_Local",
      values_to = "Hourly_Mean_UV_Dose"
    ) %>%
    mutate(
      # Remove the "mean." prefix that exactextractr adds to column names
      Datetime_Local = sub("mean.", "", Datetime_Local)
    ) %>%
    select(Datetime_Local, Country, Region, Hourly_Mean_UV_Dose)
  
  # E. Append to the master CSV file
  # If it's the very first file, write the file with headers. 
  # If it's file 2-29, just append the rows to the bottom silently.
  if (i == 1) {
    write.csv(clean_data, out_csv, row.names = FALSE)
  } else {
    write.table(clean_data, out_csv, sep = ",", append = TRUE, col.names = FALSE, row.names = FALSE)
  }
  
  print("   -> Data saved to CSV. Clearing memory...")
  
  # F. TOSS: Clear the heavy variables from RAM and force garbage collection
  rm(uv_raster, extracted_data, clean_data, utc_times, local_times, time_strings)
  gc()
}

print("=====================================================")
print(paste("TANZANIA COMPLETE! Master CSV saved at:", out_csv))
print("=====================================================")