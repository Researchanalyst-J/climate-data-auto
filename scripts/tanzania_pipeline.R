library(googledrive)
library(terra)
library(exactextractr)
library(sf)
library(geodata)
library(dplyr)
library(tidyr)
library(lubridate)

# ==============================================================================
# 1. AUTHENTICATE
# ==============================================================================
print("Authenticating with Google Drive...")
options(gargle_oauth_cache = "gdrive_token")
drive_auth(cache = "gdrive_token", email = "jacobn158@gmail.com")

# ==============================================================================
# 2. FETCH BOUNDARIES
# ==============================================================================
print("Downloading Tanzania boundaries...")
tz_map_raw <- gadm(country = "TZA", level = 1, path = tempdir())
tz_regions <- st_as_sf(tz_map_raw) %>%
  filter(NAME_1 %in% c("Arusha", "Dar es Salaam", "Dodoma", "Geita", "Kagera", 
                       "Katavi", "Kigoma", "Manyara", "Mara", "Morogoro", 
                       "Mwanza", "Pwani", "Shinyanga", "Simiyu", "Singida", 
                       "Tabora", "Tanga"))
num_regions <- nrow(tz_regions)
print(paste("Number of Tanzania regions:", num_regions))

# ==============================================================================
# 3. LOCATE FILES & CHECK PROGRESS
# ==============================================================================
drive_files <- drive_ls("ERA5_Data", pattern = "\\.nc$")
print(paste("Found", nrow(drive_files), "files."))

if (nrow(drive_files) == 0) {
  stop("No NetCDF files found in Google Drive folder.")
}

master_csv <- "Tanzania_Hourly_UV_Master.csv"
validation_log <- "Tanzania_Validation_Log.csv"

# Download existing progress if it exists to resume
processed_files <- c()
existing_log <- drive_find(validation_log)
if (nrow(existing_log) > 0) {
  print("Found previous run! Resuming progress...")
  drive_download(existing_log, path = validation_log, overwrite = TRUE)
  drive_download(drive_find(master_csv), path = master_csv, overwrite = TRUE)
  log_data <- read.csv(validation_log)
  processed_files <- log_data$File_Name
}

# ==============================================================================
# 4. PROCESS FILES (WITH AUTO-SAVE)
# ==============================================================================
for (i in seq_len(nrow(drive_files))) {
  current_drive_file <- drive_files[i, ]
  local_nc_file <- current_drive_file$name
  
  # Check if we already processed this file in a previous run
  if (local_nc_file %in% processed_files) {
    print(paste("Skipping already processed file:", local_nc_file))
    next
  }

  print(paste("Processing:", local_nc_file))

  drive_download(current_drive_file, path = local_nc_file, overwrite = TRUE)

  uv_raster <- rast(local_nc_file)
  raw_layers <- nlyr(uv_raster)
  
  utc_times <- time(uv_raster)
  local_times <- with_tz(utc_times, tzone = "Africa/Dar_es_Salaam")
  time_strings <- format(local_times, "%Y-%m-%d %H:%M:%S")

  layer_ids <- paste0("L", seq_len(raw_layers))
  names(uv_raster) <- layer_ids

  extracted_data <- exact_extract(uv_raster, tz_regions, "mean", progress = FALSE)
  extracted_data$Region <- tz_regions$NAME_1
  extracted_data$Country <- "Tanzania"

  time_mapping <- data.frame(Dummy_Name = paste0("mean.", layer_ids), Datetime_Local = time_strings)

  clean_data <- extracted_data %>%
    pivot_longer(cols = starts_with("mean.L"), names_to = "Dummy_Name", values_to = "Hourly_Mean_UV_Dose") %>%
    left_join(time_mapping, by = "Dummy_Name") %>%
    select(Datetime_Local, Country, Region, Hourly_Mean_UV_Dose)

  processed_rows <- nrow(clean_data)
  expected_rows <- raw_layers * num_regions
  status <- ifelse(processed_rows == expected_rows, "PASSED", "FAILED")
  print(paste("Validation:", status, "| Expected:", expected_rows, "| Processed:", processed_rows))

  log_entry <- data.frame(File_Name = local_nc_file, Raw_Time_Layers = raw_layers, 
                          Expected_Rows = expected_rows, Processed_Rows = processed_rows, Status = status)

  # Append or create local files
  if (!file.exists(master_csv)) {
    write.csv(clean_data, master_csv, row.names = FALSE)
    write.csv(log_entry, validation_log, row.names = FALSE)
  } else {
    write.table(clean_data, master_csv, sep = ",", append = TRUE, col.names = FALSE, row.names = FALSE)
    write.table(log_entry, validation_log, sep = ",", append = TRUE, col.names = FALSE, row.names = FALSE)
  }

  # UPLOAD IMMEDIATELY TO SAVE PROGRESS (drive_put overwrites the old version on drive)
  print("Saving progress to Google Drive...")
  drive_put(media = master_csv, path = "ERA5_Data/", name = master_csv)
  drive_put(media = validation_log, path = "ERA5_Data/", name = validation_log)

  file.remove(local_nc_file)
  rm(uv_raster, extracted_data, clean_data, utc_times, local_times, time_strings, layer_ids, time_mapping)
  gc()
}

print("Tanzania pipeline complete.")
