library(googledrive)
library(terra)
library(exactextractr)
library(sf)
library(geodata)
library(dplyr)
library(tidyr)
library(lubridate)

# 1. AUTHENTICATE
print("Authenticating with Google Drive...")
auth_json <- tempfile(fileext = ".json")
writeLines(Sys.getenv("GDRIVE_AUTH_TOKEN"), auth_json)
drive_auth(path = auth_json)

# 2. FETCH BOUNDARIES
print("Downloading Malawi boundaries...")
mw_map_raw <- gadm(country = "MWI", level = 1, path = tempdir())
mw_regions <- st_as_sf(mw_map_raw) %>%
  filter(NAME_1 %in% c("Balaka", "Blantyre", "Lilongwe", "Machinga", "Mangochi", 
                       "Mchinji", "Mulanje", "Nkhotakota", "Ntchisi", "Phalombe", 
                       "Salima", "Zomba"))
num_regions <- nrow(mw_regions)

# 3. LOCATE FILES
drive_files <- drive_ls("ERA5_Data_Malawi", pattern = "\\.nc$")
print(paste("Found", nrow(drive_files), "files."))

# 4. OUTPUT SETUP
master_csv <- "Malawi_Hourly_UV_Master.csv"
validation_log <- "Malawi_Validation_Log.csv"

# 5. THE LOOP
for (i in seq_len(nrow(drive_files))) {
  current_drive_file <- drive_files[i, ]
  local_nc_file <- current_drive_file$name
  print(paste("Processing:", local_nc_file))
  
  drive_download(current_drive_file, path = local_nc_file, overwrite = TRUE)
  
  uv_raster <- rast(local_nc_file)
  raw_layers <- nlyr(uv_raster)
  
  utc_times <- time(uv_raster)
  local_times <- with_tz(utc_times, tzone = "Africa/Blantyre")
  time_strings <- format(local_times, "%Y-%m-%d %H:%M:%S")
  
  layer_ids <- paste0("L", seq_len(raw_layers))
  names(uv_raster) <- layer_ids
  
  extracted_data <- exact_extract(uv_raster, mw_regions, 'mean', progress = FALSE)
  extracted_data$Region <- mw_regions$NAME_1
  extracted_data$Country <- "Malawi"
  
  time_mapping <- data.frame(Dummy_Name = paste0("mean.", layer_ids), Datetime_Local = time_strings)
  
  clean_data <- extracted_data %>%
    pivot_longer(cols = starts_with("mean.L"), names_to = "Dummy_Name", values_to = "Hourly_Mean_UV_Dose") %>%
    left_join(time_mapping, by = "Dummy_Name") %>%
    select(Datetime_Local, Country, Region, Hourly_Mean_UV_Dose)
  
  processed_rows <- nrow(clean_data)
  expected_rows <- raw_layers * num_regions
  status <- ifelse(processed_rows == expected_rows, "PASSED", "FAILED")
  
  log_entry <- data.frame(File_Name = local_nc_file, Raw_Time_Layers = raw_layers, 
                          Expected_Rows = expected_rows, Processed_Rows = processed_rows, Status = status)
  
  if (i == 1) {
    write.csv(clean_data, master_csv, row.names = FALSE)
    write.csv(log_entry, validation_log, row.names = FALSE)
  } else {
    write.table(clean_data, master_csv, sep = ",", append = TRUE, col.names = FALSE, row.names = FALSE)
    write.table(log_entry, validation_log, sep = ",", append = TRUE, col.names = FALSE, row.names = FALSE)
  }
  
  file.remove(local_nc_file)
  rm(uv_raster, extracted_data, clean_data, utc_times, local_times, time_strings, layer_ids, time_mapping)
  gc()
}

# 6. UPLOAD
drive_upload(media = master_csv, path = "ERA5_Data_Malawi/", name = master_csv, overwrite = TRUE)
drive_upload(media = validation_log, path = "ERA5_Data_Malawi/", name = validation_log, overwrite = TRUE)
