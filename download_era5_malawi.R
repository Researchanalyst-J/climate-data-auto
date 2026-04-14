library(ecmwfr)
library(googledrive)

# 1. Connect to Copernicus
options(keyring_backend = "env")
cds_key <- Sys.getenv("CDS_API_KEY")
wf_set_key(user = "api", key = cds_key)

# 2. Connect to Google Drive using your existing master key!
drive_auth(cache = "gdrive_token", email = TRUE)

# 3. Look inside the NEW Malawi folder
existing_files <- drive_ls("ERA5_Data_Malawi")$name

all_years <- 1940:2025
year_batches <- split(all_years, ceiling(seq_along(all_years) / 4))

target_batch <- NULL
for (batch in year_batches) {
  # Naming the files specifically for Malawi so they never get mixed up
  fname <- paste0("era5_uv_24hr_malawi_", min(batch), "_", max(batch), ".nc")
  if (!(fname %in% existing_files)) {
    target_batch <- batch
    target_fname <- fname
    break
  }
}

if (is.null(target_batch)) {
  stop("Success: All Malawi files have been downloaded to Google Drive!")
}

print(paste("Downloading:", target_fname))

# 4. Request the data using Malawi's exact coordinates
request <- list(
  dataset_short_name = "reanalysis-era5-single-levels",
  product_type   = "reanalysis",
  variable       = "downward_uv_radiation_at_the_surface", 
  year           = as.character(target_batch),
  month          = sprintf("%02d", 1:12),
  day            = sprintf("%02d", 1:31),
  time           = sprintf("%02d:00", 6:18), 
  area           = c(-9.3, 32.6, -17.1, 35.9), # Malawi Box (North, West, South, East)
  format         = "netcdf",
  target         = target_fname
)

wf_request(user = "api", request = request, transfer = TRUE, path = ".", verbose = TRUE)

# 5. Move it to the Malawi folder in Google Drive
print("Uploading to Google Drive...")
drive_upload(target_fname, path = "ERA5_Data_Malawi/")
file.remove(target_fname)
print("Malawi batch complete!")
