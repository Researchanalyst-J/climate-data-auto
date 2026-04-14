library(ecmwfr)
library(googledrive)

# 1. Connect to Copernicus
options(keyring_backend = "env")
cds_key <- Sys.getenv("CDS_API_KEY")
wf_set_key(user = "api", key = cds_key)

# 2. Connect to Google Drive using your token
drive_auth(cache = "gdrive_token", email = TRUE)

# 3. Figure out which 4-year batch is next
existing_files <- drive_ls("ERA5_Data")$name

all_years <- 1940:2025
year_batches <- split(all_years, ceiling(seq_along(all_years) / 4))

target_batch <- NULL
for (batch in year_batches) {
  # I updated the name so you know it is UV and 24-hour data
  fname <- paste0("era5_uv_24hr_", min(batch), "_", max(batch), ".nc")
  if (!(fname %in% existing_files)) {
    target_batch <- batch
    target_fname <- fname
    break
  }
}

if (is.null(target_batch)) {
  stop("Success: All files have been downloaded to Google Drive!")
}

print(paste("Downloading:", target_fname))

# 4. Request the data from Copernicus
request <- list(
  dataset_short_name = "reanalysis-era5-single-levels",
  product_type   = "reanalysis",
  variable       = "downward_uv_radiation_at_the_surface", # Switched to UV!
  year           = as.character(target_batch),
  month          = sprintf("%02d", 1:12),
  day            = sprintf("%02d", 1:31),
  time           = sprintf("%02d:00", 6:18), # All 24 hours!
  area           = c(-0.9, 29.3, -11.8, 40.5),
  format         = "netcdf",
  target         = target_fname
)

wf_request(user = "api", request = request, transfer = TRUE, path = ".", verbose = TRUE)

# 5. Move it to Google Drive and delete it from GitHub
print("Uploading to Google Drive...")
drive_upload(target_fname, path = "ERA5_Data/")
file.remove(target_fname)
print("Batch complete!")
