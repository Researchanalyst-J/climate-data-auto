library(ecmwfr)
library(googledrive)

# 1. Authenticate Copernicus (Directly inside R to fix the password error!)
cds_key <- Sys.getenv("CDS_API_KEY")
wf_set_key(user = "api", key = cds_key, service = "cds")

# 2. Log into Google Drive using your secret key
drive_auth(path = "gdrive_key.json")

# 3. Check what is already inside your Google Drive
existing_files <- drive_ls("ERA5_Data")$name

# 4. Figure out which 4-year batch is next
all_years <- 1940:2025
year_batches <- split(all_years, ceiling(seq_along(all_years) / 4))

target_batch <- NULL
for (batch in year_batches) {
  fname <- paste0("era5_", min(batch), "_", max(batch), ".nc")
  if (!(fname %in% existing_files)) {
    target_batch <- batch
    target_fname <- fname
    break
  }
}

if (is.null(target_batch)) {
  stop("Success: All files are already in Google Drive!")
}

print(paste("Downloading:", target_fname))

# 5. Request the data from Copernicus
request <- list(
  dataset_short_name = "reanalysis-era5-single-levels",
  product_type   = "reanalysis",
  variable       = "surface_solar_radiation_downwards",
  year           = as.character(target_batch),
  month          = sprintf("%02d", 1:12),
  day            = sprintf("%02d", 1:31),
  time           = sprintf("%02d:00", 6:18),
  area           = c(-0.9, 29.3, -11.8, 40.5),
  format         = "netcdf",
  target         = target_fname
)

# Download it to the GitHub server
wf_request(user = "api", request = request, transfer = TRUE, path = ".", verbose = TRUE)

# 6. Move it to Google Drive and delete it from GitHub
print("Uploading to Google Drive...")
drive_upload(target_fname, path = "ERA5_Data/")
file.remove(target_fname)
print("Done!")
