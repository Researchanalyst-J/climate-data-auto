library(ecmwfr)
library(googledrive)

# Connect to Services
options(keyring_backend = "env")
wf_set_key(user = "api", key = Sys.getenv("CDS_API_KEY"))
drive_auth(cache = "gdrive_token", email = TRUE)

# Find next 2-year batch
existing_files <- drive_ls("ERA5_Data_Malawi")$name
all_years <- 1940:2025
year_batches <- split(all_years, ceiling(seq_along(all_years) / 2))

target_batch <- NULL
for (batch in year_batches) {
  fname <- paste0("era5_uv_daylight_malawi_", min(batch), "_", max(batch), ".nc")
  if (!(fname %in% existing_files)) {
    target_batch <- batch
    target_fname <- fname
    break
  }
}

if (is.null(target_batch)) stop("All Malawi data downloaded!")

# SUBMIT THE REQUEST
print(paste("Requesting batch:", target_fname))
request <- list(
  dataset_short_name = "reanalysis-era5-single-levels",
  product_type   = "reanalysis",
  variable       = "downward_uv_radiation_at_the_surface", 
  year           = as.character(target_batch),
  month          = sprintf("%02d", 1:12), day = sprintf("%02d", 1:31),
  time           = sprintf("%02d:00", 6:18), 
  area           = c(-9.3, 32.6, -17.1, 35.9), 
  format         = "netcdf", target = target_fname
)

# Use 'transfer = FALSE' so we can handle the waiting manually
id <- wf_request(user = "api", request = request, transfer = FALSE)

# MANUAL PATIENT POLLING LOOP
repeat {
  status <- wf_status(id, user = "api")
  print(paste("Status:", status$state))
  
  if (status$state == "completed") {
    wf_transfer(id, user = "api", path = ".")
    break
  } else if (status$state %in% c("failed", "aborted")) {
    stop("Request failed on Copernicus server.")
  }
  
  print("Waiting 120 seconds before checking again...")
  Sys.sleep(120) # This is the "secret sauce" - it stops the spamming
}

# Upload to Drive
drive_upload(target_fname, path = "ERA5_Data_Malawi/")
file.remove(target_fname)
