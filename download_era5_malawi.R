library(ecmwfr)
library(googledrive)

options(keyring_backend = "env")
wf_set_key(user = "api", key = Sys.getenv("CDS_API_KEY"))
drive_auth(cache = "gdrive_token", email = TRUE)

# Focus strictly on the Malawi folder
existing_files <- drive_ls("ERA5_Data_Malawi")$name
all_years <- 1940:2025
year_batches <- split(all_years, ceiling(seq_along(all_years) / 3)) # 3 Year Batches

target_batch <- NULL
for (batch in year_batches) {
  fname <- paste0("era5_uv_daylight_malawi_", min(batch), "_", max(batch), ".nc")
  if (!(fname %in% existing_files)) {
    target_batch <- batch
    target_fname <- fname
    break
  }
}

if (is.null(target_batch)) stop("Malawi is already complete!")

print(paste("Starting Malawi Batch:", target_fname))

id <- wf_request(user = "api", transfer = FALSE, request = list(
  dataset_short_name = "reanalysis-era5-single-levels",
  product_type   = "reanalysis",
  variable       = "downward_uv_radiation_at_the_surface", 
  year           = as.character(target_batch),
  month          = sprintf("%02d", 1:12), day = sprintf("%02d", 1:31),
  time           = sprintf("%02d:00", 6:18), 
  area           = c(-9.3, 32.6, -17.1, 35.9), # Malawi
  format         = "netcdf", target = target_fname
))

# Patient Polling (Checks every 2 minutes)
repeat {
  status <- wf_status(id, user = "api")
  print(paste("Malawi Status:", status$state))
  if (status$state == "completed") {
    wf_transfer(id, user = "api", path = ".")
    break
  } else if (status$state %in% c("failed", "aborted")) {
    stop("Request failed on server.")
  }
  Sys.sleep(120) 
}

drive_upload(target_fname, path = "ERA5_Data_Malawi/")
file.remove(target_fname)
