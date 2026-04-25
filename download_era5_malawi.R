library(ecmwfr)
library(googledrive)

options(keyring_backend = "env")
wf_set_key(user = "api", key = Sys.getenv("CDS_API_KEY"))
drive_auth(path = "gdrive_token/token_file")

# Focus strictly on the Malawi folder
existing_files <- drive_ls("ERA5_Data_Malawi")$name
all_years <- 1940:2025
year_batches <- split(all_years, ceiling(seq_along(all_years) / 3))

target_batch <- NULL
for (batch in year_batches) {
  fname <- paste0("era5_uv_daylight_malawi_", min(batch), "_", max(batch), ".nc")
  if (!(fname %in% existing_files)) {
    target_batch <- batch
    target_fname <- fname
    break
  }
}

if (is.null(target_batch)) stop("Malawi is completely downloaded!")

print(paste("Starting Malawi Batch:", target_fname))

request <- list(
  dataset_short_name = "reanalysis-era5-single-levels",
  product_type   = "reanalysis",
  variable       = "downward_uv_radiation_at_the_surface",
  year           = as.character(target_batch),
  month          = sprintf("%02d", 1:12),
  day            = sprintf("%02d", 1:31),
  time           = sprintf("%02d:00", 4:16),
  area           = c(-8.8, 32.3, -17.4, 36.1),
  format         = "netcdf",
  target         = target_fname
)

# Standard Built-in Download (Fixed Rate Limit & Timeout)
# We are increasing the retry to 5 minutes (300 seconds) 
# and explicitly telling it not to be verbose to avoid spamming the logs.
wf_request(
  user = "api", 
  request = request, 
  transfer = TRUE, 
  path = ".", 
  retry = 300, 
  time_out = 18000,
  verbose = FALSE
)

drive_upload(target_fname, path = "ERA5_Data_Malawi/")
file.remove(target_fname)
