# 1. Load the required packages
if (!require("ecmwfr")) install.packages("ecmwfr")
library(ecmwfr)

# Note: We do not need API credentials here because GitHub will 
# create the .cdsapirc file for us automatically!

# 2. Define the Sequence (1940 to 2025 in 4-year batches)
all_years <- 1940:2025
batch_size <- 4
year_groups <- split(all_years, ceiling(seq_along(all_years) / batch_size))

# 3. Figure out which batch to download next
# It looks for files we have already saved
existing_files <- list.files(pattern = "tz_mw_solar_.*\\.nc")

target_batch <- NULL
for (group in year_groups) {
  fname <- paste0("tz_mw_solar_", min(group), "_", max(group), ".nc")
  if (!(fname %in% existing_files)) {
    target_batch <- group
    target_filename <- fname
    break
  }
}

# Exit if everything is completely done
if (is.null(target_batch)) {
  print("SUCCESS: All data from 1940 to 2025 has been downloaded.")
  quit(save = "no")
}

print(paste("Downloading batch:", paste(target_batch, collapse = ", ")))

# 4. The Request (Tanzania & Malawi limits)
request <- list(
  dataset_short_name = "reanalysis-era5-single-levels",
  product_type   = "reanalysis",
  variable       = "surface_solar_radiation_downwards",
  year           = as.character(target_batch),
  month          = sprintf("%02d", 1:12),
  day            = sprintf("%02d", 1:31),
  time           = sprintf("%02d:00", 6:18), # Daytime hours
  area           = c(-0.9, 29.3, -11.8, 40.5), # North, West, South, East
  format         = "netcdf",
  target         = target_filename
)

# 5. Start Request & Download
# The code will pause here and wait for the CDS server to finish
wf_request(
  user = "api", # Tells ecmwfr to use the .cdsapirc file
  request = request,
  transfer = TRUE,
  path = ".",
  verbose = TRUE
)
