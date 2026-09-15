# ==========================================
# PHASE 1: DATA INGESTION & COMPLETENESS
# ==========================================

# 1. Load required packages (install if you haven't already: install.packages(c("arrow", "dplyr", "lubridate", "tidyr")))
install.packages("arrow")
library(arrow)
library(dplyr)
library(lubridate)
library(tidyr)

# 2. Fast Loading (Phase 1a)
file_path <- "/Users/jacobninantharrakan/Desktop/R/SKIN_CANCER/IARC/file_in/tanzania/Tanzania_hourly_UV_Master.csv"

print("Loading 380MB Tanzania dataset...")
tanzania_raw <- read_csv_arrow(file_path)
print("File loaded successfully!")

# 3. Time Extraction (Phase 1b)
print("Parsing timestamps and extracting Date, Year, Month, and Hour...")
tanzania_clean <- tanzania_raw %>%
  mutate(
    # Force R to recognize this as a proper date-time object
    Datetime_Local = ymd_hms(Datetime_Local),
    
    # Extract specific components for easy grouping later
    Date = as_date(Datetime_Local),
    Year = year(Datetime_Local),
    Month = month(Datetime_Local),
    Quarter = quarter(Datetime_Local),
    Hour = hour(Datetime_Local)
  )

# 4. The Completeness Check (Phase 2a)
print("Running mathematical completeness audit...")

completeness_grid <- tanzania_clean %>%
  group_by(Region, Hour) %>%
  # Count how many rows exist for every specific hour in every region
  summarise(Total_Recorded_Hours = n(), .groups = "drop") %>%
  # Pivot the data so Regions are rows, and Hours are columns (makes it easy to read!)
  pivot_wider(names_from = Hour, values_from = Total_Recorded_Hours, names_prefix = "Hour_")

# Open the final grid in RStudio's viewer
View(completeness_grid)

print("Audit complete! Check the completeness_grid tab.")


tanzania_clean %>%
  group_by(Hour) %>%
  summarise(rows = n(), zeros = sum(Hourly_Mean_UV_Dose == 0, na.rm = TRUE))












# ==========================================
# DATA AUDIT: RAW UV DOSE COLUMN
# ==========================================

print("--- HEAD: FIRST 10 RAW UV DOSE VALUES ---")
print(head(tanzania_clean$Hourly_Mean_UV_Dose, 10))

print("--- SUMMARY STATS: RAW UV DOSE ---")
print(summary(tanzania_clean$Hourly_Mean_UV_Dose))


# ==========================================
# PHASE 2: FINAL METRIC CONVERSIONS
# ==========================================

print("Applying medical and physical conversions to raw UV data...")

# Create the final dataset directly from tanzania_clean
tanzania_final <- tanzania_clean %>%
  mutate(
    # 1. THE SPEEDOMETER: Estimated Hourly UVI 
    # (Divide by 3600 for Watts, multiply by 0.003 for Erythemal, multiply by 40 for WHO Index)
    Estimated_Hourly_UVI = (Hourly_Mean_UV_Dose / 3600) * 0.003 * 40,
    
    # 2. THE METRIC ODOMETER: Hourly Erythemal Dose (kJ/m2)
    # (Multiply by 0.003 for Erythemal, divide by 1000 for Kilojoules)
    Hourly_Erythemal_Dose_kJ = (Hourly_Mean_UV_Dose * 0.003) / 1000,
    
    # 3. THE MEDICAL ODOMETER: Hourly Standard Erythemal Dose (SED)
    # (Multiply by 0.003 for Erythemal, divide by 100 for SED standard)
    Hourly_SED = (Hourly_Mean_UV_Dose * 0.003) / 100
  )

# ==========================================
# SAFETY CHECK & SUMMARY
# ==========================================

print("--- FINAL DATASET SUMMARY: NEW METRICS ---")
print(summary(tanzania_final %>% select(Estimated_Hourly_UVI, Hourly_Erythemal_Dose_kJ, Hourly_SED)))

print("--- TOP 5 MOST EXTREME HOURS: SIDE-BY-SIDE COMPARISON ---")
top_exposure <- tanzania_final %>%
  # Select the raw dose and the three new metrics to verify the math
  select(Region, Date, Hour, Hourly_Mean_UV_Dose, Estimated_Hourly_UVI, Hourly_Erythemal_Dose_kJ, Hourly_SED) %>%
  # Sort from highest exposure to lowest
  arrange(desc(Hourly_SED)) %>%
  head(5)

print(top_exposure)


# ==========================================
# PHASE 2.1: EXPORT MASTER DATASET
# ==========================================

print("Preparing to export the massive final dataset...")

# 1. Define the exact output file path
output_file_path <- "/Users/jacobninantharrakan/Desktop/R/SKIN_CANCER/IARC/file_out/tanzania/Tanzania_Master_Converted_1940_202X.csv"

# 2. Write the file using Arrow for maximum speed
# This writes every single row and all the new columns safely.
write_csv_arrow(tanzania_final, output_file_path)

print(paste("SUCCESS: Master dataset exported safely to:", output_file_path))


# ==========================================
# PHASE 2.1: EXPORT MASTER DATASET (PARQUET)
# ==========================================

print("Preparing to export the final dataset as a compressed Parquet file...")

# 1. Define the exact output file path (Note the .parquet extension and 2025)
parquet_file_path <- "/Users/jacobninantharrakan/Desktop/R/SKIN_CANCER/IARC/file_out/tanzania/Tanzania_Master_Converted_1940_2025.parquet"

# 2. Write the file using Arrow's Parquet engine for maximum compression
write_parquet(tanzania_final, parquet_file_path)

print(paste("SUCCESS: Master dataset exported safely to:", parquet_file_path))







