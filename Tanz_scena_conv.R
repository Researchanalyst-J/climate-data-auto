
# ==============================================================================
# SCRIPT: MASTER WORKER SCENARIOS (TANZANIA)
# Generates BOTH Quarterly and Annual datasets with strict IN/OUT row validation.
# CORRECTED: Dynamic UVI Math (missing zero fix) and Zone Benchmark Naming.
# ==============================================================================

library(arrow)
library(dplyr)
library(tidyr)

# ==============================================================================
# 1. DEFINE ALL FILE PATHS
# ==============================================================================
input_path <- "/Users/jacobninantharrakan/Desktop/R/SKIN_CANCER/IARC/file_out/tanzania/Tanzania_Master_Converted_1940_2025.parquet"

# Outputs (Quarterly)
out_csv_quarterly <- "/Users/jacobninantharrakan/Desktop/R/SKIN_CANCER/IARC/file_out/tanzania/Tanzania_Quarterly_Worker_Scenarios.csv"
out_parq_quarterly <- "/Users/jacobninantharrakan/Desktop/R/SKIN_CANCER/IARC/file_out/tanzania/Tanzania_Quarterly_Worker_Scenarios.parquet"

# Outputs (Annual)
out_csv_annual <- "/Users/jacobninantharrakan/Desktop/R/SKIN_CANCER/IARC/file_out/tanzania/Tanzania_Annual_Worker_Scenarios.csv"
out_parq_annual <- "/Users/jacobninantharrakan/Desktop/R/SKIN_CANCER/IARC/file_out/tanzania/Tanzania_Annual_Worker_Scenarios.parquet"

# 1. Load the raw data
tz_data <- read_parquet(input_path)

# 2. Extract and count the unique regions
raw_regions <- sort(unique(tz_data$Region))

cat("\n======================================\n")
cat("      RAW REGIONS IN INPUT FILE       \n")
cat("======================================\n")
cat("Total Count:", length(raw_regions), "\n\n")

# 3. Print them with quotes to expose hidden spaces
cat(paste0("'", raw_regions, "'"), sep = "\n")
cat("\n======================================\n")

# ==============================================================================
# 2. LOAD DATA & DEFINE GROUPS (UPDATED TO 5-ZONE AEZ CLASSIFICATION)
# ==============================================================================
print("Loading Tanzania Master Dataset...")
tz_data <- read_parquet(input_path)

# Defining 5 Literature-Supported Zones (Numbered for High-to-Low Risk Sorting)
zone_1_semi_arid <- c("Dodoma", "Shinyanga", "Singida", "Tabora", "Simiyu")
zone_2_lake_basin <- c("Mwanza", "Mara", "Geita")
zone_3_coastal <- c("Dar es Salaam", "Pwani", "Tanga", "Morogoro")
zone_4_western_humid <- c("Kagera", "Kigoma", "Katavi")
zone_5_northern_highland <- c("Arusha", "Manyara")

# ==============================================================================
# 3. PRE-PROCESS VALIDATION (THE "IN" COUNT)
# ==============================================================================
print("Calculating Expected 'IN' Rows from Raw Data...")

# Count unique combinations that SHOULD exist after grouping
expected_daily_rows <- tz_data %>% distinct(Region, Year, Date) %>% nrow()
expected_quarterly_rows <- tz_data %>% distinct(Region, Year, Quarter) %>% nrow()
expected_annual_rows <- tz_data %>% distinct(Region, Year) %>% nrow()

# ==============================================================================
# STEP 1: CALCULATE THE BASE DAILY BURDEN (CORRECTED DYNAMIC MATH)
# ==============================================================================
print("Calculating Daily Baseline for all Scenarios...")

tz_daily_base <- tz_data %>%
  mutate(Climate_Zone = case_when(
    Region %in% zone_1_semi_arid ~ "Zone 1: Central Semi-Arid Plateau",
    Region %in% zone_2_lake_basin ~ "Zone 2: Lake Victoria Basin System",
    Region %in% zone_3_coastal ~ "Zone 3: Coastal Humid Belt",
    Region %in% zone_4_western_humid ~ "Zone 4: Western Humid/Orographic",
    Region %in% zone_5_northern_highland ~ "Zone 5: Northern Highland System",
    TRUE ~ "Unknown"
  )) %>%
  # Keep Quarter in the grouping so we can use it later
  group_by(Climate_Zone, Region, Year, Quarter, Date) %>%
  summarise(
    # -------------------------------------------------------------------------
    # Scenario A (08:00 - 17:00)
    # -------------------------------------------------------------------------
    # DYNAMIC FIX: `length(8:17)` automatically counts the 10 hours in this shift.
    # By using `sum(...) / length(...)`, we treat any dark/missing hours exactly as 0,
    # preventing the `mean()` function from artificially inflating the morning UVI.
    Daily_UVI_A = sum(Estimated_Hourly_UVI[Hour >= 8 & Hour <= 17], na.rm = TRUE) / length(8:17),
    Daily_Dose_A = sum(Hourly_Erythemal_Dose_kJ[Hour >= 8 & Hour <= 17], na.rm = TRUE),
    Daily_SED_A = sum(Hourly_SED[Hour >= 8 & Hour <= 17], na.rm = TRUE),
    
    # -------------------------------------------------------------------------
    # Scenario B (06:00 - 10:00)
    # -------------------------------------------------------------------------
    # DYNAMIC FIX: `length(6:10)` automatically counts the 5 hours in this shift.
    Daily_UVI_B = sum(Estimated_Hourly_UVI[Hour >= 6 & Hour <= 10], na.rm = TRUE) / length(6:10),
    Daily_Dose_B = sum(Hourly_Erythemal_Dose_kJ[Hour >= 6 & Hour <= 10], na.rm = TRUE),
    Daily_SED_B = sum(Hourly_SED[Hour >= 6 & Hour <= 10], na.rm = TRUE),
    
    # -------------------------------------------------------------------------
    # Scenario C (16:00 - 18:00)
    # -------------------------------------------------------------------------
    # DYNAMIC FIX: `length(16:18)` automatically counts the 3 hours in this shift.
    Daily_UVI_C = sum(Estimated_Hourly_UVI[Hour >= 16 & Hour <= 18], na.rm = TRUE) / length(16:18),
    Daily_Dose_C = sum(Hourly_Erythemal_Dose_kJ[Hour >= 16 & Hour <= 18], na.rm = TRUE),
    Daily_SED_C = sum(Hourly_SED[Hour >= 16 & Hour <= 18], na.rm = TRUE),
    
    .groups = "drop"
  )

# ==============================================================================
# STEP 2: BRANCH 1 - GENERATE QUARTERLY DATASET
# ==============================================================================
print("Generating Quarterly Aggregations...")

tz_quarterly <- tz_daily_base %>%
  group_by(Climate_Zone, Region, Year, Quarter) %>%
  summarise(
    # Scenario A
    ScenA_08to17_Quarterly_Mean_UVI = mean(Daily_UVI_A, na.rm = TRUE),
    ScenA_08to17_Quarterly_Mean_Daily_Dose_kJ = mean(Daily_Dose_A, na.rm = TRUE),
    ScenA_08to17_Total_Quarterly_Dose_kJ = sum(Daily_Dose_A, na.rm = TRUE),
    ScenA_08to17_Quarterly_Mean_Daily_SED = mean(Daily_SED_A, na.rm = TRUE),
    ScenA_08to17_Total_Quarterly_SED = sum(Daily_SED_A, na.rm = TRUE),
    
    # Scenario B
    ScenB_06to10_Quarterly_Mean_UVI = mean(Daily_UVI_B, na.rm = TRUE),
    ScenB_06to10_Quarterly_Mean_Daily_Dose_kJ = mean(Daily_Dose_B, na.rm = TRUE),
    ScenB_06to10_Total_Quarterly_Dose_kJ = sum(Daily_Dose_B, na.rm = TRUE),
    ScenB_06to10_Quarterly_Mean_Daily_SED = mean(Daily_SED_B, na.rm = TRUE),
    ScenB_06to10_Total_Quarterly_SED = sum(Daily_SED_B, na.rm = TRUE),
    
    # Scenario C
    ScenC_16to18_Quarterly_Mean_UVI = mean(Daily_UVI_C, na.rm = TRUE),
    ScenC_16to18_Quarterly_Mean_Daily_Dose_kJ = mean(Daily_Dose_C, na.rm = TRUE),
    ScenC_16to18_Total_Quarterly_Dose_kJ = sum(Daily_Dose_C, na.rm = TRUE),
    ScenC_16to18_Quarterly_Mean_Daily_SED = mean(Daily_SED_C, na.rm = TRUE),
    ScenC_16to18_Total_Quarterly_SED = sum(Daily_SED_C, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # --- NEW: Inject Zone Context Benchmarks for Quarterly Data ---
  # NOTE: "ZoneTotal" was renamed to "ZoneMean_of_RegionalTotals" for clarity, 
  # as it uses the mean() function to average the buckets (regions) properly.
  group_by(Climate_Zone, Year, Quarter) %>%
  mutate(
    # Zone A
    ScenA_ZoneMean_Quarterly_UVI = mean(ScenA_08to17_Quarterly_Mean_UVI, na.rm = TRUE),
    ScenA_ZoneMean_Quarterly_Daily_Dose_kJ = mean(ScenA_08to17_Quarterly_Mean_Daily_Dose_kJ, na.rm = TRUE),
    ScenA_ZoneMean_of_RegionalTotals_Quarterly_Dose_kJ = mean(ScenA_08to17_Total_Quarterly_Dose_kJ, na.rm = TRUE),
    ScenA_ZoneMean_Quarterly_Daily_SED = mean(ScenA_08to17_Quarterly_Mean_Daily_SED, na.rm = TRUE),
    ScenA_ZoneMean_of_RegionalTotals_Quarterly_SED = mean(ScenA_08to17_Total_Quarterly_SED, na.rm = TRUE),
    
    # Zone B
    ScenB_ZoneMean_Quarterly_UVI = mean(ScenB_06to10_Quarterly_Mean_UVI, na.rm = TRUE),
    ScenB_ZoneMean_Quarterly_Daily_Dose_kJ = mean(ScenB_06to10_Quarterly_Mean_Daily_Dose_kJ, na.rm = TRUE),
    ScenB_ZoneMean_of_RegionalTotals_Quarterly_Dose_kJ = mean(ScenB_06to10_Total_Quarterly_Dose_kJ, na.rm = TRUE),
    ScenB_ZoneMean_Quarterly_Daily_SED = mean(ScenB_06to10_Quarterly_Mean_Daily_SED, na.rm = TRUE),
    ScenB_ZoneMean_of_RegionalTotals_Quarterly_SED = mean(ScenB_06to10_Total_Quarterly_SED, na.rm = TRUE),
    
    # Zone C
    ScenC_ZoneMean_Quarterly_UVI = mean(ScenC_16to18_Quarterly_Mean_UVI, na.rm = TRUE),
    ScenC_ZoneMean_Quarterly_Daily_Dose_kJ = mean(ScenC_16to18_Quarterly_Mean_Daily_Dose_kJ, na.rm = TRUE),
    ScenC_ZoneMean_of_RegionalTotals_Quarterly_Dose_kJ = mean(ScenC_16to18_Total_Quarterly_Dose_kJ, na.rm = TRUE),
    ScenC_ZoneMean_Quarterly_Daily_SED = mean(ScenC_16to18_Quarterly_Mean_Daily_SED, na.rm = TRUE),
    ScenC_ZoneMean_of_RegionalTotals_Quarterly_SED = mean(ScenC_16to18_Total_Quarterly_SED, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(Country = "Tanzania") %>% select(Country, everything())

# ==============================================================================
# STEP 3: BRANCH 2 - GENERATE ANNUAL DATASET
# ==============================================================================
print("Generating Annual Aggregations...")

tz_annual <- tz_daily_base %>%
  # Notice Quarter is completely dropped here to sum the whole year 
  group_by(Climate_Zone, Region, Year) %>%
  summarise(
    # Scenario A
    ScenA_08to17_Yearly_Mean_UVI = mean(Daily_UVI_A, na.rm = TRUE),
    ScenA_08to17_Yearly_Mean_Daily_Dose = mean(Daily_Dose_A, na.rm = TRUE),
    ScenA_08to17_TOTAL_YEARLY_DOSE = sum(Daily_Dose_A, na.rm = TRUE),
    ScenA_08to17_Yearly_Mean_Daily_SED = mean(Daily_SED_A, na.rm = TRUE),
    ScenA_08to17_TOTAL_YEARLY_SED = sum(Daily_SED_A, na.rm = TRUE),
    
    # Scenario B
    ScenB_06to10_Yearly_Mean_UVI = mean(Daily_UVI_B, na.rm = TRUE),
    ScenB_06to10_Yearly_Mean_Daily_Dose = mean(Daily_Dose_B, na.rm = TRUE),
    ScenB_06to10_TOTAL_YEARLY_DOSE = sum(Daily_Dose_B, na.rm = TRUE),
    ScenB_06to10_Yearly_Mean_Daily_SED = mean(Daily_SED_B, na.rm = TRUE),
    ScenB_06to10_TOTAL_YEARLY_SED = sum(Daily_SED_B, na.rm = TRUE),
    
    # Scenario C
    ScenC_16to18_Yearly_Mean_UVI = mean(Daily_UVI_C, na.rm = TRUE),
    ScenC_16to18_Yearly_Mean_Daily_Dose = mean(Daily_Dose_C, na.rm = TRUE),
    ScenC_16to18_TOTAL_YEARLY_DOSE = sum(Daily_Dose_C, na.rm = TRUE),
    ScenC_16to18_Yearly_Mean_Daily_SED = mean(Daily_SED_C, na.rm = TRUE),
    ScenC_16to18_TOTAL_YEARLY_SED = sum(Daily_SED_C, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # --- NEW: Inject Zone Context Benchmarks for Annual Data ---
  group_by(Climate_Zone, Year) %>%
  mutate(
    # Scenario A Zone Benchmarks
    ScenA_ZoneMean_Yearly_UVI = mean(ScenA_08to17_Yearly_Mean_UVI, na.rm = TRUE),
    ScenA_ZoneMean_Yearly_Daily_Dose = mean(ScenA_08to17_Yearly_Mean_Daily_Dose, na.rm = TRUE),
    ScenA_ZoneMean_of_RegionalTotals_YEARLY_DOSE = mean(ScenA_08to17_TOTAL_YEARLY_DOSE, na.rm = TRUE),
    ScenA_ZoneMean_Yearly_Daily_SED = mean(ScenA_08to17_Yearly_Mean_Daily_SED, na.rm = TRUE),
    ScenA_ZoneMean_of_RegionalTotals_YEARLY_SED = mean(ScenA_08to17_TOTAL_YEARLY_SED, na.rm = TRUE),
    
    # Scenario B Zone Benchmarks
    ScenB_ZoneMean_Yearly_UVI = mean(ScenB_06to10_Yearly_Mean_UVI, na.rm = TRUE),
    ScenB_ZoneMean_Yearly_Daily_Dose = mean(ScenB_06to10_Yearly_Mean_Daily_Dose, na.rm = TRUE),
    ScenB_ZoneMean_of_RegionalTotals_YEARLY_DOSE = mean(ScenB_06to10_TOTAL_YEARLY_DOSE, na.rm = TRUE),
    ScenB_ZoneMean_Yearly_Daily_SED = mean(ScenB_06to10_Yearly_Mean_Daily_SED, na.rm = TRUE),
    ScenB_ZoneMean_of_RegionalTotals_YEARLY_SED = mean(ScenB_06to10_TOTAL_YEARLY_SED, na.rm = TRUE),
    
    # Scenario C Zone Benchmarks
    ScenC_ZoneMean_Yearly_UVI = mean(ScenC_16to18_Yearly_Mean_UVI, na.rm = TRUE),
    ScenC_ZoneMean_Yearly_Daily_Dose = mean(ScenC_16to18_Yearly_Mean_Daily_Dose, na.rm = TRUE),
    ScenC_ZoneMean_of_RegionalTotals_YEARLY_DOSE = mean(ScenC_16to18_TOTAL_YEARLY_DOSE, na.rm = TRUE),
    ScenC_ZoneMean_Yearly_Daily_SED = mean(ScenC_16to18_Yearly_Mean_Daily_SED, na.rm = TRUE),
    ScenC_ZoneMean_of_RegionalTotals_YEARLY_SED = mean(ScenC_16to18_TOTAL_YEARLY_SED, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(Country = "Tanzania") %>% select(Country, everything())

# ==============================================================================
# STEP 4: POST-PROCESS VALIDATION (THE "OUT" COUNT & MATCH)
# ==============================================================================
print("Performing Data Validation...")

# Count ACTUAL rows generated in your new dataframes
actual_daily_rows <- nrow(tz_daily_base)
actual_quarterly_rows <- nrow(tz_quarterly)
actual_annual_rows <- nrow(tz_annual)

# Print the matching report to the console
cat("\n=======================================================\n")
cat("          IN / OUT ROW VALIDATION REPORT               \n")
cat("=======================================================\n")
cat("DAILY Base : IN (Expected) =", expected_daily_rows, "| OUT (Actual) =", actual_daily_rows, 
    "->", ifelse(expected_daily_rows == actual_daily_rows, "MATCH", "FAIL"), "\n")

cat("QUARTERLY  : IN (Expected) =", expected_quarterly_rows, "| OUT (Actual) =", actual_quarterly_rows, 
    "->", ifelse(expected_quarterly_rows == actual_quarterly_rows, "MATCH", "FAIL"), "\n")

cat("ANNUAL     : IN (Expected) =", expected_annual_rows, "| OUT (Actual) =", actual_annual_rows, 
    "->", ifelse(expected_annual_rows == actual_annual_rows, "MATCH", "FAIL"), "\n")
cat("=======================================================\n\n")

# Implement a hard stop if the validation fails
if (expected_daily_rows != actual_daily_rows | 
    expected_quarterly_rows != actual_quarterly_rows | 
    expected_annual_rows != actual_annual_rows) {
  stop("VALIDATION FAILED: Data was lost or duplicated during processing. Export halted.")
}

# ==============================================================================
# STEP 5: EXPORT ALL FILES
# ==============================================================================
print("Validation Passed. Exporting all 4 files safely...")

write.csv(tz_quarterly, out_csv_quarterly, row.names = FALSE)
write_parquet(tz_quarterly, out_parq_quarterly)
print("Quarterly Files Saved!")

write.csv(tz_annual, out_csv_annual, row.names = FALSE)
write_parquet(tz_annual, out_parq_annual)
print("Annual Files Saved!")

print("PIPELINE COMPLETE.")


