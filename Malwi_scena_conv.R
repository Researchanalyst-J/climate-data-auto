
# ==============================================================================
# SCRIPT: MASTER WORKER SCENARIOS (MALAWI)
# Generates BOTH Quarterly and Annual datasets with strict IN/OUT row validation.
# CORRECTED: Fully restored all 5 metrics per scenario (Quarterly & Annual).
# ==============================================================================

library(arrow)
library(dplyr)
library(tidyr)

# ==============================================================================
# 1. DEFINE ALL FILE PATHS
# ==============================================================================
input_path <- "/Users/jacobninantharrakan/Desktop/R/SKIN_CANCER/IARC/file_out/malawi/Malawi_Master_Converted_1940_2025.parquet"

# Outputs (Quarterly)
out_csv_quarterly <- "/Users/jacobninantharrakan/Desktop/R/SKIN_CANCER/IARC/file_out/malawi/Malawi_Quarterly_Worker_Scenarios.csv"
out_parq_quarterly <- "/Users/jacobninantharrakan/Desktop/R/SKIN_CANCER/IARC/file_out/malawi/Malawi_Quarterly_Worker_Scenarios.parquet"

# Outputs (Annual)
out_csv_annual <- "/Users/jacobninantharrakan/Desktop/R/SKIN_CANCER/IARC/file_out/malawi/Malawi_Annual_Worker_Scenarios.csv"
out_parq_annual <- "/Users/jacobninantharrakan/Desktop/R/SKIN_CANCER/IARC/file_out/malawi/Malawi_Annual_Worker_Scenarios.parquet"

# Load the raw data
mw_data <- read_parquet(input_path)

# ==============================================================================
# 2. LOAD DATA & DEFINE GROUPS (Elevation Split)
# ==============================================================================
print("Loading Malawi Master Dataset...")

# Elevation grouping matching the 5 / 7 split in the v4 methodology
lowland_regions  <- c("Balaka", "Machinga", "Mangochi", "Nkhotakota", "Salima")              
highland_regions <- c("Blantyre", "Lilongwe", "Mchinji", "Mulanje", "Ntchisi", "Phalombe", "Zomba") 

# ==============================================================================
# 3. PRE-PROCESS VALIDATION (THE "IN" COUNT)
# ==============================================================================
print("Calculating Expected 'IN' Rows from Raw Data...")

expected_daily_rows <- mw_data %>% distinct(Region, Year, Date) %>% nrow()
expected_quarterly_rows <- mw_data %>% distinct(Region, Year, Quarter) %>% nrow()
expected_annual_rows <- mw_data %>% distinct(Region, Year) %>% nrow()

# ==============================================================================
# STEP 1: CALCULATE THE BASE DAILY BURDEN (CORRECTED DYNAMIC MATH)
# ==============================================================================
print("Calculating Daily Baseline for all Scenarios...")

mw_daily_base <- mw_data %>%
  mutate(Climate_Zone = case_when(
    Region %in% lowland_regions  ~ "Group 1: Lowland",
    Region %in% highland_regions ~ "Group 2: Highland",
    TRUE ~ "Unknown"
  )) %>%
  group_by(Climate_Zone, Region, Year, Quarter, Date) %>%
  summarise(
    # Scenario A (08:00 - 17:00) -> 10 Hour Shift
    Daily_UVI_A  = sum(Estimated_Hourly_UVI[Hour >= 8 & Hour <= 17], na.rm = TRUE) / length(8:17),
    Daily_Dose_A = sum(Hourly_Erythemal_Dose_kJ[Hour >= 8 & Hour <= 17], na.rm = TRUE),
    Daily_SED_A  = sum(Hourly_SED[Hour >= 8 & Hour <= 17], na.rm = TRUE),
    
    # Scenario B (06:00 - 10:00) -> 5 Hour Shift
    Daily_UVI_B  = sum(Estimated_Hourly_UVI[Hour >= 6 & Hour <= 10], na.rm = TRUE) / length(6:10),
    Daily_Dose_B = sum(Hourly_Erythemal_Dose_kJ[Hour >= 6 & Hour <= 10], na.rm = TRUE),
    Daily_SED_B  = sum(Hourly_SED[Hour >= 6 & Hour <= 10], na.rm = TRUE),
    
    # Scenario C (16:00 - 18:00) -> 3 Hour Shift
    Daily_UVI_C  = sum(Estimated_Hourly_UVI[Hour >= 16 & Hour <= 18], na.rm = TRUE) / length(16:18),
    Daily_Dose_C = sum(Hourly_Erythemal_Dose_kJ[Hour >= 16 & Hour <= 18], na.rm = TRUE),
    Daily_SED_C  = sum(Hourly_SED[Hour >= 16 & Hour <= 18], na.rm = TRUE),
    
    .groups = "drop"
  )

# ==============================================================================
# STEP 2: BRANCH 1 - GENERATE QUARTERLY DATASET
# ==============================================================================
print("Generating Quarterly Aggregations...")

mw_quarterly <- mw_daily_base %>%
  group_by(Climate_Zone, Region, Year, Quarter) %>%
  summarise(
    # Scenario A (5 Metrics)
    ScenA_08to17_Quarterly_Mean_UVI = mean(Daily_UVI_A, na.rm = TRUE),
    ScenA_08to17_Quarterly_Mean_Daily_Dose_kJ = mean(Daily_Dose_A, na.rm = TRUE),
    ScenA_08to17_Total_Quarterly_Dose_kJ = sum(Daily_Dose_A, na.rm = TRUE),
    ScenA_08to17_Quarterly_Mean_Daily_SED = mean(Daily_SED_A, na.rm = TRUE),
    ScenA_08to17_Total_Quarterly_SED = sum(Daily_SED_A, na.rm = TRUE),
    
    # Scenario B (5 Metrics)
    ScenB_06to10_Quarterly_Mean_UVI = mean(Daily_UVI_B, na.rm = TRUE),
    ScenB_06to10_Quarterly_Mean_Daily_Dose_kJ = mean(Daily_Dose_B, na.rm = TRUE),
    ScenB_06to10_Total_Quarterly_Dose_kJ = sum(Daily_Dose_B, na.rm = TRUE),
    ScenB_06to10_Quarterly_Mean_Daily_SED = mean(Daily_SED_B, na.rm = TRUE),
    ScenB_06to10_Total_Quarterly_SED = sum(Daily_SED_B, na.rm = TRUE),
    
    # Scenario C (5 Metrics)
    ScenC_16to18_Quarterly_Mean_UVI = mean(Daily_UVI_C, na.rm = TRUE),
    ScenC_16to18_Quarterly_Mean_Daily_Dose_kJ = mean(Daily_Dose_C, na.rm = TRUE),
    ScenC_16to18_Total_Quarterly_Dose_kJ = sum(Daily_Dose_C, na.rm = TRUE),
    ScenC_16to18_Quarterly_Mean_Daily_SED = mean(Daily_SED_C, na.rm = TRUE),
    ScenC_16to18_Total_Quarterly_SED = sum(Daily_SED_C, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Update naming to accurately reflect mean() logic, FULLY RESTORED 15 METRICS
  group_by(Climate_Zone, Year, Quarter) %>%
  mutate(
    # Scenario A Zone Benchmarks (5 Metrics)
    ScenA_ZoneMean_Quarterly_UVI = mean(ScenA_08to17_Quarterly_Mean_UVI, na.rm = TRUE),
    ScenA_ZoneMean_Quarterly_Daily_Dose_kJ = mean(ScenA_08to17_Quarterly_Mean_Daily_Dose_kJ, na.rm = TRUE),
    ScenA_ZoneMean_of_RegionalTotals_Quarterly_Dose_kJ = mean(ScenA_08to17_Total_Quarterly_Dose_kJ, na.rm = TRUE),
    ScenA_ZoneMean_Quarterly_Daily_SED = mean(ScenA_08to17_Quarterly_Mean_Daily_SED, na.rm = TRUE),
    ScenA_ZoneMean_of_RegionalTotals_Quarterly_SED = mean(ScenA_08to17_Total_Quarterly_SED, na.rm = TRUE),
    
    # Scenario B Zone Benchmarks (5 Metrics)
    ScenB_ZoneMean_Quarterly_UVI = mean(ScenB_06to10_Quarterly_Mean_UVI, na.rm = TRUE),
    ScenB_ZoneMean_Quarterly_Daily_Dose_kJ = mean(ScenB_06to10_Quarterly_Mean_Daily_Dose_kJ, na.rm = TRUE),
    ScenB_ZoneMean_of_RegionalTotals_Quarterly_Dose_kJ = mean(ScenB_06to10_Total_Quarterly_Dose_kJ, na.rm = TRUE),
    ScenB_ZoneMean_Quarterly_Daily_SED = mean(ScenB_06to10_Quarterly_Mean_Daily_SED, na.rm = TRUE),
    ScenB_ZoneMean_of_RegionalTotals_Quarterly_SED = mean(ScenB_06to10_Total_Quarterly_SED, na.rm = TRUE),
    
    # Scenario C Zone Benchmarks (5 Metrics)
    ScenC_ZoneMean_Quarterly_UVI = mean(ScenC_16to18_Quarterly_Mean_UVI, na.rm = TRUE),
    ScenC_ZoneMean_Quarterly_Daily_Dose_kJ = mean(ScenC_16to18_Quarterly_Mean_Daily_Dose_kJ, na.rm = TRUE),
    ScenC_ZoneMean_of_RegionalTotals_Quarterly_Dose_kJ = mean(ScenC_16to18_Total_Quarterly_Dose_kJ, na.rm = TRUE),
    ScenC_ZoneMean_Quarterly_Daily_SED = mean(ScenC_16to18_Quarterly_Mean_Daily_SED, na.rm = TRUE),
    ScenC_ZoneMean_of_RegionalTotals_Quarterly_SED = mean(ScenC_16to18_Total_Quarterly_SED, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(Country = "Malawi") %>% select(Country, everything())

# ==============================================================================
# STEP 3: BRANCH 2 - GENERATE ANNUAL DATASET
# ==============================================================================
print("Generating Annual Aggregations...")

mw_annual <- mw_daily_base %>%
  group_by(Climate_Zone, Region, Year) %>%
  summarise(
    # Scenario A (5 Metrics)
    ScenA_08to17_Yearly_Mean_UVI = mean(Daily_UVI_A, na.rm = TRUE),
    ScenA_08to17_Yearly_Mean_Daily_Dose = mean(Daily_Dose_A, na.rm = TRUE),
    ScenA_08to17_TOTAL_YEARLY_DOSE = sum(Daily_Dose_A, na.rm = TRUE),
    ScenA_08to17_Yearly_Mean_Daily_SED = mean(Daily_SED_A, na.rm = TRUE),
    ScenA_08to17_TOTAL_YEARLY_SED = sum(Daily_SED_A, na.rm = TRUE),
    
    # Scenario B (5 Metrics)
    ScenB_06to10_Yearly_Mean_UVI = mean(Daily_UVI_B, na.rm = TRUE),
    ScenB_06to10_Yearly_Mean_Daily_Dose = mean(Daily_Dose_B, na.rm = TRUE),
    ScenB_06to10_TOTAL_YEARLY_DOSE = sum(Daily_Dose_B, na.rm = TRUE),
    ScenB_06to10_Yearly_Mean_Daily_SED = mean(Daily_SED_B, na.rm = TRUE),
    ScenB_06to10_TOTAL_YEARLY_SED = sum(Daily_SED_B, na.rm = TRUE),
    
    # Scenario C (5 Metrics)
    ScenC_16to18_Yearly_Mean_UVI = mean(Daily_UVI_C, na.rm = TRUE),
    ScenC_16to18_Yearly_Mean_Daily_Dose = mean(Daily_Dose_C, na.rm = TRUE),
    ScenC_16to18_TOTAL_YEARLY_DOSE = sum(Daily_Dose_C, na.rm = TRUE),
    ScenC_16to18_Yearly_Mean_Daily_SED = mean(Daily_SED_C, na.rm = TRUE),
    ScenC_16to18_TOTAL_YEARLY_SED = sum(Daily_SED_C, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(Climate_Zone, Year) %>%
  mutate(
    # Scenario A Zone Benchmarks (5 Metrics)
    ScenA_ZoneMean_Yearly_UVI = mean(ScenA_08to17_Yearly_Mean_UVI, na.rm = TRUE),
    ScenA_ZoneMean_Yearly_Daily_Dose = mean(ScenA_08to17_Yearly_Mean_Daily_Dose, na.rm = TRUE),
    ScenA_ZoneMean_of_RegionalTotals_YEARLY_DOSE = mean(ScenA_08to17_TOTAL_YEARLY_DOSE, na.rm = TRUE),
    ScenA_ZoneMean_Yearly_Daily_SED = mean(ScenA_08to17_Yearly_Mean_Daily_SED, na.rm = TRUE),
    ScenA_ZoneMean_of_RegionalTotals_YEARLY_SED = mean(ScenA_08to17_TOTAL_YEARLY_SED, na.rm = TRUE),
    
    # Scenario B Zone Benchmarks (5 Metrics)
    ScenB_ZoneMean_Yearly_UVI = mean(ScenB_06to10_Yearly_Mean_UVI, na.rm = TRUE),
    ScenB_ZoneMean_Yearly_Daily_Dose = mean(ScenB_06to10_Yearly_Mean_Daily_Dose, na.rm = TRUE),
    ScenB_ZoneMean_of_RegionalTotals_YEARLY_DOSE = mean(ScenB_06to10_TOTAL_YEARLY_DOSE, na.rm = TRUE),
    ScenB_ZoneMean_Yearly_Daily_SED = mean(ScenB_06to10_Yearly_Mean_Daily_SED, na.rm = TRUE),
    ScenB_ZoneMean_of_RegionalTotals_YEARLY_SED = mean(ScenB_06to10_TOTAL_YEARLY_SED, na.rm = TRUE),
    
    # Scenario C Zone Benchmarks (5 Metrics)
    ScenC_ZoneMean_Yearly_UVI = mean(ScenC_16to18_Yearly_Mean_UVI, na.rm = TRUE),
    ScenC_ZoneMean_Yearly_Daily_Dose = mean(ScenC_16to18_Yearly_Mean_Daily_Dose, na.rm = TRUE),
    ScenC_ZoneMean_of_RegionalTotals_YEARLY_DOSE = mean(ScenC_16to18_TOTAL_YEARLY_DOSE, na.rm = TRUE),
    ScenC_ZoneMean_Yearly_Daily_SED = mean(ScenC_16to18_Yearly_Mean_Daily_SED, na.rm = TRUE),
    ScenC_ZoneMean_of_RegionalTotals_YEARLY_SED = mean(ScenC_16to18_TOTAL_YEARLY_SED, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(Country = "Malawi") %>% select(Country, everything())

# ==============================================================================
# STEP 4: VALIDATION
# ==============================================================================
if (expected_daily_rows != nrow(mw_daily_base) | 
    expected_quarterly_rows != nrow(mw_quarterly) | 
    expected_annual_rows != nrow(mw_annual)) {
  stop("VALIDATION FAILED: Data mismatch detected.")
}

# ==============================================================================
# STEP 5: EXPORT
# ==============================================================================
write.csv(mw_quarterly, out_csv_quarterly, row.names = FALSE)
write_parquet(mw_quarterly, out_parq_quarterly)
write.csv(mw_annual, out_csv_annual, row.names = FALSE)
write_parquet(mw_annual, out_parq_annual)
print("Malawi Pipeline Complete.")
