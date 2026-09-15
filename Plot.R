
# ==============================================================================
# SCRIPT: PLOT 1 & 2 - LONGITUDINAL REGIONAL SCENARIOS (TANZANIA)
# ==============================================================================

library(arrow)
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)

# ==============================================================================
# 0. LOAD THE SAVED DATA DIRECTLY
# ==============================================================================
print("Loading Annual Data for Plotting...")
input_annual_path <- "/Users/jacobninantharrakan/Desktop/R/SKIN_CANCER/IARC/file_out/tanzania/Tanzania_Annual_Worker_Scenarios.parquet"

# Load the data directly into the environment
tz_annual <- read_parquet(input_annual_path)


# ---------------------------------------------------------
# STEP 1: MELT THE WIDE DATA INTO A CLEAN "LONG" FORMAT
# ---------------------------------------------------------
print("Pivoting data to Long Format...")

tz_long <- tz_annual %>%
  select(-contains("ZoneMean"), -contains("ZoneTotal"), -contains("Zone_Representative")) %>% 
  pivot_longer(
    cols = starts_with("Scen"),
    names_to = "Raw_Column",
    values_to = "Value"
  ) %>%
  mutate(
    Scenario = case_when(
      grepl("ScenA", Raw_Column) ~ "Scenario A (08:00 - 17:00)",
      grepl("ScenB", Raw_Column) ~ "Scenario B (06:00 - 10:00)",
      grepl("ScenC", Raw_Column) ~ "Scenario C (16:00 - 18:00)"
    ),
    Metric = case_when(
      grepl("Yearly_Mean_UVI", Raw_Column) ~ "Mean UVI",
      grepl("Yearly_Mean_Daily_Dose", Raw_Column) ~ "Mean Daily Dose (kJ)",
      grepl("TOTAL_YEARLY_DOSE", Raw_Column) ~ "Total Yearly Dose (kJ)",
      grepl("Yearly_Mean_Daily_SED", Raw_Column) ~ "Mean Daily SED",
      grepl("TOTAL_YEARLY_SED", Raw_Column) ~ "Total Yearly SED"
    )
  ) %>%
  filter(!is.na(Metric))

# Define a strict, professional color palette for the 3 shifts
scenario_colors <- c(
  "Scenario A (08:00 - 17:00)" = "#d73027",  # Danger Red
  "Scenario B (06:00 - 10:00)" = "#4575b4",  # Safe Morning Blue
  "Scenario C (16:00 - 18:00)" = "#fee090"   # Evening Yellow/Gold
)


# ---------------------------------------------------------
# STEP 2: BUILD PLOT 1 - REGIONAL YEARLY MEAN DAILY DOSE (kJ)
# ---------------------------------------------------------
print("Generating Plot 1: Daily Dose (kJ)...")

data_dose <- tz_long %>% filter(Metric == "Mean Daily Dose (kJ)")

plot_dose <- ggplot(data_dose, aes(x = Year, y = Value, color = Scenario)) +
  geom_line(linewidth = 0.8, alpha = 0.9) +
  facet_wrap(~ Region, ncol = 4) +  
  scale_color_manual(values = scenario_colors) +
  scale_x_continuous(breaks = seq(1940, 2020, by = 20)) + 
  labs(
    title = "Tanzania: Average Daily Physical UV Dose by Region (1940 - 2025)",
    subtitle = "Comparing full-day exposure against morning and evening mitigation shifts.",
    x = "Year",
    y = "Mean Daily Dose (kJ/m²)",
    color = "Working Shift"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    strip.text = element_text(face = "bold", size = 11),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(plot_dose)


# ---------------------------------------------------------
# STEP 3: BUILD PLOT 2 - REGIONAL YEARLY MEAN DAILY SED
# ---------------------------------------------------------
print("Generating Plot 2: Daily Biological Damage (SED)...")

data_sed <- tz_long %>% filter(Metric == "Mean Daily SED")

plot_sed <- ggplot(data_sed, aes(x = Year, y = Value, color = Scenario)) +
  geom_line(linewidth = 0.8, alpha = 0.9) +
  facet_wrap(~ Region, ncol = 4) + 
  scale_color_manual(values = scenario_colors) +
  scale_x_continuous(breaks = seq(1940, 2020, by = 20)) + 
  labs(
    title = "Tanzania: Average Daily Biological UV Damage (SED) by Region",
    subtitle = "Standard Erythemal Dose accumulation over 85 continuous years.",
    x = "Year",
    y = "Mean Daily SED",
    color = "Working Shift"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    strip.text = element_text(face = "bold", size = 11),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(plot_sed)







# ==============================================================================
# SCRIPT: TOPOGRAPHICAL GROUP LINE CHARTS (TANZANIA)
# Plots the 5-Tier AEZ Mapping (High Risk to Low Risk)
# ==============================================================================

# ---------------------------------------------------------
# STEP 1: MELT AND AGGREGATE BY THE NEW CLIMATE ZONES
# ---------------------------------------------------------
tz_group_long <- tz_annual %>%
  select(-contains("ZoneMean"), -contains("ZoneTotal"), -contains("Zone_Representative")) %>%
  pivot_longer(
    cols = starts_with("Scen"),
    names_to = "Raw_Column",
    values_to = "Value"
  ) %>%
  mutate(
    Scenario = case_when(
      grepl("ScenA", Raw_Column) ~ "Scenario A (08:00 - 17:00)",
      grepl("ScenB", Raw_Column) ~ "Scenario B (06:00 - 10:00)",
      grepl("ScenC", Raw_Column) ~ "Scenario C (16:00 - 18:00)"
    ),
    Metric = case_when(
      grepl("Yearly_Mean_Daily_Dose", Raw_Column) ~ "Mean Daily Dose (kJ)",
      grepl("Yearly_Mean_Daily_SED", Raw_Column) ~ "Mean Daily SED"
    )
  ) %>%
  filter(!is.na(Metric)) %>%
  # Average the regional values into their 5-Tier Environmental Zones
  group_by(Climate_Zone, Year, Scenario, Metric) %>%
  summarise(Mean_Value = mean(Value, na.rm = TRUE), .groups = "drop") %>%
  # NEW: Format the legend text to stack the regions under the Zone name
  mutate(Climate_Zone_Detailed = case_when(
    Climate_Zone == "Zone 1: Central Semi-Arid Plateau" ~ "Zone 1: Central Semi-Arid Plateau\n(Dodoma, Shinyanga, Singida, Tabora, Simiyu)",
    Climate_Zone == "Zone 2: Lake Victoria Basin System" ~ "Zone 2: Lake Victoria Basin System\n(Mwanza, Mara, Geita)",
    Climate_Zone == "Zone 3: Coastal Humid Belt" ~ "Zone 3: Coastal Humid Belt\n(Dar es Salaam, Pwani, Tanga, Morogoro)",
    Climate_Zone == "Zone 4: Western Humid/Orographic" ~ "Zone 4: Western Humid/Orographic\n(Kagera, Kigoma, Katavi)",
    Climate_Zone == "Zone 5: Northern Highland System" ~ "Zone 5: Northern Highland System\n(Arusha, Manyara)",
    TRUE ~ Climate_Zone
  ))

# Custom thermal gradient colors matched to the detailed legend strings
group_colors <- c(
  "Zone 1: Central Semi-Arid Plateau\n(Dodoma, Shinyanga, Singida, Tabora, Simiyu)"  = "#d73027",  # Red 
  "Zone 2: Lake Victoria Basin System\n(Mwanza, Mara, Geita)"                        = "#fc8d59",  # Orange 
  "Zone 3: Coastal Humid Belt\n(Dar es Salaam, Pwani, Tanga, Morogoro)"              = "#fee090",  # Yellow 
  "Zone 4: Western Humid/Orographic\n(Kagera, Kigoma, Katavi)"                       = "#91bfdb",  # Light Blue 
  "Zone 5: Northern Highland System\n(Arusha, Manyara)"                              = "#4575b4"   # Dark Blue 
)

# ---------------------------------------------------------
# STEP 2: PLOT 3 - ZONE MEAN DAILY DOSE (kJ)
# ---------------------------------------------------------
print("Generating Plot 3: Daily Dose by Environmental Zone...")

data_dose_scenA <- tz_group_long %>% 
  filter(Scenario == "Scenario A (08:00 - 17:00)", Metric == "Mean Daily Dose (kJ)") %>%
  # Sort the legend by maximum exposure
  mutate(Climate_Zone_Detailed = fct_reorder(Climate_Zone_Detailed, Mean_Value, .fun = max, .desc = TRUE))

plot_dose_group <- ggplot(data_dose_scenA, aes(x = Year, y = Mean_Value, color = Climate_Zone_Detailed)) +
  geom_line(linewidth = 1.2, alpha = 0.9) + 
  scale_color_manual(values = group_colors) +
  scale_x_continuous(breaks = seq(1940, 2020, by = 10)) + 
  labs(
    title = "Tanzania: Average Daily Physical UV Dose by Environmental Zone (1940 - 2025)",
    subtitle = "Scenario A: Full Day Shift (08:00 - 17:00)",
    x = "Year",
    y = "Mean Daily Dose (kJ/m²)",
    color = "Environmental Zone"
  ) +
  theme_classic(base_size = 12) + 
  theme(
    plot.title = element_text(face = "bold", size = 14),
    # Moved legend to the right so the new stacked text has room
    legend.position = "right", 
    legend.title = element_text(face = "bold", size = 11),
    legend.text = element_text(size = 9),
    # Adds vertical spacing between the legend items
    legend.key.height = unit(1.2, "cm") 
  )

print(plot_dose_group)


# ---------------------------------------------------------
# STEP 3: PLOT 4 - ZONE MEAN DAILY SED
# ---------------------------------------------------------
print("Generating Plot 4: Daily SED by Environmental Zone...")

data_sed_scenA <- tz_group_long %>% 
  filter(Scenario == "Scenario A (08:00 - 17:00)", Metric == "Mean Daily SED") %>%
  # Sort the legend by maximum exposure
  mutate(Climate_Zone_Detailed = fct_reorder(Climate_Zone_Detailed, Mean_Value, .fun = max, .desc = TRUE))

plot_sed_group <- ggplot(data_sed_scenA, aes(x = Year, y = Mean_Value, color = Climate_Zone_Detailed)) +
  geom_line(linewidth = 1.2, alpha = 0.9) +
  scale_color_manual(values = group_colors) +
  scale_x_continuous(breaks = seq(1940, 2020, by = 10)) + 
  labs(
    title = "Tanzania: Average Daily Biological UV Damage by Environmental Zone",
    subtitle = "Scenario A: Full Day Shift (08:00 - 17:00)",
    x = "Year",
    y = "Mean Daily SED",
    color = "Environmental Zone"
  ) +
  theme_classic(base_size = 12) + 
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 11),
    legend.text = element_text(size = 9),
    legend.key.height = unit(1.2, "cm")
  )

print(plot_sed_group)

