# ==============================================================================
# SCRIPT: PLOTS 1 TO 12 - LONGITUDINAL SCENARIOS (TANZANIA)
# Generates comprehensive Regional & Zone charts using a 5-Year Rolling Average.
# Includes 5-Year visual anchor dots and 5-Year X-axis labels starting at 1944.
# ==============================================================================

library(arrow)
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
library(zoo) 

# ==============================================================================
# 0. LOAD THE SAVED DATA DIRECTLY
# ==============================================================================
print("Loading Annual Data for Plotting...")
input_annual_path <- "/Users/jacobninantharrakan/Desktop/R/SKIN_CANCER/IARC/file_out/tanzania/Tanzania_Annual_Worker_Scenarios.parquet"

tz_annual <- read_parquet(input_annual_path)


# ==============================================================================
# PART 1: REGIONAL LONGITUDINAL SCENARIOS (17 FACETED GRIDS)
# ==============================================================================
print("Pivoting and smoothing data for Regional Plots...")

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
  filter(!is.na(Metric)) %>%
  group_by(Region, Scenario, Metric) %>%
  arrange(Year) %>%
  mutate(Value = rollmean(Value, k = 5, fill = NA, align = "right")) %>%
  ungroup()

scenario_colors <- c(
  "Scenario A (08:00 - 17:00)" = "#d73027",  
  "Scenario B (06:00 - 10:00)" = "#4575b4",  
  "Scenario C (16:00 - 18:00)" = "#fee090"   
)

# Helper for dots and axis labels
dot_years <- seq(1944, 2024, by = 5)
axis_breaks <- c(1940, dot_years, 2028)
axis_labels <- c("1940", as.character(dot_years), "2025")

# Shared footnote
footnote <- "Data Source: Modified Copernicus Climate Change Service (C3S) ERA5 Reanalysis Data (1940–2025)."

# Shared regional theme
regional_theme <- theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    plot.caption  = element_text(hjust = 1, size = 9, face = "italic", margin = margin(t = 8)),
    strip.text    = element_text(face = "bold", size = 11),
    legend.position = "bottom",
    legend.title  = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    axis.text.x   = element_text(angle = 45, hjust = 1)
  )

# ---------------------------------------------------------
# PLOT 1 - REGIONAL YEARLY MEAN UVI (ALL SHIFTS)
# ---------------------------------------------------------
print("Generating Plot 1: Mean UVI by Region...")
data_uvi <- tz_long %>% filter(Metric == "Mean UVI")

plot_uvi <- ggplot(data_uvi, aes(x = Year, y = Value, color = Scenario)) +
  geom_line(linewidth = 0.8, alpha = 0.9, na.rm = TRUE) +
  geom_point(data = subset(data_uvi, Year %in% dot_years), size = 1.5, na.rm = TRUE) + 
  facet_wrap(~ Region, ncol = 4) +  
  scale_color_manual(values = scenario_colors) +
  scale_x_continuous(breaks = axis_breaks, labels = axis_labels, limits = c(1940, 2028)) + 
  labs(
    title    = "Tanzania Regions: 86-Year Trend (1940–2025), 5-Year Rolling Average UVI,\nAll Working Shifts (Full Day 08:00–17:00 | Morning 06:00–10:00 | Evening 16:00–18:00 UTC)",
    subtitle = "Comparing full-day intensity against morning and evening mitigation shifts.",
    caption  = footnote,
    x = "Year",
    y = "Mean UVI (5-Year Avg)",
    color = "Working Shift"
  ) +
  regional_theme
print(plot_uvi)

# ---------------------------------------------------------
# PLOT 2 - REGIONAL YEARLY MEAN DAILY DOSE (ALL SHIFTS)
# ---------------------------------------------------------
print("Generating Plot 2: Daily Dose (kJ) by Region...")
data_dose <- tz_long %>% filter(Metric == "Mean Daily Dose (kJ)")

plot_dose <- ggplot(data_dose, aes(x = Year, y = Value, color = Scenario)) +
  geom_line(linewidth = 0.8, alpha = 0.9, na.rm = TRUE) +
  geom_point(data = subset(data_dose, Year %in% dot_years), size = 1.5, na.rm = TRUE) + 
  facet_wrap(~ Region, ncol = 4) +  
  scale_color_manual(values = scenario_colors) +
  scale_x_continuous(breaks = axis_breaks, labels = axis_labels, limits = c(1940, 2028)) + 
  labs(
    title    = "Tanzania Regions: 86-Year Trend (1940–2025), 5-Year Rolling Average Daily Dose,\nAll Working Shifts (Full Day 08:00–17:00 | Morning 06:00–10:00 | Evening 16:00–18:00 UTC)",
    subtitle = "Comparing full-day exposure against morning and evening mitigation shifts.",
    caption  = footnote,
    x = "Year",
    y = "Mean Daily Dose (kJ/m² - 5-Year Avg)",
    color = "Working Shift"
  ) +
  regional_theme
print(plot_dose)

# ---------------------------------------------------------
# PLOT 3 - REGIONAL YEARLY MEAN DAILY SED (ALL SHIFTS)
# ---------------------------------------------------------
print("Generating Plot 3: Daily SED by Region...")
data_sed <- tz_long %>% filter(Metric == "Mean Daily SED")

plot_sed <- ggplot(data_sed, aes(x = Year, y = Value, color = Scenario)) +
  geom_line(linewidth = 0.8, alpha = 0.9, na.rm = TRUE) +
  geom_point(data = subset(data_sed, Year %in% dot_years), size = 1.5, na.rm = TRUE) + 
  facet_wrap(~ Region, ncol = 4) + 
  scale_color_manual(values = scenario_colors) +
  scale_x_continuous(breaks = axis_breaks, labels = axis_labels, limits = c(1940, 2028)) + 
  labs(
    title    = "Tanzania Regions: 86-Year Trend (1940–2025), 5-Year Rolling Average Daily SED,\nAll Working Shifts (Full Day 08:00–17:00 | Morning 06:00–10:00 | Evening 16:00–18:00 UTC)",
    subtitle = "Standard Erythemal Dose accumulation trends over 86 continuous years.",
    caption  = footnote,
    x = "Year",
    y = "Mean Daily SED (5-Year Avg)",
    color = "Working Shift"
  ) +
  regional_theme
print(plot_sed)


# ==============================================================================
# PART 2: ENVIRONMENTAL ZONE PLOTS (5-TIER AEZ CLASSIFICATIONS)
# ==============================================================================
print("Aggregating and smoothing data for Zone Plots...")

tz_zone_base <- tz_annual %>%
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
      grepl("Yearly_Mean_Daily_SED", Raw_Column) ~ "Mean Daily SED"
    )
  ) %>%
  filter(!is.na(Metric)) %>%
  group_by(Climate_Zone, Year, Scenario, Metric) %>%
  summarise(Mean_Value = mean(Value, na.rm = TRUE), .groups = "drop") %>%
  group_by(Climate_Zone, Scenario, Metric) %>%
  arrange(Year) %>%
  mutate(Mean_Value = rollmean(Mean_Value, k = 5, fill = NA, align = "right")) %>%
  ungroup() %>%
  mutate(Climate_Zone_Detailed = case_when(
    Climate_Zone == "Zone 1: Central Semi-Arid Plateau" ~ "Zone 1: Central Semi-Arid Plateau\n(Dodoma, Shinyanga, Singida, Tabora, Simiyu)",
    Climate_Zone == "Zone 2: Lake Victoria Basin System" ~ "Zone 2: Lake Victoria Basin System\n(Mwanza, Mara, Geita)",
    Climate_Zone == "Zone 3: Coastal Humid Belt" ~ "Zone 3: Coastal Humid Belt\n(Dar es Salaam, Pwani, Tanga, Morogoro)",
    Climate_Zone == "Zone 4: Western Humid/Orographic" ~ "Zone 4: Western Humid/Orographic\n(Kagera, Kigoma, Katavi)",
    Climate_Zone == "Zone 5: Northern Highland System" ~ "Zone 5: Northern Highland System\n(Arusha, Manyara)",
    TRUE ~ Climate_Zone
  ))

group_colors <- c(
  "Zone 1: Central Semi-Arid Plateau\n(Dodoma, Shinyanga, Singida, Tabora, Simiyu)"  = "#d73027",  
  "Zone 2: Lake Victoria Basin System\n(Mwanza, Mara, Geita)"                        = "#fc8d59",  
  "Zone 3: Coastal Humid Belt\n(Dar es Salaam, Pwani, Tanga, Morogoro)"              = "#fee090",  
  "Zone 4: Western Humid/Orographic\n(Kagera, Kigoma, Katavi)"                       = "#91bfdb",  
  "Zone 5: Northern Highland System\n(Arusha, Manyara)"                              = "#4575b4"   
)

zone_theme_opts <- theme_classic(base_size = 12) + 
  theme(
    plot.title    = element_text(face = "bold", size = 13, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    plot.caption  = element_text(hjust = 1, size = 9, face = "italic", margin = margin(t = 8)),
    legend.position = "right", 
    legend.title  = element_text(face = "bold", size = 11),
    legend.text   = element_text(size = 9),
    legend.key.height = unit(1.2, "cm"),
    axis.text.x   = element_text(angle = 45, hjust = 1)
  )

# ------------------------------------------------------------------------------
# SCENARIO A: FULL DAY SHIFT (08:00 - 17:00)
# ------------------------------------------------------------------------------
print("Generating Scenario A Zone Plots (Plots 4, 5, 6)...")

data_p4 <- tz_zone_base %>% filter(Scenario == "Scenario A (08:00 - 17:00)", Metric == "Mean UVI") %>% 
  mutate(Climate_Zone_Detailed = fct_reorder(Climate_Zone_Detailed, Mean_Value, .fun = max, .desc = TRUE, .na_rm = TRUE))
print(ggplot(data_p4, aes(x = Year, y = Mean_Value, color = Climate_Zone_Detailed)) +
        geom_line(linewidth = 1.2, alpha = 0.9, na.rm = TRUE) + 
        geom_point(data = subset(data_p4, Year %in% dot_years), size = 2.5, na.rm = TRUE) + 
        scale_color_manual(values = group_colors) +
        scale_x_continuous(breaks = axis_breaks, labels = axis_labels, limits = c(1940, 2028)) + 
        labs(title    = "Tanzania Regions: 86-Year Trend (1940–2025), 5-Year Rolling Average UVI,\nFull Day Shift (08:00–17:00 UTC)",
             subtitle = "Environmental Zone comparison across the 5-tier AEZ classification.",
             caption  = footnote,
             x = "Year", y = "Mean UVI (5-Year Avg)", color = "Environmental Zone") + 
        zone_theme_opts)

data_p5 <- tz_zone_base %>% filter(Scenario == "Scenario A (08:00 - 17:00)", Metric == "Mean Daily Dose (kJ)") %>% 
  mutate(Climate_Zone_Detailed = fct_reorder(Climate_Zone_Detailed, Mean_Value, .fun = max, .desc = TRUE, .na_rm = TRUE))
print(ggplot(data_p5, aes(x = Year, y = Mean_Value, color = Climate_Zone_Detailed)) +
        geom_line(linewidth = 1.2, alpha = 0.9, na.rm = TRUE) + 
        geom_point(data = subset(data_p5, Year %in% dot_years), size = 2.5, na.rm = TRUE) + 
        scale_color_manual(values = group_colors) +
        scale_x_continuous(breaks = axis_breaks, labels = axis_labels, limits = c(1940, 2028)) + 
        labs(title    = "Tanzania Regions: 86-Year Trend (1940–2025), 5-Year Rolling Average Daily Dose,\nFull Day Shift (08:00–17:00 UTC)",
             subtitle = "Environmental Zone comparison across the 5-tier AEZ classification.",
             caption  = footnote,
             x = "Year", y = "Mean Daily Dose (kJ/m²)", color = "Environmental Zone") + 
        zone_theme_opts)

data_p6 <- tz_zone_base %>% filter(Scenario == "Scenario A (08:00 - 17:00)", Metric == "Mean Daily SED") %>% 
  mutate(Climate_Zone_Detailed = fct_reorder(Climate_Zone_Detailed, Mean_Value, .fun = max, .desc = TRUE, .na_rm = TRUE))
print(ggplot(data_p6, aes(x = Year, y = Mean_Value, color = Climate_Zone_Detailed)) +
        geom_line(linewidth = 1.2, alpha = 0.9, na.rm = TRUE) + 
        geom_point(data = subset(data_p6, Year %in% dot_years), size = 2.5, na.rm = TRUE) + 
        scale_color_manual(values = group_colors) +
        scale_x_continuous(breaks = axis_breaks, labels = axis_labels, limits = c(1940, 2028)) + 
        labs(title    = "Tanzania Regions: 86-Year Trend (1940–2025), 5-Year Rolling Average Daily SED,\nFull Day Shift (08:00–17:00 UTC)",
             subtitle = "Environmental Zone comparison across the 5-tier AEZ classification.",
             caption  = footnote,
             x = "Year", y = "Mean Daily SED", color = "Environmental Zone") + 
        zone_theme_opts)


# ------------------------------------------------------------------------------
# SCENARIO B: MORNING SHIFT (06:00 - 10:00)
# ------------------------------------------------------------------------------
print("Generating Scenario B Zone Plots (Plots 7, 8, 9)...")

data_p7 <- tz_zone_base %>% filter(Scenario == "Scenario B (06:00 - 10:00)", Metric == "Mean UVI") %>% 
  mutate(Climate_Zone_Detailed = fct_reorder(Climate_Zone_Detailed, Mean_Value, .fun = max, .desc = TRUE, .na_rm = TRUE))
print(ggplot(data_p7, aes(x = Year, y = Mean_Value, color = Climate_Zone_Detailed)) +
        geom_line(linewidth = 1.2, alpha = 0.9, na.rm = TRUE) + 
        geom_point(data = subset(data_p7, Year %in% dot_years), size = 2.5, na.rm = TRUE) + 
        scale_color_manual(values = group_colors) +
        scale_x_continuous(breaks = axis_breaks, labels = axis_labels, limits = c(1940, 2028)) + 
        labs(title    = "Tanzania Regions: 86-Year Trend (1940–2025), 5-Year Rolling Average UVI,\nMorning Shift (06:00–10:00 UTC)",
             subtitle = "Environmental Zone comparison across the 5-tier AEZ classification.",
             caption  = footnote,
             x = "Year", y = "Mean UVI (5-Year Avg)", color = "Environmental Zone") + 
        zone_theme_opts)

data_p8 <- tz_zone_base %>% filter(Scenario == "Scenario B (06:00 - 10:00)", Metric == "Mean Daily Dose (kJ)") %>% 
  mutate(Climate_Zone_Detailed = fct_reorder(Climate_Zone_Detailed, Mean_Value, .fun = max, .desc = TRUE, .na_rm = TRUE))
print(ggplot(data_p8, aes(x = Year, y = Mean_Value, color = Climate_Zone_Detailed)) +
        geom_line(linewidth = 1.2, alpha = 0.9, na.rm = TRUE) + 
        geom_point(data = subset(data_p8, Year %in% dot_years), size = 2.5, na.rm = TRUE) + 
        scale_color_manual(values = group_colors) +
        scale_x_continuous(breaks = axis_breaks, labels = axis_labels, limits = c(1940, 2028)) + 
        labs(title    = "Tanzania Regions: 86-Year Trend (1940–2025), 5-Year Rolling Average Daily Dose,\nMorning Shift (06:00–10:00 UTC)",
             subtitle = "Environmental Zone comparison across the 5-tier AEZ classification.",
             caption  = footnote,
             x = "Year", y = "Mean Daily Dose (kJ/m²)", color = "Environmental Zone") + 
        zone_theme_opts)

data_p9 <- tz_zone_base %>% filter(Scenario == "Scenario B (06:00 - 10:00)", Metric == "Mean Daily SED") %>% 
  mutate(Climate_Zone_Detailed = fct_reorder(Climate_Zone_Detailed, Mean_Value, .fun = max, .desc = TRUE, .na_rm = TRUE))
print(ggplot(data_p9, aes(x = Year, y = Mean_Value, color = Climate_Zone_Detailed)) +
        geom_line(linewidth = 1.2, alpha = 0.9, na.rm = TRUE) + 
        geom_point(data = subset(data_p9, Year %in% dot_years), size = 2.5, na.rm = TRUE) + 
        scale_color_manual(values = group_colors) +
        scale_x_continuous(breaks = axis_breaks, labels = axis_labels, limits = c(1940, 2028)) + 
        labs(title    = "Tanzania Regions: 86-Year Trend (1940–2025), 5-Year Rolling Average Daily SED,\nMorning Shift (06:00–10:00 UTC)",
             subtitle = "Environmental Zone comparison across the 5-tier AEZ classification.",
             caption  = footnote,
             x = "Year", y = "Mean Daily SED", color = "Environmental Zone") + 
        zone_theme_opts)

# ------------------------------------------------------------------------------
# SCENARIO C: EVENING SHIFT (16:00 - 18:00)
# ------------------------------------------------------------------------------
print("Generating Scenario C Zone Plots (Plots 10, 11, 12)...")

data_p10 <- tz_zone_base %>% filter(Scenario == "Scenario C (16:00 - 18:00)", Metric == "Mean UVI") %>% 
  mutate(Climate_Zone_Detailed = fct_reorder(Climate_Zone_Detailed, Mean_Value, .fun = max, .desc = TRUE, .na_rm = TRUE))
print(ggplot(data_p10, aes(x = Year, y = Mean_Value, color = Climate_Zone_Detailed)) +
        geom_line(linewidth = 1.2, alpha = 0.9, na.rm = TRUE) + 
        geom_point(data = subset(data_p10, Year %in% dot_years), size = 2.5, na.rm = TRUE) + 
        scale_color_manual(values = group_colors) +
        scale_x_continuous(breaks = axis_breaks, labels = axis_labels, limits = c(1940, 2028)) + 
        labs(title    = "Tanzania Regions: 86-Year Trend (1940–2025), 5-Year Rolling Average UVI,\nEvening Shift (16:00–18:00 UTC)",
             subtitle = "Environmental Zone comparison across the 5-tier AEZ classification.",
             caption  = footnote,
             x = "Year", y = "Mean UVI (5-Year Avg)", color = "Environmental Zone") + 
        zone_theme_opts)

data_p11 <- tz_zone_base %>% filter(Scenario == "Scenario C (16:00 - 18:00)", Metric == "Mean Daily Dose (kJ)") %>% 
  mutate(Climate_Zone_Detailed = fct_reorder(Climate_Zone_Detailed, Mean_Value, .fun = max, .desc = TRUE, .na_rm = TRUE))
print(ggplot(data_p11, aes(x = Year, y = Mean_Value, color = Climate_Zone_Detailed)) +
        geom_line(linewidth = 1.2, alpha = 0.9, na.rm = TRUE) + 
        geom_point(data = subset(data_p11, Year %in% dot_years), size = 2.5, na.rm = TRUE) + 
        scale_color_manual(values = group_colors) +
        scale_x_continuous(breaks = axis_breaks, labels = axis_labels, limits = c(1940, 2028)) + 
        labs(title    = "Tanzania Regions: 86-Year Trend (1940–2025), 5-Year Rolling Average Daily Dose,\nEvening Shift (16:00–18:00 UTC)",
             subtitle = "Environmental Zone comparison across the 5-tier AEZ classification.",
             caption  = footnote,
             x = "Year", y = "Mean Daily Dose (kJ/m²)", color = "Environmental Zone") + 
        zone_theme_opts)

data_p12 <- tz_zone_base %>% filter(Scenario == "Scenario C (16:00 - 18:00)", Metric == "Mean Daily SED") %>% 
  mutate(Climate_Zone_Detailed = fct_reorder(Climate_Zone_Detailed, Mean_Value, .fun = max, .desc = TRUE, .na_rm = TRUE))
print(ggplot(data_p12, aes(x = Year, y = Mean_Value, color = Climate_Zone_Detailed)) +
        geom_line(linewidth = 1.2, alpha = 0.9, na.rm = TRUE) + 
        geom_point(data = subset(data_p12, Year %in% dot_years), size = 2.5, na.rm = TRUE) + 
        scale_color_manual(values = group_colors) +
        scale_x_continuous(breaks = axis_breaks, labels = axis_labels, limits = c(1940, 2028)) + 
        labs(title    = "Tanzania Regions: 86-Year Trend (1940–2025), 5-Year Rolling Average Daily SED,\nEvening Shift (16:00–18:00 UTC)",
             subtitle = "Environmental Zone comparison across the 5-tier AEZ classification.",
             caption  = footnote,
             x = "Year", y = "Mean Daily SED", color = "Environmental Zone") + 
        zone_theme_opts)

print("ALL PANELS COMPLETE.")

