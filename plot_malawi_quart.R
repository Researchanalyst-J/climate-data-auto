# ==============================================================================
# SCRIPT: SEASONAL PLOTS (MALAWI)
# Legend order is DYNAMIC per plot: ranked by each group's most recent value.
# Color encodes intensity: RED = highest exposure, BLUE = lowest exposure.
# NO DATA IS EXPORTED OR SAVED TO DISK.
# ==============================================================================

library(arrow)
library(dplyr)
library(tidyr)
library(ggplot2)
library(zoo) 

# ==============================================================================
# 1. LOAD QUARTERLY DATA
# ==============================================================================
print("Loading Quarterly Data...")
input_quarterly_path <- "/Users/jacobninantharrakan/Desktop/R/SKIN_CANCER/IARC/file_out/malawi/Malawi_Quarterly_Worker_Scenarios.parquet"

mw_quarterly <- read_parquet(input_quarterly_path)

# ==============================================================================
# 2. MATHEMATICAL AGGREGATION & FACTOR LABELLING
# ==============================================================================
print("Applying mathematical grouping...")

mw_seasonal <- mw_quarterly %>%
  mutate(Super_Season = case_when(
    Quarter %in% c(1, 4) ~ "Peak Season (Q1 & Q4)",
    Quarter %in% c(2, 3) ~ "Lower Season (Q2 & Q3)"
  )) %>%
  group_by(Climate_Zone, Region, Year, Super_Season) %>%
  summarise(
    Reg_Mean_UVI = mean(ScenA_08to17_Quarterly_Mean_UVI, na.rm = TRUE),
    Reg_Mean_Daily_Dose = mean(ScenA_08to17_Quarterly_Mean_Daily_Dose_kJ, na.rm = TRUE),
    Reg_Mean_Daily_SED = mean(ScenA_08to17_Quarterly_Mean_Daily_SED, na.rm = TRUE),
    Reg_Total_SED = sum(ScenA_08to17_Total_Quarterly_SED, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(Climate_Zone, Year, Super_Season) %>%
  summarise(
    Zone_Mean_UVI = mean(Reg_Mean_UVI, na.rm = TRUE),
    Zone_Mean_Daily_Dose = mean(Reg_Mean_Daily_Dose, na.rm = TRUE),
    Zone_Mean_Daily_SED = mean(Reg_Mean_Daily_SED, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(Climate_Zone, Super_Season) %>%
  arrange(Year) %>%
  mutate(
    Zone_Mean_UVI = rollmean(Zone_Mean_UVI, k = 5, fill = NA, align = "right"),
    Zone_Mean_Daily_Dose = rollmean(Zone_Mean_Daily_Dose, k = 5, fill = NA, align = "right"),
    Zone_Mean_Daily_SED = rollmean(Zone_Mean_Daily_SED, k = 5, fill = NA, align = "right")
  ) %>%
  ungroup() %>%
  mutate(Climate_Zone_Detailed = case_when(
    Climate_Zone == "Group 1: Lowland" ~ "Group 1: Lowland\n(Balaka, Machinga, Mangochi, Nkhotakota, Salima)",
    Climate_Zone == "Group 2: Highland" ~ "Group 2: Highland\n(Blantyre, Lilongwe, Mchinji, Mulanje, Ntchisi, Phalombe, Zomba)",
    TRUE ~ Climate_Zone
  ))

# ==============================================================================
# 3. PLOTTING SETUP
# ==============================================================================

# 3.1 Dynamic color ramp: Highest value gets Red, Lowest gets Blue.
# Since Malawi has 2 zones, this function will simply return c("#d73027", "#4575b4").
heat_to_cold_ramp <- colorRampPalette(c("#d73027", "#4575b4"))

# Ranks a data frame's zones by their most recent value (highest first) and
# attaches a matching red-to-blue color mapping as an attribute, so both
# the legend order and the color follow rank position (highest to lowest).
reorder_zones_by_latest <- function(df, value_col) {
  last_valid_year <- df %>%
    filter(!is.na(.data[[value_col]])) %>%
    summarise(y = max(Year, na.rm = TRUE)) %>%
    pull(y)
  
  ordered_zones <- df %>%
    filter(Year == last_valid_year) %>%
    arrange(desc(.data[[value_col]])) %>%
    pull(Climate_Zone_Detailed)
  
  df <- df %>% mutate(Climate_Zone_Detailed = factor(Climate_Zone_Detailed, levels = ordered_zones))
  
  rank_colors <- heat_to_cold_ramp(length(ordered_zones))
  attr(df, "rank_colors") <- setNames(rank_colors, ordered_zones)
  df
}

dot_years <- seq(1944, 2024, by = 5)
axis_breaks <- c(1940, dot_years, 2028)
axis_labels <- c("1940", as.character(dot_years), "2025")
footnote <- "Data Source: Modified Copernicus Climate Change Service (C3S) ERA5 Reanalysis Data (1940–2025)."

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

data_peak <- mw_seasonal %>% filter(Super_Season == "Peak Season (Q1 & Q4)")
data_lower <- mw_seasonal %>% filter(Super_Season == "Lower Season (Q2 & Q3)")

# ==============================================================================
# 4. GENERATE PAIRED PLOTS (SCENARIO A: 08:00 - 17:00)
# ==============================================================================
print("Generating Plot Pair 1: UVI...")

data_peak_uvi  <- reorder_zones_by_latest(data_peak,  "Zone_Mean_UVI")
data_lower_uvi <- reorder_zones_by_latest(data_lower, "Zone_Mean_UVI")

plot_uvi_peak <- ggplot(data_peak_uvi, aes(x = Year, y = Zone_Mean_UVI, color = Climate_Zone_Detailed)) +
  geom_line(linewidth = 1.2, alpha = 0.9, na.rm = TRUE) + 
  geom_point(data = subset(data_peak_uvi, Year %in% dot_years), size = 2.5, na.rm = TRUE) + 
  scale_color_manual(values = attr(data_peak_uvi, "rank_colors")) +
  scale_x_continuous(breaks = axis_breaks, labels = axis_labels, limits = c(1940, 2028)) + 
  labs(title = "PEAK EXPOSURE SEASON (Q1 & Q4 Averaged)\n86-Year Trend: 5-Year Rolling Average UVI (Full Day Shift)",
       subtitle = "High-risk months (Oct-Mar) showing heightened solar intensity in Malawi.",
       caption = footnote, x = "Year", y = "Mean UVI (5-Year Avg)", color = "Environmental Zone") + zone_theme_opts
print(plot_uvi_peak)

plot_uvi_lower <- ggplot(data_lower_uvi, aes(x = Year, y = Zone_Mean_UVI, color = Climate_Zone_Detailed)) +
  geom_line(linewidth = 1.2, alpha = 0.9, na.rm = TRUE) + 
  geom_point(data = subset(data_lower_uvi, Year %in% dot_years), size = 2.5, na.rm = TRUE) + 
  scale_color_manual(values = attr(data_lower_uvi, "rank_colors")) +
  scale_x_continuous(breaks = axis_breaks, labels = axis_labels, limits = c(1940, 2028)) + 
  labs(title = "LOWER EXPOSURE SEASON (Q2 & Q3 Averaged)\n86-Year Trend: 5-Year Rolling Average UVI (Full Day Shift)",
       subtitle = "Lower-risk months (Apr-Sep) heavily modulated by winter angles.",
       caption = footnote, x = "Year", y = "Mean UVI (5-Year Avg)", color = "Environmental Zone") + zone_theme_opts
print(plot_uvi_lower)

# ------------------------------------------------------------------------------
print("Generating Plot Pair 2: Mean Daily Dose (kJ)...")

data_peak_dose  <- reorder_zones_by_latest(data_peak,  "Zone_Mean_Daily_Dose")
data_lower_dose <- reorder_zones_by_latest(data_lower, "Zone_Mean_Daily_Dose")

plot_dose_peak <- ggplot(data_peak_dose, aes(x = Year, y = Zone_Mean_Daily_Dose, color = Climate_Zone_Detailed)) +
  geom_line(linewidth = 1.2, alpha = 0.9, na.rm = TRUE) + 
  geom_point(data = subset(data_peak_dose, Year %in% dot_years), size = 2.5, na.rm = TRUE) + 
  scale_color_manual(values = attr(data_peak_dose, "rank_colors")) +
  scale_x_continuous(breaks = axis_breaks, labels = axis_labels, limits = c(1940, 2028)) + 
  labs(title = "PEAK EXPOSURE SEASON (Q1 & Q4 Averaged)\n86-Year Trend: Mean Daily Dose (kJ/m²) (Full Day Shift)",
       subtitle = "Typical daily energy burden during high-risk months in Malawi.",
       caption = footnote, x = "Year", y = "Mean Daily Dose (kJ/m²)", color = "Environmental Zone") + zone_theme_opts
print(plot_dose_peak)

plot_dose_lower <- ggplot(data_lower_dose, aes(x = Year, y = Zone_Mean_Daily_Dose, color = Climate_Zone_Detailed)) +
  geom_line(linewidth = 1.2, alpha = 0.9, na.rm = TRUE) + 
  geom_point(data = subset(data_lower_dose, Year %in% dot_years), size = 2.5, na.rm = TRUE) + 
  scale_color_manual(values = attr(data_lower_dose, "rank_colors")) +
  scale_x_continuous(breaks = axis_breaks, labels = axis_labels, limits = c(1940, 2028)) + 
  labs(title = "LOWER EXPOSURE SEASON (Q2 & Q3 Averaged)\n86-Year Trend: Mean Daily Dose (kJ/m²) (Full Day Shift)",
       subtitle = "Typical daily energy burden during cooler/cloudier months.",
       caption = footnote, x = "Year", y = "Mean Daily Dose (kJ/m²)", color = "Environmental Zone") + zone_theme_opts
print(plot_dose_lower)

# ------------------------------------------------------------------------------
print("Generating Plot Pair 3: Mean Daily SED...")

data_peak_sed  <- reorder_zones_by_latest(data_peak,  "Zone_Mean_Daily_SED")
data_lower_sed <- reorder_zones_by_latest(data_lower, "Zone_Mean_Daily_SED")

plot_sed_peak <- ggplot(data_peak_sed, aes(x = Year, y = Zone_Mean_Daily_SED, color = Climate_Zone_Detailed)) +
  geom_line(linewidth = 1.2, alpha = 0.9, na.rm = TRUE) + 
  geom_point(data = subset(data_peak_sed, Year %in% dot_years), size = 2.5, na.rm = TRUE) + 
  scale_color_manual(values = attr(data_peak_sed, "rank_colors")) +
  scale_x_continuous(breaks = axis_breaks, labels = axis_labels, limits = c(1940, 2028)) + 
  labs(title = "PEAK EXPOSURE SEASON (Q1 & Q4 Averaged)\n86-Year Trend: Mean Daily SED (Full Day Shift)",
       subtitle = "Daily Standard Erythemal Dose accumulation during high-risk months in Malawi.",
       caption = footnote, x = "Year", y = "Mean Daily SED", color = "Environmental Zone") + zone_theme_opts
print(plot_sed_peak)

plot_sed_lower <- ggplot(data_lower_sed, aes(x = Year, y = Zone_Mean_Daily_SED, color = Climate_Zone_Detailed)) +
  geom_line(linewidth = 1.2, alpha = 0.9, na.rm = TRUE) + 
  geom_point(data = subset(data_lower_sed, Year %in% dot_years), size = 2.5, na.rm = TRUE) + 
  scale_color_manual(values = attr(data_lower_sed, "rank_colors")) +
  scale_x_continuous(breaks = axis_breaks, labels = axis_labels, limits = c(1940, 2028)) + 
  labs(title = "LOWER EXPOSURE SEASON (Q2 & Q3 Averaged)\n86-Year Trend: Mean Daily SED (Full Day Shift)",
       subtitle = "Daily Standard Erythemal Dose accumulation during cooler/cloudier months.",
       caption = footnote, x = "Year", y = "Mean Daily SED", color = "Environmental Zone") + zone_theme_opts
print(plot_sed_lower)

print("ALL SEASONAL PLOTS GENERATED.")

