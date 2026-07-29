###########################################################################################
#
#        This script prepares 2025 daily meterological data for REU project 
#
###########################################################################################

library(dplyr)

weather <- read.csv("~/Library/CloudStorage/GoogleDrive-sotstot@macalester.edu/.shortcut-targets-by-id/1cu1JV9uk9ZlcLLK4ChavB1ghdxXFLZ0P/EVOME_Heskel/2025_FieldSeasonData/2025_daily_met_summary.csv")

weather_clean <- weather %>%
  rename(site = Site) %>%
  group_by(site) %>%
  summarise(av_temp = mean(mean_temp),
            av_min_temp = mean(min_temp),
            av_max_temp = mean(max_temp),
            av_temp_range = mean(temp_range),
            av_light = mean(mean_light))

results_weather <- results %>%
  filter(Year.of.Growth == 2025,
    !is.na(species),
    !is.na(total_dry_mass),
    !is.na(total_fresh_length)
  ) %>%
  right_join(weather_clean, by = "site", relationship = "many-to-many")
