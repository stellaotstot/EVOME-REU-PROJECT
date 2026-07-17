###########################################################################################
#
#        This script merges licore 6800/600 data with growth analysis "results" data from other r script 
#
###########################################################################################

## Packages
library(dplyr)

## Merge Datasets
licordat <- read.csv("~/Library/CloudStorage/GoogleDrive-sotstot@macalester.edu/.shortcut-targets-by-id/1cu1JV9uk9ZlcLLK4ChavB1ghdxXFLZ0P/EVOME_Heskel/2025_FieldSeasonData/Ecophys_Leaf_Trait_data/AllPhysioDataTPU_initCN.csv") %>%
  filter(!is.na(Willow)) %>%
  mutate(
    Willow = case_when(
      Willow %in% c("salpul", "pulchra") ~ "pulchra",
      Willow %in% c("alaxensis", "salala") ~ "alaxensis",
      TRUE ~ Willow)) %>%
  filter(Willow %in% c("pulchra", "alaxensis")) %>%
  select(Unique_ID.x, Species, Replicate, Site, Vcmax, Jmax, TPU, Vcmax_SE, Jmax_SE, TPU_SE, Willow, Barcode.Number)
      
results_merged <- results %>%
  left_join(licordat,
            by = c("willow_ecology_barcode" = "Barcode.Number"))

##### many duplicates in licor dat, multiple leaves??? how do i know which ones to keep/filter out/ join?

## Visualizations
results_merged %>%
  ggplot(aes(x = Vcmax, 
             y = log(total_dry_mass/total_fresh_length), 
             group = willow_ecology_barcode, 
             color = species)) +
  geom_point() +
  xlab("Vcmax") +
  ylab("log ( mass / length )") +
  facet_wrap(~region) +
  scale_color_manual(values = c("darkgreen", "lightgreen")) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  geom_smooth(method = "lm")

### mass per length by jmax
results_merged %>%
  ggplot(aes(x = Jmax, 
             y = log(total_dry_mass/total_fresh_length), 
             #group = willow_ecology_barcode, #CAUSING PROBLEMS WHY
             color = species)) +
  geom_point() +
  xlab("Jmax") +
  ylab("log ( mass / length )") +
  facet_wrap(~region) +
  scale_color_manual(values = c("darkgreen", "lightgreen")) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  geom_smooth(method = "lm")

### mass per length by jmax with region and species swapped
results_merged %>%
  ggplot(aes(x = Jmax, 
             y = log(total_dry_mass/total_fresh_length), 
             #group = willow_ecology_barcode, #CAUSING PROBLEMS WHY
             color = region)) +
  geom_point() +
  xlab("Jmax") +
  ylab("log ( mass / length )") +
  facet_wrap(~species) +
  scale_color_manual(values = c("lightblue", "lightgreen", "darkblue")) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  geom_smooth(method = "lm")
