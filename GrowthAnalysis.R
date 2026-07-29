###########################################################################################
#
#        This script merges metadata collected on Google AppSheet with Ramet Growth Analysis 
#        datasheets and makes basic plots of growth over time  
#
#    --- Last updated:  2026.07.14 By Ian Shuman <ins2109@columbia.edu>
###########################################################################################

library(googlesheets4)
library(tidyverse)

#Authenticate your Google Account to access Google Sheets via API
gs4_auth(email = "sotstot@macalester.edu")

#fix errors with access (for first run)
#gs4_deauth() 
#drive_deauth()
#unlink("~/Library/Caches/gargle", recursive = T)
#gs4_auth()
#drive_auth()

#Read in the App Google Sheet
lookup <- read_sheet("1POXjAhKfYnI1wrTD6RO3CdWpS4BaSKu03nsqaTjS5Tc", range = "willow_ecology")

#Read in the growth datasheets and merge them with the app sheet to identify willows
growthfiles.dir <- "~/Library/CloudStorage/GoogleDrive-sotstot@macalester.edu/.shortcut-targets-by-id/1cu1JV9uk9ZlcLLK4ChavB1ghdxXFLZ0P/EVOME_Heskel/2026_FieldSeasonData/Shrub Growth Segments/Completed Files" #character string to locate files
results <- NULL #create empty object
for(filename in list.files(growthfiles.dir)){ #for every file in a list of files in this directory do xyz
  file <- read.csv(paste0(growthfiles.dir, "/", filename)) #1. read file and connect it to directory
  file[file == "#DIV/0!"] <- NA #Any years in the template that we don't have segments for should be NAs
  ramet_barcode <- sub("\\.csv$", "", filename) # create another object that is file name minus .csv
  lookup_filtered <- dplyr::filter(lookup, ramet_bundle_barcode == ramet_barcode) #select one row from app sheet table based on ramet number
  combined <- file %>%
    bind_cols( #bind growth file to filtered app sheet table
      lookup_filtered %>%
        select(datetime, willow_ecology_barcode, site, species, replicate, latlong, height_m, canopy_length_m, canopy_width_m, 
               ndvi_1, ndvi_2, ndvi_3, 
               thaw_base_east, thaw_base_north, thaw_base_south, thaw_base_west, 
               thaw_canopy_north, thaw_canopy_east, thaw_canopy_south, thaw_canopy_west, 
               moisture_base_north, moisture_base_east, moisture_base_south, moisture_base_west, 
               moisture_canopy_north, moisture_canopy_east, moisture_canopy_south, moisture_canopy_west #select only these columns 
    ))
  #Average the Environmental Data across N, E, S, W observations (or replicates)
  combined$thaw_base <- mean(c(combined$thaw_base_north, combined$thaw_base_east, combined$thaw_base_south, combined$thaw_base_west))
  combined$thaw_canopy <- mean(c(combined$thaw_canopy_north, combined$thaw_canopy_east, combined$thaw_canopy_south, combined$thaw_canopy_west))
  combined$moisture_base <- mean(c(combined$moisture_base_north, combined$moisture_base_east, combined$moisture_base_south, combined$moisture_base_west))
  combined$moisture_canopy <- mean(c(combined$moisture_canopy_north, combined$moisture_canopy_east, combined$moisture_canopy_south, combined$moisture_canopy_west))
  combined$ndvi <- mean(c(combined$ndvi_1, combined$ndvi_2, combined$ndvi_3))
  combined_final <- combined %>% select(datetime, willow_ecology_barcode, site, species, replicate, latlong, height_m, canopy_length_m, canopy_width_m, thaw_base, thaw_canopy, moisture_base, moisture_canopy, ndvi, Year.of.Growth, segment_count, total_fresh_length, mean_fresh_diameter, mean_fresh_length, total_dry_mass, mean_dry_mass)
  results <- rbind(results, combined_final) #adds singular willow to results objects 
}

#Create SoBro, CeBro, and NoBro regions for comparison
results <- results %>%
  mutate(
    region = case_when(
      site %in% c("FISH", "DOUG", "GRAY", "CLAR", "NUGG", "CHAN") ~ "SoBro",
      site %in% c("GALB", "HERS", "TOOL", "UOKS", "LOKS", "RUDY") ~ "CeBro",
      site %in% c("DANC", "HAPP", "MILK", "CHRI", "SAGW") ~ "NoBro",
      TRUE ~ NA_character_
    )
  )

#Order regions and sites by latitude
results <- results %>%
  mutate(
    region = factor(region, levels = c("SoBro", "CeBro", "NoBro")),
    site = factor(site, levels = c("FISH", "DOUG", "GRAY", "CLAR", "NUGG", "CHAN", "GALB", "HERS", "TOOL", "UOKS", "LOKS", "RUDY", "DANC", "HAPP", "MILK", "CHRI", "SAGW"))
  )

#filtering/nas
results$thaw_base[results$thaw_base == 999] <- NA
results$thaw_canopy[results$thaw_canopy == 999] <- NA

results <- results %>%
  mutate(latitude = as.numeric(substr(latlong, 1, 8)))

#Plot differences in growth by species
results %>%
  ggplot()+
  geom_line(aes(x = Year.of.Growth, 
                y = log(total_dry_mass/total_fresh_length), 
                group = willow_ecology_barcode, 
                color = species), 
            linewidth = 1)+
  xlab("Year")+
  ylab("log ( mass / length )")+
  facet_wrap(~region)+
  scale_color_manual(values = c("darkgreen", "lightgreen"))+
  theme_minimal()+
  theme(legend.position = "bottom")

results %>%
  ggplot()+
  geom_line(aes(x = Year.of.Growth, 
                y = as.numeric(mean_fresh_diameter), 
                group = willow_ecology_barcode, 
                color = species), 
            linewidth = 1)+
  xlab("Year")+
  ylab("Segment Diameter (cm)")+
  facet_wrap(~region)+
  scale_color_manual(values = c("darkgreen", "lightgreen"))+
  theme_minimal()+
  theme(legend.position = "bottom")

results %>% #Why does segment length decline over time in SoBro? Is that maybe related to herbivory?
  ggplot()+
  geom_line(aes(x = Year.of.Growth, 
                y = as.numeric(mean_fresh_length), 
                group = willow_ecology_barcode, 
                color = species), 
            linewidth = 1)+
  xlab("Year")+
  ylab("Mean Segment Length (cm)")+
  facet_wrap(~region)+
  scale_color_manual(values = c("darkgreen", "lightgreen"))+
  theme_minimal()+
  theme(legend.position = "bottom")

#Are the observed differences due to stem size or tissue density or both?
#As segments get fatter, salala segements get taller faster than salpul segments
results %>%
  ggplot()+
  geom_point(aes(y = as.numeric(mean_fresh_length), 
                x = as.numeric(mean_fresh_diameter), 
                group = willow_ecology_barcode, 
                color = species), size = 2)+
  ylab("Mean Segment Length (cm)")+
  xlab("Mean Segment Diameter (cm)")+
  facet_wrap(~region)+
  scale_color_manual(values = c("darkgreen", "lightgreen"))+
  theme_minimal()+
  theme(legend.position = "bottom")

#As segments get fatter, salala segments get heavier faster than salpul segments
results %>%
  ggplot()+
  geom_point(aes(x = as.numeric(mean_fresh_diameter), 
                y = as.numeric(mean_dry_mass), 
                group = willow_ecology_barcode, 
                color = species), 
            size = 2)+
  xlab("Mean Segment Diameter")+
  ylab("Mean Segment Mass (g)")+
  facet_wrap(~region)+
  scale_color_manual(values = c("darkgreen", "lightgreen"))+
  theme_minimal()+
  theme(legend.position = "bottom")

#So it seems like salala segments are both larger and denser than salpul segments
#Does that trend vary between regions? 




#Plot differences in growth by region
results %>%
  ggplot()+
  geom_line(aes(x = Year.of.Growth, 
                y = log(total_dry_mass/total_fresh_length), 
                group = willow_ecology_barcode, 
                color = region), 
            linewidth = 1)+
  xlab("Year")+
  ylab("log ( mass / length )")+
  facet_wrap(~species)+
  scale_color_manual(values = c("red", "black"))+
  theme_minimal()+
  theme(legend.position = "bottom")

results %>%
  ggplot()+
  geom_line(aes(x = Year.of.Growth, 
                y = as.numeric(mean_fresh_diameter), 
                group = willow_ecology_barcode, 
                color = region), 
            linewidth = 1)+
  xlab("Year")+
  ylab("Segment Diameter (cm)")+
  facet_wrap(~species)+
  scale_color_manual(values = c("red", "black"))+
  theme_minimal()+
  theme(legend.position = "bottom")

results %>%
  ggplot()+
  geom_line(aes(x = Year.of.Growth, 
                y = as.numeric(mean_fresh_length), 
                group = willow_ecology_barcode, 
                color = region), 
            linewidth = 1)+
  xlab("Year")+
  ylab("Mean Segment Length (cm)")+
  facet_wrap(~species)+
  scale_color_manual(values = c("red", "black", "blue"))+
  theme_minimal()+
  theme(legend.position = "bottom")

### log of mass per length vs year across region and species (with lm fitted lines)
results %>%
  ggplot(aes(x = Year.of.Growth, 
             y = log(total_dry_mass/total_fresh_length), 
             color = region)) +
  geom_point() +
  geom_smooth(method = "lm") +
  xlab("Year") +
  ylab("log ( mass / length )") +
  facet_wrap(~species) +
  scale_color_manual(values = c("red", "black", "blue"))+
  theme_minimal() +
  theme(legend.position = "bottom")
           

#Internal check to compare mass/length with the cross sectional area of segments
results %>%
  ggplot()+
  geom_point(aes(x = (((as.numeric(mean_fresh_diameter))^2)*pi)/4, 
                y = total_dry_mass/total_fresh_length, 
                group = willow_ecology_barcode))+
  geom_smooth(aes(x = (((as.numeric(mean_fresh_diameter))^2)*pi)/4, 
                  y = total_dry_mass/total_fresh_length), method = "lm")+
  xlab("Cross Sectional Area")+
  ylab("mass / length")+
  facet_wrap(~species)+
  theme_minimal()+
  theme(legend.position = "bottom")









