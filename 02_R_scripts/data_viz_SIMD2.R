# data viz of Scottish Index of Multiple Deprivation - Geographic Access to Services Indicators
# open source data from https://statistics.gov.scot/data/scottish-index-of-multiple-deprivation---geographic-access-to-services-indicators

# 1 LIBRARIES----

library(dplyr)
library(readr)
library(ggplot2)
#library(tidyverse)

# 2 LOAD DATA
# travel time(tt)
# data from https://statistics.gov.scot/data/scottish-index-of-multiple-deprivation---geographic-access-to-services-indicators

tt_data <- read_csv('./00_raw_data/geo_access_services.csv')

# postcode lookup file - sheet 2 from https://www.gov.scot/publications/scottish-index-of-multiple-deprivation-2020v2-postcode-look-up/
postcodes <- read_csv('./00_raw_data/postcodes.csv')

# filter for just post office data
# and
# merge travel time data with postcode data based on common FeatureCodes
# and
# save out the Post codes linked to our FeatureCodes

tt_data_po <- tt_data %>% 
  filter(Destination == 'Post Office') %>%
  group_by(FeatureCode) %>% dplyr::mutate(id = row_number()) %>% 
  left_join(postcodes %>% group_by(FeatureCode) %>% dplyr::mutate(id = row_number())) %>% 
  select(-id) 
 
write.csv(tt_data_po, './01_tidy_data/our_postcodes.csv')

# paste column of postcodes into Batch geocoding web application https://www.doogal.co.uk/BatchGeocoding
# output from web application gives:
# lat, long, easting, northing, urban/rural classification

lat_long <- read_csv('./00_raw_data/locations-2.csv') %>% 
  rename(Postcode = Address)

# merge the lat_long data with the tt_data_po dataset based on common Postcodes variable
tt_data_po_lat_long <- tt_data_po %>% 
  group_by(Postcode) %>% dplyr::mutate(id = row_number()) %>% 
  left_join(lat_long %>% group_by(Postcode) %>% dplyr::mutate(id = row_number())) %>% 
  select(-id)

# try to associate postcodes with regions (health boards?)
# 2023-2 Scottish Postcode Directory Files has links between post codes and numerical values of health boards
# also breaks down postcodes by PostcodeDistrict and PostcodeSector
# https://www.nrscotland.gov.uk/statistics-and-data/geography/our-products/scottish-postcode-directory/2023-2
# download 'LargeUser.csv' dataset
# import this file

hb <- read_csv('./00_raw_data/SmallUser.csv') %>% 
  select(1:3, 18)

# merge the hb data with the tt_data_po_lat_long dataset based on common Postcodes variable

tt_data_po_lat_long_hb <- tt_data_po_lat_long %>%
  group_by(Postcode) %>% dplyr::mutate(id = row_number()) %>% 
  left_join(hb %>% group_by(Postcode) %>% dplyr::mutate(id = row_number())) %>% 
  select(-id)

table(hb$HealthBoardArea1995Code)
colnames(hb)

  
# our_postcodes <- tt_data_po %>%
#   ungroup() %>% 
#   select(10) %>% 
#   write.csv('./01_tidy_data/our_postcodes.csv')
  
min(tt_data_po$Value)

tt_data_po_sliced <- tt_data_po %>% 
  slice(1:100) %>%
  rename(travel_type = 'Method of Travel') %>% 
  #mutate(travel_type = factor(travel_type, levels = c('Car', 'Public Transport'))) %>% 
  mutate_if(is.character, factor)

glimpse(tt_data_po_sliced)
table(tt_data_po_sliced$FeatureName)
colnames(tt_data_po_sliced)

tt_data_po_sliced_plot <- tt_data_po_sliced %>%
  select(1, 2, 7, 9) %>%
  group_by(FeatureCode) %>% 
  #filter(FeatureCode %in% c('S01012716', 'S01009070')) %>% 
  mutate(x=1, y=1) %>% 
  ggplot() +
  #geom_point(shape="\u2620", size = 10, family = "Arial Unicode MS")
  geom_point(aes(x = x, y = y, size = Value, colour = travel_type), alpha = 0.5)+
  scale_color_manual(values = c("Car" = "#c41c22", "Public Transport" = "grey30")) +
  scale_x_continuous(expand=c(0,0)) + 
  scale_y_continuous(expand=c(0,0)) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())

tt_data_po_sliced_plot

tt_data_po_sliced_plot_facet <- tt_data_po_sliced_plot +
  facet_wrap(~FeatureName)

tt_data_po_sliced_plot_facet

ggsave(tt_data_po_sliced_plot, filename = "./03_plots/tt_data_po_sliced_plot1.png", dpi = 300, type = "cairo")  
