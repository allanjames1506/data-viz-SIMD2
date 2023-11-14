# data viz of Scottish Index of Multiple Deprivation - Geographic Access to Services Indicators
# open source data from https://statistics.gov.scot/data/scottish-index-of-multiple-deprivation---geographic-access-to-services-indicators

# 1 Libraries----

library(dplyr)
library(readr)
library(ggplot2)
# library(rgdal)
# library(broom)
library(showtext)
library(ggtext)
library(ggridges)
library(sf)
library(geofacet)
library(rnaturalearth)
library(patchwork)
library(ggpubr)
library(jpeg)
library(maps)
library(DescTools)

# 1. Set fonts----
font_add_google("Luckiest Guy","ramp")
font_add_google("Bebas Neue","beb")
font_add_google("Fira Sans","fira")
font_add_google("Raleway","ral")
font_add_google("Bitter","bit")
font_add_google("Roboto", "roboto")
font_add_google("Anton", "anton")
font_add_google("Ultra", "ultra")
font_add_google("Abril Fatface", "abril")
font_add_google("Luckiest Guy", "lucky")
font_add_google("Rye", "rye")
showtext_auto()
showtext_opts(dpi = 320)

# 2. Plot size----
gg_record(
  dir = file.path(tempdir(),"recording"),
  device = "png",
  width = 20,
  height = 20,
  units = "cm",
  dpi = 300
)


# 3. Load data----

# Scottish postcode directory
# https://www.nrscotland.gov.uk/statistics-and-data/geography/our-products/scottish-postcode-directory/2023-2
# download 'SmallUser.csv' dataset
# obtain postcodes associated with the 'HealthBoardArea2019Code' and 'DataZone2011Code'variable
# DataZone2011Code variable needed to merge with the travel time 'geographic-access-to-services-indicators' dataset
hb_area_2019 <- read_csv('./00_raw_data/SmallUser.csv') %>% 
  select(1:3, 16, 23) %>% 
  rename(FeatureCode = DataZone2011Code)

# obtain postcodes associated with the local authority 'CouncilArea2019Code' and 'DataZone2011Code'variable
# DataZone2011Code variable needed to merge with the travel time 'geographic-access-to-services-indicators' dataset
la_area_2019 <- read_csv('./00_raw_data/SmallUser.csv') %>% 
  select(1:3, 11, 23) %>% 
  rename(FeatureCode = DataZone2011Code)

# travel time(tt)
# data from https://statistics.gov.scot/data/scottish-index-of-multiple-deprivation---geographic-access-to-services-indicators
tt_data <- read_csv('./00_raw_data/geo_access_services.csv') %>% 
  rename(travel_type = 'Method of Travel')

# Health Board area
# merge the travel time dataset with the health board identity data set 
tt_data_hb <- tt_data %>% 
  group_by(FeatureCode) %>% 
  mutate(id = row_number()) %>% 
  left_join(hb_area_2019 %>% group_by(FeatureCode) %>% mutate(id = row_number())) %>% 
  select(-id) %>% 
  ungroup()

tt_data_hb_po <- tt_data_hb %>% 
  filter(Destination == 'Post Office')

# how many post offices per health board area?
tt_data_hb_po_summary <- tt_data_hb_po %>% 
  group_by(HealthBoardArea2019Code) %>% 
  summarise(no_of_po = n()) %>% 
  rename(HBCode = HealthBoardArea2019Code) %>% 
  na.omit() %>% 
  ungroup()

# mean travel time on public transport to post office per health board
tt_data_hb_po_tt_summary <- tt_data_hb_po %>%
  filter(travel_type == 'Public Transport') %>% 
  group_by(HealthBoardArea2019Code) %>% 
  summarise(mean_tt = mean(Value)) %>% 
  rename(HBCode = HealthBoardArea2019Code) %>% 
  na.omit() %>% 
  ungroup()

# Local Authority area
# merge the travel time dataset with the local authority identity data set 
tt_data_la <- tt_data %>% 
  group_by(FeatureCode) %>% 
  mutate(id = row_number()) %>% 
  left_join(la_area_2019 %>% group_by(FeatureCode) %>% mutate(id = row_number())) %>% 
  select(-id) %>% 
  ungroup()

tt_data_la_po <- tt_data_la %>% 
  filter(Destination == 'Post Office')

# how many post offices per local authority area?
tt_data_la_po_summary <- tt_data_la_po %>% 
  group_by(CouncilArea2019Code) %>% 
  summarise(no_of_po = n()) %>% 
  rename(code = CouncilArea2019Code) %>% 
  na.omit() %>% 
  ungroup()

# mean travel time on public transport to post office per health board
tt_data_la_po_tt_summary <- tt_data_la_po %>%
  filter(travel_type == 'Public Transport') %>% 
  group_by(CouncilArea2019Code) %>% 
  summarise(mean_tt = mean(Value)) %>% 
  rename(code = CouncilArea2019Code) %>% 
  na.omit() %>% 
  ungroup()

# 4. Map prep----
# *4.1 nhs boards----
# nhs boards map prep
# https://spatialdata.gov.scot/geonetwork/srv/eng/catalog.search#/metadata/f12c3826-4b4b-40e6-bf4f-77b9ed01dc14
# or (download Shapefiles from https://www.data.gov.uk/dataset/27d0fe5f-79bb-4116-aec9-a8e565ff756a/nhs-health-boards-scotland)
map_nhs <- sf::read_sf('./00_raw_data/SG_NHS_HealthBoards_2019/SG_NHS_HealthBoards_2019.shp')

# *4.2 local authority council----
# local authority map prep map
# https://data.spatialhub.scot/dataset/local_authority_boundaries-is
map_local_authority <- sf::read_sf('./00_raw_data/Local_Authority_Boundaries_-_Scotland-sh_las_pub_las/pub_las.shp')

# need some values to map - number of post offices per population?

# *4.3 population estimates nhs health boards----

# https://www.opendata.nhs.scot/dataset/population-estimates/resource/27a72cc8-d6d8-430c-8b4f-3109a9ceadb1
# select 2021 population estimates
health_board_pop <- read_csv('./00_raw_data/health_board_population_estimates.csv') %>% 
  filter(Sex == 'All', Year == 2021, HB != 'S92000003') %>%
  select(HB, AllAges) %>% 
  rename(HBCode = HB,
         pop = AllAges)

# merge with number of post offices (tt_data_hb_po_summary) and make new variable post ofices per 10k population
health_board_pop_po <- health_board_pop %>% 
  left_join(tt_data_hb_po_summary, by = 'HBCode') %>% 
  mutate(po_per_10k = no_of_po/pop *10000)

# merge with tt (tt_data_hb_po_tt_summary)
health_board_pop_po_tt <- health_board_pop_po %>% 
  left_join(tt_data_hb_po_tt_summary, by = 'HBCode')

map_nhs_pop_po_tt <- map_nhs %>% 
  left_join(health_board_pop_po_tt, by = 'HBCode')

ggplot(map_nhs_pop_po_tt, aes(fill = mean_tt))+
  geom_sf()

# *4.4 population estimates local authority council areas----
# https://www.opendata.nhs.scot/dataset/population-estimates/resource/09ebfefb-33f4-4f6a-8312-2d14e2b02ace
# select 2021 population estimates
local_authority_pop <- read_csv('./00_raw_data/local_authority_population_estimate.csv') %>% 
  filter(Sex == 'All', Year == 2021, CA != 'S92000003') %>%
  select(CA, AllAges) %>% 
  rename(code = CA,
         pop = AllAges)

# merge with number of post offices (tt_data_la_po_summary) and make new variable post ofices per 10k population
local_authority_pop_po <- local_authority_pop %>% 
  left_join(tt_data_la_po_summary, by = 'code') %>% 
  mutate(po_per_10k = no_of_po/pop *10000)

# merge with tt (tt_data_la_po_tt_summary)
local_authority_pop_po_tt <- local_authority_pop_po %>% 
  left_join(tt_data_la_po_tt_summary, by = 'code')

map_local_authority_pop_po_tt <- map_local_authority %>% 
  left_join(local_authority_pop_po_tt, by = 'code')

ggplot(map_local_authority_pop_po_tt, aes(fill = mean_tt))+
  geom_sf()

# *4.5 mean travel time gradient classes for maps---- 
# max(map_nhs_pop_po_tt$mean_tt)

# transform the gradient into value classes and plot the results in an appropriate way
# Create classes
clean_nhs_tt <- map_nhs_pop_po_tt %>%
  mutate(clss = case_when(
    mean_tt < 8 ~ "1",
    mean_tt < 9 ~ "2",
    mean_tt < 10 ~ "3",
    mean_tt < 11 ~ "4",
    mean_tt < 12 ~ "5",
    TRUE ~ "6"
  ))

# min(map_local_authority_pop_po_tt$mean_tt)
clean_la_tt <- map_local_authority_pop_po_tt %>%
  mutate(clss = case_when(
    mean_tt < 7 ~ "1",
    mean_tt < 8 ~ "2",
    mean_tt < 9 ~ "3",
    mean_tt < 10 ~ "4",
    mean_tt < 11 ~ "5",
    mean_tt < 12 ~ "6",
    TRUE ~ "7"
  ))

# *4.6 Set color palettes----

pal_po <- c("#a23339","#ee9b00","#e9d8a6","#94d2bd","#0a9396","#21435f")

pal_po_la <- c('#fef0d9','#fdd49e','#fdbb84','#fc8d59','#ef6548','#d7301f','#990000')


# *4.7 Set color background----

bck_po <- "#d6d2c4"

# *4.8 Set theme---- 
theme_custom <- theme_void()+
  theme(
    plot.margin = margin(1,1,10,1,"pt"),
    plot.background = element_rect(fill=bck_po,color=NA),
    legend.position = "bottom",
    legend.title = element_text(hjust=0.5,color="white",face="bold"),
    legend.text = element_text(color="white")
  )

# 5. Make chloropeth maps----
# LEGO plots inspired by BjnNowak
# https://r-graph-gallery.com/web-choropleth-map-lego-style.html
# https://github.com/BjnNowak/TidyTuesday/blob/main/SC_Sport.R

# *5.1 initial nhs chloropeth----
ggplot(clean_nhs_tt, aes(fill=clss))+
  geom_sf()+
  labs(fill="Member of a sport association")+
  guides(
    fill=guide_legend(
      nrow=1,
      title.position="top",
      label.position="bottom"
    )
  )+
  scale_fill_manual(
    values=pal_po,
    label=c("< 8 min","< 9 min","< 10 min","< 11 min","< 12 min", "≥ 12 min")
  )+
  theme_custom


# *5.2 Make NHS grid----

grd_nhs_tt <- st_make_grid(
  clean_nhs_tt, # map name 
  n = c(60,60) # number of cells per longitude/latitude
)%>%
  # convert back to sf object
  st_sf()%>%
  # add a unique id to each cell 
  # (will be useful later to get back centroids data)
  mutate(id=row_number())

# *5.3 Extract NHS centroids----
cent_nhs_tt <- grd_nhs_tt %>%
  st_centroid()

# Take a look at the results
ggplot()+
  geom_sf(grd_nhs_tt, mapping = aes(geometry=geometry))+
  geom_sf(cent_nhs_tt, mapping = aes(geometry=geometry), pch=21, size=0.5)+
  theme_void()

# Intersect centroids with basemap
cent_nhs_tt_clean <- cent_nhs_tt %>%
  st_intersection(clean_nhs_tt)

# Make a centroid without geom
# (convert from sf object to tibble)
cent_nhs_tt_no_geom <- cent_nhs_tt_clean %>%
  st_drop_geometry()

# Join with grid thanks to id column
grd_nhs_tt_clean <- grd_nhs_tt %>%
  #filter(id%in%sel)%>%
  left_join(cent_nhs_tt_no_geom)

# *5.4 Make local authority grid----
# local authority
grd_la_tt <- st_make_grid(
  clean_la_tt, # map name 
  n = c(60,60) # number of cells per longitude/latitude
)%>%
  # convert back to sf object
  st_sf()%>%
  # add a unique id to each cell 
  # (will be useful later to get back centroids data)
  mutate(id=row_number())

# *5.5 Extract local authority centroids----
cent_la_tt <- grd_la_tt %>%
  st_centroid()

# Take a look at the results
ggplot()+
  geom_sf(grd_la_tt, mapping = aes(geometry=geometry))+
  geom_sf(cent_la_tt, mapping = aes(geometry=geometry), pch=21, size=0.5)+
  theme_void()

# Intersect centroids with basemap
cent_la_tt_clean <- cent_la_tt %>%
  st_intersection(clean_la_tt)

# Make a centroid without geom
# (convert from sf object to tibble)
cent_la_tt_no_geom <- cent_la_tt_clean %>%
  st_drop_geometry()

# Join with grid thanks to id column
grd_la_tt_clean <- grd_la_tt %>%
  #filter(id%in%sel)%>%
  left_join(cent_la_tt_no_geom)

# 6. Make final LEGO maps----
# make the map, combining the choropleth and the grid above

# *6.1 nhs board area----
p1 <- ggplot() +
  geom_sf(
    # drop_na() is one way to suppress the cells outside the country
    grd_nhs_tt_clean %>% tidyr::drop_na(), 
    mapping = aes(geometry = geometry, fill = clss)
  ) +
  geom_sf(cent_nhs_tt_clean, mapping = aes(geometry = geometry), fill=NA, pch=21, size=0.5) +
  guides(
    color = 'none',
    fill = guide_legend(
      keywidth = 4, keyheight = 1, nrow = 1,
      title.position = "top", label.position = "bottom"
    )
  ) +
  scale_fill_manual(
    values = pal_po,
    label = c("< 8 min","< 9 min","< 10 min","< 11 min","< 12 min", "≥ 12 min")
  ) +
  labs(
    title="How close is your Post Office?",
    fill="<b>Travel time to a Post Office</b><br>average minutes on public transport",
    caption="**Data** SIMD - Geographic Access to Services Indicators **| Plot** Allan James"
  ) +
  theme_void() +
  theme(
    #plot.margin = margin(1, 0, 1, 0, "cm"),
    plot.margin = unit(c(5, 70, 5, 70), "pt"),
    plot.background = element_rect(fill = bck_po, color = NA),
    legend.position = "bottom",
    legend.spacing.x = unit(0, 'cm'),
    legend.title = element_markdown(
      size = 45, family = "ral", hjust = 0.5, lineheight = 0.45,
      color = "#21435f",
      margin = margin(0, 0, -0.5, 0, "cm")
    ),
    legend.text = element_text(
      size = 35, family = "fira" , color = "#21435f",
      margin = margin(-0.5, 0, 0, 0, "cm")
    ),
    plot.title = element_markdown(
      size = 75, family = "roboto", color = "#a23339", hjust = 0.5, face = "bold",
      margin = margin(0.5, 0, 0.5, 0, "cm")
    ),
    plot.caption = element_markdown(
      size = 35, family = "fira", color = "#21435f",hjust = 0.5,
      margin = margin(0.75, 0, 0, 0, "cm")
    )
  )

ggsave('./03_plots/nhs_boards_po_plot1.png', dpi = 300, height = 20, width = 15, units = 'cm')

# gg_playback(
#   name = file.path(tempdir(), "recording", "post_office_lego_gif.gif"),
#   first_image_duration = 5,
#   last_image_duration = 15,
#   frame_duration = .4,
#   image_resize = 400,
#   last_as_first = TRUE
# )

# *6.2 local authority area----
ggplot() +
  geom_sf(
    # drop_na() is one way to suppress the cells outside the country
    grd_la_tt_clean %>% tidyr::drop_na(), 
    mapping = aes(geometry = geometry, fill = clss)
  ) +
  geom_sf(cent_la_tt_clean, mapping = aes(geometry = geometry), fill=NA, pch=21, size=0.5) +
  guides(
    color = 'none',
    fill = guide_legend(
      keywidth = 4, keyheight = 1, nrow = 1,
      title.position = "top", label.position = "bottom"
    )
  ) +
  scale_fill_manual(
    values = pal_po_la,
    label = c("< 7 min", "< 8 min", "< 9 min", "< 10 min", "< 11 min", "< 12 min", "≥ 12 min")
  ) +
  labs(
    title="How close is your Post Office?",
    fill="<b>Travel time to a Post Office</b><br>average minutes on public transport",
    caption="**Data** SIMD - Geographic Access to Services Indicators **| Plot** Allan James"
  ) +
  theme_void() +
  theme(
    #plot.margin = margin(1, 0, 1, 0, "cm"),
    plot.margin = unit(c(5, 70, 5, 70), "pt"),
    plot.background = element_rect(fill = bck_po, color = NA),
    legend.position = "bottom",
    legend.spacing.x = unit(0, 'cm'),
    legend.title = element_markdown(
      size = 45, family = "ral", hjust = 0.5, lineheight = 0.45,
      color = "#21435f",
      margin = margin(0, 0, -0.5, 0, "cm")
    ),
    legend.text = element_text(
      size = 35, family = "fira" , color = "#21435f",
      margin = margin(-0.5, 0, 0, 0, "cm")
    ),
    plot.title = element_markdown(
      size = 75, family = "roboto", color = "#a23339", hjust = 0.5, face = "bold",
      margin = margin(0.5, 0, 0.5, 0, "cm")
    ),
    plot.caption = element_markdown(
      size = 35, family = "fira", color = "#21435f",hjust = 0.5,
      margin = margin(0.75, 0, 0, 0, "cm")
    )
  )

ggsave('./03_plots/la_boards_po_plot1.png', dpi = 300, height = 20, width = 15, units = 'cm')


# 7. NHS Boards facetted plots Rural/Urban----
# travel time(tt)
# data from https://statistics.gov.scot/data/scottish-index-of-multiple-deprivation---geographic-access-to-services-indicators

tt_data <- read_csv('./00_raw_data/geo_access_services.csv') %>%
  rename(travel_type = 'Method of Travel')

# postcode lookup file - sheet 2 from https://www.gov.scot/publications/scottish-index-of-multiple-deprivation-2020v2-postcode-look-up/
postcodes <- read_csv('./00_raw_data/postcodes.csv')

# filter for just post office data
# and
# merge travel time data with postcode data based on common FeatureCodes
# and
# save out the Post codes linked to our FeatureCodes

tt_data_po <- tt_data %>%
  filter(Destination == 'Post Office',
         travel_type == 'Public Transport') %>%
  group_by(FeatureCode) %>% dplyr::mutate(id = row_number()) %>%
  left_join(postcodes %>% group_by(FeatureCode) %>% dplyr::mutate(id = row_number())) %>%
  select(-id) %>%
  ungroup()

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
  select(-id) %>%
  ungroup()

tt_data_po_lat_long_hb_2019 <- tt_data_po_lat_long %>%
  group_by(Postcode) %>% dplyr::mutate(id = row_number()) %>%
  left_join(hb_area_2019 %>% group_by(Postcode) %>% dplyr::mutate(id = row_number())) %>%
  select(-id) %>%
  ungroup() %>% 
  na.omit()

tt_data_po_lat_long_la_2019 <- tt_data_po_lat_long %>%
  group_by(Postcode) %>% dplyr::mutate(id = row_number()) %>%
  left_join(la_area_2019 %>% group_by(Postcode) %>% dplyr::mutate(id = row_number())) %>%
  select(-id) %>%
  ungroup() %>% 
  na.omit()

# get health board 2019 codes (14 health boards)
# https://ckan.publishing.service.gov.uk/dataset/health-boards-april-2019-names-and-codes-in-scotland1/resource/821be305-205d-4ca4-bd37-b62e1eebe56e

tt_data_po_lat_long_hb_2019_cleaned <- tt_data_po_lat_long_hb_2019 %>%
  na.omit() %>%
  mutate(Area = case_when(HealthBoardArea2019Code == 'S08000015' ~ 'Ayrshire and Arran',
                          HealthBoardArea2019Code == 'S08000016' ~ 'Borders',
                          HealthBoardArea2019Code == 'S08000017' ~ 'Dumfries and Galloway',
                          HealthBoardArea2019Code == 'S08000019' ~ 'Forth Valley',
                          HealthBoardArea2019Code == 'S08000020' ~ 'Grampian',
                          HealthBoardArea2019Code == 'S08000022' ~ 'Highland',
                          HealthBoardArea2019Code == 'S08000024' ~ 'Lothian',
                          HealthBoardArea2019Code == 'S08000025' ~ 'Orkney',
                          HealthBoardArea2019Code == 'S08000026' ~ 'Shetland',
                          HealthBoardArea2019Code == 'S08000028' ~ 'Western Isle',
                          HealthBoardArea2019Code == 'S08000029' ~ 'Fife',
                          HealthBoardArea2019Code == 'S08000030' ~ 'Tayside',
                          HealthBoardArea2019Code == 'S08000031' ~ 'Greater Glasgow and Clyde',
                          HealthBoardArea2019Code == 'S08000032' ~ 'Lanarkshire',
                          TRUE ~ 'NA')) %>%
  select(-c(3:6)) %>%
  rename(rural_urban = 'Rural/urban classification') %>%
  mutate(rural_urban = case_when(rural_urban == 'Large urban area' ~ 'Urban',
                                 rural_urban == 'Other urban area' ~ 'Urban',
                                 rural_urban == 'Accessible rural area' ~ 'Rural',
                                 rural_urban == 'Remote rural area' ~ 'Remote rural',
                                 rural_urban == 'Very remote rural area' ~ 'Very remote rural',
                                 rural_urban == 'Accessible small town' ~ 'Small town',
                                 TRUE ~ rural_urban)) %>% 
  mutate_at(c('Latitude', 'Longitude'), as.numeric) %>% 
  mutate(rural_urban = factor(rural_urban, levels = c('Very remote rural', 'Remote rural', 'Rural', 
                                                      'Very remote small town', 'Remote small town', 'Small town',
                                                      'Urban')))

#levels(tt_data_po_lat_long_hb_2019_cleaned$rural_urban)

tt_data_po_lat_long_la_2019_cleaned <- tt_data_po_lat_long_la_2019 %>%
  na.omit() %>%
  mutate(Area = case_when(CouncilArea2019Code == 'S12000005' ~ 'Clackmannanshire',
                          CouncilArea2019Code == 'S12000006' ~ 'Dumfries & Galloway',
                          CouncilArea2019Code == 'S12000008' ~ 'East Ayrshire',
                          CouncilArea2019Code == 'S12000010' ~ 'East Lothian',
                          CouncilArea2019Code == 'S12000011' ~ 'East Renfrewshire',
                          CouncilArea2019Code == 'S12000013' ~ 'Eilean Siar',
                          CouncilArea2019Code == 'S12000014' ~ 'Falkirk',
                          CouncilArea2019Code == 'S12000015' ~ 'Fife',
                          CouncilArea2019Code == 'S12000017' ~ 'Highland',
                          CouncilArea2019Code == 'S12000018' ~ 'Inverclyde',
                          CouncilArea2019Code == 'S12000019' ~ 'Midlothian',
                          CouncilArea2019Code == 'S12000020' ~ 'Moray',
                          CouncilArea2019Code == 'S12000021' ~ 'North Ayrshire',
                          CouncilArea2019Code == 'S12000023' ~ 'Orkney Islands',
                          CouncilArea2019Code == 'S12000024' ~ 'Perth & Kinross',
                          CouncilArea2019Code == 'S12000026' ~ 'Scottish Borders',
                          CouncilArea2019Code == 'S12000027' ~ 'Shetland Islands',
                          CouncilArea2019Code == 'S12000028' ~ 'South Ayrshire',
                          CouncilArea2019Code == 'S12000029' ~ 'South Lanarkshire',
                          CouncilArea2019Code == 'S12000030' ~ 'Stirling',
                          CouncilArea2019Code == 'S12000033' ~ 'Aberdeen City',
                          CouncilArea2019Code == 'S12000034' ~ 'Aberdeenshire',
                          CouncilArea2019Code == 'S12000035' ~ 'Argyll & Bute',
                          CouncilArea2019Code == 'S12000036' ~ 'Edinburgh',
                          CouncilArea2019Code == 'S12000038' ~ 'Renfrewshire',
                          CouncilArea2019Code == 'S12000039' ~ 'West Dunbartonshire',
                          CouncilArea2019Code == 'S12000040' ~ 'West Lothian',
                          CouncilArea2019Code == 'S12000041' ~ 'Angus',
                          CouncilArea2019Code == 'S12000042' ~ 'Dundee City',
                          CouncilArea2019Code == 'S12000044' ~ 'North Lanarkshire',
                          CouncilArea2019Code == 'S12000045' ~ 'East Dunbartonshire',
                          CouncilArea2019Code == 'S12000046' ~ 'Glasgow City',
                          CouncilArea2019Code == 'S12000047' ~ 'Fife',
                          CouncilArea2019Code == 'S12000048' ~ 'Perth & Kinross',
                          CouncilArea2019Code == 'S12000049' ~ 'Glasgow City',
                          CouncilArea2019Code == 'S12000050' ~ 'North Lanarkshire',
                          TRUE ~ 'NA')) %>%
  select(-c(3:6)) %>%
  rename(rural_urban = 'Rural/urban classification') %>%
  mutate(rural_urban = case_when(rural_urban == 'Large urban area' ~ 'Urban',
                                 rural_urban == 'Other urban area' ~ 'Urban',
                                 rural_urban == 'Accessible rural area' ~ 'Rural',
                                 rural_urban == 'Remote rural area' ~ 'Remote rural',
                                 rural_urban == 'Very remote rural area' ~ 'Very remote rural',
                                 rural_urban == 'Accessible small town' ~ 'Small town',
                                 TRUE ~ rural_urban)) %>% 
  mutate_at(c('Latitude', 'Longitude'), as.numeric) %>% 
  mutate(rural_urban = factor(rural_urban, levels = c('Very remote rural', 'Remote rural', 'Rural', 
                                                      'Very remote small town', 'Remote small town', 'Small town',
                                                      'Urban')))

# 8. Ridge plots----
# https://cran.r-project.org/web/packages/ggridges/vignettes/introduction.html
# https://www.youtube.com/watch?v=VgIlwMpUsBQ

ridge_hb_plot <- ggplot(tt_data_po_lat_long_hb_2019_cleaned, aes(y = rural_urban, x = Value,  fill = rural_urban)) +
  geom_density_ridges(alpha = 0.6) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  theme_ridges(grid = FALSE, center_axis_labels = TRUE) +
  theme(
    legend.position = "none") +
  facet_geo(~ Area, grid = "nhs_scot_grid")

ridge_hb_plot

# https://icolorpalette.com/download/palette/534946_color_palette.jpg

p2 <- ggplot(tt_data_po_lat_long_hb_2019_cleaned, aes(y = rural_urban, x = Value)) +
  geom_density_ridges2(aes(fill = rural_urban), scale = 3, alpha = 0.7, colour = "#67606e") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_fill_manual(values = c("#549b16", "#9be65a", "#c1f098", "#806c32", "#bfa65d", "#ddd0aa","#565a5c")) +
  coord_cartesian(clip = "off") +
  theme_ridges(grid = FALSE, center_axis_labels = TRUE, font_size = 12) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = '#c0cdbb', color = NA),
    strip.text = element_text(face = "bold", color = "chartreuse4",
                              hjust = 0, size = 12),
    strip.background = element_rect(fill = "#c0cdbb")) +
  facet_geo(~ Area, grid = "nhs_scot_grid") +
  labs(y= "", x = "minutes"
  )

p2 
ggsave('./03_plots/nhs_boards_facet_ridge_plot2.png', dpi = 300, height = 30, width = 24, units = 'cm')

# 9. rnaturalearth map of scotland----
# https://luisdva.github.io/rstats/mapssf-eng/

scotland <- ne_countries(geounit = "scotland", type = "map_units", returnclass = "sf", scale = "small")
st_centroid(scotland)
# centroid -4.1 lon, 56.6 lat

#scotland

p3 <- ggplot() +
  geom_sf(data=scotland, lwd = 4, colour = '#DCD8EA', fill = '#0439E3') +
  coord_sf(xlim = c(-8, 0), ylim = c(54, 59.5)) +
  #geom_sf(data=pts,aes(shape=sp,color=sp))+
  theme(panel.background = element_rect(fill = "#FE851E"),
        panel.grid = element_line(colour = 'transparent'), 
        line = element_blank(), 
        rect = element_blank()) + 
  coord_sf(crs  = "+proj=laea +x_0=0 +y_0=0 +lon_0 = -35 +lat_0 = 56.6")

p3

theme(panel.background = element_rect(fill = "#8088d0"),
      panel.grid = element_line(size = 0.1))

#p2 + inset_element(p3, left = 0, bottom = 0.6, right = 0.4, top = 1, align_to = 'full')

ggsave('./03_plots/scotmap__facet_ridge_plot2.png', dpi = 300, height = 24, width = 20, units = 'cm')



# 9.1* Irn Bru----

coordinates_ayr <- data.frame(
  place = 'AYR TOON',
  longitude = -4.629,
  latitude = 55.458
)

coordinates_sf_ayr <- coordinates_ayr |>  sf::st_as_sf(coords = c("longitude","latitude"), crs = 4326)
font <- 'anton'

p_bru1 <- ggplot() +
  geom_sf(data=scotland, lwd = 4, colour = '#DCD8EA', fill = '#0439E3') +
  theme(plot.caption = element_text(family = font, hjust = 0.5, size = 6, color = "#0439E3", margin = margin(t = -25)),
        panel.background = element_rect(fill = "#FE851E"),
        panel.grid = element_line(colour = 'transparent'), 
        line = element_blank(), 
        rect = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank()) +
  geom_sf(data = coordinates_sf_ayr, size = 4, pch=21, stroke = 2, fill = 'grey30') +
  #geom_sf_text(data = coordinates_sf_ayr, aes(label = place), nudge_x = 1.1, nudge_y = 0.25, colour = '#FE851E', family = 'anton', size = 5) +
  coord_sf(xlim = c(-7.5, -0.5), ylim = c(54, 59.5)) +
  annotate("text", y = 59.5, x = -4, label = "SCOTLAND", lineheight = 0.75, family = font,  size = 7, color = "#0439E3", vjust = 0.5) +
  annotate("text", y = 59.1, x = -4, label = "AS A F1 GRAND PRIX CIRCUIT", lineheight = 0.75, family = font,  size = 5, color = "#0439E3", vjust = 0.5) +
  annotate("text", y = 55.75, x = -4.4, label = "AYR", lineheight = 0.75, family = font,  size = 5, color = "#FE851E", hjust = 0) +
  annotate("text", y = 55.45, x = -4.25, label = "TOON", lineheight = 0.75, family = font,  size = 5, color = "#FE851E", hjust = 0) +
  annotate("text", y = 55.15, x = -4.75, label = "CENNA", lineheight = 0.75, family = font,  size = 5, color = "#FE851E", hjust = 0) +
  labs(caption = "Design: Allan James | @allanjames1506")
  
p_bru1
#+ coord_sf(crs  = "+proj=laea +x_0=0 +y_0=0 +lon_0 = -35 +lat_0 = 56.6")

# save plot ---------------------------------------------------------------
ggsave('./03_plots/scotmap_irn_bru.png', dpi = 320, height = 12, width = 9, units = 'cm')

# 9.2* Tunnocks----
# Import the image

mallow <- '#f5ebec'
chocolate <- '#7B3F00'
dark_chocolate <- '#352728'

img <- readJPEG('./00_raw_data/tunnocks_t_cake_cropped3.jpg')

coordinates_lewis_hamilton <- data.frame(
  place = c('LEWIS', 'HAMILTON'),
  longitude = c(-6.6616, -4.0323),
  latitude = c(58.2416, 55.7754)
)

coordinates_sf_lewis_hamilton <- coordinates_lewis_hamilton |>  sf::st_as_sf(coords = c("longitude","latitude"), crs = 4326)

annotation1 <- data.frame(
  x = -4,
  y = 59.35,
  label = "Scotland"
)

annotation2 <- data.frame(
  x = -4,
  y = 54.25,
  label = "as a F1 Grand Prix circuit"
)

annotation3 <- data.frame(
  x = -4,
  y = 53.9,
  label = "design: Allan James | @allanjames1506"
)

p_tunn1 <- ggplot() +
  background_image(img) +
  geom_sf(data=scotland, lwd = 4, colour = chocolate, fill = mallow) +
  theme(panel.grid = element_line(colour = 'transparent'), 
        line = element_blank(), 
        rect = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank()) +
  geom_sf(data = coordinates_sf_lewis_hamilton, size = 10, pch=21, stroke = 3, fill = '#EE1A2E', colour = '#FFE404') +
  #geom_sf_text(data = coordinates_sf_lewis_hamilton, aes(label = place), nudge_x = 0.5, nudge_y = 0.5, colour = '#DCD8EA', family = 'anton', size = 10) +
  geom_sf_label(data = coordinates_sf_lewis_hamilton, aes(label = place), nudge_x = 0.3, nudge_y = 0.55, family = 'ultra', colour = '#EE1A2E', fill = '#FFE404', size = 5, label.size = 0.5) +
  coord_sf(xlim = c(-7.5, -0.5), ylim = c(54, 59.5)) +
  geom_label(data = annotation1, aes(x=x, y=y, label=label),
             stat = 'identity', 
             color = "#AB2733", 
             size = 6 , family = "ultra", hjust = 0.5) +
  geom_label(data = annotation2, aes(x=x, y=y, label=label),
             stat = 'identity', 
             color = "#AB2733", 
             size = 4 , family = "ultra", hjust = 0.5) +
  geom_label(data = annotation3, aes(x=x, y=y, label=label),
             stat = 'identity', 
             color = dark_chocolate, 
             size = 1.5 , family = "ultra", hjust = 0.5)

p_tunn1

ggsave('./03_plots/scotmap_tunn_tcake.png', dpi = 320, height = 12, width = 9, units = 'cm')

# 9.3* Tennents----

coordinates_tennents <- data.frame(
  place = c('T','T', 'T', 'T', 'T'),
  longitude = c(-3.7, -2.7196, -2.0076, -3.0689, -5),
  latitude = c(54.65, 56.0584, 57.6934, 58.6373, 58.65)
)

coordinates_turnberry <- data.frame(
  place = 'TURN-BERRY',
  longitude = -4.8350,
  latitude = 55.3113
)


#55.4241
#-4.7469
coordinates_sf_tennents <- coordinates_tennents |>  sf::st_as_sf(coords = c("longitude","latitude"), crs = 4326)
coordinates_sf_turnberry <- coordinates_turnberry |>  sf::st_as_sf(coords = c("longitude","latitude"), crs = 4326)

p_tennents1 <- ggplot() +
  geom_sf(data=scotland, lwd = 4, colour = 'white', fill = '#B82B35') +
  theme(panel.background = element_rect(fill = "#ECD747"),
        panel.grid = element_line(colour = 'transparent'), 
        line = element_blank(), 
        rect = element_blank(),
        #axis.text.x = element_blank(),
        #axis.text.y = element_blank(),
        axis.ticks = element_blank()) +
  geom_sf(data = coordinates_sf_tennents, size = 4, pch=21, stroke = 2, fill = '#ECD747', colour = '#F6A11D') +
  geom_sf(data = coordinates_sf_turnberry, size = 4, pch=21, stroke = 2, fill = '#ECD747', colour = '#F6A11D') +
  geom_sf_text(data = coordinates_sf_tennents, aes(label = place), nudge_x = 0.6, nudge_y = 0.2, colour = 'white', family = 'ultra', size = 10) +
  geom_sf_text(data = coordinates_sf_tennents, aes(label = place), nudge_x = 0.6, nudge_y = 0.2, colour = '#ECD747', family = 'ultra', size = 9) +
  geom_sf_text(data = coordinates_sf_tennents, aes(label = place), nudge_x = 0.6, nudge_y = 0.2, colour = 'black', family = 'ultra', size = 8) +
  geom_sf_text(data = coordinates_sf_tennents, aes(label = place), nudge_x = 0.6, nudge_y = 0.2, colour = '#B82B35', family = 'ultra', size = 7) +
  #geom_sf_text(data = coordinates_sf_turnberry, aes(label = place), nudge_x = 0.6, nudge_y = 0.3, colour = 'black', family = 'ultra', size = 6) +
  geom_sf_label(data = coordinates_sf_turnberry, aes(label = place), nudge_x = 0.6, nudge_y = 0.35, family = 'ultra', colour = 'black', size = 6, label.size = 0.5) +
  coord_sf(xlim = c(-7.5, -0.5), ylim = c(54, 59.5)) 

p_tennents1


# 9.4* Glenfiddich----

# get the bounding box of points ------------------------------------------
# Calculate the range of longitude and latitude

#get the bounding box numbers for lat long
# scotland

# Bounding box:  xmin: -6.149981 ymin: 54.60094 xmax: -1.959281 ymax: 58.635

min_lon_scot <- -6.149981
max_lon_scot <- -1.959281
min_lat_scot <- 54.60094
max_lat_scot <- 58.635

# calculate the grid of points --------------------------------------------
df_points_scot <- expand_grid(x = seq(from = min_lon_scot, to = max_lon_scot, length.out = 10),
                              y = seq(from = min_lat_scot, to = max_lat_scot, length.out = 10))

# get the outline of points of map ----------------------------------------
glimpse(scotland$geometry)

scotland_unlist <- unlist(as_tibble(scotland))[c(TRUE,TRUE)]
scotland_unlist

scotland_unlist_lon <- scotland_unlist[64:87]

scotland_unlist_lat <- scotland_unlist[88:111]

st_centroid(scotland)
# centroid -4.1 lon, 56.6 lat

scotland_unlist_lon_tibble <- as_tibble(scotland_unlist_lon) %>% 
  rename(x = value)

scotland_unlist_lon_tibble$x_num = as.numeric(as.character(scotland_unlist_lon_tibble$x)) 

class(scotland_unlist_lon_tibble$x_num)
 
scotland_unlist_lon_tibble <- scotland_unlist_lon_tibble %>% 
  mutate(x_num_edit = case_when(x_num < -4.1 ~ x_num + 0.3,
                                x_num > -4.1 ~ x_num - 0.3)) 
#%>% 
  dplyr::select(3) %>% 
  rename(x = x_num_edit)

scotland_unlist_lat_tibble <- as_tibble(scotland_unlist_lat) %>% 
  rename(y = value)

scotland_unlist_lat_tibble$y_num = as.numeric(as.character(scotland_unlist_lat_tibble$y)) 

scotland_unlist_lat_tibble <- scotland_unlist_lat_tibble %>% 
  dplyr::select(2) %>% 
  rename(y = y_num)
  
scotland_unlist_lat_lon_tibble <- scotland_unlist_lon_tibble %>% 
  cbind(scotland_unlist_lat_tibble)
  
# calculate the points from the grid within the bounding box line ---------

# seed
set.seed(777)

# kente colors
colors <- c("#00aff0", "#000000", "#fec000", "#00af50", "#ffff00", "#a6a6a6", "#f15be9", "#fafbfc", "#6c3b2b", "#ff0000", "#70309f", "#c0c0c0")

# create data frame for plotting ------------------------------------------
map_df <- data.frame(PtInPoly(df_points_scot, scotland_unlist_lat_lon_tibble)) %>% 
  filter(pip == 1) %>% 
  group_by(y) %>% 
  mutate(id = cur_group_id()) %>%
  mutate(color = sample(colors, 1, replace = TRUE)) %>%
  ungroup()

ggplot() +
  geom_path(data = map_df, aes(x, y, group = id, color = color), linewidth = 5, inherit.aes = FALSE) +
  geom_sf(data=scotland, lwd = 4, colour = '#A02423', fill = 'transparent') +
  scale_color_identity() +
  coord_sf(xlim = c(-7.5, -0.5), ylim = c(54, 59.5)) +
  #annotate("text", y = 11, x = -4.35, label = "G\nh\na\nn\na", lineheight = 0.75, family = font, fontface = "bold", size = 14, color = "#000000", vjust = "top") +
  #annotate("text", y = 7.4, x = -4.6, label = "Kente\ncloth\ncolors", lineheight = 0.9, family = font, size = 6, color = "#000000", vjust = "top", hjust = "left") +
  #scale_x_continuous(limits = c(-6.5, NA)) +
  #coord_map() +
  theme_void() +
  theme(plot.caption = element_text(family = font, hjust = 0.5, size = 7, color = "#000000", margin = margin(t = 25)),
        plot.caption.position = "plot",
        plot.margin = unit(c(0.5, 1, 1, 1), "cm"),
        panel.background = element_rect(color = NA, fill = "#f7f7f5"),
        plot.background = element_rect(color = NA, fill = "#f7f7f5")) +
  labs(caption = "#30DayMapChallenge | Day 8: Africa | Design: Ryan Hart")


annotation1_lando <- data.frame(
  x = -4,
  y = 59.35,
  label = "Land O'Norris"
)

annotation2_lando <- data.frame(
  x = -4,
  y = 54.2,
  label = "design: Allan James | @allanjames1506"
)

lando <- ggplot() +
  geom_sf(data=scotland, lwd = 4, colour = '#FF8000', fill = '#DBFB51') +
  theme(panel.background = element_rect(fill = "#47c7fc"),
        panel.grid = element_line(colour = 'transparent'), 
        line = element_blank(), 
        rect = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank()) +
  coord_sf(xlim = c(-7.5, -0.5), ylim = c(54, 59.5)) +
  annotate("text", y = 57, x = -4.3, label = "LN", lineheight = 0.75, family = 'anton',  size = 14, color = "#47c7fc", hjust = 0.2, alpha = 0.9) +
  annotate("text", y = 56.4, x = -4.3, label = "04", lineheight = 0.75, family = 'anton',  size = 14, color = "#FF8000", hjust = 0.5, alpha = 0.5) +
  geom_label(data = annotation1_lando, aes(x=x, y=y, label=label),
             stat = 'identity', 
             color = "#000000",
             size = 8 , family = "rye", hjust = 0.5) +
  geom_label(data = annotation2_lando, aes(x=x, y=y, label=label),
             stat = 'identity', 
             color = "#000000", 
             size = 2.5 , family = "anton", hjust = 0.5)

lando

ggsave('./03_plots/lando_bday.png', dpi = 320, height = 12, width = 9, units = 'cm')

#10 curratedmess Kente map to learn from ----

# 30DayMapChallenge | November 8, 2023 | Africa

# libraries ---------------------------------------------------------------
library(tidyverse)
library(showtext)
library(maps)
library(sf)
library(DescTools)

# add font ----------------------------------------------------------------
font_add_google(name = "Besley", family = "Besley")
font <- "Besley"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

# get map boundary data ---------------------------------------------------
ghana <- map_data('world')[map_data('world')$region == "Ghana",] 

# get the bounding box of points ------------------------------------------
# Calculate the range of longitude and latitude
min_lon <- min(ghana$long)
max_lon <- max(ghana$long)
min_lat <- min(ghana$lat)
max_lat <- max(ghana$lat)

# calculate the grid of points --------------------------------------------
df_points <- expand_grid(x = seq(from = min_lon, to = max_lon, length.out = 100),
                         y = seq(from = min_lat, to = max_lat, length.out = 100))

# get the outline of points of map ----------------------------------------
ghana_map_line <- ghana %>% 
  dplyr::select(1:2) %>% 
  rename(x = long, y = lat)

# calculate the points from the grid within the bounding box line ---------

# seed
set.seed(777)

# kente colors
colors <- c("#00aff0", "#000000", "#fec000", "#00af50", "#ffff00", "#a6a6a6", "#f15be9", "#fafbfc", "#6c3b2b", "#ff0000", "#70309f", "#c0c0c0")

# create data frame for plotting ------------------------------------------
map_df <- data.frame(PtInPoly(df_points, ghana_map_line)) %>% 
  filter(pip == 1) %>% 
  group_by(y) %>% 
  mutate(id = cur_group_id()) %>%
  mutate(color = sample(colors, 1, replace = TRUE)) %>%
  ungroup()

# create plot -------------------------------------------------------------
ggplot() +
  geom_path(data = map_df, aes(x, y, group = id, color = color), linewidth = 1.3) +
  scale_color_identity() +
  annotate("text", y = 11, x = -4.35, label = "G\nh\na\nn\na", lineheight = 0.75, family = font, fontface = "bold", size = 14, color = "#000000", vjust = "top") +
  annotate("text", y = 7.4, x = -4.6, label = "Kente\ncloth\ncolors", lineheight = 0.9, family = font, size = 6, color = "#000000", vjust = "top", hjust = "left") +
  scale_x_continuous(limits = c(-4.7, NA)) +
  coord_map() +
  theme_void() +
  theme(plot.caption = element_text(family = font, hjust = 0.5, size = 7, color = "#000000", margin = margin(t = 25)),
        plot.caption.position = "plot",
        plot.margin = unit(c(0.5, 1, 1, 1), "cm"),
        panel.background = element_rect(color = NA, fill = "#f7f7f5"),
        plot.background = element_rect(color = NA, fill = "#f7f7f5")) +
  labs(caption = "#30DayMapChallenge | Day 8: Africa | Design: Ryan Hart")

# save plot ---------------------------------------------------------------
ggsave(paste0("ghana_kente_colors_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)

