# data viz of Scottish Index of Multiple Deprivation - Geographic Access to Services Indicators
# open source data from https://statistics.gov.scot/data/scottish-index-of-multiple-deprivation---geographic-access-to-services-indicators

# 1 LIBRARIES----

library(dplyr)
library(readr)
library(ggplot2)
library(rgdal)
library(broom)
#library(tidyverse)
library(showtext)
library(ggtext)
library(patchwork)

# 2 LOAD DATA
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

# try to associate postcodes with regions (health boards?)
# 2023-2 Scottish Postcode Directory Files has links between post codes and numerical values of health boards
# also breaks down postcodes by PostcodeDistrict and PostcodeSector
# https://www.nrscotland.gov.uk/statistics-and-data/geography/our-products/scottish-postcode-directory/2023-2
# download 'LargeUser.csv' dataset
# import this file

hb <- read_csv('./00_raw_data/SmallUser.csv') %>% 
  select(1:3, 18)

table(hb$HealthBoardArea1995Code)

# merge the hb data with the tt_data_po_lat_long dataset based on common Postcodes variable

tt_data_po_lat_long_hb <- tt_data_po_lat_long %>%
  group_by(Postcode) %>% dplyr::mutate(id = row_number()) %>% 
  left_join(hb %>% group_by(Postcode) %>% dplyr::mutate(id = row_number())) %>% 
  select(-id) %>%
  ungroup()

# https://www.opendata.nhs.scot/mn_MN/dataset/geography-codes-and-labels/resource/d1fd7380-ffd9-4854-ab2c-574c266085df?view_id=cbbe9fef-9766-4591-b55a-c34b666d3195

tt_data_po_lat_long_hb_cleaned <- tt_data_po_lat_long_hb %>% 
  na.omit() %>% 
  mutate(Area = case_when(HealthBoardArea1995Code == '01' ~ 'Highland',
                          HealthBoardArea1995Code == '02' ~ 'Grampian',
                          HealthBoardArea1995Code == '03' ~ 'Tayside',
                          HealthBoardArea1995Code == '04' ~ 'Fife',
                          HealthBoardArea1995Code == '05' ~ 'Lothian',
                          HealthBoardArea1995Code == '06' ~ 'Borders',
                          HealthBoardArea1995Code == '07' ~ 'Forth Valley',
                          HealthBoardArea1995Code == '08' ~ 'Argyll and Clyde',
                          HealthBoardArea1995Code == '09' ~ 'Greater Glasgow',
                          HealthBoardArea1995Code == '10' ~ 'Lanarkshire',
                          HealthBoardArea1995Code == '11' ~ 'Ayrshire and Arran',
                          HealthBoardArea1995Code == '12' ~ 'Dumfries and Galloway',
                          HealthBoardArea1995Code == '13' ~ 'Orkney',
                          HealthBoardArea1995Code == '14' ~ 'Shetland',
                          HealthBoardArea1995Code == '15' ~ 'Western Isles',
                          TRUE ~ 'NA')) %>% 
  select(-c(3:6)) %>% 
  rename(rural_urban = 'Rural/urban classification') %>% 
  mutate_at(c('Latitude', 'Longitude'), as.numeric)

glimpse(tt_data_po_lat_long_hb_cleaned)

# summarise to Postcode district

pc_district <- tt_data_po_lat_long_hb_cleaned %>%
  group_by(PostcodeDistrict) %>% 
  mutate(latitude_mean = mean(Latitude), longitude_mean = mean(Longitude)) %>% 
  ungroup() %>% 
  select(1, 3, 5, 11, 12, 15:17)

colnames(pc_district)
glimpse(pc_district)
  
pc_district_summary <- pc_district %>%
  group_by(PostcodeDistrict, Area, travel_type, rural_urban, latitude_mean, longitude_mean) %>% 
  summarise(district_mean = mean(Value)) %>% 
  ungroup() %>% 
  mutate_if(is.character, factor)

pc_district_summary_plot <- pc_district_summary %>%
  rename(name = Area) %>% 
  #slice(1:100) %>% 
  group_by(PostcodeDistrict) %>% 
  #filter(FeatureCode %in% c('S01012716', 'S01009070')) %>% 
  #mutate(x=1, y=1) %>% 
  ggplot() +
  #geom_point(shape="\u2620", size = 10, family = "Arial Unicode MS")
  geom_point(aes(x = longitude_mean, y = latitude_mean, size = district_mean, shape= rural_urban, colour = rural_urban), position = 'jitter') +
  scale_shape_manual(values = c(22, 22, 22, 22, 22, 22, 22, 22)) +
  scale_color_manual(values = c("Accessible rural area" = "#8c510a", "Accessible small town" = "#bf812d",
                                "Large urban area" = "#dfc27d", "Other urban area" = "#f6e8c3",
                                "Remote rural area" = "#c7eae5", "Remote small town" = "#80cdc1",
                                "Very remote rural area" = "#35978f", "Very remote small town" = "#01665e")) +
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

 
pc_district_summary_plot_facet_geo <- pc_district_summary %>% 
  group_by(Area, rural_urban) %>% 
  summarise(NHS_area_mean = mean(district_mean)) %>% 
  ggplot(aes(rural_urban, NHS_area_mean, fill = rural_urban)) +
  geom_col() +
  coord_flip() +
  theme_bw() 
  
#+
  facet_geo(~ state)

  

pc_district_summary_plot

pc_district_summary_plot_facet_geo

??facet_warp

pc_district_summary_plot_facet <- pc_district_summary_plot +
  facet_wrap(~PostcodeDistrict)

pc_district_summary_plot_facet

# NHS Boards map
# https://www.marmoe.xyz/2018/09/04/shapefiles-in-r-with-ggplot2-rgdal/
# download Shapefiles from https://www.data.gov.uk/dataset/27d0fe5f-79bb-4116-aec9-a8e565ff756a/nhs-health-boards-scotland
# and move files to the 00_raw_data directory

NHSBoards <- readOGR(dsn = "./00_raw_data/SG_NHS_HealthBoards_2019", 
                     layer = "SG_NHS_HealthBoards_2019")
  
table(NHSBoards_tidy$HBName)
glimpse(NHSBoards_tidy)

NHSBoards$id <- row.names(NHSBoards)
NHSBoards_tidy <- left_join(NHSBoards_tidy, NHSBoards@data)

NHSBoards_tidy <- tidy(NHSBoards)

NHSBoards_tidy_GG <- NHSBoards_tidy %>% 
  filter(HBName == 'Greater Glasgow and Clyde')

ggplot(NHSBoards_tidy, aes(x = long, y = lat, group = group)) +
  geom_polygon(color = "black", size = 0.1, fill = "lightgrey") +
  coord_equal() +
  theme_minimal()

NHSBoards$id <- row.names(NHSBoards)
NHSBoards_tidy <- left_join(NHSBoards_tidy, NHSBoards@data)

board_area_summary <- pc_district %>%
  group_by(Area) %>% 
  summarise(board_mean = mean(Value)) %>% 
  ungroup() %>% 
  mutate_if(is.character, factor) %>% 
  rename(HBName = Area) %>% 
  mutate(HBName = case_when(HBName == 'Greater Glasgow' ~ 'Greater Glasgow and Clyde', TRUE ~ HBName))

NHSBoards_tidy <- left_join(NHSBoards_tidy, board_area_summary)

ggplot(NHSBoards_tidy, aes(x = long, y = lat, group = group, fill = board_mean)) +
  geom_polygon(color = "black", size = 0.1) +
  coord_equal() +
  theme_void() +
  labs(title = "Hospital Density in Scotland (2018)") +
  theme(plot.title = element_text(margin = margin(t = 40, b = -40)))

table(NHSBoards_tidy$HBName)

check_sys_mapshaper()
?summarise
table(pc_district_summary$rural_urban)
table(tt_data_po_lat_long_hb_cleaned$Area)
colnames(hb)
glimpse(pc_district_summary)

  
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



# Set fonts
font_add_google("Luckiest Guy","ramp")
font_add_google("Bebas Neue","beb")
font_add_google("Fira Sans","fira")
font_add_google("Raleway","ral")
font_add_google("Bitter","bit")
font_add_google("Roboto", "roboto")
showtext_auto()

# Plot size
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 20, 
  height = 20, 
  units = "cm", 
  dpi = 300 
)


# Short function to create %!in% operator 
'%!in%' <- function(x,y)!('%in%'(x,y))

# Load data
###########

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

#table(tt_data_hb_po$HealthBoardArea2019Code)

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

#table(tt_data_hb_po$HealthBoardArea2019Code)

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

# french sport club example
# this way
map <- read_sf('./00_raw_data/population-francaise-par-departement-2018/population-francaise-par-departement-2018.shp') 
# or
map2<-sf::read_sf('https://github.com/BjnNowak/lego_map/raw/main/data/france_sport.gpkg')

# map it
ggplot(map2, aes(fill=value))+
  geom_sf()

# nhs boards map prep
# https://spatialdata.gov.scot/geonetwork/srv/eng/catalog.search#/metadata/f12c3826-4b4b-40e6-bf4f-77b9ed01dc14
# or (download Shapefiles from https://www.data.gov.uk/dataset/27d0fe5f-79bb-4116-aec9-a8e565ff756a/nhs-health-boards-scotland)
map_nhs <- sf::read_sf('./00_raw_data/SG_NHS_HealthBoards_2019/SG_NHS_HealthBoards_2019.shp')

# local authority map prep map
# https://data.spatialhub.scot/dataset/local_authority_boundaries-is
map_local_authority <- sf::read_sf('./00_raw_data/Local_Authority_Boundaries_-_Scotland-sh_las_pub_las/pub_las.shp')

# need some values to map - number of post offices per population?

# population estimates
# health board population estimates
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

# local authority population estimates
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

max(map_nhs_pop_po_tt$mean_tt)
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

# Set color palette
pal <- c("#bb3e03","#ee9b00","#e9d8a6","#94d2bd","#0a9396","#005f73")

pal_po <- c("#a23339","#ee9b00","#e9d8a6","#94d2bd","#0a9396","#21435f")

# Set color background
bck <- "#001219"

bck_po <- "#d6d2c4"

# Set theme 
theme_custom <- theme_void()+
  theme(
    plot.margin = margin(1,1,10,1,"pt"),
    plot.background = element_rect(fill=bck,color=NA),
    legend.position = "bottom",
    legend.title = element_text(hjust=0.5,color="white",face="bold"),
    legend.text = element_text(color="white")
  )

# Make choropleth
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
    values=pal,
    label=c("< 8 min","< 9 min","< 10 min","< 11 min","< 12 min", "≥ 12 min")
  )+
  theme_custom

# Make grid
grd_nhs_tt <- st_make_grid(
  clean_nhs_tt, # map name 
  n = c(60,60) # number of cells per longitude/latitude
)%>%
  # convert back to sf object
  st_sf()%>%
  # add a unique id to each cell 
  # (will be useful later to get back centroids data)
  mutate(id=row_number())

# Extract centroids
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

# make the map, combining the choropleth and the grid above
p1 <- ggplot() +
  geom_sf(
    # drop_na() is one way to suppress the cells outside the country
    grd_nhs_tt_clean %>% drop_na(), 
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

p1

p1 + draw_image(
  "https://upload.wikimedia.org/wikipedia/commons/c/c1/Stamp_UK_Penny_Red_pl148.jpg",
  x = 3, y = 59, width = 2, height = 1.5
)

ggsave('./03_plots/nhs_boards_po_plot1.png', dpi = 300, height = 20, width = 15, units = 'cm')

gg_playback(
  name = file.path(tempdir(), "recording", "post_office_lego_gif.gif"),
  first_image_duration = 5,
  last_image_duration = 15,
  frame_duration = .4,
  image_resize = 400,
  last_as_first = TRUE
)


  # guides(
  #   fill = guide_legend(
  #     nrow = 1,
  #     title.position = "top",
  #     label.position = "bottom"
  #   )
  # ) +
  # scale_fill_manual(
  #   values = pal,
  #   label = c("< 8 min","< 9 min","< 10 min","< 11 min","< 12 min", "≥ 12 min")
  # ) +
  # theme_custom

# map_localities <- sf::read_sf('./00_raw_data/Localities2020boundaries/Localities2020_MHW.shp')
# 
# map_councils <- sf::read_sf('./00_raw_data/Community_Council_Boundaries/pub_commcnc.shp')
# 
# map_councils_1995 <- sf::read_sf('./00_raw_data/admin_area_changes_1995_onwards/admin_area_changes_1995_onwards.shp')
# 
# map_councils_2001 <- sf::read_sf('./00_raw_data/Scotland_ca_2001/Scotland_ca_2001.shp')
# 
# map_councils_laulevel2_2011 <- sf::read_sf('./00_raw_data/Scotland_laulevel2_2011_clipped/Scotland_laulevel2_2011_clipped.shp')




# Data : https://www.observatoire-des-territoires.gouv.fr/nombre-de-licencies-sportifs
data <- read_delim('./00_raw_data/population-francaise-par-departement-2018/clubs-data-2019.csv',delim=';')%>%
  group_by(codgeo)%>%
  filter(an==max(an))%>%
  ungroup()%>%
  select(code_depart=codgeo,nb_licsport)

# Keep only metropolitant France
dom<-c("971","972","973","974","975","976")

glimpse(clean)

clean<-map%>%
  st_transform(crs='EPSG:2154')%>%
  dplyr::mutate(
    surf=as.numeric(st_area(map))/1000000)

clean <- clean %>% 
  mutate(dens=population/surf
  )%>%
  filter(code_depart%!in%dom)

# Make grid and centroids
grd<-st_make_grid(clean, n = c(60,60))%>%
  st_sf()%>%
  mutate(id=row_number())


cent<-grd%>%
  st_centroid()%>%
  st_intersection(clean)%>%
  left_join(data)%>%
  mutate(per_lic=nb_licsport/population*10*100)%>%
  mutate(cl_lic=case_when(
    per_lic<18~"1",
    per_lic<20~"2",
    per_lic<22~"3",
    per_lic<24~"4",
    per_lic<26~"5",
    TRUE~"6"
  ))
