#-------------------------#
#### Loading packages ####
#-----------------------#
if(!require(pacman))install.packages("pacman")
pacman::p_load(tidyverse, sf, here, lubridate, terra, tidyterra, ggplot2, colorspace, ggnewscale, patchwork, cowplot, ggrepel, ggspatial, RColorBrewer, viridis) 

#### Parameters ####
WGS = 4326
UTM = 25829

## Define device to read plots out as e.g. tiff/jpeg
device <- "tiff"

## define units for plot size - usually mm
units <- "mm"

## define plot resolution in dpi - 300 usually minimum
dpi <- 300

out_path <- here("outputs","Figures")

#-----------------------#
#### Importing data ####
#---------------------#

land <- st_read('raw_data/spatial_data/map_layers/IOS_data_repo/IOS_land/land/UKHO_satellite_derived_coastline_ios_v1.shp')
st_crs(land) # = WGS 84
land_UTM = st_transform(land, crs = UTM)

six_nm <- st_read('raw_data/spatial_data/map_layers/IOS_data_repo/IOS_marine_boundaries/Marine_boundaries/6_nm_boundary_buffer.shp')
st_crs(six_nm)
six_nm <- st_transform(six_nm, crs = WGS)
st_crs(six_nm)

# Creating Central Place point
CP <- data.frame(lat = "49.892165", lon = "-6.332963")
CP <- st_as_sf(CP, coords = c('lon', 'lat'), crs = WGS)
CP$Name = "Gugh"
CP_UTM <- st_transform(CP, crs = UTM)

extent_WGS = st_read("raw_data/spatial_data/map_layers/extent/extent_WGS.shp")

uk = st_read("raw_data/spatial_data/map_layers/map_data.gpkg", layer = "uk coastline")
cornwall = st_intersection(uk, extent_WGS)

twelve_nm <- st_read('raw_data/spatial_data/map_layers/IOS_data_repo/IOS_marine_boundaries/Marine_boundaries/12_nm_boundary_buffer.shp')
st_crs(twelve_nm)

mcz <- st_read('raw_data/spatial_data/map_layers/IOS_data_repo/IOS_MPAs_shapefiles/MPAs/ios_mcz_wgs84.shp')

sac_marine <- st_read('raw_data/spatial_data/map_layers/IOS_data_repo/IOS_MPAs_shapefiles/MPAs/ios_sac_marine_wgs84.shp')

spa_marine <- st_read('raw_data/spatial_data/map_layers/IOS_data_repo/IOS_MPAs_shapefiles/MPAs/ios_spa_marine_wgs84.shp')

contours <- st_read('raw_data/spatial_data/map_layers/IOS_data_repo/IOS_bathymetry/Bathymetry/contours.shp')
contours <- st_transform(contours, crs = WGS)

# Bathymetry
bathy = rast('raw_data/spatial_data/map_layers/bathy.tif')
st_crs(bathy) 
bathy <- crop(bathy, extent_WGS)
bathy[bathy>0] <- NA

# Tracking data to geometry
tracks <- read.csv('outputs/WorkingDataFrames/LBB_filtered.csv')
tracks <- st_as_sf(tracks, coords = c("X", "Y"), crs = WGS) # creating geom sf from dataframe
# ggplot() + geom_path(data = ESH, aes(x = Lon, y = Lat, col = ID))
tracks_UTM = st_transform(tracks, crs = UTM)

# Load in trips
trips <- read.csv('outputs/WorkingDataFrames/LBB_trips.csv')
trips <- st_as_sf(trips, coords = c("X", "Y"), crs = WGS) # creating geom sf from dataframe
st_crs(trips)
trips_UTM = st_transform(trips, crs = 25829)

# Load in trips with metrics
# Load in trips
trips_withmetrics <- read.csv('outputs/WorkingDataFrames/LBB_withtripmetrics.csv')
trips_withmetrics <- st_as_sf(trips_withmetrics, coords = c("X", "Y"), crs = st_crs(WGS)) # creating geom sf from dataframe
trips_withmetrics$DateTime = parse_date_time(trips_withmetrics$DateTime, tz = "GMT", orders = "Ymd HMS")
trips_withmetrics <- subset(trips_withmetrics, Completetrip == "Complete")
trips_withmetrics_UTM = st_transform(trips_withmetrics, crs = UTM)

# Load in fishing effort data
fishing_effort_short_sum = readRDS('outputs/WorkingDataFrames/fishing_effort_short_sum.rds')
fishing_effort_short_sum_WGS = st_transform(fishing_effort_short_sum, crs = WGS)

fishing_effort_long_sum = readRDS('outputs/WorkingDataFrames/fishing_effort_long_sum.rds')
fishing_effort_long_sum_WGS = st_transform(fishing_effort_long_sum, crs = WGS)

#-----------------#
#### Base map ####
#---------------#

base = ggplot() + 
  geom_spatraster(data = bathy) + 
  scale_fill_continuous_sequential('Teal', 
                                   na.value = 'transparent', 
                                   rev = FALSE) +
  geom_sf(data = six_nm, 
          mapping = aes(shape = '6 nautical miles'), 
          fill = 'transparent', 
          linewidth = 0.2, col = 'red', 
          linetype = "dashed") +
  scale_shape_manual(values = 3, 
                     guide = guide_legend(title = NULL)) +
  geom_sf(data = land,
          fill = "black",
          col = "black",
          linewidth = 0.05) +
  theme(panel.border = element_rect(colour = 'black',
                                    fill = 'transparent'),
        panel.background = element_blank(), 
        axis.ticks = element_line(colour = "black"),
        axis.text = element_text(colour = "black"),
        legend.justification = 'bottom') + 
  guides(fill = guide_colourbar(title = 'Depth (m)',
                                frame.colour = 'black',
                                ticks.colour = 'black'))
base

#-----------------------------------------#
#### Map of all tracks and study area ####
#---------------------------------------#
pal_trips <- brewer.pal(7,"RdPu")

base + geom_path(data = trips_withmetrics,
                 mapping = aes(x = Lon, y = Lat, col = ID), #as.factor so its discrete
                 linewidth = 0.2) +
  scale_colour_manual(values = pal_trips,
                      guide = guide_legend(title = 'Bird ID')) +
  theme(legend.key = element_blank()) +
  coord_sf(expand = F)

#-------------------------------#
#### Map of population KDEs ####
#-----------------------------#
pal_kdes = "RdPu"

# Combining nm to plot
nm = six_nm %>% rename(., Id = OBJECTID) %>% select(Id, geometry) %>% 
  rbind(., twelve_nm) %>% 
  mutate(., Id = c("6 nautical miles", "12 nautical miles"))

pop_kde_map <- ggplot() + 
  geom_spatraster(data = bathy) + 
  scale_fill_continuous_sequential('Teal', 
                                   na.value = 'transparent', 
                                   rev = FALSE,
                                   name = "Depth (m)", guide = guide_colourbar(frame.colour = "black",
                                                                               ticks.colour = "black")) +
  geom_sf(data = uk,
          size = 0.3, fill = "grey60",
          colour = "grey40",
          inherit.aes = FALSE) +
  geom_sf(data = land,
          size = 0.3, fill = "grey60",
          colour = "grey40",
          inherit.aes = FALSE) +
  new_scale_fill() +
  geom_sf(data = skde_pop, aes(geometry = geometry, fill = contlabel, col = contlabel, alpha = contlabel)) +
  scale_fill_discrete_sequential(pal_kdes, na.value = 'transparent', rev = F) + 
  scale_color_discrete_sequential(pal_kdes, na.value = 'transparent', rev = F) +
  scale_alpha_discrete(limits = rev(levels(skde_pop$contlabel)), range = c(0.7, 0.3), guide = NULL) +
  labs(x = "Longitude", y = "Latitude", fill = "% UD", colour = "% UD") + 
  geom_sf(data = nm, 
          mapping = aes(linetype = Id), 
          fill = 'transparent', 
          linewidth = 0.2,
          color = "black") +
  scale_linetype_manual(values = c("6 nautical miles" = "dotted", "12 nautical miles" = "dashed"), 
                        name = "Boundaries") +
  geom_sf(data = CP, col = "white", fill = "black", shape = 23, size = 2, mapping = aes(shape = "Gugh")) +
  annotation_north_arrow(location = 'tl',
                         width = unit(0.5, 'cm'), 
                         height = unit(0.8,'cm')) + 
  scale_x_continuous("Longitude",
                     position = "bottom",
                     breaks = c(-8.5, -7.5, -6.5, -5.5)) + 
  scale_y_continuous("Latitude",
                     position = "left",
                     breaks = c(49, 49.5, 50, 50.5)) +
  theme(panel.border = element_rect(colour = 'black',
                                    fill = 'transparent'),
        panel.background = element_blank(), 
        axis.ticks = element_line(colour = "black"),
        axis.text = element_text(colour = "black"),
        legend.justification = 'bottom',
        legend.spacing = unit(20, "pt"),
        legend.key = element_rect(fill = "white")) +
  coord_sf(xlim = c(-8, -5.6), ylim = c(48.9, 50.1), expand = F)
pop_kde_map 

ggsave(plot = pop_kde_map, filename = "pop_kde_map.tiff",
       device = device,
       path = out_path ,units = units, width = 300, height = 175, dpi = dpi)

#--------------------------------------------------------------------#
#### Map of trips and short term fishing effort ####
#------------------------------------------------------------------#
pal_trips <- mako(7, end = 0.8)

short_fishing_effort_map = ggplot() + geom_sf(data = fishing_effort_short_sum_WGS, mapping = aes(fill = effort_hours), colour = "transparent") +
  scale_fill_continuous_sequential('Reds', 
                                   na.value = 'transparent', 
                                   rev = T, 
                                   guide = guide_colourbar(title = "Fishing effort (hours)", 
                                                           frame.colour = 'black', 
                                                           ticks.colour = 'black',
                                                           title.position = "top",
                                                           #barwidth = unit(4, "cm"),
                                                           title.theme = element_text(size = 10),
                                                           label.theme = element_text(size = 10))) +
  geom_sf(data = land, fill = "grey60", col = "grey60") +
  geom_path(data = trips_withmetrics,
                     mapping = aes(x = Lon, y = Lat, col = ID), #as.factor so its discrete
                     linewidth = 0.2) +
  scale_colour_manual(values = pal_trips, guide = NULL) +
  geom_sf(data = nm, 
          fill = 'transparent', 
          linewidth = 0.2,
          color = "black",
          linetype = c("6 nautical miles" = "dotted", "12 nautical miles" = "dashed")) +
  scale_linetype(guide = NULL) +
  theme(legend.key = element_blank()) +
  geom_sf(data = CP, col = "white", fill = "black", shape = 23, size = 2) +
  annotation_north_arrow(location = 'tl',
                         width = unit(0.5, 'cm'), 
                         height = unit(0.8,'cm')) + 
  #annotation_scale(height = unit(0.2,'cm'),
                   #location = 'br', 
                   #width_hint = 0.15) +
  scale_x_continuous("Longitude",
                     position = "bottom",
                     breaks = c(-8.5, -7.5, -6.5, -5.5)) + 
  scale_y_continuous("Latitude",
                     position = "left",
                     breaks = c(49, 49.5, 50, 50.5)) +
  theme(panel.border = element_rect(colour = 'black',
                                    fill = 'transparent'),
        panel.background = element_blank(), 
        axis.ticks = element_line(colour = "black"),
        axis.text = element_text(colour = "black", size = 12),
        axis.title = element_text(colour = "black", size = 14),
        legend.position = "right") +
  coord_sf(expand = T)
short_fishing_effort_map 

# Function to extract legend
g_legend <- function(a.gplot) {
  tmp <- ggplotGrob(a.gplot)
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

s_legend <- g_legend(short_fishing_effort_map)

s_no_legend <- short_fishing_effort_map + theme(legend.position = "none")
s_final <- s_no_legend + annotation_custom(grob = legend, xmin = -7.5, xmax = -8.5, ymin = 49, ymax = 49.5)
s_final 

ggsave(plot = short_fishing_effort_map, filename = "short_fishing_effort_map.tiff",
       device = device,
       path = out_path ,units = units, width = 300, height = 175, dpi = dpi)
  
#-------------------------------------------------------------#
#### Map of trips and long term fishing effort in hours ####
#-----------------------------------------------------------#

long_fishing_effort_map = ggplot() + geom_sf(data = fishing_effort_long_sum_WGS, mapping = aes(fill = effort_hours), colour = "transparent") +
  scale_fill_continuous_sequential('Reds', 
                                   na.value = 'transparent', 
                                   rev = T, 
                                   guide = guide_colourbar(title = "Fishing effort (hours)", 
                                                           frame.colour = 'black', 
                                                           ticks.colour = 'black',
                                                           title.position = "top",
                                                           #barwidth = unit(4, "cm"),
                                                           title.theme = element_text(size = 10),
                                                           label.theme = element_text(size = 10),
                                                           order = 3)) +
  geom_sf(data = land, fill = "grey60", col = "grey60") +
  geom_path(data = trips_withmetrics,
            mapping = aes(x = Lon, y = Lat, col = ID), #as.factor so its discrete
            linewidth = 0.2) +
  scale_colour_manual(values = pal_trips,
                      guide = guide_legend(title = 'Bird ID',
                                           title.position = 'top',
                                           order = 1)) +
  theme(legend.key = element_blank()) +
  geom_sf(data = CP, col = "white", fill = "black", shape = 23, size = 2, mapping = aes(shape = "Gugh")) +
  geom_sf(data = nm, 
          mapping = aes(linetype = Id), 
          fill = 'transparent', 
          linewidth = 0.2,
          color = "black") +
  scale_linetype_manual(values = c("6 nautical miles" = "dotted", "12 nautical miles" = "dashed"),
                        guide = guide_legend(title = 'Boundaries', title.position = 'top', title.theme = element_text(size = 10), order = 2)) +
  annotation_north_arrow(location = 'tl',
                         width = unit(0.5, 'cm'), 
                         height = unit(0.8,'cm')) + 
  scale_x_continuous("Longitude",
                     position = "bottom",
                     breaks = c(-8.5, -7.5, -6.5, -5.5)) + 
  scale_y_continuous("Latitude",
                     position = "left",
                     breaks = c(49, 49.5, 50, 50.5)) +
  theme(panel.border = element_rect(colour = 'black',
                                    fill = 'transparent'),
        panel.background = element_blank(), 
        axis.ticks = element_line(colour = "black"),
        axis.text = element_text(colour = "black", size = 12),
        axis.title = element_text(colour = "black", size = 14),
        legend.position = "right") +
  coord_sf(expand = T)
long_fishing_effort_map


ggsave(plot = long_fishing_effort_map, filename = "long_fishing_effort_map.tiff",
       device = device,
       path = out_path ,units = units, width = 300, height = 175, dpi = dpi)

# Panel plot
maps = (short_fishing_effort_map/long_fishing_effort_map) + plot_annotation(tag_levels = 'A')
maps

png(paste0(out_path,"/fishing_effort_maps.png"), width = 400, height = 400, units = units, res = dpi)
print(maps)
dev.off()

#### Inset map ####

# Creating an inset map of Cornwall, highlighting IoS in red
cornwall_inset = ggplot() + 
  geom_sf(data = uk,
          fill = "grey",
          col = "NA") + 
  geom_sf(data = land,
          fill = 'red',
          col = "red") +
  coord_sf(xlim = c(-6.55, -5),
           ylim = c(49.8, 50.6)) +
  theme_void() +
  theme(panel.background = element_rect(fill = 'white'),
        axis.line = element_line(colour = "black"))
cornwall_inset

# Creating an inset map of the UK coastline, highlighting IoS in red
uk_inset = ggplot() + 
  geom_sf(data = uk) +
  geom_sf(data = land,
          fill = 'red',
          col = "red")
uk_inset

# Final
final = ggdraw() +
  draw_plot(pop_kde_map) +
  draw_plot(cornwall_inset,
            x = 0.11,
            width = 0.2,
            y = 0.32,
            height = 0.2)
final


#### Adding labels ####

# Reading geopackage
map_data <- st_layers('raw_data/spatial_data/map_layers/map_data.gpkg')

# Looking at layer names
map_data

# Loading in points of interest layer
poi = st_read("raw_data/spatial_data/map_layers/map_data.gpkg", layer = "POI")

# Subsetting to get just island names
islands = poi[poi$fontheight == "Large",] # Subsetting only island names 

# Separating and creating new columns for lat and lon to convert from geom_sf to geom.
islands <- islands %>% 
  dplyr::mutate(Lon = sf::st_coordinates(.)[,1], Lat = sf::st_coordinates(.)[,2])

map_labelled = base + 
  geom_point(data = islands,
             shape = 20,
             col = 'red',
             size = 1,
             mapping = aes(x = Lon, y = Lat)) +
  geom_label_repel(data = islands,
                   mapping = aes(x = Lon,
                                 y = Lat,
                                 label = islands$distname),
                   box.padding = unit(0.6, "lines"),
                   point.padding = unit(0, "lines"),
                   max.overlaps = 1000,
                   size = 2,
                   direction = "both",
                   label.padding = unit(0.15,
                                        "lines"),
                   force = 2)
map_labelled

# Adding north arrow and scale bar
map_labelled + 
  annotation_north_arrow(location = 'tl',
                         width = unit(0.5, 'cm'), 
                         height = unit(0.8,'cm')) + 
  annotation_scale(height = unit(0.2,'cm'),
                   location = 'br')
