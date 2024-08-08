#-------------------------#
#### Loading packages ####
#-----------------------#

if(!require(pacman))install.packages("pacman")
pacman::p_load(tidyverse, sf, here, lubridate, terra, tidyterra, ggplot2, colorspace, ggnewscale, patchwork, cowplot, ggrepel, ggspatial, RColorBrewer, viridis) 

#----------------------------#
#### Defining parameters ####
#--------------------------#

WGS = 4326
UTM = 25829

## Define device to read plots out as e.g. tiff/jpeg
device <- "tiff"

## define units for plot size - usually mm
units <- "mm"

## define plot resolution in dpi - 300 usually minimum
dpi <- 300

out_path <- here("outputs","Figures")

#------------------------------#
#### Importing data layers ####
#----------------------------#

land <- st_read('raw_data/spatial_data/map_layers/IOS_data_repo/IOS_land/land/UKHO_satellite_derived_coastline_ios_v1.shp')
st_crs(land) # = WGS 84
land_UTM = st_transform(land, crs = 25829)

gugh <- st_read('raw_data/spatial_data/map_layers/Gugh/Gugh.shp')

six_nm <- st_read('raw_data/spatial_data/map_layers/IOS_data_repo/IOS_marine_boundaries/Marine_boundaries/6_nm_boundary_buffer.shp')
st_crs(six_nm)
six_nm <- st_transform(six_nm, crs = st_crs(WGS))
st_crs(six_nm)

twelve_nm <- st_read('raw_data/spatial_data/map_layers/IOS_data_repo/IOS_marine_boundaries/Marine_boundaries/12_nm_boundary_buffer.shp')

mcz <- st_read('raw_data/spatial_data/map_layers/IOS_data_repo/IOS_MPAs_shapefiles/MPAs/ios_mcz_wgs84.shp')

sac_marine <- st_read('raw_data/spatial_data/map_layers/IOS_data_repo/IOS_MPAs_shapefiles/MPAs/ios_sac_marine_wgs84.shp')

spa_marine <- st_read('raw_data/spatial_data/map_layers/IOS_data_repo/IOS_MPAs_shapefiles/MPAs/ios_spa_marine_wgs84.shp')

contours <- st_read('raw_data/spatial_data/map_layers/IOS_data_repo/IOS_bathymetry/Bathymetry/contours.shp')
contours <- st_transform(contours, crs = st_crs(land))
st_crs(contours)
contours_clip_12nm <- st_intersection(contours, twelve_nm)

# Creating Central Place point
CP <- data.frame(lat = "49.892165", lon = "-6.332963")
CP <- st_as_sf(CP, coords = c('lon', 'lat'), crs = WGS)
CP_UTM <- st_transform(CP, crs = 25829)

# Tracking data to geometry
tracks <- read.csv('outputs/WorkingDataFrames/LBB_filtered.csv')
tracks <- st_as_sf(tracks, coords = c("X", "Y"), crs = WGS) # creating geom sf from dataframe
# ggplot() + geom_path(data = ESH, aes(x = Lon, y = Lat, col = ID))
tracks_UTM = st_transform(tracks, crs = 25829)

# Load in trips
trips <- read.csv('outputs/WorkingDataFrames/LBB_trips.csv')
trips <- st_as_sf(trips, coords = c("X", "Y"), crs = st_crs(land)) # creating geom sf from dataframe
st_crs(trips)
trips_UTM = st_transform(trips, crs = 25829)

# Load in trips with metrics
# Load in trips
trips_withmetrics <- read.csv('outputs/WorkingDataFrames/LBB_withtripmetrics.csv')
trips_withmetrics <- st_as_sf(trips_withmetrics, coords = c("X", "Y"), crs = WGS) # creating geom sf from dataframe
trips_withmetrics$DateTime = parse_date_time(trips_withmetrics$DateTime, tz = "GMT", orders = "Ymd HMS")
trips_withmetrics_comp <- subset(trips_withmetrics, Completetrip == "Complete")
trips_withmetrics_UTM = st_transform(trips_withmetrics, crs = 25829)

#---------------------------#
#### Creating an extent ####
#-------------------------#

# Extracting minimum long and lat
minlat = min(tracks$Lat) - 0.1
maxlat = max(tracks$Lat) + 0.1
minlon = min(tracks$Lon) - 0.1
maxlon = max(tracks$Lon) + 0.1

# Joining them to make columns
lon = c(minlon, maxlon)
lat = c(minlat, maxlat)

# Creating a data frame to extract lat and lon from
extent_coord_df = data.frame(lon, lat)

# Creating a data frame with lon and lat in order
extent_df = data.frame(cbind(Lon = extent_coord_df$lon[c(1,2,2,1,1)], Lat = extent_coord_df$lat[c(1,1,2,2,1)]))

# Creating a geom sf
extent_WGS = extent_df %>%
  st_as_sf(coords = c("Lon", "Lat"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")

# Creating an extent in UTM
extent_UTM = st_transform(extent_WGS, crs = 25829)

# Plotting to check extent
ggplot() + geom_sf(data = extent_WGS) + geom_sf(data = tracks)
ggplot() + geom_sf(data = extent_WGS) + geom_sf(data = trips)

# Save extent polygon 
st_write(extent_WGS, "raw_data/spatial_data/map_layers/extent/extent_WGS.shp", append = FALSE)

#----------------------------------------#
#### Creating minimum convex polygon ####
#--------------------------------------#

mcp = st_convex_hull(st_union(trips_withmetrics))

mcp_UTM = st_transform(mcp, crs = UTM)

ggplot() + geom_sf(data = mcp)

# Crop bathymetry to mcp
bathy = rast('raw_data/spatial_data/map_layers/bathy.tif')
st_crs(bathy) 
bathy <- crop(bathy, extent_WGS)

#--------------------------------------#
#### Importing fishing effort data ####
#------------------------------------#

# Loading in short term GFW data (30/05/24 to 22/06/24)
fishing_short <- readRDS("raw_data/spatial_data/GFW_data/new/all_fishing_short.Rdata")

# Loading in long term GFW data 
fishing_long <- readRDS("raw_data/spatial_data/GFW_data/new/all_fishing_long.Rdata")

# Turn the nested fishing dataset into something plottable
fishing_short_crop <- fishing_short %>% 
  dplyr::select(-c(regions, boundingBox, event_info)) %>% #remove excess columns
  unnest_wider(col=c(vessel), names_sep = "_") %>% #extract specific columns from nested data
  mutate(year = lubridate::year(start)) %>% #create year column for start of fishing event
  st_as_sf(coords = c('lon', 'lat'), crs= 4326, remove=FALSE) %>% #turn lat-lon (start point?) into sf geometry
  st_crop(extent_WGS) #crop to extent

fishing_long_crop <- fishing_long %>% 
  dplyr::select(-c(regions, boundingBox, event_info)) %>% #remove excess columns
  unnest_wider(col=c(vessel), names_sep = "_") %>% #extract specific columns from nested data
  st_as_sf(coords = c('lon', 'lat'), crs= 4326, remove=FALSE)  %>% #turn lat-lon (start point?) into sf geometry
  st_crop(extent_WGS) #crop to extent

# Transforming cropped fishing events to metric crs UTM zone 29
fishing_short_crop_UTM <- st_transform(fishing_short_crop, crs = 25829)
fishing_long_crop_UTM <- st_transform(fishing_long_crop, crs = 25829)

# Save
write_rds(fishing_long_crop, "raw_data/spatial_data/GFW_data/fishing_long_crop.rds")
write_rds(fishing_short_crop, "raw_data/spatial_data/GFW_data/fishing_short_crop.rds")

# Read in fishing data
fishing_short_crop = readRDS("raw_data/spatial_data/GFW_data/fishing_short_crop.rds")
fishing_long_crop = readRDS("raw_data/spatial_data/GFW_data/fishing_long_crop.rds")

#------------------------#
#### Plotting tracks ####
#----------------------#

# Creating a colour palette for Bird ID
pal <- brewer.pal(7,"RdPu")

map_tracks_pop = ggplot() +
  geom_sf(data = land, col = "black") +# adding new scale to change aesthetics for Bird ID
  geom_path(data = tracks,
            mapping = aes(x = Lon, y = Lat, col = ID),
            linewidth = 0.2) +
  scale_colour_manual(values = pal,
                      guide = guide_legend(title = 'Bird ID')) +
  theme(legend.key = element_blank()) +
  geom_sf(data = CP, col = "blue", fill = "blue", shape = 23, size = 0.5) +
  new_scale("shape") +
  scale_shape_manual(guide = guide_legend(title = NULL)) +
  scale_x_continuous("Longitude",
                     position = "bottom",
                     breaks = c(-8.5, -7.5, -6.5, -5.5)) + 
  scale_y_continuous("Latitude",
                     position = "left",
                     breaks = c(49, 49.5, 50, 50.5))
map_tracks_pop

# Panel for each individual
map_tracks_ind = map_tracks_pop + facet_wrap(~ID, nrow = 4, ncol = 2)
map_tracks_ind

#------------------------------------#
#### Calculating & plotting KDEs ####
#----------------------------------#

# Calculate individual KDEs for plotting
skde_ind <- trips_withmetrics %>%
  select(ID, Lon, Lat) %>%
  group_by(ID) %>% # grouping by ID to get individual
  eks::st_kde() %>%
  eks::st_get_contour(., cont=c(25,50,75,95)) 

# Calculate population KDEs for plotting
skde_pop <- trips_withmetrics %>%
  select(ID, Lon, Lat) %>%
  eks::st_kde() %>%
  eks::st_get_contour(., cont=c(25,50,75,95)) 

# Plotting individual kdes
tracks_ind_kde <- ggplot() + geom_sf(data = land,
                                     size = 0.3, fill = "grey70",
                                     colour = NA,
                                     inherit.aes = FALSE) + 
  geom_sf(data = skde_ind, aes(geometry = geometry, fill = contlabel, colour = contlabel)) +
  scale_fill_discrete_sequential('Purp', 
                                 na.value = 'transparent', 
                                 rev = FALSE) + 
  scale_fill_discrete_sequential('Purp',
                                 na.value = 'transparent',
                                 rev = FALSE) +
  scale_color_discrete_sequential('Purp', 
                                  na.value = 'transparent', 
                                  rev = FALSE) +
  #scale_alpha_discrete(limits = rev(levels(skde_tracks_ind$contlabel)),"% UD", range = c(1, 0.1)) +
  coord_sf(xlim = c(-8, -5.5),
           ylim = c(48.9, 50.1),
           expand=FALSE) + #widest view
  labs(x = "Longitude",
       y = "Latitude",
       fill = "KDE",
       colour = "KDE") +
  theme_bw(base_size = 9) +
  new_scale_color() +
  theme(strip.background =element_rect(fill="grey95")) +
  facet_wrap(~ ID,  nrow = 2, ncol = 4) +
  geom_sf(data = CP, col = "red", fill = "red", shape = 23, size = 0.5)
tracks_ind_kde

#Plotting population kdes
tracks_pop_kde <- ggplot() + geom_sf(data = land,
                                     size = 0.3, fill = "grey70",
                                     colour = NA,
                                     inherit.aes = FALSE) + 
  geom_sf(data = skde_pop, aes(geometry = geometry, fill = contlabel, col = contlabel, alpha = contlabel)) +
  scale_fill_discrete_sequential('Purp',
                                 na.value = 'transparent',
                                 rev = FALSE) +
  scale_color_discrete_sequential('Purp', 
                                  na.value = 'transparent',
                                  rev = FALSE) +
  scale_alpha_discrete(limits = rev(levels(skde_pop$contlabel)),"% UD", range = c(1, 0.1)) +
  coord_sf(xlim = c(-8, -5.5),
           ylim = c(48.9, 50.1),
           expand=FALSE) + #widest view
  labs(x = "Longitude",
       y = "Latitude",
       fill = "KDE",
       colour = "KDE") +
  theme_bw(base_size = 9) +
  new_scale_color() +
  theme(strip.background = element_rect(fill="grey95"))
tracks_pop_kde

saveRDS(skde_pop, "outputs/WorkingDataFrames/skde_pop.rds")

#---------------------------------------------------#
#### Calculating & plotting fishing effort KDEs ####
#-------------------------------------------------#

# Extract yearly sample sizes for long term data
samplesizes <- fishing_long_crop %>% 
  st_drop_geometry() %>%
  group_by(year) %>% 
  tally()

# Calculate KDEs for plotting
skde_fishing_short <- fishing_short_crop %>%
  select(id, year, lon, lat) %>%
  group_by(year) %>%
  eks::st_kde() %>%
  eks::st_get_contour(., cont=c(25, 50, 75, 95)) 

# Calculate KDEs for plotting
skde_fishing_long <- fishing_long_crop %>%
  select(id, year, lon, lat) %>%
  group_by(year) %>%
  eks::st_kde() %>%
  eks::st_get_contour(., cont=c(25, 50, 75, 95)) 

# Plotting KDEs for SHORT TERM fishing
skde_fishing_short_plot <- ggplot() + geom_sf(data = land,
                          size = 0.3, fill = "grey70",
                          colour = NA,
                          inherit.aes = FALSE) + 
  geom_sf(data = skde_fishing_short,
          aes(geometry = geometry, fill = contlabel, colour = contlabel)) +
  scale_fill_discrete_sequential('Reds', 
                                 na.value = 'transparent', 
                                 rev = FALSE) + 
  scale_colour_discrete_sequential('Reds', 
                                 na.value = 'transparent',
                                 rev = FALSE) +
  labs(x = "Longitude",
       y = "Latitude",
       fill = "KDE",
       colour = "KDE") +
  theme_bw(base_size = 9) +
  new_scale_color() +
  theme(strip.background =element_rect(fill="grey95")) +
  geom_path(data = tracks,
            mapping = aes(x = Lon, y = Lat, col = ID),
            linewidth = 0.2) +
  scale_colour_manual(values = pal,
                      guide = guide_legend(title = 'Bird ID')) +
  theme(legend.key = element_blank())
skde_fishing_short_plot

# Plotting KDEs for LONG TERM fishing
skde_fishing_long_plot <- ggplot() + geom_sf(data = land,
                                              size = 0.3, fill = "grey70",
                                              colour = NA,
                                              inherit.aes = FALSE) + 
  geom_sf(data = skde_fishing_long,
          aes(geometry = geometry, fill = contlabel, colour = contlabel)) +
  scale_fill_discrete_sequential('Reds', 
                                 na.value = 'transparent', 
                                 rev = FALSE) + 
  scale_colour_discrete_sequential('Reds', 
                                   na.value = 'transparent',
                                   rev = FALSE) +
  geom_text(data = samplesizes,
            aes(label=paste0('n = ', n),
                x = Inf, y = Inf), #sample sizes
            vjust = 2, hjust = 1.2, size = 2,
            colour = "black",
            inherit.aes = FALSE) +
  labs(x = "Longitude",
       y = "Latitude",
       fill = "KDE",
       colour = "KDE") +
  theme_bw(base_size = 9) +
  new_scale_color() +
  theme(strip.background =element_rect(fill="grey95")) +
  geom_path(data = tracks,
            mapping = aes(x = Lon, y = Lat, col = ID),
            linewidth = 0.2) +
  scale_colour_manual(values = pal,
                      guide = guide_legend(title = 'Bird ID')) +
  theme(legend.key = element_blank()) +
  facet_wrap(~year)
skde_fishing_long_plot

#------------------------#
#### Creating a grid ####
#----------------------#
area = 30 #km

# Setting area of grid
cell_area <- units::as_units(area, "km^2") 

# Calculating size of hexagon from area
grid_spacing <- sqrt(2*cell_area/sqrt(3))

# Creating grids
grid <- extent_UTM %>% 
  st_make_grid(square = F, cellsize = grid_spacing)

grid <- grid %>%
  st_sf() %>% # making grid a geom sf
  mutate(grid_id = 1:length(lengths(grid))) # add grid ID

# Crop grid to mcp
grid_crop = st_intersects(mcp_UTM, grid)
grid = grid[unlist(grid_crop), ]

# Plotting grid
ggplot() + geom_sf(data = grid, fill = "transparent") + 
  geom_sf(data = fishing_long_crop_UTM, size = 0.5, col = "blue") + 
  geom_sf(data = fishing_short_crop_UTM, size = 0.5, col = "red") +
  geom_sf(data = land, fill = "black") +
  geom_sf(data = extent_WGS, fill = "transparent")

#--------------------------------#
#### Filtering tracking data ####
#------------------------------#

# Removing fixes at central place and incomplete trips for analysis
trips_filtered = filter(trips_withmetrics_UTM, atCP == "No", Completetrip == "Complete")

# Checking
ggplot() + geom_sf(data = land) +
  geom_sf(data = six_nm, fill = 'transparent', 
          linewidth = 0.2, col = 'red', 
          linetype = "dashed") +
  geom_sf(data = trips_filtered, size = 0.2) +
  geom_sf(data = CP, col = "blue", fill = "blue", shape = 23, size = 0.5) +
  coord_sf(xlim = c(-6.4, -6.2), ylim = c(49.8, 49.95))

#------------------------------------------#
#### Creating centroids for grid cells ####
#----------------------------------------#
# Creating centroids
grid_centroid = st_centroid(grid)

# Plot to make sure its worked
ggplot() + geom_sf(data = grid, fill = "transparent") + 
  geom_sf(data = grid_centroid)

# Calculate distance in km from centroid to central place
grid$CP_dist_km = as.numeric(st_distance(grid_centroid, CP_UTM)/1000)

#--------------------------------------------#
#### Calculating fixes in each grid cell ####
#------------------------------------------#

# Visualizing grid and trips
ggplot() + geom_sf(data = land, fill = "black") +
  geom_sf(data = trips_filtered, size = 0.2, col = "blue") +
  geom_sf(data = grid, fill = "transparent")

# Counting number of fixes in each grid cell
grid$n_fix_count = lengths(st_intersects(grid, trips_filtered))

# Counting number of fixes in each grid cell per km2
grid$n_fix_count_per_km = grid$n_fix_count/area

#-------------------------------------------#
#### Grid for short term fishing effort ####
#-----------------------------------------#

# Converting fishing periods into 5 minute intervals to match tracking data
fishing_short_crop_UTM_intervals = fishing_short_crop_UTM %>% 
  mutate(new = map2(start, end, ~ { #loop over the columns 'start', 'end' (suffix)
    v1 <- c(seq(.x, .y, by = "5 min"), .y) #create a sequence by '5 minute'
    v1[-c(1, length(v1))] <- floor_date(v1[-c(1, length(v1))], "5 min") #use floor_date for the elements that are not the first or the last
    tibble(start = v1[-length(v1)], end = v1[-1]) 
  }), .keep = "unused") %>% #create a tibble by removing the last and first observation from the sequence to create the new 'Start', 'End' columns
  unnest(new) #unnest the list column

# Creating start and end datetime
study_start = parse_date_time(min(trips_withmetrics$DateTime), tz = "GMT", orders = "Ymd HMS")
study_end = parse_date_time(max(trips_withmetrics$DateTime), tz = "GMT", orders = "Ymd HMS")

# Study period interval 
study_period = interval(study_start, study_end)

# Cropping data to study period
fishing_short_crop_UTM = fishing_short_crop_UTM_intervals[fishing_short_crop_UTM_intervals$start %within% study_period,]

# Calculating fishing effort in hours for each grid cell
fishing_short_crop_UTM$effort_hours = difftime(fishing_short_crop_UTM$end, fishing_short_crop_UTM$start, units = "hours")

# Creating column with number of fishing events in each grid cell
grid_short = grid %>% mutate (., n_events = lengths(st_intersects(grid, fishing_short_crop_UTM)))

# Remove cells with value of 0 (i.e. no fishing events inside that cell) because otherwise it doesn't work
grid_short_filtered = filter(grid_short, n_events > 0)

# Joining grid with fishing data
fishing_effort_short = st_join(grid_short_filtered, fishing_short_crop_UTM)

# Converting effort_hours numeric rather than difftime
fishing_effort_short$effort_hours = as.numeric(fishing_effort_short$effort_hours)

# Column for fishing effort in days
fishing_effort_short$effort_days = fishing_effort_short$effort_hours/24

# Summing multiple cell entries to get total hours per grid cell for study period
fishing_effort_short_sum = fishing_effort_short %>% 
  group_by(grid_id) %>% #grouping by grid id
  summarise(effort_hours = sum(effort_hours)) #total hours per grid cell

# Calculating mean daily fishing effort in hours per grid cell
n_days = as.numeric(difftime(study_end, study_start, units = c("days")))

fishing_effort_short_sum$mean_daily_effort_hours = fishing_effort_short_sum$effort_hours/n_days

# Calculating mean daily fishing effort in hours per grid cell per km2
fishing_effort_short_sum$mean_daily_effort_hours_per_km2 = fishing_effort_short_sum$mean_daily_effort_hours/area

# Calculating fishing effort per km2 in hours
fishing_effort_short_sum$effort_hours_per_km = fishing_effort_short_sum$effort_hours/area

#------------------------------------------#
#### Grid for long term fishing effort ####
#----------------------------------------#

# Calculating fishing effort in hours for each grid cell
fishing_long_crop_UTM$effort_hours = difftime(fishing_long_crop$end, fishing_long_crop$start, units = "hours")

# Creating column with number of fishing events in each grid cell
grid_long = grid %>% mutate (., n_events = lengths(st_intersects(grid, fishing_long_crop_UTM)))

# Remove cells with value of 0 (i.e. no fishing events inside that cell) because otherwise it doesn't work
grid_long_filtered = filter(grid_long, n_events > 0)

# Joining grid with fishing data
fishing_effort_long = st_join(grid_long_filtered, fishing_long_crop_UTM)

# Converting effort_hours numeric rather than difftime
fishing_effort_long$effort_hours = as.numeric(fishing_effort_long$effort_hours)

# Column for fishing effort in days
fishing_effort_long$effort_days = fishing_effort_long$effort_hours/24

# Summing multiple cell entries to get total hours per grid cell
fishing_effort_long_sum = fishing_effort_long %>% 
  group_by(grid_id) %>% #grouping by grid id
  summarise(effort_hours = sum(effort_hours)) #total hours per grid cell

# Calculating fishing effort per km2 in hours
fishing_effort_long_sum$effort_hours_per_km2 = fishing_effort_long_sum$effort_hours/area

# Mean annual fishing effort per grid cell in a relative year
fishing_effort_long_sum$mean_annual_effort_hours = fishing_effort_long_sum$effort_hours/12

# Mean annual fishing effort per grid cell per km2 in a relative year
fishing_effort_long_sum$mean_annual_effort_hours_per_km2 = fishing_effort_long_sum$mean_annual_effort_hours/area

# Saving
write_rds(fishing_effort_short_sum, "outputs/WorkingDataFrames/fishing_effort_short_sum.rds")
write_rds(fishing_effort_long_sum, "outputs/WorkingDataFrames/fishing_effort_long_sum.rds")

#---------------------------#
#### Joining dataframes ####
#-------------------------#
# Drop geometries to join
grid_df = st_drop_geometry(grid)
fishing_effort_short_sum_df = st_drop_geometry(fishing_effort_short_sum)
fishing_effort_long_sum_df = st_drop_geometry(fishing_effort_long_sum)

# Joining
fishing_effort_df <- grid_df %>%
  left_join(., fishing_effort_short_sum_df, by = "grid_id") %>%
  left_join(., fishing_effort_long_sum_df, by = "grid_id", suffix = c("_short", "_long")) # add suffix to distinguish variables as short and long

# NAs to zeroes
fishing_effort_df[is.na(fishing_effort_df)] <- 0

# Save data frame
write_rds(fishing_effort_df, "outputs/WorkingDataFrames/df_fishing_effort.rds")

#-----------------------#
#### Plotting grids ####
#---------------------#

# Transforming to WGS 84 for plotting
fishing_effort_short_sum_WGS <- st_transform(fishing_effort_short_sum, crs = st_crs(WGS))
fishing_effort_long_sum_WGS <- st_transform(fishing_effort_long_sum, crs = st_crs(WGS))

# Plot effort in hours 
# SHORT
effort_hours_short = ggplot() + 
  geom_sf(data = fishing_effort_short_sum_WGS, mapping = aes(fill = effort_hours), colour = "transparent") +
  scale_fill_continuous_sequential('Reds', 
                                   na.value = 'transparent', 
                                   rev = T) +
  geom_sf(data = land, fill = "black", col = "black") +
  geom_sf(data = six_nm, 
          mapping = aes(shape = '6 nautical miles'), 
          fill = 'transparent', 
          linewidth = 0.2, col = 'red', 
          linetype = "dashed") +
  scale_shape_manual(values = 3, 
                     guide = guide_legend(title = NULL)) +
  theme_bw()
effort_hours_short

# Plot mean daily effort in hours for study period
mean_daily_effort_hours = ggplot() + 
  geom_sf(data = fishing_effort_short_sum_WGS, mapping = aes(fill = mean_daily_effort_hours/30), colour = "transparent") +
  scale_fill_continuous_sequential('Reds', 
                                   na.value = 'transparent', 
                                   rev = T) +
  geom_sf(data = land, fill = "black", col = "black") +
  geom_sf(data = six_nm, 
          mapping = aes(shape = '6 nautical miles'), 
          fill = 'transparent', 
          linewidth = 0.2, col = 'red', 
          linetype = "dashed") +
  scale_shape_manual(values = 3, 
                     guide = guide_legend(title = NULL)) +
  theme_bw()
mean_daily_effort_hours

# LONG
effort_hours_long = ggplot() + 
  geom_sf(data = fishing_effort_long_sum_WGS, mapping = aes(fill = effort_hours), colour = "transparent") +
  scale_fill_continuous_sequential('Reds', 
                                   na.value = 'transparent', 
                                   rev = T) +
  geom_sf(data = land, fill = "black") +
  geom_sf(data = six_nm, 
          mapping = aes(shape = '6 nautical miles'), 
          fill = 'transparent', 
          linewidth = 0.2, col = 'red', 
          linetype = "dashed") +
  scale_shape_manual(values = 3, 
                     guide = guide_legend(title = NULL)) +
  theme_bw()
effort_hours_long

#----------------------------------#
#### Classifying distal points ####
#--------------------------------#

# Import trip metrics
trip_metrics = read.csv("outputs/SummaryDataFrames/LBB_tripmetrics.csv")
trip_metrics = subset(trip_metrics, Completetrip == "Complete")

# Making distal point a geom sf
trip_metrics = trip_metrics %>% mutate(geometry_distal = st_as_sf(., coords=c("Distal_lon","Distal_lat"), crs= 4326)$geometry)

# Categorizing terrestrial as overlapping with land shape and marine as opposite
terrestrial = lengths(st_intersects(trip_metrics$geometry_distal, land)) > 0
marine = !terrestrial

# Creating a column that has TRUE as being terrestrial
trip_metrics$terrestrial_distal = lengths(st_intersects(trip_metrics$geometry_distal, land)) > 0

# Sub-setting terrestrial trips
terrestrial_distal = trip_metrics[trip_metrics$terrestrial_distal == "TRUE",]

# Percentage trips with terrestrial distal point
(nrow(terrestrial_distal)/nrow(trip_metrics))*100

# Plot to see if done correctly
ggplot() + geom_sf(data = land) +
  geom_point(data = terrestrial_distal, mapping = aes(x = Distal_lon, y = Distal_lat), size = 0.1) +
  scale_colour_manual(guide = guide_legend(title = 'Trip')) +
  theme(legend.key = element_blank()) +
  geom_sf(data = CP, col = "blue", fill = "blue", shape = 23, size = 0.5) +
  new_scale("shape") +
  scale_shape_manual(guide = guide_legend(title = NULL)) +
  facet_wrap(~ ID)

# Plot terrestrial trips
terrestrial_trips = trips %>% filter(TripID %in% unique(terrestrial_distal$TripID))

map_terrestrial_trips = ggplot() + geom_sf(data = land, col = "grey") +
  geom_path(data = terrestrial_trips,
            mapping = aes(x = Lon, y = Lat), #as.factor so its discrete
            linewidth = 0.2) +
  theme(legend.key = element_blank()) +
  geom_sf(data = CP, col = "blue", fill = "blue", shape = 23, size = 0.5) +
  new_scale("shape") +
  scale_shape_manual(guide = guide_legend(title = NULL)) +
  facet_wrap(~ ID) +
  theme_bw()
map_terrestrial_trips 

## Land classification where distal points are
# Load in LCM data
lcm_2021 = st_layers("raw_data/spatial_data/map_layers/LCM/lcm-2021-vec.gpkg")

# Loading in LCM vector layer
lcm = st_read("raw_data/spatial_data/map_layers/LCM/lcm-2021-vec.gpkg", layer = "lcm_2021")

#Trandforming from OSBB36
lcm_WGS = st_transform(lcm, crs = WGS)
lcm_UTM = st_transform(lcm, crs = UTM)
lcm_crop = st_intersection(land_UTM, lcm_UTM)

# Tidy data frame
df_terrestrial_distal = terrestrial_distal %>% select(TripID, Trip_duration, Total_distance, Max_distance, Distal_lon, Distal_lat) %>% 
  mutate(., Trip_duration = Trip_duration/60, Total_distance = Total_distance/1000, Max_distance = Max_distance/1000)

# Transforming terrestrial point to UTM
df_terrestrial_distal_WGS = st_as_sf(df_terrestrial_distal, coords = c("Distal_lon", "Distal_lat"), crs = WGS)
df_terrestrial_distal_UTM = st_transform(df_terrestrial_distal_WGS, crs = UTM)

# Where points intersect
TD_LCM = st_intersects(df_terrestrial_distal_UTM, lcm_UTM)
terrestrial_distal_overlap = lcm_UTM[unlist(TD_LCM), ]

# Plotting to which points are where
ggplot() + geom_sf(data = land, fill = "darkgreen") +
  geom_sf(data = terrestrial_distal_overlap, aes(fill = X_mode)) +
  geom_point(data = df_terrestrial_distal, mapping = aes(x = Distal_lon, y = Distal_lat, col = TripID), size = 0.1)
