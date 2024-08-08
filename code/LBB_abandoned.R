#------------------------------------------#
#### Calculating fishing event density ####
#----------------------------------------#

# Count density of points in each grid cell
# https://gis.stackexchange.com/questions/323698/counting-points-in-polygons-with-sf-package-of-r
# Short
grid_short_sf$n_events = lengths(st_intersects(grid_short_sf, fishing_short_crop_UTM))
grid_short_sf$area <- st_area(grid_short) #area of each "square"
grid_short_sf$area_km2 <- units::set_units(grid_short_sf$area, "km^2") # converting area to km2
grid_short_sf$density = grid_short_sf$n_events/grid_short_sf$area_km2 # calculating density

# Long
grid_long_sf$n_events = lengths(st_intersects(grid_long_sf, fishing_long_crop_UTM))
grid_long_sf$area <- st_area(grid_long) #area of each "square"
grid_long_sf$area_km2 <- units::set_units(grid_long_sf$area, "km^2") # converting area to km2
grid_long_sf$density = grid_long_sf$n_events/grid_long_sf$area_km2 # calculating density

# remove grid without value of 0 (i.e. no points in side that grid)
event_count_short = filter(grid_short_sf, n_events > 0)
event_count_short$density = as.numeric(event_count_short$density)

event_count_long = filter(grid_long_sf, n_events > 0)
event_count_long$density = as.numeric(event_count_long$density)

# Transforming grid to WGS84 for plotting
event_count_short_WGS <- st_transform(event_count_short, crs = st_crs(land))
event_count_long_WGS <- st_transform(event_count_long, crs = st_crs(land))

# Plotting grid
# Short
ggplot() + 
  geom_sf(data = event_count_short_WGS, mapping = aes(fill = density), colour = "transparent") +
  scale_fill_continuous_sequential('Reds', 
                                   na.value = 'transparent', 
                                   rev = T) +
  geom_path(data = tracks,
            mapping = aes(x = Lon, y = Lat, col = ID),
            linewidth = 0.2) +
  scale_colour_manual(values = pal,
                      guide = guide_legend(title = 'Bird ID')) +
  facet_wrap(~ID)


# Long
ggplot() + 
  geom_sf(data = event_count_long, mapping = aes(fill = density), colour = "transparent") +
  scale_fill_continuous_sequential('Reds', 
                                   na.value = 'transparent', 
                                   rev = T)

#### Map to decide distance thresholds to categorise trips ####
buff = st_buffer(CP, dist = 500)
g_buff = st_buffer(gugh, dist = 200)

ggplot() + geom_sf(data = g_buff, fill = "pink") +
  geom_sf(data = gugh, fill = "grey") +
  geom_sf(data = buff, fill = "transparent") +
  geom_sf(data = CP, fill = "red", col = "red") +
  geom_sf(data = tracks) +
  coord_sf(xlim = c(-6.341, -6.324),
           ylim = c(49.887, 49.899))

#--------------------------#
#### Classifying trips ####
#------------------------#
# Categorizing terrestrial as overlapping with land shape and marine as opposite
terrestrial = lengths(st_intersects(trips, land)) > 0
marine = !terrestrial

# Creating a column that has TRUE as being terrestrial
trips$terrestrial = lengths(st_intersects(trips, land)) > 0

# Sub-setting terrestrial trips
terrestrial_trips = trips[trips$terrestrial == "TRUE",]

# Plot to see if done correctly
ggplot() + geom_sf(data = land) +
  geom_point(data = terrestrial_trips, mapping = aes(x = Lon, y = Lat, col = as.factor(trip_num))) +
  scale_colour_manual(values = pal_trips,
                      guide = guide_legend(title = 'Trip')) +
  theme(legend.key = element_blank()) +
  geom_sf(data = CP, col = "blue", fill = "blue", shape = 23, size = 0.5) +
  new_scale("shape") +
  scale_shape_manual(guide = guide_legend(title = NULL)) +
  facet_wrap(~ ID)

# Amount of time spent in marine vs. terrestrial environment for each individual
# Summing minutes from all trips per individual to get total duration
trip_class = trips %>% 
  group_by(ID) %>% #grouping by id
  summarise(total_duration = sum(difftime))

# Summing minutes from all terrestrial trips per individual
trip_class$terrestrial_duration = terrestrial_trips %>% 
  group_by(ID) %>% #grouping by id
  summarise(terrestrial_duration = sum(difftime))

# Subtracting to get minutes for marine trips
trip_class$marine_duration = trip_class$total_duration - trip_class$terrestrial_duration$terrestrial_duration

# Proportion time spent in marine vs. terrestrial environment for each individual
trip_class$terrestrial_proportion = trip_class$terrestrial_duration$terrestrial_duration/trip_class$total_duration

#------------------------#
#### TRIPS: PLOTTING ####
#----------------------#

pal_trips <- viridis(39)

# Map with all trips
map_trips = base + 
  new_scale_colour() + 
  #geom_point(data = trips, aes(x = Lon, y = Lat, col = as.factor(trip_num)), alpha = 0.8, size = 0.5 ) +
  geom_path(data = trips,
            mapping = aes(x = Lon, y = Lat, col = as.factor(trip_num)), #as.factor so its discrete
            linewidth = 0.2) +
  scale_colour_manual(values = pal_trips,
                      guide = guide_legend(title = 'Bird ID')) +
  theme(legend.key = element_blank()) +
  geom_sf(data = CP, col = "blue", fill = "blue", shape = 23, size = 0.5) +
  new_scale("shape") +
  scale_shape_manual(guide = guide_legend(title = NULL)) +
  facet_wrap(~ ID)
map_trips

# Plot trips with short term fishing effort
effort_short_trips = mean_daily_effort_hours +
  new_scale_colour() + 
  #geom_point(data = trips, aes(x = Lon, y = Lat, col = as.factor(trip_num)), alpha = 0.8, size = 0.5 ) +
  geom_path(data = trips,
            mapping = aes(x = Lon, y = Lat, col = as.factor(trip_num)),
            linewidth = 0.2) +
  scale_colour_manual(values = pal_trips,
                      guide = guide_legend(title = 'Trip no.')) +
  theme(legend.key = element_blank()) +
  geom_sf(data = CP, col = "blue", fill = "blue", shape = 23, size = 0.5) +
  new_scale("shape") +
  scale_shape_manual(guide = guide_legend(title = NULL)) +
  facet_wrap(~ ID)
effort_short_trips

## Define parameters for reading out plots
## Define device to read plots out as e.g. tiff/jpeg

device <- "tiff"

## define units for plot size - usually mm

units <- "mm"

## define plot resolution in dpi - 300 usually minimum

dpi <- 300

out_path <- here("outputs","Figures")

# Saving plot
ggsave(plot = effort_short_trips, filename = "short_fishing_effort_trips.tiff",
       device = device,
       path = out_path ,units = units, width = 200, height = 175, dpi = dpi,   
)

#--------------------------------------------------------------------#
#### Calculating the proportion of land & sea in individual KDEs ####
#------------------------------------------------------------------#

# Calculate KDEs for each trip
skde_trips <- trips_withmetrics_UTM %>%
  select(TripID, ID, Lon, Lat) %>%
  group_by(TripID) %>%
  eks::st_kde() %>%
  eks::st_get_contour(., cont = c(25, 50))

# Save data frame
write_csv(skde_trips, "outputs/WorkingDataFrames/skde_trips.csv")

# add BirdID
skde_trips$BirdID = substr(skde_trips$TripID, 1, 6)

# Create column to ID of each KDE e.g. LBB001_1_25
skde_trips <- skde_trips %>% mutate(kde_ID = paste0(TripID, "_", contlabel)) 

# Counting events in each KDE
skde_trips$n_events = lengths(st_intersects(skde_trips, fishing_short_crop_UTM))

# Intersection between land and KDEs
intersection_land = st_intersection(skde_trips, land_UTM)

# Column for area of intersections
intersection_land <- intersection_land %>%
  select(BirdID, TripID, kde_ID, geometry) %>% # selecting wanted columns
  mutate(., land_area_km = as.numeric(st_area(geometry)/1000000)) # calculating area of intersections in km

# Summing the area of all intersections in each trip to get total area of land in core area
intersection_land_sum = intersection_land %>% 
  dplyr::group_by(kde_ID) %>% #grouping by TripID
  summarise(land_area = sum(land_area_km))

# Remove geometry column to join
skde_trips_df = st_drop_geometry(skde_trips)
intersection_land_sum_df = st_drop_geometry(intersection_land_sum)

# Join
kde_trips <- skde_trips_df %>%
  left_join(intersection_land_sum_df, by='kde_ID')

# Total area
kde_trips$total_area = as.numeric(st_area(skde_trips)/1000000)

# Turning NAs in land_area to zeros
kde_trips$land_area[is.na(kde_trips$land_area)] <- 0

# Calculating sea area from total area - land area
kde_trips$sea_area = kde_trips$total_area - kde_trips$land_area

# Calculating proportion
kde_trips$land_prop = kde_trips$land_area/kde_trips$total_area
kde_trips$sea_prop = kde_trips$sea_area/kde_trips$total_area

# Save data frame
write_csv(kde_trips, "outputs/WorkingDataFrames/kde_trips_df.csv")

# Plotting them
trips_kde <- ggplot() + geom_sf(data = land, size = 0.3, fill = "grey70", colour = NA,
                                inherit.aes = FALSE) + 
  geom_sf(data = skde_trips, aes(geometry = geometry, fill = contlabel, colour = contlabel)) +
  scale_fill_discrete_sequential('Purp', 
                                 na.value = 'transparent', 
                                 rev = FALSE) + scale_fill_discrete_sequential('Purp', 
                                                                               na.value = 'transparent',
                                                                               rev = FALSE) +
  scale_color_discrete_sequential('Purp', 
                                  na.value = 'transparent', 
                                  rev = FALSE) +
  coord_sf(xlim = c(-8, -5.5),
           ylim = c(48.9, 50.1),
           expand=FALSE) + #widest view
  labs(x = "Longitude",
       y = "Latitude",
       fill = "KDE",
       colour = "KDE") +
  theme_bw(base_size = 9) +
  new_scale_color() +
  theme(strip.background = element_rect(fill="grey95")) +
  facet_wrap(~ BirdID) 
#geom_sf(data = CP, col = "red", fill = "red", shape = 23, size = 0.5)
trips_kde