#### Loading packages ####

if(!require(pacman))install.packages("pacman")
pacman::p_load(tidyverse, sf, here, lubridate, terra, tidyterra, ggplot2, colorspace, ggnewscale, patchwork, cowplot, ggrepel, ggspatial, RColorBrewer, viridis) 

#### Setting parameters ####
WGS = 4326
UTM = 25829

#### Loading in data layers ####
fishing_effort = read.csv("outputs/WorkingDataFrames/fishing_effort.csv", header = TRUE, sep = ",")

# Tracking data to geometry
tracks <- read.csv('outputs/WorkingDataFrames/LBB_filtered.csv')
tracks <- st_as_sf(tracks, coords = c("X", "Y"), crs = WGS) # creating geom sf from dataframe
tracks_UTM = st_transform(tracks, crs = UTM)

effort_short = readRDS("outputs/WorkingDataFrames/effort_short.rds")

grid = readRDS("outputs/WorkingDataFrames/grid.rds")

# output = Create data frame for the number of grid cells occupied by an individual tracked bird that overlap with grid cells with fishing effort

#-----------------------------------------------------------------------#
#### 1. Splitting tracking data into new data frames per individual ####
#-----------------------------------------------------------------------#

splits <- split(tracks_UTM, tracks_UTM$ID)

#--------------------------#
#### 2. Calculating NC ####
#------------------------#
# NC = the total number of grid cells occupied by an individual tracked bird

# Applying function to find the grid cells that contain a fix for each individual
ind_grid_overlap = lapply(splits, function(w) { 
  grid %>% mutate(., n_ind_fixes = lengths(st_intersects(grid, w))) %>%
    filter(., n_ind_fixes > 0) }) # Remove grid cells that don't contain a fix

#--------------------------#
#### 3. Calculating NO ####
#------------------------#
# NO = the number of grid cells occupied by an individual tracked bird that overlap with grid cells with fishing effort

# Applying a function to find the grid cells occupied by an individual tracked bird that overlap with grid cells with fishing effort
ind_fishing_overlap = lapply(ind_grid_overlap, function(w) { 
  w %>% mutate(., overlap = lengths(st_intersects(w, effort_short))) %>%
    filter(., overlap > 0)}) # Remove grid cells that don't contain fishing effort

#### Creating new data frame to calculate spatial overlap from ####
spat_overlap = data.frame(nc = sapply(ind_grid_overlap,function(x){nrow(x)}),
                          no = sapply(ind_fishing_overlap,function(x){nrow(x)}))

# Row name to column
spat_overlap <- tibble::rownames_to_column(spat_overlap, "BirdID") # Row name to column

#-------------------------------------------------------------------------------------#
#### 3. Calculating spatial overlap between an individual bird and fishing effort ####
#-----------------------------------------------------------------------------------#
# Spatial overlap (%) = 100(no/nc)
spat_overlap$perc_overlap = 100*(spat_overlap$no/spat_overlap$nc)

#-------------------------------------------------------------------------------------#
#### OPTIONAL: Splitting list of data frames into data frames for each individual ####
#------------------------------------------------------------------------------------#

# Creating list of names for splits
split_names <- c("LBB001", "LBB003", "LBB005", "LBB006", "LBB008", "LBB009", "LBB010")

# Loop to create separate data frames for each individual
for (i in 1:length(ind_grid_overlap)) {
  assign(split_names[i], ind_grid_overlap[[i]])
}

#### FEI ####

# FEI pertains to an individual bird per day in the study period.
#The term fi is the fishing effort (number of vessel hours) in grid cell i occupied by a bird during its track
# di is the relative density contribution for all location estimates for an individual shark summed in grid cell i of its track (that is, location estimates of an individual shark were weighted by the inverse of the number of total individual sharks of a single species on the same relative day of their track, and with the weights for each species normalized to one; see ‘Spatial density analysis’)
# n is the number of grid cells occupied by an individual shark during its track in a given month of a given year
