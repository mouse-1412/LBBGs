# Check/install remotes
if (!require("remotes"))
  if(!require(pacman))install.packages("pacman")
if(!require(devtools))install.packages("devtools")
pacman::p_load(usethis, gfwr, tidyverse, sf, eks, rnaturalearth) 

#--------------#
#### TOKEN ####
#------------#
key <- gfw_auth()

#-------------------#
# GET VESSEL IDs ####
#-------------------#

# Extract vessel ID's from GFW dataset (>10K vessels so broken into 3 largest countries and remainder)
big3_trawlers <- get_vessel_info(query = "flag IN ('GBR','ESP','FRA') AND geartype = 'trawlers'",
                                 search_type = "advanced", 
                                 dataset = "fishing_vessel")

other_trawlers <- get_vessel_info(query = "flag IN ('BEL','DEU','DNK','IRL','LTU','NLD','NOR','POL','PRT','SWE') AND geartype = 'trawlers'",
                                  search_type = "advanced", 
                                  dataset = "fishing_vessel")

ios_trawlers <- rbind(big3_trawlers,other_trawlers) #putting two together to make list longer than 10K

# Summarise full list of vessels extracted from GFW
message("In the GFW dataset of IOS's 'trawler' vessels, there are ",length(unique(ios_trawlers$id))," vessel ID's, of which ", length(unique(ios_trawlers$shipname)), " have publicly available names" )

# Turn vessel ID's into vector 
vessel_ids <- ios_trawlers$id#[1:150] #remove the [1:150] to get full list

#----------------------------------------#
#### GET FISHING EVENTS (SHORT TERM) ####
#--------------------------------------#
start = "2024-05-30" # Ymd format e.g. 2024-05-30
end = "2024-06-23"

# Define a function to fetch data for a chunk of vessel IDs
fetch_data <- function(ids) {
  # Make the vessel string by concatenating IDs with commas
  vessel_string <- paste(ids, collapse = ',')
  
  # Make the API call to fetch GFW fishing event data
  data <- get_event(event_type = "fishing",
                    vessel = vessel_string,
                    start_date = start,
                    end_date = end
  )
  return(data)
}

# Split the vessel IDs into chunks of n
n <- 100 # change based on how many chunks you want to split it into
chunks <- split(vessel_ids, ceiling(seq_along(vessel_ids)/n))

# Initialize an empty list to store results
all_fishing_short <- tibble()

# Loop through each chunk and fetch data
for (chunk in chunks) {
  # Fetch data for the current chunk
  chunk_data <- fetch_data(chunk)
  
  # Append the data to the list
  all_fishing_short <- bind_rows(all_fishing_short, chunk_data)
}

# Summarise full fishing events dataset
message("In the GFW dataset of IOS's trawler fishing events, there are ",nrow(all_fishing_short)," fishing events")

# Save data
saveRDS(all_fishing_short, file = "all_fishing_short.Rdata")

#---------------------------------------#
#### GET FISHING EVENTS (LONG TERM) ####
#-------------------------------------#
# Creating dataframe of dates
start_dates = seq(ymd("2014-05-30"), ymd("2024-05-30"), by = "year")
end_dates = seq(ymd("2014-06-10"), ymd("2024-06-10"), by = "year")

s1 = "2014-05-30" # Ymd format e.g. 2024-05-30
e1 = "2019-06-23"

# Define a function to fetch data for a chunk of vessel IDs
fetch_data <- function(ids, s1, e1) {
  # Make the vessel string by concatenating IDs with commas
  vessel_string <- paste(ids, collapse = ',')
  # Make the API call to fetch GFW fishing event data
  data <- get_event(event_type = "fishing",
                    vessel = vessel_string,
                    start_date = s1,
                    end_date = e1)
  return(data)
}

# Split the vessel IDs into chunks of n
n <- 400 # change based on how many chunks you want to split it into
chunks <- split(vessel_ids, ceiling(seq_along(vessel_ids)/n))

# Initialize an empty list to store results
all_fishing_long_1 <- tibble()

# Loop through each chunk and fetch data
for (chunk in chunks) { # for each chunk in the list chunks do this...
  # Fetch data for the current chunk
  chunk_data <- fetch_data(chunk, s1, e1)
  # Append the data to the list
  all_fishing_long_1 <- bind_rows(all_fishing_long_1, chunk_data)
}

# Summarise full fishing events dataset
message("In the GFW dataset of IOS's trawler fishing events, there are ",nrow(all_fishing_long_1)," fishing events")

# DOWNLOADING 2nd THIRD ####
s2 = "2019-06-23"
e2 = "2021-06-23"

# Define a function to fetch data for a chunk of vessel IDs
fetch_data <- function(ids, s2, e2) {
  # Make the vessel string by concatenating IDs with commas
  vessel_string <- paste(ids, collapse = ',')
  # Make the API call to fetch GFW fishing event data
  data <- get_event(event_type = "fishing",
                    vessel = vessel_string,
                    start_date = s2,
                    end_date = e2)
  return(data)
}

# Split the vessel IDs into chunks of n
n <- 400 # change based on how many chunks you want to split it into
chunks2 <- split(vessel_ids, ceiling(seq_along(vessel_ids)/n))

# Initialize an empty list to store results
all_fishing_long_2 <- tibble()

# Loop through each chunk and fetch data
for (chunk in chunks) { # for each chunk in the list chunks do this...
  # Fetch data for the current chunk
  chunk_data <- fetch_data(chunk, s2, e2)
  # Append the data to the list
  all_fishing_long_2 <- bind_rows(all_fishing_long_2, chunk_data)
}

# Summarise full fishing events dataset
message("In the GFW dataset of IOS's trawler fishing events, there are ",nrow(all_fishing_long)," fishing events")

# NEXT THIRD
s3 = "2021-06-23"
e3 = "2024-06-23"

# Define a function to fetch data for a chunk of vessel IDs
fetch_data <- function(ids, s3, e3) {
  # Make the vessel string by concatenating IDs with commas
  vessel_string <- paste(ids, collapse = ',')
  # Make the API call to fetch GFW fishing event data
  data <- get_event(event_type = "fishing",
                    vessel = vessel_string,
                    start_date = s3,
                    end_date = e3)
  return(data)
}

# Split the vessel IDs into chunks of n
n <- 400 # change based on how many chunks you want to split it into
chunks3 <- split(vessel_ids, ceiling(seq_along(vessel_ids)/n))

# Initialize an empty list to store results
all_fishing_long_3 <- tibble()

# Loop through each chunk and fetch data
for (chunk in chunks) { # for each chunk in the list chunks do this...
  # Fetch data for the current chunk
  chunk_data <- fetch_data(chunk, s3, e3)
  # Append the data to the list
  all_fishing_long_3 <- bind_rows(all_fishing_long_3, chunk_data)
}

# Join together
all_fishing_long <- bind_rows(list(all_fishing_long_1, all_fishing_long_2, all_fishing_long_3))
head(all_fishing_long)

# Filtering for dates between 30/05 and 22/06 for each year
all_fishing_long$year <- format(all_fishing_long$end, "%Y")
all_fishing_long_filtered <- all_fishing_long %>% filter(start >= as.Date(paste(year, 05, 30, sep = "-")), end <=as.Date(paste(year, 06, 23, sep = "-")))

# Checking for duplicates
sum(duplicated(all_fishing_long_filtered))
all_fishing_long_unique = unique(all_fishing_long_filtered)

# Save data
saveRDS(all_fishing_long_unique, file = "all_fishing_long.Rdata")
