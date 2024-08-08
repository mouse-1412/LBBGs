#### Loading packages ####

if(!require(pacman))install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, here, lubridate, data.table, dplyr)

#### LOADING DATA ####
df_diet <- read.csv("raw_data/Diet.csv", header = TRUE)
df_diet <- subset(df_diet, species == "LBBG")
summary(diet)

# CHANGING CLASSES
df_diet$date <- as.Date(df_diet$date, "%d/%m/%Y")
df_diet$time <- as.ITime(df_diet$time, "%H:%M")

#### FREQUENCY OF OCCURENCE ####
# the percentage of pellets in which a prey item occurred. number of times prey item occurred/total number of prey items i.e. number of rows
n = nrow(df_diet)

gadids = nrow(subset(df_diet, df_diet$family == "gadidae"))
(gadids/n)*100

nrow(subset(df_diet, df_diet$family == "belonidae"))


# Type
otoliths = nrow(subset(df_diet, df_diet$type == "otolith"))
(otoliths/n)*100

bone = (nrow(subset(df_diet, df_diet$type == "bone"))/n)*100
(bone/n)*100

plastic = (nrow(subset(df_diet, df_diet$type == "plastic"))/n)*100
(plastic/n)*100

# Create data frame which FO for each type
FO_type <- data.frame(prey_type = na.omit(unique(df_diet$type)), Frequency = c(length(which(df_diet$type == "otolith")), 
                 length(which(df_diet$type == "plastic")),
                 length(which(df_diet$type == "cephalopod beak")),
                 length(which(df_diet$type == "bone")),
                 length(which(df_diet$type == "crab claw"))), FO = FO$Frequency/n)

unique(df_diet$suspected_common)

x = (length(which(df_diet$suspected_common == "garfish"))/n)*100

# PLOT

