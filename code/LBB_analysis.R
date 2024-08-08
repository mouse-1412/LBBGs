#----------------------#
#### Load packages ####
#--------------------#

if(!require(pacman))install.packages("pacman")
pacman::p_load(tidyverse, sf, here, lubridate, terra, tidyterra, ggplot2, colorspace, ggnewscale, patchwork, cowplot, ggrepel, ggspatial, RColorBrewer, viridis, pscl, lme4) 

# Makes numbers bigger
options(scipen = 9999)

#---------------------#
#### Load in data ####
#-------------------#

df_fishing_effort = readRDS("outputs/WorkingDataFrames/df_fishing_effort.rds")
t(t(sapply(df_fishing_effort, class))) # check classes of data

kde_trips_df = read.csv("outputs/WorkingDataFrames/kde_trips_df.csv", header = TRUE)
t(t(sapply(kde_trips_df, class))) # check classes of data

# Seabird metrics
Seabird_Tagging_Metadata <- read_csv("raw_data/spatial_data/LBBG/Seabird_Tagging_Metadata.csv")
Seabird_Tagging_Metadata = Seabird_Tagging_Metadata %>% filter(., Species == "LBB")
mean(Seabird_Tagging_Metadata$Weight)
range(Seabird_Tagging_Metadata$Weight)

#--------------------------#
#### Foraging behavior ####
#------------------------#
#-----------------------#
#### Fishing effort ####
#---------------------#
#### 1. Descriptive stats ####

#Import data 
fishing_long_crop_mcp = readRDS("raw_data/spatial_data/GFW_data/fishing_long_crop_mcp.rds")
fishing_short_crop_mcp = readRDS("raw_data/spatial_data/GFW_data/fishing_short_crop_mcp.rds")

#Mean hours of fishing effort for study period in study area
mean(df_fishing_effort$effort_hours_short)
sd(df_fishing_effort$effort_hours_short)/sqrt(length((df_fishing_effort$effort_hours_short)))
sum(df_fishing_effort$effort_hours_short)/24

#Mean hours of fishing effort for study period for all years since 2014 in study area
mean(df_fishing_effort$effort_hours_long)
sum(df_fishing_effort$effort_hours_long)/24
sd(df_fishing_effort$effort_hours_long)/sqrt(length((df_fishing_effort$effort_hours_long)))

# Number of unique fishing vessels
length(unique(fishing_short_crop_mcp$vessel_name))
length(unique(fishing_long_crop_mcp$vessel_name))

# Flags
# short
unique(fishing_short_crop_mcp$vessel_flag)
length(unique(fishing_short_crop_mcp$vessel_flag))

nrow(filter(fishing_short_crop_mcp, vessel_flag == "GBR"))
nrow(filter(fishing_short_crop_mcp, vessel_flag == "FRA"))
nrow(filter(fishing_short_crop_mcp, vessel_flag == "BEL"))
nrow(filter(fishing_short_crop_mcp, vessel_flag == "IRL"))

# long
unique(fishing_long_crop_mcp$vessel_flag)
length(unique(fishing_long_crop_mcp$vessel_flag))

nrow(filter(fishing_long_crop_mcp, vessel_flag == "GBR"))
nrow(filter(fishing_long_crop_mcp, vessel_flag == "FRA"))
nrow(filter(fishing_long_crop_mcp, vessel_flag == "BEL"))
nrow(filter(fishing_long_crop_mcp, vessel_flag == "IRL"))
nrow(filter(fishing_long_crop_mcp, vessel_flag == "ESP"))


#-------------------------------------------------------#
#### 1. Number of fixes ~ Short-term fishing effort ####
#-----------------------------------------------------#

# Response variable = Number of fixes in a grid cell
# Explanatory variables = Fishing effort in hours in a grid cell

#### 1.1 Preparation ####

## Plot of all explanatory variables

plot(log((n_fix_count+1)) ~ effort_hours_short, data = df_fishing_effort, xlab = "Fishing effort (hours)", ylab = "Number of fixes")
plot(log(n_fix_count+1) ~ effort_hours_long, data = df_fishing_effort)
plot(log(n_fix_count+1) ~ log(CP_dist_km), data = df_fishing_effort)
plot(log(effort_hours_long+1) ~ log(CP_dist_km), data = df_fishing_effort)
plot(effort_hours_short ~ effort_hours_long, data = df_fishing_effort)

# Percentage of zeros in data
100*sum(df_fishing_effort$n_fix_count  == 0)/nrow(df_fishing_effort)

##### 1.1.1 Assessing normality #####

# Histogram of response variable
hist(df_fishing_effort$n_fix_count)
# Data is right skewed which is typical of a Poisson distributed variable.

#QQ norm to assess normality
qqnorm(df_fishing_effort$n_fix_count, main = NULL) #it is right-skewed which is typical of a Poisson distribution
qqline(df_fishing_effort$n_fix_count)

# Checking for normality with Shapiro-Wilks test.
shapiro.test(fishing_effort_df_filtered$n_fix_count) 
# Test suggests that the data is not normal (p < 2.2e-16). However, the test has limitations for large datasets, it is more sensitive to small deviations, increasing type 2 error.

#-----------------------#
#### 1. 2. Analysis ####
#---------------------#

# The effect of short-term fishing effort on the number of fixes in a grid cell.

##### Linear model with transformed data ####

m1.full <- lm(n_fix_count_log ~ effort_hours_short + effort_hours_long + CP_dist_km, data = df_fishing_effort)

summary(m1.full)

par(mfrow = c(2, 2))
plot(lm1.full)
par(mfrow = c(1, 1))

plot(effort_hours_short ~ effort_hours_long, data = fishing_effort_df_filtered)

##### GLM with poisson distribution ####

# testing for overdispersion
mean(df_fishing_effort$n_fix_count)/var(df_fishing_effort$n_fix_count)

m2.full <- glm(n_fix_count ~ effort_hours_short + effort_hours_long + CP_dist_km, data = df_fishing_effort, family = poisson(link = log))
#this model type was used because the response variable is the number of fixes that occurred within an interval of space and mean and variance were approximately equal.
summary(m2.full)

par(mfrow = c(2, 2))
plot(m2.full)
par(mfrow = c(1, 1))

# Checking for over-dispersion
m2.full$deviance/m2.full$df.residual

## Centering LTFE and CP_dist ##
df_fishing_effort$LTFE_centered <- as.numeric(scale(df_fishing_effort$effort_hours_long, center = TRUE, scale = FALSE))
df_fishing_effort$CP_dist_centered <- as.numeric(scale(df_fishing_effort$CP_dist_km, center = TRUE, scale = FALSE))

## Visualize the interaction ##
library(interactions)
interact_plot(m3.full, pred = LTFE_centered, modx = CP_dist_centered)

##### GLM Quasipoisson ####
#non-centered
m3.full <- glm(n_fix_count ~ effort_hours_short + effort_hours_long*CP_dist_km, data = df_fishing_effort, family = quasipoisson(link = log))

# centered
m3.full <- glm(n_fix_count ~ effort_hours_short + LTFE_centered*CP_dist_centered, data = df_fishing_effort, family = quasipoisson(link = log))

summary(m3.full)

par(mfrow = c(2, 2))
plot(m3.full)
par(mfrow = c(1, 1))

# Dispersion statistic
E2 <- resid(m3.full, type = "pearson")
N  <- nrow(df_fishing_effort)
p  <- length(coef(m3.full))  
sum(E2^2) / (N - p)

###### Significance testing ######
# ask if the removal of variable has made the model significantly worse:
summary(m.reduced.1 <- update(m3.full, . ~ . - effort_hours_short))
anova(m.reduced.1, m3.full, test = "F") # No, so we can drop it

# ask if the removal of long term FE has made the model significantly worse
summary(m.reduced.2 <- update(m3.full, . ~ . - LTFE_centered))
anova(m.reduced.2, m3.full, test = "F")
# Removal of long effort doesn't make model significantly worse but can't remove it because interaction is significant

#Removal of these 2 variables does make model significantly worse so they are retained in MAM.
summary(m.reduced.3 <- update(m3.full, . ~ . - LTFE_centered:CP_dist_centered))
anova(m.reduced.3, m3.full, test = "F")

summary(m.reduced.4 <- update(m3.full, . ~ . - CP_dist_centered))
anova(m.reduced.4, m3.full, test = "F")

# From looking at these we can see that estimate for CP_dist is increased when long effort is included, therefore significance of long effort is caused by interaction between the 2.
summary(glm(n_fix_count ~ effort_hours_long, data = df_fishing_effort, family = quasipoisson(link = log)))
summary(glm(n_fix_count ~ CP_dist_km, data = df_fishing_effort, family = quasipoisson(link = log)))
summary(glm(n_fix_count ~ effort_hours_long + CP_dist_km, data = df_fishing_effort, family = quasipoisson(link = log)))

# MAM retains CP_dist, long term FE and their interaction
summary(m.reduced.1)

# chosen to do full model because a-priori reason to include all variables
# p-values for significant variables from comparing full model to model without variable
# without LFTE
m.no_LFT = glm(n_fix_count ~ CP_dist_km + effort_hours_long:CP_dist_km, data = df_fishing_effort, family = quasipoisson(link = log))
anova(m.reduced.1, m.no_LFT, test = "F")

# without CP_dist
m.no_CP_dist = glm(n_fix_count ~ effort_hours_long + effort_hours_long:CP_dist_km, data = df_fishing_effort, family = quasipoisson(link = log))
anova(m.reduced.1, m.no_CP_dist, test = "F")

# without effort_hours_long:CP_dist_km
m.no_INT = glm(n_fix_count ~ effort_hours_long + CP_dist_km, data = df_fishing_effort, family = quasipoisson(link = log))
anova(m.reduced.1, m.no_INT, test = "F")

# Back transform interaction estimate
exp(0.00008101) #1.000081 so, positive

# Back transform LTFE estimate
exp(-0.00168146) #0.99832 so, positive

# Back transform CP_dist estimate
exp(-0.03989664) # 0.9608888 so, positive

##### Negative Binomial #####
m5.full = glm.nb(n_fix_count ~ effort_hours_short + effort_hours_long + CP_dist_km + CP_dist_km:effort_hours_long, data = df_fishing_effort)

summary(m5.full)

##### Zero inflated (ZIP) ####

m4.full <- zeroinfl(n_fix_count ~ effort_hours_short + effort_hours_long + CP_dist_km ## Predictor for the Poisson process
               | effort_hours_short + effort_hours_long + CP_dist_km, ## Predictor for the Bernoulli process;
               dist = 'poisson',
               data = df_fishing_effort)

summary(m4.full)

# Dispersion statistic
E2 <- resid(m4.full, type = "pearson")
N  <- nrow(df_fishing_effort)
p  <- length(coef(m4.full))  
sum(E2^2) / (N - p)

#### 2. Plotting ####
##### Parameters #####
## Define device to read plots out as e.g. tiff/jpeg
device <- "tiff"

## define units for plot size - usually mm
units <- "mm"

## define plot resolution in dpi - 300 usually minimum
dpi <- 300

out_path <- here("outputs","Figures")

#### BASE R ####
##### n_fixes ~ STFE #####
m.STFE <- glm(n_fix_count ~ effort_hours_short, data = df_fishing_effort, family = quasipoisson(link = log))
p.STFE <- predict(m.STFE, type = "response")

plot(log((n_fix_count+1)) ~ effort_hours_short, data = df_fishing_effort, 
     xlab = "Short-term fishing effort (h)", 
     ylab = "Log( Number of fixes + 1 )",
     col = "#00000088",
     pch = 20,
     bty = "l",
     cex.lab = 1.5,
     cex.axis = 1.4)
lines(log((p.STFE[order(df_fishing_effort$effort_hours_short)]+1)) ~ sort(df_fishing_effort$effort_hours_short), col = "red", lty = "dashed")

##### n_fix ~ long term fishing effort #####
# Predicted values for long term fishing effort
m.effort_hours_long <- glm(n_fix_count ~ effort_hours_long, data = df_fishing_effort, family = quasipoisson(link = log))
p.effort_hours_long <- predict(m.effort_hours_long, type = "response")

plot(log(n_fix_count+1) ~ effort_hours_long, data = df_fishing_effort,
     xlab = "Long term fishing effort (h)",
     ylab = "Log( Number of fixes + 1 )",
     col = "#00000088",
     pch = 20,
     bty = "l",
     cex.lab = 1.5,
     cex.axis = 1.4)
lines(log((p.effort_hours_long[order(df_fishing_effort$effort_hours_long)]+1)) ~ sort(df_fishing_effort$effort_hours_long), col = "red", lty = "dashed")

##### n_fix ~ CP_dist #####
# Predicted values
m.CP_dist_km  <- glm(n_fix_count ~ CP_dist_km, data = df_fishing_effort, family = quasipoisson(link = log))
p.CP_dist_km <- predict(m.CP_dist_km, type = "response")

# Base r plot
plot(log((n_fix_count+1)) ~ CP_dist_km, data = df_fishing_effort, 
     xlab = "Distance to Central Place (km)",
     ylab = "Log( Number of fixes + 1 )",
     col = "#00000088",
     pch = 20,
     bty = "l",
     cex.lab = 1.5,
     cex.axis = 1.4)
lines(log((p.CP_dist_km[order(df_fishing_effort$CP_dist_km)]+1)) ~ sort(df_fishing_effort$CP_dist_km), col = "red")

# Three panel plot
# Base R
par(mfrow=c(1,1))

#### GGPLOT ####

##### n_fix ~ LTFE #####
ggplot(df_fishing_effort, aes(x = effort_hours_long, y = n_fix_count)) + 
  geom_point() +
  #stat_function(data = df_fishing_effort, fun = f)
  geom_smooth(method=glm , color="red", fill="#69b3a2", se=TRUE, method.args = list(family = quasipoisson(link = "log"))) +
  theme_classic()

## Transformed response variable
LTFE = ggplot(df_fishing_effort, aes(x = effort_hours_long, y = log((n_fix_count+1)))) + 
  geom_point(alpha = 0.4) +
  #stat_function(data = df_fishing_effort, fun = f)
  geom_smooth(method = glm , method.args = list(family = quasipoisson(link = "log")), color="red", fill="#69b3a2", se=TRUE, formula = log((y+1)) ~ x) +
  xlab("Long-term fishing effort (hours)") +
  ylab("Log(Number of Fixes + 1)") +
  theme_classic() +
  theme(axis.text = element_text(colour = "black"),
        axis.ticks = element_line(colour = "black"))
LTFE

##### n_fix ~ CP_dist #####

CP_dist = ggplot(df_fishing_effort, aes(x = CP_dist_km, y = log((n_fix_count+1)))) + 
  geom_point(alpha = 0.4) +
  #stat_function(data = df_fishing_effort, fun = f)
  geom_smooth(method = glm, color="red", fill="#69b3a2", se=TRUE, method.args = list(family = quasipoisson(link = "log")), formula = log((y+1)) ~ x) +
  xlab("Distance to Central Place (km)") +
  ylab("Log(Number of Fixes + 1)") +
  theme_classic() +
  theme(axis.text = element_text(colour = "black"),
        axis.ticks = element_line(colour = "black"))
CP_dist

##### LTFE ~ CP_dist #####
#ggplot
LTFE_CP_dist = ggplot(df_fishing_effort, aes(y = effort_hours_long, x = CP_dist_km)) + 
  geom_point(alpha = 0.4) +
  #stat_function(data = df_fishing_effort, fun = f)
  geom_smooth(method = glm, color="red", fill="#69b3a2", se=TRUE, method.args = list(family = quasipoisson(link = "log")), formula = y ~ x) +
  xlab("Distance to Central Place (km)") +
  ylab("Long-term fishing effort (hours)") +
  theme_classic() +
  theme(axis.text = element_text(colour = "black"),
        axis.ticks = element_line(colour = "black"))
LTFE_CP_dist

# Multiplo\t
plot_m_results = (LTFE|CP_dist|LTFE_CP_dist) + plot_annotation(tag_levels = 'A')
plot_m_results

# Save
png(paste0(out_path,"/plot_m_results.png"), width = 400, height = 200, units = units, res = dpi)
print(plot_m_results)
dev.off()

#### Centered data ####

##### n_fix ~ LTFE_centered #####
# Create a data frame for predictions for LTFE_centered
ltfe_seq <- seq(min(df_fishing_effort$LTFE_centered), max(df_fishing_effort$LTFE_centered), length.out = 100)
pred_data_ltfe <- data.frame(
  effort_hours_short = mean(df_fishing_effort$effort_hours_short),
  LTFE_centered = ltfe_seq,
  CP_dist_centered = mean(df_fishing_effort$CP_dist_centered)
)
pred_data_ltfe$predicted_n_fix_count <- predict(m3.full, newdata = pred_data_ltfe, type = "response")

# Plot
LTFE_C = ggplot(df_fishing_effort, aes(x = LTFE_centered, y = n_fix_count + 1)) +  # Adding 1 to avoid log(0)
  geom_point(alpha = 0.5, position = position_jitter(height = 0.1)) +  # Scatterplot with jitter and transparency
  geom_line(data = pred_data_ltfe, aes(x = LTFE_centered, y = predicted_n_fix_count + 1), color = "#ca171d", size = 1) +
  scale_y_log10() +  # Log scale for y-axis
  labs(x = "Centered Long-term Fishing Effort (h)",
       y = "Log ( Number of fixes + 1 )") +
  theme_classic() +
  theme(axis.title = element_text(colour = "black", size = 14),
        axis.text = element_text(colour = "black", size = 12),
        axis.ticks = element_line(colour = "black"))

LTFE_C 

##### n_fix ~ CP_dist centered #####
# Create a data frame for predictions for CP_dist_centered
cp_dist_seq <- seq(min(df_fishing_effort$CP_dist_centered), max(df_fishing_effort$CP_dist_centered), length.out = 100)
pred_data_cp_dist <- data.frame(
  effort_hours_short = mean(df_fishing_effort$effort_hours_short),
  LTFE_centered = mean(df_fishing_effort$LTFE_centered),
  CP_dist_centered = cp_dist_seq
)
pred_data_cp_dist$predicted_n_fix_count <- predict(m3.full, newdata = pred_data_cp_dist, type = "response")

# Plot
CP_C = ggplot(df_fishing_effort, aes(x = CP_dist_centered, y = n_fix_count + 1)) +  # Adding 1 to avoid log(0)
  geom_point(alpha = 0.5, position = position_jitter(height = 0.1)) +  # Scatterplot with jitter and transparency
  geom_line(data = pred_data_cp_dist, aes(x = CP_dist_centered, y = predicted_n_fix_count + 1), color = "#ca171d", size = 1) +
  scale_y_log10() +  # Log scale for y-axis
  labs(x = "Centered Distance to the Central Place (km)",
       y = "Log ( Number of fixes + 1 )") +
  theme_classic() +
  theme(axis.title = element_text(colour = "black", size = 14),
        axis.text = element_text(colour = "black", size = 12),
        axis.ticks = element_line(colour = "black"))
CP_C

##### n_fix ~ LTFE_centered*CP_dist_centered #####
# Create a grid of LTFE_centered and CP_dist_centered
ltfe_seq <- seq(min(df_fishing_effort$LTFE_centered), max(df_fishing_effort$LTFE_centered), length.out = 50)
cp_dist_seq <- seq(min(df_fishing_effort$CP_dist_centered), max(df_fishing_effort$CP_dist_centered), length.out = 50)
grid_data <- expand.grid(
  LTFE_centered = ltfe_seq,
  CP_dist_centered = cp_dist_seq,
  effort_hours_short = mean(df_fishing_effort$effort_hours_short)
)
grid_data$predicted_n_fix_count <- predict(m3.full, newdata = grid_data, type = "response")

# Plot
INT_C = ggplot(grid_data, aes(x = LTFE_centered, y = CP_dist_centered, fill = log(predicted_n_fix_count + 1))) +  # Adding 1 to avoid log(0)
  geom_tile() +
  #scale_fill_gradient(low = "#fcf3ef", high = "#ca171d", name = "Log ( Number of fixes + 1 )", guide = guide_colourbar(frame.colour = 'black', ticks.colour = 'black')) +
  scale_fill_continuous_sequential('Reds', na.value = 'transparent', rev = T, name = "Log (Number of fixes + 1)", guide = guide_colourbar(frame.colour = 'black', ticks.colour = 'black', direction = "vertical", title.position = "top", barheight = unit(5, "cm"))) +
  labs(x = "Centered Long-term Fishing Effort (h)",
       y = "Centered Distance to the Central Place (km)") +
  theme_minimal() +
  theme(axis.title = element_text(colour = "black", size = 14),
        axis.text = element_text(colour = "black", size = 12),
        legend.position = "right",
        legend.text = element_text(size = 10, margin = margin(t = 1)),
        legend.box.margin = margin(t = 2, r = 0, b = 0, l = 0),  # Add space above legend
        legend.title = element_text(size = 10))
INT_C

plot_results = ((LTFE_C+CP_C) / INT_C) + plot_annotation(tag_levels = 'A')
plot_results

png(paste0(out_path,"/plot_results.png"), width = 300, height = 300, units = units, res = dpi)
print(plot_results)
dev.off()

##### 2.5. Foraging behavior plots #####
# Import data
tripmetrics = read.csv("outputs/SummaryDataFrames/LBB_tripmetrics.csv")
tripmetrics_comp =  tripmetrics %>% filter(., Completetrip == "Complete") %>%
  mutate(., Total_distance = Total_distance/1000, 
         Max_distance = Max_distance/1000, Trip_duration = Trip_duration/60, # Converting distance to km and time to hours
         Tripstart = parse_date_time(Tripstart, tz = "GMT", orders = "Ymd HMS"),
         Tripend = parse_date_time(Tripend, tz = "GMT", orders = "Ymd HMS"))

# Making it into a long data frame with trip metric as a column
trips.long <- tripmetrics_comp %>%
  select(ID, TripID, Trip_duration, Total_distance, Max_distance) %>%
  gather(key = "trip.metric", value = "value", c(Trip_duration, Total_distance, Max_distance)) %>%
  rename(group = ID)

# Making individuals as grouping factor
trips.long$group <- factor(trips.long$group, levels = c("LBB001", "LBB003", "LBB005", "LBB006", "LBB008", "LBB009", "LBB010"))
levels(trips.long$group) <- c("LBB001", "LBB003", "LBB005", "LBB006", "LBB008", "LBB009", "LBB010")

# Trip metrics as grouping factor to facet wrap by
trips.long$trip.metricLong <- as.factor(trips.long$trip.metric)
levels(trips.long$trip.metricLong)
levels(trips.long$trip.metricLong) <- c("Max Distance", "Total Distance", "Trip Duration" )

# Colour palette
cols = viridis(7)

# Plot
trip_metrics <- ggplot(trips.long, aes(x = group, y = value))+
  facet_wrap(.~trip.metricLong, scales = "free_y")+
  geom_point(aes(col = group), alpha = 0.4, position = position_jitter(width = 0.2))+
  scale_color_manual(values = cols)+
  #scale_y_log10()+
  guides(colour = guide_legend(override.aes = list(alpha = 2)))+
  #geom_errorbar(data = model.estimates, aes(x = ColYrLong, ymin = 10^(predicted - std.error), ymax = 10^(predicted + std.error)), inherit.aes = F)+
  #geom_errorbar(data = model.estimates, aes(x = group, ymin = conf.low_invlog, ymax = conf.high_invlog), inherit.aes = F, width = 0.3)+
  #geom_point(data = model.estimates, aes(x = group, y = predicted_invlog), inherit.aes = F)+
  #geom_vline(xintercept = c(4.5))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        panel.grid.minor.x = element_blank(), panel.border = element_rect(fill = NA),
        panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank())+
  labs(x = "Individual", y = "Value (Distance in km; Duration in hours)")

trip_metrics

##### 2.6. Filtered trip duration histogram #####

plot_tripdurations = ggplot(tripmetrics_comp, aes(x = Trip_duration))+
  geom_histogram(binwidth = 0.25, col = "black", fill = "grey") + # each bar = 1/2 hour
  scale_x_continuous(breaks = seq(0, 35, 1.5)) + # major axis tick = 5 hours
  labs(x = "Trip duration (hours)", y = "Frequency") +
  theme_minimal() +
  theme(axis.title = element_text(colour = "black", size = 14),
        axis.text = element_text(colour = "black", size = 10))
plot_tripdurations

species_code = "LBB"
ggsave(plot = plot_tripdurations, filename = paste0(species_code, "_tripdurations.tiff"),
       device = device,
       path = out_path ,units = units, width = 400, height = 200, dpi = dpi)

##### 2.6. Unfiltered trip duration histogram #####
# Import data
df_alltripdurations = readRDS("outputs/SummaryDataFrames/LBB_alltripdurations.rds")
df_alltripdurations$Trip_duration_h = df_alltripdurations$Trip_duration

# Plot
plot_alltripdurations = ggplot(df_alltripdurations, aes(x = as.numeric(Trip_duration_h))) +
  geom_histogram(binwidth = 15, col = "black", fill = "grey")+ # each bar = 30 minutes
  #scale_x_continuous(breaks = seq(0, as.numeric(max(df_alltripdurations$Trip_duration_h)), 1))+ # major axis tick = 0.5 hour
  scale_x_continuous(breaks = seq(0, as.numeric(max(df_alltripdurations$Trip_duration)), 60))+
  geom_vline(xintercept = c(60), lty = "dashed") +
  theme_light()+
  theme(axis.title = element_text(colour = "black", size = 14),
        axis.text = element_text(colour = "black", size = 10),
        axis.ticks = element_line(colour = "black")) +
  labs(x = "Trip duration (minutes)", y = "Number of trips")
plot_alltripdurations

# Save
species_code = "LBB"
ggsave(plot = plot_alltripdurations, filename = paste0(species_code, "_alltripdurations.tiff"),
       device = device,
       path = out_path ,units = units, width = 400, height = 200, dpi = dpi,   
)

##### 2.7. Time of day #####
# Long data frame
df_time = tripmetrics_comp %>% 
  mutate(Starttime = format(as.POSIXct(Tripstart), format = "%H"),
         Endtime = format(as.POSIXct(Tripend), format = "%H")) %>%
  select(TripID, Starttime, Endtime) %>%
  pivot_longer(., cols = Starttime:Endtime, names_to = 'Time', values_to = 'Hour')

df_time$Hour[df_time$Hour == "00"] <- "24" # Midnight to 24

# Plot
ggplot(df_time, aes(x = Hour, fill = Time)) +
  geom_bar(position = 'dodge', col = "black")

# Number of trips at sea in a given hour
tracks_withmetrics = read.csv("outputs/WorkingDataFrames/LBB_withtripmetrics.csv")
tracks_withmetrics = tracks_withmetrics %>%
  mutate(., DateTime = with_tz(DateTime, tz = "GMT"),
         Total_distance = Total_distance/1000, 
         Max_distance = Max_distance/1000, Trip_duration = Trip_duration/60, 
         Tripstart = parse_date_time(Tripstart, tz = "GMT", orders = "Ymd HMS"),
         Tripend = parse_date_time(Tripend, tz = "GMT", orders = "Ymd HMS")) %>%
  filter(., atCP == "No")

df = tracks_withmetrics %>% select(DateTime, TripID, Tripstart, Tripend, atCP, Lat, Lon) %>% 
  mutate(., Starttime = format(as.POSIXct(Tripstart), format = "%H"),
         Endtime = format(as.POSIXct(Tripend), format = "%H"),
         Hour = format(as.POSIXct(DateTime), format = "%H")) 

df2 = df %>% group_by(Hour) %>% summarise(., n = n())

twelve = filter(df, Hour == "12")

ggplot(df, aes(x = Hour)) +
  geom_bar() +
  xlab("Hour") +
  ylab("Number of fixes at sea")

max(df$DateTime)

Dep_sunpos <- tracks_withmetrics %>%
  mutate(date = as_date(DateTime)) %>%
  select(date, lat = Lat, lon = Lon)

SunlightTimes = getSunlightTimes(data = Dep_sunpos, tz = "GMT")

# fishery events in a given hour

cp_dist_data <- expand.grid(
  effort_hours_short = mean(df_fishing_effort$effort_hours_short, na.rm = TRUE),
  LTFE_centered = mean(df_fishing_effort$LTFE_centered, na.rm = TRUE),
  CP_dist_centered = seq(min(df_fishing_effort$CP_dist_centered, na.rm = TRUE),
                         max(df_fishing_effort$CP_dist_centered, na.rm = TRUE), length.out = 100)
)

cp_dist_data$predicted_n_fix_count <- predict(m3.full, newdata = cp_dist_data, type = "response")

plot_cp_dist <- ggplot(cp_dist_data, aes(x = CP_dist_centered, y = predicted_n_fix_count)) +
  geom_line(color = "blue") +
  geom_smooth(method = "glm", method.args = list(family = quasipoisson(link = "log")), se = FALSE, color = "red") +
  labs(title = "Predicted n_fix_count by Centered CP_dist",
       x = "Centered CP_dist",
       y = "Predicted n_fix_count") +
  theme_minimal()
plot_cp_dist

cp_dist_data <- expand.grid(
  effort_hours_short = mean(df_fishing_effort$effort_hours_short, na.rm = TRUE),
  LTFE_centered = mean(df_fishing_effort$LTFE_centered, na.rm = TRUE),
  CP_dist_centered = seq(min(df_fishing_effort$CP_dist_centered, na.rm = TRUE),
                         max(df_fishing_effort$CP_dist_centered, na.rm = TRUE), length.out = 100)
)

cp_dist_data$predicted_n_fix_count <- predict(m3.full, newdata = cp_dist_data, type = "response")
plot_cp_dist <- ggplot(cp_dist_data, aes(x = CP_dist_centered, y = predicted_n_fix_count)) +
  geom_point(color = "blue") +
  geom_smooth(method = "glm", method.args = list(family = quasipoisson(link = "log")), se = FALSE, color = "red") +
  labs(title = "Predicted n_fix_count by Centered CP_dist",
       x = "Centered CP_dist",
       y = "Predicted n_fix_count") +
  theme_minimal()

