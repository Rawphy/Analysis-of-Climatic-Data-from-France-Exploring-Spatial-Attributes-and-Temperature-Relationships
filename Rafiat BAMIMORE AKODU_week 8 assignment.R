# Install and load necessary libraries
install.packages("scatterplot3d")
library(scatterplot3d)
install.packages(c("ggplot2", "maps", "mapdata"))
library(ggplot2)
library(maps)
library(mapdata)

# Loading the dataset
clim <- read.csv("https://userpage.fu-berlin.de/soga/data/raw-data/Climfrance.csv", sep = ";")
str(clim)

# it was noticed that the atitude and pmeand is in character instaed of numeric so this issue need to be fixed 

# Fixing the issues
clim$altitude <- as.numeric(gsub(",", ".", clim$altitude))
clim$p_mean <- as.numeric(gsub(",", ".", clim$p_mean))

# Inspecting the structure again to verify if the issues is fixed
str(clim)

# plotting the map of france 

# Fetch map data for plotting
world_map <- map_data("world")
world_map <- subset(world_map, region %in% c("France")) 

#plotting the map
ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "purple", color = "black") +
  geom_point(data = climfrar, aes(x = lon, y = lat), color = "red", size = 2) +
  labs(
    title = "Climate Station Locations in France",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal()

#EXERCISE 1

climfrar <- clim[1:34, ]
clim_mdl <- lm(t_mean ~ altitude + lat + lon, data = climfrar)

summary(clim_mdl)
# Coefficients:
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 37.2650364  2.6220099  14.212 7.29e-15 ***
 # altitude    -0.0064139  0.0008688  -7.383 3.17e-08 ***
  #lat         -0.5339603  0.0557546  -9.577 1.24e-10 ***
  #lon          0.0321010  0.0395728   0.811    0.424   

# EXERCISE 2
#loading the intial model
clim_mdl <- lm(t_mean ~ altitude + lat + lon, data = climfrar)

#excluding the non-significant variables which is the longitude
mdl_new <- lm(t_mean ~ altitude  + lat, data = climfrar)
summary(mdl_new)

#Coefficients:
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 37.9147567  2.4828724   15.27 5.68e-16 ***
  #altitude    -0.0062643  0.0008443   -7.42 2.34e-08 ***
  #lat         -0.5465325  0.0532610  -10.26 1.72e-11 ***

# temperature prediction 
data <- data.frame(
  altitude = c(1215, 2890),
  lat = c(44.18, 42.94)
)
temp_pred <- predict(mdl_new, data, interval ="p", level = 0.95)
temp_pred

#EXERCISE 3 
scatterplot3d(
  x = climfrar$altitude,
  y = climfrar$lat,
  z = climfrar$t_mean,
  pch = 19,                # Solid points
  color = "blue",          # Point color
  xlab = "Altitude",
  ylab = "Latitude",
  zlab = "Mean Temperature",
  main = "3D Scatterplot of Climate Data"
)
summary(mdl_new)
#Coefficients:
  #Estimate Std. Error t value Pr(>|t|)    
#Intercept) 37.9147567  2.4828724   15.27 5.68e-16 ***
  #altitude    -0.0062643  0.0008443   -7.42 2.34e-08 ***
  lat -0.5465325  0.0532610  -10.26 1.72e-11 ***