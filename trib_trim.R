library(sp)
library(tibbletime)
library(readr)
library(sf)
library(ggplot2)
library(RColorBrewer)
library(ggspatial)
library(raster)
library(ggmap)
library(mapview)
library(gganimate)
require(tidyverse)
require(dplyr)
require(lubridate)
require(fuzzyjoin)
require(prettymapr)
require(leaflet)
require(leaflet.extras)
require(leaflet.extras2)
require(viridis)
require(magick)
require(htmlwidgets)
require(webshot)
install.packages("patchwork")
require(patchwork)
#Load and mutate USGS temp data at Moose 06/14-28
##make sure to edit .txt of USGS temp data to delete all the metadata above the column names##
temp_june <- read_delim("~/Library/CloudStorage/GoogleDrive-cwill104@uwyo.edu/.shortcut-targets-by-id/1JM2QoknF8B3Pc75S9OrH4J_-4ONf7H81/WYACT Team Aquatic Shared/Project Data/Streams/Temp/USGS temp 2024 summer/USGS_temp_june.txt", 
                             delim = "\t", escape_double = FALSE, 
                             trim_ws = TRUE)
temp_june <- as_tibble(temp_june)
temp_june<-temp_june%>%
 slice(-1)%>%
  dplyr::select(-c(1,2,4,6))
colnames(temp_june)[2]<-"Temp_USGS"
colnames(temp_june)[1]<-"date_time"
temp_june$date_time<-as.POSIXct(temp_june$date_time, format = "%Y-%m-%d %H:%M", tz = "UTC")


# Load existing trimmed data for June
trimdata2_DamPac_june <- read_csv("~/Library/CloudStorage/GoogleDrive-cwill104@uwyo.edu/.shortcut-targets-by-id/1JM2QoknF8B3Pc75S9OrH4J_-4ONf7H81/WYACT Team Aquatic Shared/Project Data/FLAMe/Snake R. Files/SnakeDamPac/2024-06-15_SnakeDamPac/trimmed_data.csv")
#trimdata2 <- read.csv("Google Drive/My Drive/WYACT Team Aquatic Shared/Project Data/FLAMe/Snake R. Files/SnakeDamPac/2024-06-15_SnakeDamPac/trimmed_data.csv")
trimdata2_DamPac_june$date_time <- as.POSIXct(trimdata2_DamPac_june$date_time, format='%Y-%m-%d %H:%M:%S', tz='UTC')
trimdata2_DamPac_june$X <- NULL
trimdata2_DamPac_june$X.1 <- NULL
#PacDB
trimdata2_PacDB_june<-read_csv('/Users/chuckwilliams/Library/CloudStorage/GoogleDrive-cwill104@uwyo.edu/.shortcut-targets-by-id/1JM2QoknF8B3Pc75S9OrH4J_-4ONf7H81/WYACT Team Aquatic Shared/Project Data/FLAMe/Snake R. Files/SnakePacDB/2024-06-14_SnakePacDB/trimmed_data.csv')
trimdata2_PacDB_june$date_time <- as.POSIXct(trimdata2_PacDB_june$date_time, format='%Y-%m-%d %H:%M:%S', tz='UTC')
trimdata2_PacDB_june$X <- NULL
trimdata2_PacDB_june$X.1 <- NULL
#DBMoose
trimdata2_DBMoose_june<-read_csv('/Users/chuckwilliams/Library/CloudStorage/GoogleDrive-cwill104@uwyo.edu/.shortcut-targets-by-id/1JM2QoknF8B3Pc75S9OrH4J_-4ONf7H81/WYACT Team Aquatic Shared/Project Data/FLAMe/Snake R. Files/SnakeDBMoose/2024-06-28_SnakeDBMoose/trimmed_data.csv')
trimdata2_DBMoose_june$date_time <- as.POSIXct(trimdata2_DBMoose_june$date_time, format='%Y-%m-%d %H:%M:%S', tz='UTC')
trimdata2_DBMoose_june$X <- NULL
trimdata2_DBMoose_june$X.1 <- NULL

#Combine Trimmed data

june_run<-bind_rows(trimdata2_DamPac_june,trimdata2_PacDB_june,trimdata2_DBMoose_june)



#Merge june_run and temp_june
june_merge<-june_run%>%
  fuzzy_left_join(temp_june,by=c('date_time'='date_time'), 
                  match_fun = function(x, y) abs(difftime(x, y, units = "mins")) <= 7.5)%>%
  filter(!is.na(temp))

#Difference btwn observed temp and USGStemp
june_merge$temp <- as.numeric(june_merge$temp)
june_merge$Temp_USGS <- as.numeric(june_merge$Temp_USGS)

june_merge$relative_temp<-june_merge$temp-june_merge$Temp_USGS
# Make a spatial dataframe
june_sf<-st_as_sf(june_merge, coords = c('longitude','latitude'), crs=4326)
june_sf_temp<-st_as_sf(june_run, coords = c('longitude','latitude'), crs=4326)


####Interactive Map####

mapview(june_sf,zcol='relative_temp',lwd=0,legend=TRUE)
  

pal <- colorNumeric(palette = "viridis", domain = june_sf$temp)


june_reltemp_map<-leaflet(june_sf) %>%
  addProviderTiles("CartoDB.Positron") %>%  # Add a base map layer
  addCircleMarkers(
    color = ~pal_stand_rel(relative_temp),  # Color by 'relative_temp'
    radius = 5,  # Fixed radius for the markers
    popup = ~paste("Date & Time: ", date_time.x, "<br>", "Temp: ", relative_temp),  # Popup with date_time.x and relative_temp
    stroke = FALSE,  # Optional: Remove borders around the circles
    fillOpacity = 0.8  # Optional: Adjust the transparency of the circles
  ) %>%
  addLegend("bottomright", pal = pal_stand_obs, values = ~temp, title = "Observed Temp June")%>%
  addScaleBar(position = "bottomleft")
print(june_reltemp_map)
saveWidget(june_temp_stand_map,"/Users/chuckwilliams/Library/CloudStorage/GoogleDrive-cwill104@uwyo.edu/.shortcut-targets-by-id/1JM2QoknF8B3Pc75S9OrH4J_-4ONf7H81/WYACT Team Aquatic Shared/Project Data/FLAMe/Maps/june_temp_stand_map.html")
webshot("/Users/chuckwilliams/Library/CloudStorage/GoogleDrive-cwill104@uwyo.edu/.shortcut-targets-by-id/1JM2QoknF8B3Pc75S9OrH4J_-4ONf7H81/WYACT Team Aquatic Shared/Project Data/FLAMe/Maps/june_temp_stand_map.html", file = "/Users/chuckwilliams/Library/CloudStorage/GoogleDrive-cwill104@uwyo.edu/.shortcut-targets-by-id/1JM2QoknF8B3Pc75S9OrH4J_-4ONf7H81/WYACT Team Aquatic Shared/Project Data/FLAMe/Maps/PNG/june_temp_stand_map.png")

june_oxbow<-leaflet(june_sf) %>%
  addProviderTiles("CartoDB.Positron") %>%  # Add a base map layer
  addCircleMarkers(
    color = ~pal_stand_rel(relative_temp),  # Color by 'relative_temp'
    radius = 5,  # Fixed radius for the markers
    popup = ~paste("Date & Time: ", date_time.x, "<br>", "Temp: ", relative_temp),  # Popup with date_time.x and relative_temp
    stroke = FALSE,  # Optional: Remove borders around the circles
    fillOpacity = 0.8  # Optional: Adjust the transparency of the circles
  ) %>%
  addLegend("bottomright", pal = pal_stand, values = ~relative_temp, title = "Relative Temp June")%>%
  addScaleBar(position = "bottomleft")%>%
  setView(lat=43.85622,lng =-110.54825,zoom = 16)%>%
  addPopups(lat=43.85987,lng = -110.54821,
             popup = ~paste("Date & Time: ", date_time.x, "<br>", "Temp: ", relative_temp),
            options = popupOptions(closeButton = FALSE))
saveWidget(june_oxbow,"/Users/chuckwilliams/Library/CloudStorage/GoogleDrive-cwill104@uwyo.edu/.shortcut-targets-by-id/1JM2QoknF8B3Pc75S9OrH4J_-4ONf7H81/WYACT Team Aquatic Shared/Project Data/FLAMe/Snake R. Files/june_oxbow.html")
webshot("/Users/chuckwilliams/Library/CloudStorage/GoogleDrive-cwill104@uwyo.edu/.shortcut-targets-by-id/1JM2QoknF8B3Pc75S9OrH4J_-4ONf7H81/WYACT Team Aquatic Shared/Project Data/FLAMe/Snake R. Files/june_oxbow.html", file = "/Users/chuckwilliams/Library/CloudStorage/GoogleDrive-cwill104@uwyo.edu/.shortcut-targets-by-id/1JM2QoknF8B3Pc75S9OrH4J_-4ONf7H81/WYACT Team Aquatic Shared/Project Data/FLAMe/Maps/PNG/june_oxbow_popup.png")
####Create GIF####
# Set the directory containing your .png files
dir_path <- "/Users/chuckwilliams/Library/CloudStorage/GoogleDrive-cwill104@uwyo.edu/.shortcut-targets-by-id/1JM2QoknF8B3Pc75S9OrH4J_-4ONf7H81/WYACT Team Aquatic Shared/Project Data/FLAMe/Maps/PNG"

# List all .png files in the directory
frames <- list.files(path = dir_path, pattern = "*.png", full.names = TRUE)

# Check if files were found
if (length(frames) == 0) {
  stop("No .png files found in the directory!")
}

# Read the images into a list
img_list <- lapply(frames, image_read)

# Combine the images into an animated gif
gif <- image_animate(image_join(img_list), fps = .5)

# Save the animated gif
image_write(gif, "/Users/chuckwilliams/Library/CloudStorage/GoogleDrive-cwill104@uwyo.edu/.shortcut-targets-by-id/1JM2QoknF8B3Pc75S9OrH4J_-4ONf7H81/WYACT Team Aquatic Shared/Project Data/FLAMe/Maps/PNG/test.gif")

####July####
#Load and mutate USGS temp data at Moose 07/25-26
temp_july <- read_delim("/Users/chuckwilliams/Library/CloudStorage/GoogleDrive-cwill104@uwyo.edu/.shortcut-targets-by-id/1JM2QoknF8B3Pc75S9OrH4J_-4ONf7H81/WYACT Team Aquatic Shared/Project Data/Streams/Temp/USGS temp 2024 summer/USGS_temp_2024_07_25.txt", 
                        delim = "\t", escape_double = FALSE, 
                        trim_ws = TRUE)
temp_july<-temp_july%>%
  slice(-1)%>%
  dplyr::select(-c(1,2,4,6))
colnames(temp_july)[2]<-"Temp_USGS"
colnames(temp_july)[1]<-"date_time"
temp_july$date_time<-as.POSIXct(temp_july$date_time, format = "%Y-%m-%d %H:%M", tz = "UTC")

# Load existing trimmed data for July
#DamPac
trimdata2_DamPac_july <- read_csv("~/Library/CloudStorage/GoogleDrive-cwill104@uwyo.edu/.shortcut-targets-by-id/1JM2QoknF8B3Pc75S9OrH4J_-4ONf7H81/WYACT Team Aquatic Shared/Project Data/FLAMe/Snake R. Files/SnakeDamPac/2024-07-25_SnakeDamPac/trimmed_data.csv")
#trimdata2 <- read.csv("Google Drive/My Drive/WYACT Team Aquatic Shared/Project Data/FLAMe/Snake R. Files/SnakeDamPac/2024-06-15_SnakeDamPac/trimmed_data.csv")
trimdata2_DamPac_july$date_time <- as.POSIXct(trimdata2_DamPac_july$date_time, format='%Y-%m-%d %H:%M:%S', tz='UTC')
trimdata2_DamPac_july$X <- NULL
trimdata2_DamPac_july$X.1 <- NULL
#PacMoose
trimdata2_PacMoose_july<-read_csv('/Users/chuckwilliams/Library/CloudStorage/GoogleDrive-cwill104@uwyo.edu/.shortcut-targets-by-id/1JM2QoknF8B3Pc75S9OrH4J_-4ONf7H81/WYACT Team Aquatic Shared/Project Data/FLAMe/Snake R. Files/2024-07-26_SnakePacMoose/trimmed_data.csv')
trimdata2_PacMoose_july$date_time <- as.POSIXct(trimdata2_PacMoose_july$date_time, format='%Y-%m-%d %H:%M:%S', tz='UTC')
trimdata2_PacMoose_july$X <- NULL
trimdata2_PacMoose_july$X.1 <- NULL

#Combine Trimmed data

july_run<-bind_rows(trimdata2_DamPac_july,trimdata2_PacMoose_july)



#Merge june_run and temp_june
july_merge<-july_run%>%
  fuzzy_left_join(temp_july,by=c('date_time'='date_time'), 
                  match_fun = function(x, y) abs(difftime(x, y, units = "mins")) <= 7.5)%>%
  filter(!is.na(temp))

#Difference btwn observed temp and USGStemp
july_merge$temp <- as.numeric(july_merge$temp)
july_merge$Temp_USGS <- as.numeric(july_merge$Temp_USGS)

july_merge$relative_temp<-july_merge$temp-july_merge$Temp_USGS
# Make a spatial dataframe
july_sf<-st_as_sf(july_merge, coords = c('longitude','latitude'), crs=4326)

#Interactive Map July
mapview(july_sf,zcol='relative_temp',lwd=0,legend=TRUE)


pal <- colorNumeric(palette = "viridis", domain = july_sf$temp)

july_reltemp_map<-leaflet(july_sf) %>%
  addProviderTiles("CartoDB.Positron") %>%  # Add a base map layer
  addCircleMarkers(
    color = ~pal_stand_rel(relative_temp),  # Color by 'relative_temp'
    radius = 5,  # Fixed radius for the markers
    popup = ~paste("Date & Time: ", date_time.x, "<br>", "Temp: ", relative_temp),  # Popup with date_time.x and relative_temp
    stroke = FALSE,  # Optional: Remove borders around the circles
    fillOpacity = 0.8  # Optional: Adjust the transparency of the circles
  ) %>%
  #addLegend("bottomright", pal = pal_stand_obs, values = ~temp, title = "Observed Temp July")%>%
  addScaleBar(position = "bottomleft")
print(july_reltemp_map)
saveWidget(july_temp_stand_map,"/Users/chuckwilliams/Library/CloudStorage/GoogleDrive-cwill104@uwyo.edu/.shortcut-targets-by-id/1JM2QoknF8B3Pc75S9OrH4J_-4ONf7H81/WYACT Team Aquatic Shared/Project Data/FLAMe/Maps/july_temp_stand_map.html")
webshot("/Users/chuckwilliams/Library/CloudStorage/GoogleDrive-cwill104@uwyo.edu/.shortcut-targets-by-id/1JM2QoknF8B3Pc75S9OrH4J_-4ONf7H81/WYACT Team Aquatic Shared/Project Data/FLAMe/Maps/july_temp_stand_map.html", file = "/Users/chuckwilliams/Library/CloudStorage/GoogleDrive-cwill104@uwyo.edu/.shortcut-targets-by-id/1JM2QoknF8B3Pc75S9OrH4J_-4ONf7H81/WYACT Team Aquatic Shared/Project Data/FLAMe/Maps/PNG/july_temp_stand_map.png")


####August####
#Load and mutate USGS temp data at Moose 08/02-08/13 and 08/21-08/22
temp_aug1 <- read_delim("/Users/chuckwilliams/Library/CloudStorage/GoogleDrive-cwill104@uwyo.edu/.shortcut-targets-by-id/1JM2QoknF8B3Pc75S9OrH4J_-4ONf7H81/WYACT Team Aquatic Shared/Project Data/Streams/Temp/USGS temp 2024 summer/USGS_temp_aug1.txt", 
                        delim = "\t", escape_double = FALSE, 
                        trim_ws = TRUE)
temp_aug1<-temp_aug1%>%
  slice(-1)%>%
  dplyr::select(-c(1,2,4,6))
colnames(temp_aug1)[2]<-"Temp_USGS"
colnames(temp_aug1)[1]<-"date_time"
temp_aug1$date_time<-as.POSIXct(temp_aug1$date_time, format = "%Y-%m-%d %H:%M", tz = "UTC")

temp_aug2 <- read_delim("/Users/chuckwilliams/Library/CloudStorage/GoogleDrive-cwill104@uwyo.edu/.shortcut-targets-by-id/1JM2QoknF8B3Pc75S9OrH4J_-4ONf7H81/WYACT Team Aquatic Shared/Project Data/Streams/Temp/USGS temp 2024 summer/USGS_temp_aug2.txt", 
                        delim = "\t", escape_double = FALSE, 
                        trim_ws = TRUE)
temp_aug2<-temp_aug2%>%
  slice(-1)%>%
  dplyr::select(-c(1,2,4,6))
colnames(temp_aug2)[2]<-"Temp_USGS"
colnames(temp_aug2)[1]<-"date_time"
temp_aug2$date_time<-as.POSIXct(temp_aug2$date_time, format = "%Y-%m-%d %H:%M", tz = "UTC")

# Load existing trimmed data for Aug run 1
trimdata2_DamPac_aug1 <- read_csv("~/Library/CloudStorage/GoogleDrive-cwill104@uwyo.edu/.shortcut-targets-by-id/1JM2QoknF8B3Pc75S9OrH4J_-4ONf7H81/WYACT Team Aquatic Shared/Project Data/FLAMe/Snake R. Files/SnakeDamPac/2024-08-13_SnakeDamPac/trimmed_data.csv")
#trimdata2 <- read.csv("Google Drive/My Drive/WYACT Team Aquatic Shared/Project Data/FLAMe/Snake R. Files/SnakeDamPac/2024-06-15_SnakeDamPac/trimmed_data.csv")
trimdata2_DamPac_aug1$date_time <- as.POSIXct(trimdata2_DamPac_aug1$date_time, format='%Y-%m-%d %H:%M:%S', tz='UTC')
trimdata2_DamPac_aug1$X <- NULL
trimdata2_DamPac_aug1$X.1 <- NULL
#PacDB
trimdata2_PacDB_aug1<-read_csv('/Users/chuckwilliams/Library/CloudStorage/GoogleDrive-cwill104@uwyo.edu/.shortcut-targets-by-id/1JM2QoknF8B3Pc75S9OrH4J_-4ONf7H81/WYACT Team Aquatic Shared/Project Data/FLAMe/Snake R. Files/SnakePacDB/2024-08-02_SnakePacDB/trimmed_data.csv')
trimdata2_PacDB_aug1$date_time <- as.POSIXct(trimdata2_PacDB_aug1$date_time, format='%Y-%m-%d %H:%M:%S', tz='UTC')
trimdata2_PacDB_aug1$X <- NULL
trimdata2_PacDB_aug1$X.1 <- NULL
#DBMoose
trimdata2_DBMoose_aug1<-read_csv('/Users/chuckwilliams/Library/CloudStorage/GoogleDrive-cwill104@uwyo.edu/.shortcut-targets-by-id/1JM2QoknF8B3Pc75S9OrH4J_-4ONf7H81/WYACT Team Aquatic Shared/Project Data/FLAMe/Snake R. Files/SnakeDBMoose/2024-08-08_SnakeDBMoose/trimmed_data.csv')
trimdata2_DBMoose_aug1$date_time <- as.POSIXct(trimdata2_DBMoose_aug1$date_time, format='%Y-%m-%d %H:%M:%S', tz='UTC')
trimdata2_DBMoose_aug1$X <- NULL
trimdata2_DBMoose_aug1$X.1 <- NULL

# Load existing trimmed data for Aug run 2
trimdata2_DamDB_aug2 <- read_csv("/Users/chuckwilliams/Library/CloudStorage/GoogleDrive-cwill104@uwyo.edu/.shortcut-targets-by-id/1JM2QoknF8B3Pc75S9OrH4J_-4ONf7H81/WYACT Team Aquatic Shared/Project Data/FLAMe/Snake R. Files/2024-08-21_SnakeDamDB/trimmed_data.csv")
#trimdata2 <- read.csv("Google Drive/My Drive/WYACT Team Aquatic Shared/Project Data/FLAMe/Snake R. Files/SnakeDamPac/2024-06-15_SnakeDamPac/trimmed_data.csv")
trimdata2_DamDB_aug2$date_time <- as.POSIXct(trimdata2_DamDB_aug2$date_time, format='%Y-%m-%d %H:%M:%S', tz='UTC')
trimdata2_DamDB_aug2$X <- NULL
trimdata2_DamDB_aug2$X.1 <- NULL

#DBMoose
trimdata2_DBMoose_aug2<-read_csv('/Users/chuckwilliams/Library/CloudStorage/GoogleDrive-cwill104@uwyo.edu/.shortcut-targets-by-id/1JM2QoknF8B3Pc75S9OrH4J_-4ONf7H81/WYACT Team Aquatic Shared/Project Data/FLAMe/Snake R. Files/SnakeDBMoose/2024-08-22_SnakeDBMoose/trimmed_data.csv')
trimdata2_DBMoose_aug2$date_time <- as.POSIXct(trimdata2_DBMoose_aug2$date_time, format='%Y-%m-%d %H:%M:%S', tz='UTC')
trimdata2_DBMoose_aug2$X <- NULL
trimdata2_DBMoose_aug2$X.1 <- NULL

#Combine Trimmed data

aug1_run<-bind_rows(trimdata2_DamPac_aug1,trimdata2_PacDB_aug1,trimdata2_DBMoose_aug1)
aug2_run<-bind_rows(trimdata2_DamDB_aug2,trimdata2_DBMoose_aug2)

#Merge aug1_run and temp_aug1
aug1_merge<-aug1_run%>%
  fuzzy_left_join(temp_aug1,by=c('date_time'='date_time'), 
                  match_fun = function(x, y) abs(difftime(x, y, units = "mins")) <= 7.5)%>%
  filter(!is.na(temp))

#Merge aug2_run and temp_aug2
aug2_merge<-aug2_run%>%
  fuzzy_left_join(temp_aug2,by=c('date_time'='date_time'), 
                  match_fun = function(x, y) abs(difftime(x, y, units = "mins")) <= 7.5)%>%
  filter(!is.na(temp))

#Difference btwn observed temp and USGStemp Aug1
aug1_merge$temp <- as.numeric(aug1_merge$temp)
aug1_merge$Temp_USGS <- as.numeric(aug1_merge$Temp_USGS)

aug1_merge$relative_temp<-aug1_merge$temp-aug1_merge$Temp_USGS
# Make a spatial dataframe Aug1
aug1_sf<-st_as_sf(aug1_merge, coords = c('longitude','latitude'), crs=4326)

#Difference btwn observed temp and USGStemp Aug2
aug2_merge$temp <- as.numeric(aug2_merge$temp)
aug2_merge$Temp_USGS <- as.numeric(aug2_merge$Temp_USGS)

aug2_merge$relative_temp<-aug2_merge$temp-aug2_merge$Temp_USGS
# Make a spatial dataframe Aug2
aug2_sf<-st_as_sf(aug2_merge, coords = c('longitude','latitude'), crs=4326)

#Interactive Map Aug1
mapview(aug1_sf,zcol='relative_temp',lwd=0,legend=TRUE)


pal <- colorNumeric(palette = "viridis", domain = aug1_sf$relative_temp)

aug1_reltemp_map<-leaflet(aug1_sf) %>%
  addProviderTiles("CartoDB.Positron") %>%  # Add a base map layer
  addCircleMarkers(
    color = ~pal_stand_rel(relative_temp),  # Color by 'relative_temp'
    radius = 5,  # Fixed radius for the markers
    popup = ~paste("Date & Time: ", date_time.x, "<br>", "Temp: ", relative_temp),  # Popup with date_time.x and relative_temp
    stroke = FALSE,  # Optional: Remove borders around the circles
    fillOpacity = 0.8  # Optional: Adjust the transparency of the circles
  ) %>%
 # addLegend("bottomright", pal = pal_stand_obs, values = ~temp, title = "Observed Temp Early August")%>%
  addScaleBar(position = "bottomleft")
print(aug1_reltemp_map)
saveWidget(aug1_temp_stand_map,"/Users/chuckwilliams/Library/CloudStorage/GoogleDrive-cwill104@uwyo.edu/.shortcut-targets-by-id/1JM2QoknF8B3Pc75S9OrH4J_-4ONf7H81/WYACT Team Aquatic Shared/Project Data/FLAMe/Maps/aug1_reltemp_map.html")
webshot("/Users/chuckwilliams/Library/CloudStorage/GoogleDrive-cwill104@uwyo.edu/.shortcut-targets-by-id/1JM2QoknF8B3Pc75S9OrH4J_-4ONf7H81/WYACT Team Aquatic Shared/Project Data/FLAMe/Maps/aug1_temp_stand_map.html", file = "/Users/chuckwilliams/Library/CloudStorage/GoogleDrive-cwill104@uwyo.edu/.shortcut-targets-by-id/1JM2QoknF8B3Pc75S9OrH4J_-4ONf7H81/WYACT Team Aquatic Shared/Project Data/FLAMe/Maps/PNG/aug1_temp_stand_map.png")

#Interactive Map Aug2
mapview(aug2_sf,zcol='relative_temp',lwd=0,legend=TRUE)


pal <- colorNumeric(palette = "viridis", domain = aug2_sf$relative_temp)

aug2_reltemp_map<-leaflet(aug2_sf) %>%
  addProviderTiles("CartoDB.Positron") %>%  # Add a base map layer
  addCircleMarkers(
    color = ~pal_stand_rel(relative_temp),  # Color by 'relative_temp'
    radius = 5,  # Fixed radius for the markers
    popup = ~paste("Date & Time: ", date_time.x, "<br>", "Temp: ",relative_temp),  # Popup with date_time.x and relative_temp
    stroke = FALSE,  # Optional: Remove borders around the circles
    fillOpacity = 0.8  # Optional: Adjust the transparency of the circles
  ) %>%
  #addLegend("bottomright", pal = pal_stand_obs, values = ~temp, title = "Observed Temp Late August")%>%
  addScaleBar(position = "bottomleft")
print(aug2_reltemp_map)
saveWidget(aug2_temp_stand_map,"/Users/chuckwilliams/Library/CloudStorage/GoogleDrive-cwill104@uwyo.edu/.shortcut-targets-by-id/1JM2QoknF8B3Pc75S9OrH4J_-4ONf7H81/WYACT Team Aquatic Shared/Project Data/FLAMe/Maps/aug2_temp_stand_map.html")
webshot("/Users/chuckwilliams/Library/CloudStorage/GoogleDrive-cwill104@uwyo.edu/.shortcut-targets-by-id/1JM2QoknF8B3Pc75S9OrH4J_-4ONf7H81/WYACT Team Aquatic Shared/Project Data/FLAMe/Maps/aug2_temp_stand_map.html", file = "/Users/chuckwilliams/Library/CloudStorage/GoogleDrive-cwill104@uwyo.edu/.shortcut-targets-by-id/1JM2QoknF8B3Pc75S9OrH4J_-4ONf7H81/WYACT Team Aquatic Shared/Project Data/FLAMe/Maps/PNG/aug2_temp_stand_map.png")


####September####
#Load and mutate USGS temp data at Moose 09/04-09/06 and 09/20-09/23
temp_sept1 <- read_delim("/Users/chuckwilliams/Library/CloudStorage/GoogleDrive-cwill104@uwyo.edu/.shortcut-targets-by-id/1JM2QoknF8B3Pc75S9OrH4J_-4ONf7H81/WYACT Team Aquatic Shared/Project Data/Streams/Temp/USGS temp 2024 summer/USGS_temp_sept1.txt", 
                        delim = "\t", escape_double = FALSE, 
                        trim_ws = TRUE)
temp_sept1<-temp_sept1%>%
  slice(-1)%>%
  dplyr::select(-c(1,2,4,6))
colnames(temp_sept1)[2]<-"Temp_USGS"
colnames(temp_sept1)[1]<-"date_time"
temp_sept1$date_time<-as.POSIXct(temp_sept1$date_time, format = "%Y-%m-%d %H:%M", tz = "UTC")

temp_sept2 <- read_delim("/Users/chuckwilliams/Library/CloudStorage/GoogleDrive-cwill104@uwyo.edu/.shortcut-targets-by-id/1JM2QoknF8B3Pc75S9OrH4J_-4ONf7H81/WYACT Team Aquatic Shared/Project Data/Streams/Temp/USGS temp 2024 summer/USGS_temp_sept2.txt", 
                        delim = "\t", escape_double = FALSE, 
                        trim_ws = TRUE)
temp_sept2<-temp_sept2%>%
  slice(-1)%>%
  dplyr::select(-c(1,2,4,6))
colnames(temp_sept2)[2]<-"Temp_USGS"
colnames(temp_sept2)[1]<-"date_time"
temp_sept2$date_time<-as.POSIXct(temp_sept2$date_time, format = "%Y-%m-%d %H:%M", tz = "UTC")

# Load existing trimmed data for Sept1
trimdata2_DamDB_sept1 <- read_csv("/Users/chuckwilliams/Library/CloudStorage/GoogleDrive-cwill104@uwyo.edu/.shortcut-targets-by-id/1JM2QoknF8B3Pc75S9OrH4J_-4ONf7H81/WYACT Team Aquatic Shared/Project Data/FLAMe/Snake R. Files/2024-09-06_SnakeDamDB/trimmed_data.csv")
#trimdata2 <- read.csv("Google Drive/My Drive/WYACT Team Aquatic Shared/Project Data/FLAMe/Snake R. Files/SnakeDamPac/2024-06-15_SnakeDamPac/trimmed_data.csv")
trimdata2_DamDB_sept1$date_time <- as.POSIXct(trimdata2_DamDB_sept1$date_time, format='%Y-%m-%d %H:%M:%S', tz='UTC')
trimdata2_DamDB_sept1$X <- NULL
trimdata2_DamDB_sept1$X.1 <- NULL

#DBMoose
trimdata2_DBMoose_sept1<-read_csv('/Users/chuckwilliams/Library/CloudStorage/GoogleDrive-cwill104@uwyo.edu/.shortcut-targets-by-id/1JM2QoknF8B3Pc75S9OrH4J_-4ONf7H81/WYACT Team Aquatic Shared/Project Data/FLAMe/Snake R. Files/SnakeDBMoose/2024-09-04_SnakeDBMoose/trimmed_data.csv')
trimdata2_DBMoose_sept1$date_time <- as.POSIXct(trimdata2_DBMoose_sept1$date_time, format='%Y-%m-%d %H:%M:%S', tz='UTC')
trimdata2_DBMoose_sept1$X <- NULL
trimdata2_DBMoose_sept1$X.1 <- NULL

# Load existing trimmed data for Sept2
trimdata2_DamPac_sept2 <- read_csv("~/Library/CloudStorage/GoogleDrive-cwill104@uwyo.edu/.shortcut-targets-by-id/1JM2QoknF8B3Pc75S9OrH4J_-4ONf7H81/WYACT Team Aquatic Shared/Project Data/FLAMe/Snake R. Files/SnakeDamPac/2024-09-22_SnakeDamPac/trimmed_data.csv")
#trimdata2 <- read.csv("Google Drive/My Drive/WYACT Team Aquatic Shared/Project Data/FLAMe/Snake R. Files/SnakeDamPac/2024-06-15_SnakeDamPac/trimmed_data.csv")
trimdata2_DamPac_sept2$date_time <- as.POSIXct(trimdata2_DamPac_sept2$date_time, format='%Y-%m-%d %H:%M:%S', tz='UTC')
trimdata2_DamPac_sept2$X <- NULL
trimdata2_DamPac_sept2$X.1 <- NULL
#PacDB
trimdata2_PacDB_sept2<-read_csv('/Users/chuckwilliams/Library/CloudStorage/GoogleDrive-cwill104@uwyo.edu/.shortcut-targets-by-id/1JM2QoknF8B3Pc75S9OrH4J_-4ONf7H81/WYACT Team Aquatic Shared/Project Data/FLAMe/Snake R. Files/SnakePacDB/2024-09-23_SnakePacDB/trimmed_data.csv')
trimdata2_PacDB_sept2$date_time <- as.POSIXct(trimdata2_PacDB_sept2$date_time, format='%Y-%m-%d %H:%M:%S', tz='UTC')
trimdata2_PacDB_sept2$X <- NULL
trimdata2_PacDB_sept2$X.1 <- NULL
#DBMoose
trimdata2_DBMoose_sept2<-read_csv('/Users/chuckwilliams/Library/CloudStorage/GoogleDrive-cwill104@uwyo.edu/.shortcut-targets-by-id/1JM2QoknF8B3Pc75S9OrH4J_-4ONf7H81/WYACT Team Aquatic Shared/Project Data/FLAMe/Snake R. Files/SnakeDBMoose/2024-09-20_SnakeDBMoose/trimmed_data.csv')
trimdata2_DBMoose_sept2$date_time <- as.POSIXct(trimdata2_DBMoose_sept2$date_time, format='%Y-%m-%d %H:%M:%S', tz='UTC')
trimdata2_DBMoose_sept2$X <- NULL
trimdata2_DBMoose_sept2$X.1 <- NULL

#Combine Trimmed Data
sept1_run<-bind_rows(trimdata2_DamDB_sept1,trimdata2_DBMoose_sept1)
sept2_run<-bind_rows(trimdata2_DamPac_sept2,trimdata2_PacDB_sept2,trimdata2_DBMoose_sept2)

#Merge aug1_run and temp_aug1
sept1_merge<-sept1_run%>%
  fuzzy_left_join(temp_sept1,by=c('date_time'='date_time'), 
                  match_fun = function(x, y) abs(difftime(x, y, units = "mins")) <= 7.5)%>%
  filter(!is.na(temp))

#Merge aug2_run and temp_aug2
sept2_merge<-sept2_run%>%
  fuzzy_left_join(temp_sept2,by=c('date_time'='date_time'), 
                  match_fun = function(x, y) abs(difftime(x, y, units = "mins")) <= 7.5)%>%
  filter(!is.na(temp))

#Difference btwn observed temp and USGStemp sept1
sept1_merge$temp <- as.numeric(sept1_merge$temp)
sept1_merge$Temp_USGS <- as.numeric(sept1_merge$Temp_USGS)

sept1_merge$relative_temp<-sept1_merge$temp-sept1_merge$Temp_USGS
# Make a spatial dataframe sept1
sept1_sf<-st_as_sf(sept1_merge, coords = c('longitude','latitude'), crs=4326)

#Difference btwn observed temp and USGStemp Aug2
sept2_merge$temp <- as.numeric(sept2_merge$temp)
sept2_merge$Temp_USGS <- as.numeric(sept2_merge$Temp_USGS)

sept2_merge$relative_temp<-sept2_merge$temp-sept2_merge$Temp_USGS
# Make a spatial dataframe Aug2
sept2_sf<-st_as_sf(sept2_merge, coords = c('longitude','latitude'), crs=4326)

#Interactive Map Sept1
mapview(sept1_sf,zcol='relative_temp',lwd=0,legend=TRUE)


pal <- colorNumeric(palette = "viridis", domain = sept1_sf$relative_temp)

sept1_reltemp_map<-leaflet(sept1_sf) %>%
  addProviderTiles("CartoDB.Positron") %>%  # Add a base map layer
  addCircleMarkers(
    color = ~pal_stand_rel(relative_temp),  # Color by 'relative_temp'
    radius = 5,  # Fixed radius for the markers
    popup = ~paste("Date & Time: ", date_time.x, "<br>", "Temp: ", relative_temp),  # Popup with date_time.x and relative_temp
    stroke = FALSE,  # Optional: Remove borders around the circles
    fillOpacity = 0.8  # Optional: Adjust the transparency of the circles
  ) %>%
 # addLegend("bottomright", pal = pal_stand_obs, values = ~temp, title = "Observed Temp Early September")%>%
  addScaleBar(position = "bottomleft")
print(sept1_reltemp_map)
saveWidget(sept1_temp_stand_map,"/Users/chuckwilliams/Library/CloudStorage/GoogleDrive-cwill104@uwyo.edu/.shortcut-targets-by-id/1JM2QoknF8B3Pc75S9OrH4J_-4ONf7H81/WYACT Team Aquatic Shared/Project Data/FLAMe/Maps/sept1_temp_stand_map.html")
webshot("/Users/chuckwilliams/Library/CloudStorage/GoogleDrive-cwill104@uwyo.edu/.shortcut-targets-by-id/1JM2QoknF8B3Pc75S9OrH4J_-4ONf7H81/WYACT Team Aquatic Shared/Project Data/FLAMe/Maps/sept1_temp_stand_map.html", file = "/Users/chuckwilliams/Library/CloudStorage/GoogleDrive-cwill104@uwyo.edu/.shortcut-targets-by-id/1JM2QoknF8B3Pc75S9OrH4J_-4ONf7H81/WYACT Team Aquatic Shared/Project Data/FLAMe/Maps/PNG/sept1_temp_stand_map.png")

#Interactive Map Sept2
mapview(sept2_sf,zcol='relative_temp',lwd=0,legend=TRUE)


pal <- colorNumeric(palette = "viridis", domain = sept2_sf$relative_temp)

sept2_reltemp_map<-leaflet(sept2_sf) %>%
  addProviderTiles("CartoDB.Positron") %>%  # Add a base map layer
  addCircleMarkers(
    color = ~pal_stand_rel(relative_temp),  # Color by 'relative_temp'
    radius = 5,  # Fixed radius for the markers
    popup = ~paste("Date & Time: ", date_time.x, "<br>", "Temp: ", relative_temp),  # Popup with date_time.x and relative_temp
    stroke = FALSE,  # Optional: Remove borders around the circles
    fillOpacity = 0.8  # Optional: Adjust the transparency of the circles
  ) %>%
  #addLegend("bottomright", pal = pal_stand_obs, values = ~temp, title = "Observed Temp Late September")%>%
  addScaleBar(position = "bottomleft")
print(sept2_reltemp_map)
saveWidget(sept2_temp_stand_map,"/Users/chuckwilliams/Library/CloudStorage/GoogleDrive-cwill104@uwyo.edu/.shortcut-targets-by-id/1JM2QoknF8B3Pc75S9OrH4J_-4ONf7H81/WYACT Team Aquatic Shared/Project Data/FLAMe/Maps/sept2_temp_stand_map.html")
webshot("/Users/chuckwilliams/Library/CloudStorage/GoogleDrive-cwill104@uwyo.edu/.shortcut-targets-by-id/1JM2QoknF8B3Pc75S9OrH4J_-4ONf7H81/WYACT Team Aquatic Shared/Project Data/FLAMe/Maps/sept2_temp_stand_map.html", file = "/Users/chuckwilliams/Library/CloudStorage/GoogleDrive-cwill104@uwyo.edu/.shortcut-targets-by-id/1JM2QoknF8B3Pc75S9OrH4J_-4ONf7H81/WYACT Team Aquatic Shared/Project Data/FLAMe/Maps/PNG/sept2_temp_stand_map.png")

library(patchwork)



####Standardizing Rel Temp Scale####
# Get the min and max temperature values across all datasets to define a common scale
min_temp <- min(c(min(june_sf$relative_temp), min(july_sf$relative_temp), min(aug1_sf$relative_temp), min(aug2_sf$relative_temp), min(sept1_sf$relative_temp), min(sept2_sf$relative_temp)), na.rm = TRUE)
max_temp <- max(c(max(june_sf$relative_temp), max(july_sf$relative_temp), max(aug1_sf$relative_temp), max(aug2_sf$relative_temp), max(sept1_sf$relative_temp), max(sept2_sf$relative_temp)), na.rm = TRUE)

# Define a common color palette for relative temperature values
pal_stand_rel <- colorNumeric(palette = "viridis", domain = c(min_temp, max_temp))

#Standardizing Observed temp scale
min_temp_obs <- min(c(min(june_sf$temp), min(july_sf$temp), min(aug1_sf$temp), min(aug2_sf$temp), min(sept1_sf$temp), min(sept2_sf$temp)), na.rm = TRUE)
max_temp_obs <- max(c(max(june_sf$temp), max(july_sf$temp), max(aug1_sf$temp), max(aug2_sf$temp), max(sept1_sf$temp), max(sept2_sf$temp)), na.rm = TRUE)

pal_stand_obs <- colorNumeric(palette = "viridis", domain = c(min_temp_obs, max_temp_obs))

all_runs<-bind_rows(june_sf,july_sf,aug1_sf,aug2_sf,sept1_sf,sept2_sf)
mapview(all_runs,zcol='temp',lwd=0,legend=TRUE)

all_reltemp_map<-leaflet(all_runs) %>%
  addProviderTiles("CartoDB.Positron") %>%  # Add a base map layer
  addCircleMarkers(
    color = ~pal_stand_rel(relative_temp),  # Color by 'relative_temp'
    radius = 5,  # Fixed radius for the markers
    popup = ~paste("Date & Time: ", date_time.x, "<br>", "Temp: ", relative_temp),  # Popup with date_time.x and relative_temp
    stroke = FALSE,  # Optional: Remove borders around the circles
    fillOpacity = 0.8  # Optional: Adjust the transparency of the circles
  ) %>%
  addLegend("bottomright", pal = pal_stand_rel, values = ~relative_temp, title = "Relative Temperature (°C)")%>%
  addScaleBar(position = "bottomleft")
print(all_reltemp_map)
saveWidget(sept2_temp_stand_map,"/Users/chuckwilliams/Library/CloudStorage/GoogleDrive-cwill104@uwyo.edu/.shortcut-targets-by-id/1JM2QoknF8B3Pc75S9OrH4J_-4ONf7H81/WYACT Team Aquatic Shared/Project Data/FLAMe/Maps/sept2_temp_stand_map.html")
webshot("/Users/chuckwilliams/Library/CloudStorage/GoogleDrive-cwill104@uwyo.edu/.shortcut-targets-by-id/1JM2QoknF8B3Pc75S9OrH4J_-4ONf7H81/WYACT Team Aquatic Shared/Project Data/FLAMe/Maps/sept2_temp_stand_map.html", file = "/Users/chuckwilliams/Library/CloudStorage/GoogleDrive-cwill104@uwyo.edu/.shortcut-targets-by-id/1JM2QoknF8B3Pc75S9OrH4J_-4ONf7H81/WYACT Team Aquatic Shared/Project Data/FLAMe/Maps/PNG/sept2_temp_stand_map.png")

library(leaflet)

all_temp_stand_map <- leaflet(all_runs) %>%
  addProviderTiles("CartoDB.Positron") %>%  # Add a base map layer
  addCircleMarkers(
    color = ~pal_stand_obs(temp),  # Color by 'relative_temp'
    radius = 5,  # Fixed radius for the markers
    popup = ~paste("Date & Time: ", date_time.x, "<br>", "Temp: ", temp),  # Popup with date_time.x and relative_temp
    stroke = FALSE,  # Optional: Remove borders around the circles
    fillOpacity = 0.8  # Optional: Adjust the transparency of the circles
  ) %>%
  addLegend(
    position = "bottomright", 
    pal = pal_stand_obs, 
    values = ~temp, 
    title = "Temperature (°C)",
    opacity = 1,
    labFormat = labelFormat(prefix = "", suffix = "°C"),  # You can adjust the labels here
    labelOptions = labelOptions(
      style = list(
        "font-size" = "16px",  # Increase the font size for labels
        "font-weight" = "bold" # Optional: Make font bold
      )
    ),
    titleOptions = titleOptions(
      style = list(
        "font-size" = "18px",  # Increase the font size for the title
        "font-weight" = "bold" # Optional: Make title bold
      )
    )
  ) %>%
  addScaleBar(position = "bottomleft")

# View the map
all_temp_stand_map

####SHINY APP####
library(shiny)
library(leaflet)
library(dplyr)
library(fastmap)
library(shiny)
library(leaflet)

june_reltemp_map<-leaflet(june_sf) %>%
  addProviderTiles("CartoDB.Positron") %>%  # Add a base map layer
  addCircleMarkers(
    color = ~pal_stand_rel(relative_temp),  # Color by 'relative_temp'
    radius = 5,  # Fixed radius for the markers
    popup = ~paste("Date & Time: ", date_time.x, "<br>", "Temp: ", relative_temp),  # Popup with date_time.x and relative_temp
    stroke = FALSE,  # Optional: Remove borders around the circles
    fillOpacity = 0.8  # Optional: Adjust the transparency of the circles
  ) %>%
  addLegend("bottomright", pal = pal_stand_obs, values = ~temp, title = "Observed Temp June")%>%
  addScaleBar(position = "bottomleft")

june_temp_stand_map<-leaflet(june_sf) %>%
  addProviderTiles("CartoDB.Positron") %>%  # Add a base map layer
  addCircleMarkers(
    color = ~pal_stand_obs(temp),  # Color by 'relative_temp'
    radius = 5,  # Fixed radius for the markers
    popup = ~paste("Date & Time: ", date_time.x, "<br>", "Temp: ", temp),  # Popup with date_time.x and relative_temp
    stroke = FALSE,  # Optional: Remove borders around the circles
    fillOpacity = 0.8  # Optional: Adjust the transparency of the circles
  ) %>%
  addLegend("bottomright", pal = pal_stand_obs, values = ~temp, title = "Observed Temp June")%>%
  addScaleBar(position = "bottomleft")

# Store the maps in a list (for easy access)
maps <- list(
  run1_rel = june_reltemp_map, run1_obs = june_temp_stand_map
  #run2_rel = july_reltemp_map, run2_obs = july_temp_stand_map,
  #run3_rel = aug1_reltemp_map, run3_obs = aug1_temp_stand_map,
  #run4_rel = aug2_reltemp_map, run4_obs = aug2_temp_stand_map,
 # run5_rel = sept1_reltemp_map, run5_obs = sept1_temp_stand_map,
 # run6_rel = sept2_reltemp_map, run6_obs = sept2_temp_stand_map
)

# Define UI
ui <- fluidPage(
  titlePanel("Interactive Map for 6 Runs and 2 Variables"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("run", "Select Run", choices = 1:1, selected = 1),
      radioButtons("tempType", "Select Temperature Type",
                   choices = c("Relative Temperature" = "relative_temp",
                               "Observed Temperature" = "temp"),
                   selected = "relative_temp")
    ),
    
    mainPanel(
      leafletOutput("maps")  # The map output
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive expression to select the appropriate map based on input
  selected_map <- reactive({
    run <- as.character(input$run)
    temp_type <- switch(input$tempType,
                        "relative_temp" = "relative_temp",
                        "temp" = "temp")
    
    map_key <- paste("run", run, temp_type, sep = "_")  # Create the map key like "run1_rel"
    maps[[map_key]]  # Return the selected map
  })
  
  # Render the leaflet map
  output$map <- renderLeaflet({
    selected_map()  # Render the map selected by the user
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)

ui<-fluidPage(
  titlePanel("Temp"),
  leafletOutput("mymap"),
  fluidRow(column(2,
                  (sliderInput("slider1","Month"),
                  min=0,max=12,value=06),
                  radioButtons("radio","Temperature Type"),
                  choices=list("relative_temp"="relative_temp","temp"="temp"),
                  selected="relative_temp"))
)

server<-function(input,output,session){
  output$mymap<-renderLeaflet({
    
    leaflet(all_runs) %>%
      addProviderTiles("CartoDB.Positron") %>%  # Add a base map layer
      addCircleMarkers(
        color = ~pal_stand_rel(relative_temp),  # Color by 'relative_temp'
        radius = 5,  # Fixed radius for the markers
        popup = ~paste("Date & Time: ", date_time.x, "<br>", "Temp: ", relative_temp),  # Popup with date_time.x and relative_temp
        stroke = FALSE,  # Optional: Remove borders around the circles
        fillOpacity = 0.8  # Optional: Adjust the transparency of the circles
      ) %>%
      addLegend("bottomright", pal = pal_stand_rel, values = ~relative_temp, title = "Relative Temperature (°C)")%>%
      addScaleBar(position = "bottomleft")
  })
}

shinyApp(ui,server)
