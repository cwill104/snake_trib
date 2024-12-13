require(tidyverse)
require(readr)
require(ggplot2)
####Load our temp data####
spread<-read_csv('/Users/chuckwilliams/Library/CloudStorage/GoogleDrive-cwill104@uwyo.edu/.shortcut-targets-by-id/1JM2QoknF8B3Pc75S9OrH4J_-4ONf7H81/WYACT Team Aquatic Shared/Project Data/Streams/Temp/2024 spring/spread_20782183.csv',skip = 1)%>%
  select(-c(4,5,6,7,8))
  colnames(spread)[2]<-"datetime"
  colnames(spread)[3]<-"temp"
blacktail<-read_csv('/Users/chuckwilliams/Library/CloudStorage/GoogleDrive-cwill104@uwyo.edu/.shortcut-targets-by-id/1JM2QoknF8B3Pc75S9OrH4J_-4ONf7H81/WYACT Team Aquatic Shared/Project Data/Streams/Temp/2024 spring/Blacktail_21118710.csv',skip = 1)%>%
  select(-c(4,5,6,7,8,9))
colnames(blacktail)[2]<-"datetime"
colnames(blacktail)[3]<-"temp"
upperbarbc<-spread<-read_csv('/Users/chuckwilliams/Library/CloudStorage/GoogleDrive-cwill104@uwyo.edu/.shortcut-targets-by-id/1JM2QoknF8B3Pc75S9OrH4J_-4ONf7H81/WYACT Team Aquatic Shared/Project Data/Streams/Temp/2024 spring/upperbarbc_21118713.csv',skip = 1)%>%
  select(-c(4,5,6,7,8))
colnames(upperbarbc)[2]<-"datetime"
colnames(upperbarbc)[3]<-"temp"

####Load USGS data####
#Snake at Moose#
moose<- read_delim("/Users/chuckwilliams/Library/CloudStorage/GoogleDrive-cwill104@uwyo.edu/.shortcut-targets-by-id/1JM2QoknF8B3Pc75S9OrH4J_-4ONf7H81/WYACT Team Aquatic Shared/Project Data/Streams/Temp/USGS temp 2024 summer/2024_SnakeMoose.txt", 
                                      delim = "\t", escape_double = FALSE, 
                                      trim_ws = TRUE)%>%
  select(-c(1,2,4,6))%>%
  slice(-1)
colnames(moose)[2]<-"temp"
#Snake at Jackson Dam
dam<-read_delim("/Users/chuckwilliams/Library/CloudStorage/GoogleDrive-cwill104@uwyo.edu/.shortcut-targets-by-id/1JM2QoknF8B3Pc75S9OrH4J_-4ONf7H81/WYACT Team Aquatic Shared/Project Data/Streams/Temp/USGS temp 2024 summer/2024_Dam.txt", 
                delim = "\t", escape_double = FALSE, 
                trim_ws = TRUE)%>%
  select(-c(1,2,4,6))%>%
  slice(-1)
colnames(dam)[2]<-"temp"
#Pacific Creek
pac<-read_delim("/Users/chuckwilliams/Library/CloudStorage/GoogleDrive-cwill104@uwyo.edu/.shortcut-targets-by-id/1JM2QoknF8B3Pc75S9OrH4J_-4ONf7H81/WYACT Team Aquatic Shared/Project Data/Streams/Temp/USGS temp 2024 summer/2024_Pac.txt", 
                delim = "\t", escape_double = FALSE, 
                trim_ws = TRUE)%>%
  select(-c(1,2,4,6))%>%
  slice(-1)
colnames(pac)[2]<-"temp"
#Buffalo Fork 
bf<-read_delim("/Users/chuckwilliams/Library/CloudStorage/GoogleDrive-cwill104@uwyo.edu/.shortcut-targets-by-id/1JM2QoknF8B3Pc75S9OrH4J_-4ONf7H81/WYACT Team Aquatic Shared/Project Data/Streams/Temp/USGS temp 2024 summer/2024_BF.txt", 
               delim = "\t", escape_double = FALSE, 
               trim_ws = TRUE)%>%
  select(-c(1,2,4,6))%>%
  slice(-1)
colnames(bf)[2]<-"temp"

#Correct Time format#
spread$datetime<-as.POSIXct(spread$datetime, format = "%m/%d/%y %I:%M:%S %p", tz = "UTC")
blacktail$datetime<-as.POSIXct(blacktail$datetime, format = "%m/%d/%y %I:%M:%S %p", tz = "UTC")
upperbarbc$datetime<-as.POSIXct(upperbarbc$datetime, format = "%m/%d/%y %I:%M:%S %p", tz = "UTC")
moose$datetime<-as.POSIXct(moose$datetime, format = "%Y-%m-%d %H:%M", tz = "UTC")
dam$datetime<-as.POSIXct(dam$datetime, format = "%Y-%m-%d %H:%M", tz = "UTC")
pac$datetime<-as.POSIXct(pac$datetime, format = "%Y-%m-%d %H:%M", tz = "UTC")
bf$datetime<-as.POSIXct(bf$datetime, format = "%Y-%m-%d %H:%M", tz = "UTC")

#Add location column#
spread$location<-"Spread Creek"
blacktail$location<-"Blacktail Creek"
upperbarbc$location<-"Upper Bar B.C. Creek"
moose$location<-"Snake River near Moose"
dam$location<-"Snake River near Moran"
pac$location<-"Pacific Creek"
bf$location<-"Buffalo Fork River"

#Bind all dataframes together#
usgs_temp<-bind_rows(moose,dam,pac,bf)

usgs_temp$temp <- as.numeric(usgs_temp$temp)
#Plot date temp data#
ggplot(usgs_temp,aes(x=datetime,y=temp,color=location))+
  geom_line(size=1,alpha=0.3)+
  geom_smooth(method = "loess")
#Plot max daily temp
usgs_temp$date <- as.Date(usgs_temp$datetime)
usgs_temp_max<-usgs_temp%>%
  group_by(date,location)%>%
  summarise(max_temp=max(temp,na.rm = TRUE))

ggplot(usgs_temp,aes(x=date,y=max_temp,color=location))+
  geom_line(size=1)
  #geom_rect(aes(xmin = min(usgs_temp_max$date), xmax = max(usgs_temp_max$date), ymin = 20, ymax = 25), 
            #fill = "lightgray", alpha = .01)


temp_fish<-ggplot(usgs_temp_max, aes(x = date, y = max_temp, color = location)) +
  geom_line(size = .8) +
  scale_color_manual(values = c("Snake River near Moose" = "white", "Snake River near Moran" = "black", "Buffalo Fork River" = "blue", "Pacific Creek"="orange")) +  # Custom color palette
  labs(x = "Date",y="Max Daily Temperture (°C)") +
  theme_minimal(base_size = 15) +  # Use minimal theme with a larger base font size
  theme(
    plot.title = element_text(color = "black", size = 18, hjust = 0.5),  # Title color and size
    axis.title = element_text(color = "black", size = 14),  # Axis titles
    axis.text = element_text(color = "black", size = 12),  # Axis ticks
    legend.title = element_blank(),  # Legend title size
    legend.text = element_text(color = "black", size = 12),  # Legend text size
    legend.position = "top"  # Position legend at the top
  )
print(temp_fish)
ggsave("~/Library/CloudStorage/GoogleDrive-cwill104@uwyo.edu/.shortcut-targets-by-id/1JM2QoknF8B3Pc75S9OrH4J_-4ONf7H81/WYACT Team Aquatic Shared/Project Data/FLAMe/Maps/temp_fish.png", plot = temp_fish, width = 12, height = 6)
