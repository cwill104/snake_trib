require(dplyr)
####Load Temp and Flow Data for Pacific Creek####
#Pacific Creek Temp
pac<-read_delim("/Users/chuckwilliams/Library/CloudStorage/GoogleDrive-cwill104@uwyo.edu/.shortcut-targets-by-id/1JM2QoknF8B3Pc75S9OrH4J_-4ONf7H81/WYACT Team Aquatic Shared/Project Data/Streams/Temp/USGS temp 2024 summer/2024_Pac.txt", 
                delim = "\t", escape_double = FALSE, 
                trim_ws = TRUE)%>%
  select(-c(1,2,4,6))%>%
  slice(-1)
colnames(pac)[2]<-"temp"

#Pacific Creek Flow
pac_flow<-read_delim("/Users/chuckwilliams/Library/CloudStorage/GoogleDrive-cwill104@uwyo.edu/.shortcut-targets-by-id/1JM2QoknF8B3Pc75S9OrH4J_-4ONf7H81/WYACT Team Aquatic Shared/Project Data/Streams/Q/pacific_flow_2024.txt", 
                delim = "\t", escape_double = FALSE, 
                trim_ws = TRUE)%>%
  dplyr::select(-c(1,2,4,6))%>%
  slice(-1)
colnames(pac_flow)[2]<-"Q"
# Correct timestamps
pac$datetime<-as.POSIXct(pac$datetime, format = "%Y-%m-%d %H:%M", tz = "UTC")
pac_flow$datetime<-as.POSIXct(pac_flow$datetime, format = "%Y-%m-%d %H:%M", tz = "UTC")

merged_pac <- pac %>%
  left_join(pac_flow, by = "datetime")

#Snake at Jackson Dam
dam<-read_delim("/Users/chuckwilliams/Library/CloudStorage/GoogleDrive-cwill104@uwyo.edu/.shortcut-targets-by-id/1JM2QoknF8B3Pc75S9OrH4J_-4ONf7H81/WYACT Team Aquatic Shared/Project Data/Streams/Temp/USGS temp 2024 summer/2024_Dam.txt", 
                delim = "\t", escape_double = FALSE, 
                trim_ws = TRUE)%>%
  select(-c(1,2,4,6))%>%
  slice(-1)
colnames(dam)[2]<-"temp"

#Snake at Dam Flow
dam_flow<-read_delim("/Users/chuckwilliams/Library/CloudStorage/GoogleDrive-cwill104@uwyo.edu/.shortcut-targets-by-id/1JM2QoknF8B3Pc75S9OrH4J_-4ONf7H81/WYACT Team Aquatic Shared/Project Data/Streams/Q/SnakeMoran_flow_2024.txt", 
                     delim = "\t", escape_double = FALSE, 
                     trim_ws = TRUE)%>%
  dplyr::select(-c(1,2,4,6))%>%
  slice(-1)
colnames(dam_flow)[2]<-"Q"
dam_flow$location<-"Snake River near Moran"

#Snake at Moose Flow
moose_flow<-read_delim("/Users/chuckwilliams/Library/CloudStorage/GoogleDrive-cwill104@uwyo.edu/.shortcut-targets-by-id/1JM2QoknF8B3Pc75S9OrH4J_-4ONf7H81/WYACT Team Aquatic Shared/Project Data/Streams/Q/SnakeMoose_flow_2024.txt", 
                     delim = "\t", escape_double = FALSE, 
                     trim_ws = TRUE)%>%
  dplyr::select(-c(1,2,4,6))%>%
  slice(-1)
colnames(moose_flow)[2]<-"Q"
moose_flow$location<-"Snake River near Moose"
#Correct time stamps
dam$datetime<-as.POSIXct(dam$datetime, format = "%Y-%m-%d %H:%M", tz = "UTC")
dam_flow$datetime<-as.POSIXct(dam_flow$datetime, format = "%Y-%m-%d %H:%M", tz = "UTC")
moose_flow$datetime<-as.POSIXct(moose_flow$datetime, format = "%Y-%m-%d %H:%M", tz = "UTC")

#merge dam temp and flow
merged_dam <- dam %>%
  left_join(dam_flow, by = "datetime")
#bind pac and dam
#flow and temp datafram
temp_flow<-bind_rows(merged_dam,merged_pac)
#change to numeric
temp_flow$temp <- as.numeric(temp_flow$temp)
temp_flow$Q <- as.numeric(temp_flow$Q)
# Assuming merged_pac is your data frame with 'datetime', 'temp', and 'Q'
ggplot(merged_pac, aes(x = datetime, y = temp,color=location)) +
  geom_line(size=1)+
  scale_y_continuous(
    name = "Temperature (°C)",  # Label for primary y-axis
    sec.axis = sec_axis(~ . * (max(merged_pac$Q) / max(merged_pac$temp)), name = "Q")  # Secondary y-axis scaling
  ) +
  geom_line(aes(y = Q), color = "red", linetype = "dashed") +  # Plot Q on secondary y-axis
  labs(title = "Temperature and Q over Time", x = "Datetime") +
  theme_minimal()

# Define the scaling factor between temp and Q for the secondary axis
scaling_factor <- max(merged_pac$temp, na.rm = TRUE) / max(merged_pac$Q, na.rm = TRUE)

# Create the plot with temp on the primary y-axis and Q on the secondary y-axis
tempflow<-ggplot(temp_flow_max, aes(x = date)) +
  geom_line(aes(y = max_temp, color = location),size=.8) +  # Plot temperature on primary y-axis
  geom_line(aes(y = max_Q * scaling_factor, color = location), linetype = "dashed",size=.8) +
  scale_color_manual(values = c("Pacific Creek" = "blue", "Snake River near Moran" = "red"))+# Plot Q on secondary y-axis, transformed
  scale_y_continuous(
    name = "Temperature (°C)",  # Primary y-axis label
    sec.axis = sec_axis(~ . / scaling_factor, name = "Flow Rate (Q)")  # Secondary y-axis label and scaling
  ) +
  labs(title = "Temperature and Flow Rate Over Time", x = "Datetime") +
  theme_minimal()

print(tempflow)
ggsave("~/Library/CloudStorage/GoogleDrive-cwill104@uwyo.edu/.shortcut-targets-by-id/1JM2QoknF8B3Pc75S9OrH4J_-4ONf7H81/WYACT Team Aquatic Shared/Project Data/FLAMe/Maps/tempflow.png", plot = tempflow, height = 4.5,width = 6.5,units = 'in',dpi = 1200)

#Finding daily max for Temp and Flow r
temp_flow$date <- as.Date(temp_flow$datetime)
temp_flow_max<-temp_flow%>%
  group_by(date,location)%>%
  summarise(max_temp = max(temp, na.rm = TRUE),
            max_Q = max(Q, na.rm = TRUE))



pac_max_merged<- pac_temp_merged %>%
  left_join(pac_Q_merged, by = "date")
pac_max_merged$Q <- as.numeric(merged_pac$Q)
####Flow Graph####
dam_moose_flow<-bind_rows(dam_flow,moose_flow)
dam_moose_flow$date<-as.Date(dam_moose_flow$datetime)
dam_moose_flow$Q <- as.numeric(dam_moose_flow$Q)
dam_moose_flow<-dam_moose_flow%>%
  group_by(date,location)%>%
  summarise(max_Q = max(Q, na.rm = TRUE))

q_fish<-ggplot(dam_moose_flow, aes(x = date, y = max_Q, color = location)) +
  geom_line(size = .8) +
  scale_color_manual(values = c("Snake River near Moose" = "white", "Snake River near Moran" = "black")) +  # Custom color palette
  labs(x = "Date",y="Max Daily Discharge (cfs)") +
  theme_minimal(base_size = 15) +  # Use minimal theme with a larger base font size
  theme(
    plot.title = element_text(color = "black", size = 18, hjust = 0.5),  # Title color and size
    axis.title = element_text(color = "black", size = 14),  # Axis titles
    axis.text = element_text(color = "black", size = 12),  # Axis ticks
    legend.title = element_blank(),  # Legend title size
    legend.text = element_text(color = "black", size = 12),  # Legend text size
    legend.position = "top")+
  scale_x_date(
    breaks = seq(from = as.Date("2024-05-01"), to = as.Date("2024-11-01"), by = "1 month"),  # Set date breaks every month
    labels = scales::date_format("%b %Y")  # Format the labels as "Month Year" (e.g., Jan 2021)
  )
print(q_fish)
ggsave("~/Library/CloudStorage/GoogleDrive-cwill104@uwyo.edu/.shortcut-targets-by-id/1JM2QoknF8B3Pc75S9OrH4J_-4ONf7H81/WYACT Team Aquatic Shared/Project Data/FLAMe/Maps/q_fish.png", plot = q_fish, width = 12, height = 6)
