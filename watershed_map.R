require(sf)
require(ggplot2)
library(maps)
library(mapdata)
require(dplyr)
require(ggspatial)
####Watershed Map####
#Mainstem S.R. for subbasin watershed
river_line1<-read_sf('/Users/chuckwilliams/Library/CloudStorage/GoogleDrive-cwill104@uwyo.edu/.shortcut-targets-by-id/1JM2QoknF8B3Pc75S9OrH4J_-4ONf7H81/WYACT Team Aquatic Shared/Project Data/FLAMe/Shapefiles/Snake_River/Subregon S.R. Jackson/Shape/NHDFlowline.shp')
river_line2<-read_sf('/Users/chuckwilliams/Library/CloudStorage/GoogleDrive-cwill104@uwyo.edu/.shortcut-targets-by-id/1JM2QoknF8B3Pc75S9OrH4J_-4ONf7H81/WYACT Team Aquatic Shared/Project Data/FLAMe/Shapefiles/Snake_River/Subregion S.R. Boise/Shape/NHDFlowline.shp')
river_line3<-read_sf('/Users/chuckwilliams/Library/CloudStorage/GoogleDrive-cwill104@uwyo.edu/.shortcut-targets-by-id/1JM2QoknF8B3Pc75S9OrH4J_-4ONf7H81/WYACT Team Aquatic Shared/Project Data/FLAMe/Shapefiles/Snake_River/Subregion S.R. Hells/Shape/NHDFlowline.shp')
river_line<-bind_rows(river_line1,river_line2,river_line3)
mainstem <- river_line[river_line$gnis_name == "Snake River", ]
mainstem_filter<-
mainstem <- st_zm(mainstem)
#Watershed above Moose
watershed_shp<-read_sf('/Users/chuckwilliams/Library/CloudStorage/GoogleDrive-cwill104@uwyo.edu/.shortcut-targets-by-id/1JM2QoknF8B3Pc75S9OrH4J_-4ONf7H81/WYACT Team Aquatic Shared/Project Data/FLAMe/Shapefiles/Snake_River/moose_watershed_outline.shp')
#UpperSnake Watershed
uppersnake_shp<-read_sf("/Users/chuckwilliams/Library/CloudStorage/GoogleDrive-cwill104@uwyo.edu/.shortcut-targets-by-id/1JM2QoknF8B3Pc75S9OrH4J_-4ONf7H81/WYACT Team Aquatic Shared/Project Data/FLAMe/Shapefiles/Snake_River/uppersnake_flowline.shp")
#State polygons
wyoming <- map_data("state", region = "wyoming")
idaho<-map_data("state",region = "idaho")
washington<-map_data("state",region = "washington")
oregon<-map_data("state",region = "oregon")
montana<-map_data("state",region = "montana")
cities<-data.frame(
  city=c("Boise","Jackson","Laramie"),
  lon=c(-116.19930,-110.76622,-105.60059),
  lat=c(43.61545,43.47883,41.30979))
# Lakes and Reservoirs of Snake subbasins
waterbody1<-read_sf('/Users/chuckwilliams/Library/CloudStorage/GoogleDrive-cwill104@uwyo.edu/.shortcut-targets-by-id/1JM2QoknF8B3Pc75S9OrH4J_-4ONf7H81/WYACT Team Aquatic Shared/Project Data/FLAMe/Shapefiles/Snake_River/Subregion S.R. Jackson/Shape/NHDWaterbody.shp')
waterbody2<-read_sf('/Users/chuckwilliams/Library/CloudStorage/GoogleDrive-cwill104@uwyo.edu/.shortcut-targets-by-id/1JM2QoknF8B3Pc75S9OrH4J_-4ONf7H81/WYACT Team Aquatic Shared/Project Data/FLAMe/Shapefiles/Snake_River/Subregion S.R. Boise/Shape/NHDWaterbody.shp')
waterbody3<-read_sf('/Users/chuckwilliams/Library/CloudStorage/GoogleDrive-cwill104@uwyo.edu/.shortcut-targets-by-id/1JM2QoknF8B3Pc75S9OrH4J_-4ONf7H81/WYACT Team Aquatic Shared/Project Data/FLAMe/Shapefiles/Snake_River/Subregion S.R. Hells/Shape/NHDWaterbody.shp')

waterbody<-bind_rows(waterbody1,waterbody2,waterbody3)

waterbody$gnis_name <- as.character(waterbody$gnis_name)



lake_reservoirs<- waterbody %>%
  filter(gnis_name%in% c("Jackson Lake", "Palisades Reservoir", "American Falls Reservoir","Lake Walcott","Brownlee Reservoir","Oxbow Reservoir"))
#lakes_reservoirs_spatial <- waterbody %>%
 # filter(gnis_name %in%lakes_reservoirs$gnis_name)

lakes_reservoirs<-st_zm(lakes_reservoirs)



subregion<-ggplot() +
  geom_polygon(data = wyoming, aes(x = long, y = lat, group = group), fill = "gray", color = "black",alpha = 0.3) +
  geom_polygon(data = idaho, aes(x = long, y = lat, group = group), fill = "gray", color = "black",alpha = 0.3) +
  #geom_polygon(data = washington, aes(x = long, y = lat, group = group), fill = "gray", color = "black",alpha = 0.3) +
#  geom_polygon(data = oregon, aes(x = long, y = lat, group = group), fill = "gray", color = "black",alpha = 0.3) +
 # geom_polygon(data = montana, aes(x = long, y = lat, group = group), fill = "gray", color = "black",alpha = 0.3) +
  geom_sf(data=mainstem,color='blue',size=2)+
  #geom_text(data = mainstem, aes(label = 'Snake River'), size = 5, vjust = -1, hjust = 0)+
  geom_sf(data = uppersnake_outline, color = "black", size = .8, alpha = 0.3)+
  #geom_sf(data = tribs_spatial, color = "blue", size = 0.2)+
  geom_sf(data = lake_reservoirs, fill = "blue", color = "blue", size = 0.5) +
  geom_point(data = cities, aes(x = lon, y = lat), color = "black", size = 2) +
  geom_text(data = cities, aes(x = lon, y = lat, label = city), hjust=-.2, vjust = 0, size = 3) +
  theme_void()
print(subregion)
ggsave("~/Library/CloudStorage/GoogleDrive-cwill104@uwyo.edu/.shortcut-targets-by-id/1JM2QoknF8B3Pc75S9OrH4J_-4ONf7H81/WYACT Team Aquatic Shared/Project Data/FLAMe/Maps/sitemap.png", plot = subregion, height = 4.5,width = 6.5,units = 'in',dpi = 1200)

#Watershed from Moose#
site_flowlines<-read_sf("/Users/chuckwilliams/Downloads/NHD_H_17040101_HU8_Shape/Shape/NHDFlowline.shp")
site_flowlines<-st_zm(site_flowlines)
#Watershed from Palisades to Moose
site_flowlines_pali<-read_sf("/Users/chuckwilliams/Library/CloudStorage/GoogleDrive-cwill104@uwyo.edu/.shortcut-targets-by-id/1JM2QoknF8B3Pc75S9OrH4J_-4ONf7H81/WYACT Team Aquatic Shared/Project Data/FLAMe/Shapefiles/Snake_River/NHD_H_17040103_HU8_Shape_SnakeMoosetoGrays/Shape/NHDFlowline.shp")
site_flowlines_pali<-st_zm(site_flowlines_pali)
#correct coordinate system
site_flowlines <- st_transform(site_flowlines, crs = 4326)
site_flowlines_pali<-st_transform(site_flowlines_pali,crs=4326)
#select for only Snake R
site_mainstem <- site_flowlines[site_flowlines$gnis_name == "Snake River", ]
site_mainstem <- st_zm(site_mainstem)
site_mainstem_pali <- site_flowlines_pali[site_flowlines_pali$gnis_name == "Snake River", ]
site_mainstem_pali <- st_zm(site_mainstem_pali)

#trib flowlines
site_flowlines_nonspatial <- st_drop_geometry(site_flowlines)
site_flowlines_nonspatial$gnis_name <- as.character(site_flowlines_nonspatial$gnis_name)
tribs <- site_flowlines_nonspatial %>%
  filter(!is.na(gnis_name))
 # filter(gnis_name%in% c("Pacific Creek", "Spread Creek", "Buffalo Fork","Cottonwood Creek","Blacktail Creek","Blackrock Creek"))
tribs_spatial <- site_flowlines %>%
  filter(gnis_name %in% tribs$gnis_name)
tribs_spatial<-st_zm(tribs_spatial)
# Labels for tribs
tribs_unique <- tribs_spatial %>%
  distinct(gnis_name, .keep_all = TRUE)%>%
  filter(gnis_name %in% c("Pacific Creek", "Spread Creek", "Buffalo Fork","Snake River"))
#Jackson and Jenny Lake 
GNTP_waterbody<-read_sf('/Users/chuckwilliams/Downloads/NHD_H_17040101_HU8_Shape/Shape/NHDWaterbody.shp')
jackson_waterbody<-GNTP_waterbody[GNTP_waterbody$gnis_name == "Jackson Lake", ]
jenny_waterbody<-GNTP_waterbody[GNTP_waterbody$gnis_name == "Jenny Lake", ]
#Upper Snake Lake shp
uppersnakelake_shp<-read_sf("/Users/chuckwilliams/Library/CloudStorage/GoogleDrive-cwill104@uwyo.edu/.shortcut-targets-by-id/1JM2QoknF8B3Pc75S9OrH4J_-4ONf7H81/WYACT Team Aquatic Shared/Project Data/FLAMe/Shapefiles/Snake_River/UpperSnake_Lakes.shp")
uppersnake_outline<-read_sf("/Users/chuckwilliams/Downloads/Continental US Medium Resolution (1) 2/area-of-interest.shp")
uppersnake_watershed<-ggplot()+
  geom_sf(data = uppersnake_shp, fill = "lightblue", color = "lightblue", size = 0.8)+
  geom_sf(data = uppersnake_outline, fill = "lightgray", color = "black", size = 0.8, alpha = 0.1)+
  geom_sf(data = uppersnakelake_shp, fill = "blue", color = "blue", size = 0.5, alpha = 1) +
  #geom_sf(data = tribs_spatial, color = "lightblue", size = 1)+
  #geom_sf(data = jenny_waterbody, fill = "lightblue", color = "blue", size = 0.5, alpha = 0.4) +
  geom_sf(data=site_mainstem,color='blue',size=2)+
  geom_sf(data=site_mainstem_pali,color='blue',size=2)+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         height = unit(1, "cm"), width = unit(1, "cm")) +
  #geom_sf_text(data = tribs_unique, aes(label = gnis_name), size = 3, color = "black", fontface = "bold", angle=45, check_overlap = TRUE)+
  theme_void()
print(uppersnake_watershed)
ggsave("~/Library/CloudStorage/GoogleDrive-cwill104@uwyo.edu/.shortcut-targets-by-id/1JM2QoknF8B3Pc75S9OrH4J_-4ONf7H81/WYACT Team Aquatic Shared/Project Data/FLAMe/Maps/uppersnake.png", plot = uppersnake_watershed, height = 4.5,width = 6.5,units = 'in',dpi = 1200)
#### Map of Dam to Moose####
#Load data for zoomed in area
raft_shp<-read_sf("/Users/chuckwilliams/Downloads/Selected Area (1)/area-of-interest.shp")  
#Make sure on same coordiante system
site_flowlines <- st_transform(site_flowlines, crs = 4326)
raft_shp <- st_transform(raft_shp, crs = 4326)
site_mainstem <- st_transform(site_mainstem, crs = 4326)
jackson_waterbody<-st_transform(jackson_waterbody,crs=4326)
tribs_spatial<-st_transform(tribs_spatial,crs = 4326)
#crop flow lines to area
tribs_within_area <- st_intersection(site_flowlines, raft_shp)
#crop tribs to area
tribs_within_area2<-st_intersection(tribs_spatial,raft_shp)
#crop mainstem to area
mainstem_within_area <- st_intersection(site_mainstem, raft_shp)
#crop jackson lake 
jackson_within_area<-st_intersection(jackson_waterbody,raft_shp)
#crop current stn df
current_spatial <- current %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326, agr = "constant")
current_raft<-st_intersection(current_spatial,raft_shp)
rafting_area<-ggplot()+
  geom_sf(data = raft_shp, fill = "lightgray", color = "black", size = 0, alpha = 0.1)+
  geom_sf(data = tribs_within_area2, color = "lightblue", size = 1)+
  geom_sf(data = jackson_within_area, fill = "blue", color = "blue", size = 0.5, alpha = 0.4) +
  geom_sf(data = jenny_waterbody, fill = "blue", color = "blue", size = 0.5, alpha = 0.4) +
  geom_sf(data=mainstem_within_area,color='blue',size=2)+ 
  geom_sf(data = current_raft, color = "black", size = 1.5) +  # Add points with temperature data
  scale_color_viridis_c(option = "D", na.value = "gray") + 
 # coord_sf(xlim = c(-110.555,-110.541),ylim = c(43.851,43.866))+# Color scale for temperature
  theme_void()
  
print(rafting_area)  
ggsave("~/Library/CloudStorage/GoogleDrive-cwill104@uwyo.edu/.shortcut-targets-by-id/1JM2QoknF8B3Pc75S9OrH4J_-4ONf7H81/WYACT Team Aquatic Shared/Project Data/FLAMe/Maps/zoom_stn_map.png",plot=rafting_area, height = 4.5,width = 6.5,units = 'in',dpi = 1200)

####StreamTempNetwork Map####
#Load data
current<-read.csv('/Users/chuckwilliams/Library/CloudStorage/GoogleDrive-cwill104@uwyo.edu/.shortcut-targets-by-id/1JM2QoknF8B3Pc75S9OrH4J_-4ONf7H81/WYACT Team Aquatic Shared/Project Data/Streams/Locations/active_sites_UWYO.csv')

uppersnake_stn<-ggplot()+
  geom_sf(data = uppersnake_shp, fill = "lightblue", color = "lightblue", size = 0.8)+
  geom_sf(data = uppersnake_outline, fill = "lightgray", color = "black", size = 0.8, alpha = 0.1)+
  geom_sf(data = uppersnakelake_shp, fill = "blue", color = "blue", size = 0.5, alpha = 1) +
  #geom_point(data = current, aes(x = long, y = lat), color = "black", size = 1) +
  geom_sf(data=site_mainstem,color='blue',size=2)+
  geom_sf(data=site_mainstem_pali,color='blue',size=2)+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         height = unit(1, "cm"), width = unit(1, "cm")) +
  #geom_text(data = current, aes(x = long, y = lat, label = site), 
            #color = "black", size = 2, vjust = -1, hjust = 0.5,check_overlap = TRUE) +  
  theme_void()
   
   
print(uppersnake_stn)
ggsave("~/Library/CloudStorage/GoogleDrive-cwill104@uwyo.edu/.shortcut-targets-by-id/1JM2QoknF8B3Pc75S9OrH4J_-4ONf7H81/WYACT Team Aquatic Shared/Project Data/FLAMe/Maps/uppersnake_map.png", plot = uppersnake_stn, height = 4.5,width = 6.5,units = 'in',dpi = 1200)

####Discharge locations maps####
Q_loc<-read_sf("/Users/chuckwilliams/Downloads/Selected Area (2)/area-of-interest.shp")
Q_loc<-st_transform(Q_loc,crs=4326)
tribs_within_area3<-st_intersection(tribs_spatial,Q_loc)
#crop mainstem to area
mainstem_within_area3 <- st_intersection(site_mainstem, Q_loc)
#crop jackson lake 
jackson_within_area3<-st_intersection(jackson_waterbody,Q_loc)

Q_area<-ggplot()+
  geom_sf(data = Q_loc, fill = "lightgray", color = "black", size = 0, alpha = 0.1)+
  geom_sf(data = tribs_within_area3, color = "lightblue", size = 1)+
  geom_sf(data = jackson_within_area3, fill = "blue", color = "blue", size = 0.5, alpha = 0.4) +
  geom_sf(data = jenny_waterbody, fill = "blue", color = "blue", size = 0.5, alpha = 0.4) +
  geom_sf(data=mainstem_within_area3,color='blue',size=2)+
  annotation_scale(location = "tr")+
 # geom_sf(data = june_sf, aes(color = relative_temp), size = 3, shape = 16) +  # Add points with temperature data
 # scale_color_viridis_c(option = "D", na.value = "gray") +  # Color scale for temperature
  theme_void()

print(Q_area)  
ggsave("~/Library/CloudStorage/GoogleDrive-cwill104@uwyo.edu/.shortcut-targets-by-id/1JM2QoknF8B3Pc75S9OrH4J_-4ONf7H81/WYACT Team Aquatic Shared/Project Data/FLAMe/Maps/Q_location_map.png",plot=Q_area, height = 4.5,width = 6.5,units = 'in',dpi = 1200)
