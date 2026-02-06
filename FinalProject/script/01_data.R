library(tidyr)
library(dplyr)
library(glatos)
library(sf)
library(tidyverse)



getwd()
SNSdet <- read.csv("FinalProject/data/SNSdet_allyrs.csv")
SNSfsh <- read.csv("FinalProject/data/SNSfsh_allyrs.csv")

glimpse(SNSdet)

## Remove columns for types of tags not used in analysis

SNSdetcln <- SNSdet %>% 
  filter(Event == "Detection") %>% 
  dplyr::select(-Event, -Species, -tagtype, -SensorType, -avgSensorValue, -avgComputedValue, -Frequency, -avgPower,
         -LocationCode, -AntennaID, -AltFishID)

Detection_Data <- names(SNSdetcln)

Detection_Data

Detection_Unit <- c("YYYY-MM-DD HH:MM:SS", "Identifier", "Identifier", "Integer",
          "YYYY-MM-DD HH:MM:SS", "YYYY-MM-DD HH:MM:SS", "Identifier", "Identifier", "Identifier", "Identifier", 
          "Universal Transverse Mercator", "Universal Transverse Mercator", "Kilometers", 
          "Universal Transverse Mercator", "Identifier")

Detection_Description <- c("Date when event occurs", "ID code; One code per fish", "Tag code; Identifies the tag being recorded",
                 "Number of times the tag pinged the receiver during the detection", "Time and date of the beginning of the detection",
                 "Time and date of the end of the detection", "Describes how the tag is coded", 
                 "Short code that differentiates the tag from other tags with the same codespace", "Code that identifies the receiver",
                 "Code that corresponds with a specific deployment for said receiver", "Coordinates of receiver", 
                 "Coordinates of receiver", "UTM zone; in this case always 19","Code that corresponds to certain sites"
                 )

Detection_metatable <- cbind(Detection_Data, Detection_Unit, Detection_Description)



SNSfshcln <- SNSfsh %>% 
  dplyr::select(FishID, TagID, CaptureDate, ForkLength, TotalLength, Mass, Recapture, InitialRelease, Sex)

Fish_Data <- names(SNSfshcln)

Fish_Data

Fish_Unit <- c("Identifier", "Identifier", "YYYY-MM-DD HH:MM:SS", "Centimeters", "Centimeters", "Kilograms", 
               "Flag", "Flag", "Category")



Fish_Description <- c("ID code; One code per fish", "Tag code; Identifies the tag being recorded", "Date that fish was captured",
                           "Length measured along the side of body from tip of nose to fork in caudal fin",
                           "Length measured along the side of body from tip of nose to tip of upper caudal fin",
                           "Weight of fish", "1 if fish had been captured before", "1 if this is the first time fish had been captured"
)

Fish_Description

Fish_metatable <- cbind(Fish_Data, Fish_Unit, Fish_Description)

Fish_metatable

#write.csv(SNSdetcln, "FinalProject/outputs/SNSdet.csv", row.names = FALSE)
#write.csv(Detection_metatable, "FinalProject/outputs/SNSdetmetadata.csv", row.names = FALSE)
#write.csv(SNSfshcln, "FinalProject/outputs/SNSfsh.csv", row.names = FALSE)
#write.csv(Detection_metatable, "FinalProject/outputs/SNSfshmetadata.csv", row.names = FALSE)

number_fish <- SNSfshcln %>% 
  summarise(n_distinct(FishID))
number_fish


glimpse(SNSdetcln)


detglat <- SNSdetcln %>% 
  mutate(detection_timestamp_utc = as.POSIXct(LastTS),
         transmitter_id = as.character(IDCode),
         transmitter_codespace = as.character(TagId),
         receiver_sn = RxID) %>% 
  filter(! is.na(Easting)) %>% 
  st_as_sf(coords = c("Easting", "Northing"), crs = 26919) %>%   # NAD83 / UTM zone 19N
  st_transform(4269) %>%
  mutate(
    deploy_long = st_coordinates(.)[, 1],
    deploy_lat = st_coordinates(.)[, 2],
    animal_id = FishID) %>%
  st_drop_geometry() %>% 
  filter(deploy_lat < 45.1)

glimpse(detglat)


det_sf <- detglat %>%
  st_as_sf(coords = c("deploy_long", "deploy_lat"), crs = 4326)

bb <- st_bbox(det_sf)

osm <- osm.raster(bb, type = "cartolight")

osm_df <- as.data.frame(osm, xy = TRUE)

det_sf_3857 <- st_transform(det_sf, 3857)

ggplot() +
  geom_raster(data = osm_df, aes(x = x, y = y, fill = layer.1)) +
  scale_fill_gradient(low = "grey90", high = "grey60") +
  geom_sf(data = det_sf_3857, color = "red", size = 0.5) +
  coord_sf(crs = 3857) +
  theme_classic() +
  labs(title = "Detection Locations",
       x = "Longitude",
       y = "Latitude")











