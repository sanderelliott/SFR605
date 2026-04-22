library(tidyr)
library(dplyr)
library(glatos)
library(sf)
library(tidyverse)



getwd()
SNShst <- read.csv("FinalProject/data/SNShst_allyrs.csv")
SNSfsh <- read.csv("FinalProject/data/SNSfsh_allyrs.csv")

glimpse(SNShst)

## Remove columns for types of tags not used in analysis

SNSdetcln <- SNShst %>% 
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

glimpse(SNSfsh)

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


## Departures with seasons ---- 

detglat_sf <- SNSdetcln %>% 
  mutate(
    detection_timestamp_utc = as.POSIXct(LastTS),
    transmitter_id = as.character(IDCode),
    transmitter_codespace = as.character(TagId),
    receiver_sn = RxID,
    animal_id = FishID
  ) %>% 
  filter(!is.na(Easting)) %>% 
  st_as_sf(coords = c("Easting", "Northing"), crs = 26919) %>%   # UTM 19N
  st_transform(4326) %>%                                         # WGS84 lat/long
  mutate(
    deploy_long = st_coordinates(.)[,1],
    deploy_lat  = st_coordinates(.)[,2]
  ) %>% 
  filter(deploy_lat < 45.1) ## some false detections way north

glimpse(detglat_sf)


detglat_sf_3857 <- st_transform(detglat_sf, 3857)


glat_sf <- detglat_sf %>% 
  mutate(array = case_when(deploy_lat < 44.49 & deploy_long > -69.2 ~ 
                             "penob_bay",
                           deploy_lat > 44.49 & deploy_long > -69.2 ~
                             "penob_riv",
                           deploy_long < -69.2 ~
                             "ken_r"))


detglat_sf_3857a <- st_transform(glat_sf, 3857)


glat_events <- detection_events(glat_sf, location_col = "array", condense = TRUE) %>% 
  mutate(m_leave = month(last_detection),
         y_leave = year(last_detection), 
         d_leave = yday(last_detection),
         m_leave = month(last_detection),
         y_arrive = year(first_detection),
         d_arrive = yday(first_detection))

glat_events2 <- detection_events(glat_sf, location_col = "array", condense = FALSE)

arrive <- glat_events2 %>% 
  filter(arrive == 1)

depart <- glat_events2 %>% 
  filter(depart == 1)

penob_events <- glat_events %>%
  arrange(animal_id, last_detection) %>% 
  group_by(animal_id) %>%
  mutate(is_last = row_number() == n()) %>%   # TRUE only for final row
  filter(!(is_last & location == "penob_riv")) %>%  # drop only final penob_riv
  dplyr::select(-is_last) %>%
  ungroup()


depart_m <- penob_events %>% 
  filter(location == "penob_riv") %>% 
  mutate(m_leave = factor(m_leave, levels = 1:12, labels = month.abb))

penob_cyc <- penob_events %>% 
  filter(animal_id %in% depart_m$animal_id)

yrs <- unique(penob_events$y_leave)
yrs


for (i in 1:length(yrs)) {
  yr <- yrs[i]
  yr.rows <- penob_events[penob_events[,"y_leave"] == yr, 'd_leave']
  assign(paste0("penob.d.", yr), yr.rows$d_leave)
}

depart_early <- depart_m %>% 
  filter(d_leave < 240)

depart_early[depart_early[,'d_leave'] == 136,]

depart_spring <- depart_early %>% 
  filter(d_leave < 136)

depart_summer <- depart_early %>% 
  filter(d_leave > 136)

depart_fall <- depart_m %>% 
  filter(d_leave > 240)

min(depart_spring$d_leave)
max(depart_spring$d_leave)


min(depart_summer$d_leave)
max(depart_summer$d_leave)

min(depart_fall$d_leave)
max(depart_fall$d_leave)

fish_depart <- SNSfshcln %>% 
  filter(FishID %in% depart_m$animal_id & InitialRelease == 1)

departfsh <- fish_depart %>%
  rename(animal_id = FishID)

depart_full <- depart_m %>%
  left_join(departfsh, by = "animal_id") %>% 
  mutate(CaptureDate = as.POSIXct(CaptureDate, format = "%Y-%m-%d %H:%M:%S"),
         day_cap = as.numeric(difftime(last_detection, CaptureDate, units = "days")),
         day_syst = res_time_sec/86400) %>% 
  filter(day_syst > 2)


glimpse(depart_full)

depart_spring <- depart_early %>% 
  filter(d_leave < 136)

depart_summer <- depart_early %>% 
  filter(d_leave > 136)

depart_fall <- depart_m %>% 
  filter(d_leave > 240)

min(depart_spring$d_leave)

max(depart_summer$d_leave)

min(depart_fall$d_leave)
max(depart_fall$d_leave)

depart_ssn <- depart_full %>% 
  mutate(season = case_when(d_leave > 240 ~ "fall",
                            d_leave < 136 ~ "spring",
                            d_leave > 136 & d_leave < 240 ~ "summer")) %>% 
  rename(year = y_leave)
glimpse(depart_ssn)

# Flow data ----

library(dataRetrieval)

API_USGS_PAT = "UMOm8b4Xd4NQmdHXvgJgloj4ROEJXneryqIYXVhU"


site <- "01036390"
pcode <- "00065"  # gage height

raw <- readNWISdv("01036390", "00065", "1900-01-01", Sys.Date())


dat <- raw %>%
  rename(gage_height = X_00065_00003) %>% 
  mutate(
    jday = yday(Date),
    year = year(Date),
    season = case_when(
      jday >= 87  & jday <= 136 ~ "spring",
      jday >= 137 & jday <= 217 ~ "summer",
      jday >= 254 & jday <= 321 ~ "fall",
      TRUE ~ NA_character_)
  )

season_dat <- dat %>% filter(!is.na(season))

seasonal_means <- season_dat %>%
  group_by(year, season) %>%
  summarize(
    seasonal_mean = mean(gage_height, na.rm = TRUE),
    n_days = n(),
    .groups = "drop"
  )


ggplot(seasonal_means, aes(x = year, y = seasonal_mean, color = season)) +
  geom_line() +
  theme_bw() +
  labs(title = "Seasonal Mean Gage Height\nUSGS 01036390 Penobscot River at Eddington, ME",
       y = "Gage Height (ft)")


glimpse(depart_ssn)
glimpse(seasonal_means)

depart_flow <- depart_ssn %>%
  left_join(seasonal_means, by = c("year", "season"))

glimpse(depart_flow)

unique(depart_flow$season)

unique(depart_flow$animal_id)

snsdep <- depart_flow %>% 
  dplyr::select(season, animal_id, year, seasonal_mean, ForkLength, Mass, day_syst, day_cap) %>% 
  rename(mean_flow = seasonal_mean) %>% 
  filter(!is.na(ForkLength))









