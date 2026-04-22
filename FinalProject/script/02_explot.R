library(rosm)
library(raster) 


glimpse(SNSdetcln)

bb <- st_bbox(detglat_sf)
osm <- osm.raster(bb, type = "cartolight")
osm_df <- as.data.frame(osm, xy = TRUE)

detglat_sf_3857 <- st_transform(detglat_sf, 3857)

ggplot() +
  geom_raster(data = osm_df,
              aes(x = x, y = y,
                  fill = rgb(layer.1/255, layer.2/255, layer.3/255))) +
  scale_fill_identity() +
  geom_sf(data = detglat_sf_3857, color = "red", size = 0.5) +
  coord_sf(crs = 3857) +
  theme_minimal() +
  labs(title = "Detection Locations",
       x = "LONG",
       y = "LAT")

## Create Three arrays ----

glat_sf <- detglat_sf %>% 
  mutate(array = case_when(deploy_lat < 44.49 & deploy_long > -69.2 ~ 
                             "penob_bay",
                           deploy_lat > 44.49 & deploy_long > -69.2 ~
                             "penob_riv",
                           deploy_long < -69.2 ~
                             "ken_r"))


detglat_sf_3857a <- st_transform(glat_sf, 3857)

ggplot() +
  geom_raster(data = osm_df,
              aes(x = x, y = y,
                  fill = rgb(layer.1/255, layer.2/255, layer.3/255))) +
  scale_fill_identity() +
  geom_sf(data = detglat_sf_3857a, aes(color = array)) +
  coord_sf(crs = 3857) +
  theme_minimal() +
  labs(title = "Detection Locations",
       x = "LONG",
       y = "LAT")


## Abacus Plots ----

abacus_plot(glat_sf, location_col = "array")

ggplot(glat_sf, aes(x=detection_timestamp_utc, y=transmitter_id, color=array))+
  geom_point()+xlab("Detection Timestamp") + ylab("Transmitter ID")


## Create Events ----

ggplot(depart_m, aes(x = d_leave)) +
  geom_histogram(bins = 52, boundary = 0.5) +
  coord_cartesian(xlim = c(1, 365)) +
  theme_classic() +
  labs(
    title = "Departures by Week",
    subtitle = "Shortnose Sturgeon from Penobscot River",
    x = "Week",
    y = "Frequency")

ggplot(depart_m, aes(x = d_leave)) +
  geom_histogram(bins = 52, boundary = 0.5) +
  coord_cartesian(xlim = c(1, 365)) +
  theme_classic() +
  facet_wrap(~y_leave) +
  labs(
    title = "Departures by Week Over Multiple Years",
    subtitle = "Shortnose Sturgeon from Penobscot River",
    x = "Week",
    y = "Frequency")

ggplot(depart_m, aes(x = d_leave, fill = factor(y_leave))) +
  geom_histogram(
    bins = 52,
    boundary = 0.5,
    position = "stack" 
  ) +
  coord_cartesian(xlim = c(1, 365)) +
  theme_classic() +
  labs(
    title = "Departures by Week Over Multiple Years",
    subtitle = "Shortnose Sturgeon from Penobscot River",
    x = "Julian Day",
    y = "Frequency",
    fill = "Year"
  )

ggplot(depart_m, aes(x = d_leave)) +
  geom_density(fill = "steelblue", alpha = 0.4) +
  coord_cartesian(xlim = c(1, 365)) +
  theme_classic() +
  labs(
    title = "Density of Departure Julian Days",
    subtitle = "Shortnose Sturgeon from Penobscot River",
    x = "Julian Day",
    y = "Density"
  )

ggplot(depart_m, aes(x = d_leave)) +
  geom_histogram(aes(y = ..density..), bins = 52, fill = "grey80", color = "white") +
  geom_density(color = "blue", size = 1) +
  coord_cartesian(xlim = c(1, 365)) +
  theme_classic() +
  labs(
    title = "Departure Timing Distribution",
    subtitle = "Shortnose Sturgeon from Penobscot River",
    x = "Julian Day",
    y = "Density"
  )

## Abacus plot of animals in penob events


ggplot(penob_cyc, aes(
  y = animal_id,
  x = first_detection,
  xend = last_detection,
  yend = animal_id,
  color = location)) +
  geom_segment(linewidth = 1.2, alpha = 0.8) +
  labs(
    x = "Detection Interval",
    y = "Transmitter ID",
    color = "Location") +
  geom_point(aes(x = first_detection), size = 2) + 
  geom_point(aes(x = last_detection), size = 2) +
  theme_classic()



