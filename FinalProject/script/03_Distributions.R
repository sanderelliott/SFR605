1## Evaluate Data

## KS tests

yrs <- unique(penob_events$y_leave)
yrs


for (i in 1:length(yrs)) {
  yr <- yrs[i]
  yr.rows <- penob_events[penob_events[,"y_leave"] == yr, 'd_leave']
    assign(paste0("penob.d.", yr), yr.rows$d_leave)
}

ggplot(depart_m, aes(x = d_leave)) +
  geom_histogram(bins = 52, boundary = 0.5) +

  theme_classic() +
 
  labs(
    title = "Departures by Week Over Multiple Years",
    subtitle = "Shortnose Sturgeon from Penobscot River",
    x = "Week",
    y = "Frequency")

ggplot(depart_m, aes(x = d_leave, y = y_leave)) +
  geom_point() +
  geom_vline(xintercept =240) +
  geom_vline(xintercept =136) +
  theme_classic() +
  xlab("Departure Date") + 
  ylab("Year")


ggplot(depart_m, aes(x = d_leave)) +
  geom_histogram(bins = 52, boundary = 0.5) +
  geom_vline(xintercept =240) +
  geom_vline(xintercept =136) +
  coord_cartesian(xlim = c(1, 365)) +
  theme_classic() +
  facet_wrap(~y_leave) +
  labs(
    title = "Departures by Week Over Multiple Years",
    subtitle = "Shortnose Sturgeon from Penobscot River",
    x = "Week",
    y = "Frequency")

ggplot(depart_m, aes(x = d_leave)) +
  geom_histogram(aes(y = ..density..), bins = 52, fill = "grey80", color = "white") +
  geom_density(color = "blue", size = 1) +
  coord_cartesian(xlim = c(1, 365)) +
  geom_vline(xintercept =240) +
  geom_vline(xintercept =136) +
  theme_classic() +
  labs(
    title = "Departure Timing Distribution",
    subtitle = "Shortnose Sturgeon from Penobscot River",
    x = "Julian Day",
    y = "Density"
  )

ggplot(depart_m, aes(x = d_arrive)) +
  geom_histogram(aes(y = ..density..), bins = 52, fill = "grey80", color = "white") +
  geom_density(color = "blue", size = 1) +
  coord_cartesian(xlim = c(1, 365)) +
  geom_vline(xintercept =240) +
  geom_vline(xintercept =136) +
  theme_classic() +
  labs(
    title = "Arrival Timing Distribution",
    subtitle = "Shortnose Sturgeon from Penobscot River",
    x = "Julian Day",
    y = "Density"
  )

depart_early <- depart_m %>% 
  filter(d_leave < 240)

ggplot(depart_early, aes(x = d_leave, y = y_leave)) +
  geom_point() +
  theme_classic() +
  xlab("Departure Date") + 
  ylab("Year")

depart_early[depart_early[,'d_leave'] == 136,]

depart_spring <- depart_early %>% 
  filter(d_leave < 136)

ggplot(depart_spring, aes(x = d_leave)) +
  geom_histogram(aes(y = ..density..), fill = "grey80", color = "white") +
  geom_density(color = "blue", size = 1) +
  theme_classic() +
  labs(
    title = "Departure Timing Distribution",
    subtitle = "Shortnose Sturgeon from Penobscot River",
    x = "Julian Day",
    y = "Density"
  )

depart_summer <- depart_early %>% 
  filter(d_leave > 136)

ggplot(depart_summer, aes(x = d_leave)) +
  geom_histogram(aes(y = ..density..), fill = "grey80", color = "white") +
  geom_density(color = "blue", size = 1) +
  theme_classic() +
  labs(
    title = "Departure Timing Distribution",
    subtitle = "Shortnose Sturgeon from Penobscot River",
    x = "Julian Day",
    y = "Density"
  )

depart_fall <- depart_m %>% 
  filter(d_leave > 240)

ggplot(depart_fall, aes(x = d_leave)) +
  geom_histogram(aes(y = ..density..), fill = "grey80", color = "white") +
  geom_density(color = "blue", size = 1) +
  theme_classic() +
  labs(
    title = "Departure Timing Distribution",
    subtitle = "Shortnose Sturgeon from Penobscot River",
    x = "Julian Day",
    y = "Density"
  )

min(depart_spring$d_leave)
max(depart_spring$d_leave)


min(depart_summer$d_leave)
max(depart_summer$d_leave)

min(depart_fall$d_leave)
max(depart_fall$d_leave)

depart_ssn <- depart_m %>% 
  mutate(season <- case_when())

## Fish in departures

fish_depart <- SNSfshcln %>% 
  filter(FishID %in% depart_m$animal_id & InitialRelease == 1)

ggplot(fish_depart, aes(x = ForkLength)) +
  geom_histogram(aes(y = ..density..), fill = "grey80", color = "white") +
  geom_density(color = "blue", size = 1) +
  theme_classic() +
  labs(
    title = "Fork length",
    subtitle = "Shortnose Sturgeon from Penobscot River",
    x = "Length",
    y = "Frequency")

ggplot(fish_depart, aes(x = Mass)) +
  geom_histogram(aes(y = ..density..), fill = "grey80", color = "white") +
  geom_density(color = "blue", size = 1) +
  theme_classic() +
  labs(
    title = "Mass",
    subtitle = "Shortnose Sturgeon from Penobscot River",
    x = "Mass",
    y = "Frequency")

departfsh <- fish_depart %>%
  rename(animal_id = FishID)

depart_full <- depart_m %>%
  left_join(departfsh, by = "animal_id") %>% 
  mutate(CaptureDate = as.POSIXct(CaptureDate, format = "%Y-%m-%d %H:%M:%S"),
         day_cap = as.numeric(difftime(last_detection, CaptureDate, units = "days")),
         day_syst = res_time_sec/86400) %>% 
  filter(day_syst > 2)


glimpse(depart_full)

ggplot(depart_full, aes(x = day_cap)) +
  geom_histogram(aes(y = ..density..), fill = "grey80", color = "white") +
  geom_density(color = "blue", size = 1) +
  theme_classic() +
  labs(
    title = "Days after capture",
    subtitle = "Shortnose Sturgeon from Penobscot River",
    x = "Mass",
    y = "Frequency")

ggplot(depart_full, aes(x = day_syst)) +
  geom_histogram(aes(y = ..density..), fill = "grey80", color = "white") +
  geom_density(color = "blue", size = 1) +
  theme_classic() +
  labs(
    title = "Days in System",
    subtitle = "Shortnose Sturgeon from Penobscot River",
    x = "Mass",
    y = "Frequency")





