1## Evaluate Data

## KS tests

yrs <- unique(snsdep$year)
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


ggplot(depart_early, aes(x = d_leave, y = y_leave)) +
  geom_point() +
  theme_classic() +
  xlab("Departure Date") + 
  ylab("Year")


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



## Fish in departures



ggplot(snsdep, aes(x = ForkLength)) +
  geom_histogram(aes(y = ..density..), fill = "grey80", color = "white") +
  geom_density(color = "blue", size = 1) +
  theme_classic() +
  labs(
    title = "Fork length",
    subtitle = "Shortnose Sturgeon from Penobscot River",
    x = "Length",
    y = "Frequency")

ggplot(snsdep, aes(x = Mass)) +
  geom_histogram(aes(y = ..density..), fill = "grey80", color = "white") +
  geom_density(color = "blue", size = 1) +
  theme_classic() +
  labs(
    title = "Mass",
    subtitle = "Shortnose Sturgeon from Penobscot River",
    x = "Mass",
    y = "Frequency")


ggplot(snsdep, aes(x = day_cap)) +
  geom_histogram(aes(y = ..density..), fill = "grey80", color = "white") +
  geom_density(color = "blue", size = 1) +
  theme_classic() +
  labs(
    title = "Days after capture",
    subtitle = "Shortnose Sturgeon from Penobscot River",
    x = "Mass",
    y = "Frequency")

ggplot(snsdep, aes(x = day_syst)) +
  geom_histogram(aes(y = ..density..), fill = "grey80", color = "white") +
  geom_density(color = "blue", size = 1) +
  theme_classic() +
  labs(
    title = "Days in System",
    subtitle = "Shortnose Sturgeon from Penobscot River",
    x = "Mass",
    y = "Frequency")




