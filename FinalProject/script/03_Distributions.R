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

depart_early <- depart_m %>% 
  filter(d_leave < 240)

ggplot(depart_early, aes(x = d_leave, y = y_leave)) +
  geom_point() +
  theme_classic() +
  xlab("Departure Date") + 
  ylab("Year")

