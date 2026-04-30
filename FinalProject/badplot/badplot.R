# 1. Plot seasonal flow (all seasons)
plot(seasonal_means$year,
     seasonal_means$seasonal_mean,
     type = "n",
     xlab = "Year",
     ylab = "Seasonal Mean Flow (ft)",
     main = "Detection History with Flow")

# Add each season separately (makes it even more chaotic)
seasons <- unique(seasonal_means$season)
cols <- c("spring" = "green", "summer" = "orange", "fall" = "blue")

for (s in seasons) {
  ss <- subset(seasonal_means, season == s)
  lines(ss$year, ss$seasonal_mean, col = cols[s], lwd = 3)
}

# 2. Overlay transmitter IDs (as factor)
par(new = TRUE)

plot(glat_sf$detection_timestamp_utc,
     as.numeric(factor(glat_sf$transmitter_id)),
     pch = 16,
     col = rgb(1, 0, 0, 0.2),
     axes = FALSE,
     xlab = "",
     ylab = "",
     cex = 0.5)

# 3. Add fake second axis for transmitter IDs
axis(4, col.axis = "red")
mtext("Transmitter ID", side = 4, line = 3, col = "red")


## worser

# 1. Plot seasonal flow (all seasons)
plot(seasonal_means$year,
     seasonal_means$seasonal_mean,
     type = "n",
     xlab = "Year",
     ylab = "Seasonal Mean Flow (ft)",
     main = "Detection History with Flow")
legend("topleft",
       legend = names(cols),
       col = cols,
       lwd = 3,
       title = "Flow Season")

# Add each season separately
seasons <- unique(seasonal_means$season)
cols <- c("spring" = "green", "summer" = "orange", "fall" = "blue")

for (s in seasons) {
  ss <- subset(seasonal_means, season == s)
  lines(ss$year, ss$seasonal_mean, col = cols[s], lwd = 3)
}

# 2. Overlay transmitter IDs (as factor) colored by RxID
par(new = TRUE)

# Build a color palette for RxID
rxid_factor <- factor(glat_sf$RxID)
rxid_colors <- rainbow(length(levels(rxid_factor)))[rxid_factor]

plot(glat_sf$detection_timestamp_utc,
     as.numeric(factor(glat_sf$transmitter_id)),
     pch = 16,
     col = rxid_colors,
     axes = FALSE,
     xlab = "",
     ylab = "",
     cex = 0.4)
legend("bottomright",
       legend = levels(rxid_factor),
       col = rainbow(length(levels(rxid_factor))),
       pch = 16,
       cex = 0.5,
       title = "RxID")

# 3. Add fake second axis for transmitter IDs
axis(4, col.axis = "red")
mtext("Transmitter ID (factor, nonsense scale)", side = 4, line = 3, col = "red")
