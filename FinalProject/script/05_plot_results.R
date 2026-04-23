## Results

glimpse(snsdep)

library(ggplot2)
library(dplyr)
library(tidyr)

# New data for prediction
newdat <- expand.grid(
  arrive_ssn = levels(snsdep$arrive_ssn)
)

# Population-level predictions only
pp <- fitted(
  fit_simp,
  newdata = newdat,
  re_formula = NA,
  summary = TRUE
) %>%
  as.data.frame() %>%
  mutate(arrive_ssn = newdat$arrive_ssn) %>%
  pivot_longer(
    cols = starts_with("Estimate"),
    names_to = "season",
    values_to = "prob"
  )

# Clean season names
pp$season <- recode(pp$season,
                    "Estimate.spring" = "spring",
                    "Estimate.summer" = "summer",
                    "Estimate.fall" = "fall")

# Plot
ggplot(pp, aes(x = arrive_ssn, y = prob, fill = season)) +
  geom_col(position = "dodge") +
  labs(
    x = "Arrival season",
    y = "Predicted probability of departure season",
    fill = "Departure season",
    title = "Predicted departure probabilities by arrival season"
  ) +
  theme_bw(base_size = 14)

# Arrival plot

pp <- as.data.frame(
  fitted(
    fit_simp,
    newdata = newdat,
    re_formula = NA,
    summary = TRUE
  )
)

pp$arrive_ssn <- newdat$arrive_ssn

pp_long <- pp %>%
  select(
    arrive_ssn,
    `Estimate.P(Y = spring)`, `Q2.5.P(Y = spring)`, `Q97.5.P(Y = spring)`,
    `Estimate.P(Y = summer)`, `Q2.5.P(Y = summer)`, `Q97.5.P(Y = summer)`,
    `Estimate.P(Y = fall)`,   `Q2.5.P(Y = fall)`,   `Q97.5.P(Y = fall)`
  ) %>%
  pivot_longer(
    cols = -arrive_ssn,
    names_to = c(".value", "season"),
    names_pattern = "(.*)\\.P\\(Y = (.*)\\)"
  ) %>%
  rename(
    prob = Estimate,
    prob_low = `Q2.5`,
    prob_high = `Q97.5`
  )


arrivals <- unique(pp_long$arrive_ssn)
seasons  <- unique(pp_long$season)

prob_mat <- tapply(pp_long$prob,
                   list(pp_long$season, pp_long$arrive_ssn),
                   mean)

par(mar = c(6, 6, 4, 2))

bp <- barplot(prob_mat,
              beside = TRUE,
              col = c("brown", "forestgreen", "goldenrod"),
              ylim = c(0, 1),
              ylab = "Predicted probability",
              xlab = "Arrival season",
              main = "Predicted departure probabilities by arrival season")

legend("topright", legend = seasons,
       fill = c("brown", "forestgreen", "goldenrod"))

# Extract CI bounds in the same order as prob_mat
low  <- tapply(pp_long$prob_low,  list(pp_long$season, pp_long$arrive_ssn), mean)
high <- tapply(pp_long$prob_high, list(pp_long$season, pp_long$arrive_ssn), mean)

# Add error bars
arrows(bp, low, bp, high,
       angle = 90, code = 3, length = 0.05)


### Anomoly plot 

# Sequence of summer flow anomaly values
flow_seq <- seq(
  min(snsdep$summer_flow_z, na.rm = TRUE),
  max(snsdep$summer_flow_z, na.rm = TRUE),
  length.out = 100
)

# New data for prediction
newdat_flow <- expand.grid(
  summer_flow_z = flow_seq,
  arrive_ssn = "summer",
  Mass = mean(snsdep$Mass, na.rm = TRUE),
  day_cap = mean(snsdep$day_cap, na.rm = TRUE),
  day_syst = mean(snsdep$day_syst, na.rm = TRUE)
)

# Get summary predictions
pp <- as.data.frame(
  fitted(
    fit_summer_z,
    newdata = newdat_flow,
    re_formula = NA,
    summary = TRUE
  )
)

# Add flow values
pp$summer_flow_z <- newdat_flow$summer_flow_z

# Reshape
pp_long <- pp %>%
  select(
    summer_flow_z,
    `Estimate.P(Y = spring)`, `Q2.5.P(Y = spring)`, `Q97.5.P(Y = spring)`,
    `Estimate.P(Y = summer)`, `Q2.5.P(Y = summer)`, `Q97.5.P(Y = summer)`,
    `Estimate.P(Y = fall)`,   `Q2.5.P(Y = fall)`,   `Q97.5.P(Y = fall)`
  ) %>%
  pivot_longer(
    cols = -summer_flow_z,
    names_to = c(".value", "season"),
    names_pattern = "(.*)\\.P\\(Y = (.*)\\)"
  ) %>%
  rename(
    prob = Estimate,
    prob_low = `Q2.5`,
    prob_high = `Q97.5`
  )

par(mar = c(6, 6, 4, 2))

plot(pp_long$summer_flow_z, pp_long$prob,
     type = "n",
     ylim = c(0, 1),
     xlab = "Summer flow anomaly (z-score)",
     ylab = "Predicted probability",
     main = "Effect of summer flow anomalies on departure timing")

cols <- c("spring" = "brown",
          "summer" = "forestgreen",
          "fall"   = "goldenrod")

seasons <- unique(pp_long$season)

for (s in seasons) {
  dat <- subset(pp_long, season == s)
  
  # CI ribbon
  polygon(
    c(dat$summer_flow_z, rev(dat$summer_flow_z)),
    c(dat$prob_low, rev(dat$prob_high)),
    col = adjustcolor(cols[s], alpha.f = 0.2),
    border = NA
  )
  
  # Median line
  lines(dat$summer_flow_z, dat$prob, col = cols[s], lwd = 2)
}

legend("topright", legend = seasons, col = cols, lwd = 2, bty = "n")

tab <- table(snsdep$arrive_ssn, snsdep$season)

mosaicplot(tab,
           color = TRUE,
           main = "Observed relationship between arrival and departure season",
           xlab = "Arrival season",
           ylab = "Departure season")

