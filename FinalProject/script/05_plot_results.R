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
