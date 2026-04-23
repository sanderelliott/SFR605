
glimpse(snsdep)

snsdep$season <- factor(snsdep$season, levels = c("spring", "summer", "fall"))


mult_fish <- snsdep %>% 
  group_by(animal_id) %>% 
  summarise(n())

snsdep %>% 
     group_by(arrive_ssn) %>% 
     summarise(n())






library(brms)

#Try 1
fit <- brm(
  formula = season ~ 
    mean_flow + ForkLength + Mass + day_syst + day_cap +
    (1 | year) + (1 | animal_id),
  data = snsdep,
  family = categorical(),
  chains = 4,
  cores = 4,
  iter = 4000
)

# try 2
fit0 <- brm(
  season ~ 1 + (1 | animal_id) + (1 | year),
  data = snsdep,
  family = categorical(),
  chains = 1, iter = 500
)

# Try 3
glimpse(snsdep)
snsdep$season <- factor(snsdep$season, levels = c("spring", "summer", "fall"))
snsdep$arrive_ssn <- factor(snsdep$arrive_ssn,
                            levels = c("summer", "spring", "fall"))



priors <- c(
  # Fixed effects
  set_prior("normal(0, 1)", class = "b", dpar = "musummer"),
  set_prior("normal(0, 1)", class = "b", dpar = "mufall"),
  
  # Intercepts
  set_prior("normal(0, 1)", class = "Intercept", dpar = "musummer"),
  set_prior("normal(0, 1)", class = "Intercept", dpar = "mufall"),
  
  # Random effect SDs
  set_prior("exponential(1)", class = "sd", dpar = "musummer"),
  set_prior("exponential(1)", class = "sd", dpar = "mufall")
)


fit_arrive_ssn <- brm(
  season ~ spring_flow + summer_flow + fall_flow  + Mass + arrive_ssn +
    (1 | animal_id) + (1 | year),
  data = snsdep,
  family = categorical(),
  prior = priors,
  chains = 4, cores = 4, iter = 6000,
  control = list(adapt_delta = 0.99, max_treedepth = 15)
)

summary(fit_arrive_ssn)

fit_int <- brm(
  season ~ arrive_ssn * day_syst +
    Mass * day_cap +
    spring_flow + summer_flow + fall_flow +
    (1 | animal_id) + (1 | year),
  data = snsdep,
  family = categorical(),
  prior = priors,   # the ones we already built
  chains = 4, cores = 4, iter = 6000,
  control = list(adapt_delta = 0.99, max_treedepth = 15)
)


summary(fit_int)

loo(fit_arrive_ssn, fit_int)

fit_simp <- brm(
  season ~ arrive_ssn + (1 | animal_id) + (1 | year),
  data = snsdep,
  family = categorical(),
  prior = priors,   # the ones we already built
  chains = 4, cores = 4, iter = 6000,
  control = list(adapt_delta = 0.99, max_treedepth = 15)
)

summary(fit_int)

fit_spring_z <- brm(
  season ~ spring_flow_z + arrive_ssn + Mass + day_cap + day_syst +
    (1 | animal_id) + (1 | year),
  data = snsdep,
  family = categorical(),
  chains = 4, iter = 6000, cores = 4,
  control = list(adapt_delta = 0.99)
)

summary(fit_spring_z)

fit_summer_z <- brm(
  season ~ summer_flow_z + arrive_ssn + Mass + day_cap + day_syst +
    (1 | animal_id) + (1 | year),
  data = snsdep,
  family = categorical()
)

summary(fit_summer_z)

fit_fall_z <- brm(
  season ~ fall_flow_z + arrive_ssn + Mass + day_cap + day_syst +
    (1 | animal_id) + (1 | year),
  data = snsdep,
  family = categorical()
)

summary(fit_fall_z)

loo(fit_spring_z, fit_summer_z, fit_fall_z)



fit_spring_summer_z <- brm(
  season ~ spring_flow_z * summer_flow_z + arrive_ssn + Mass + day_cap + day_syst +
    (1 | animal_id) + (1 | year),
  data = snsdep,
  family = categorical()
)


fit_spring_fall_z <- brm(
  season ~ spring_flow_z * fall_flow_z + arrive_ssn + Mass + day_cap + day_syst +
    (1 | animal_id) + (1 | year),
  data = snsdep,
  family = categorical()
)

fit_summer_fall_z <- brm(
  season ~ summer_flow_z * fall_flow_z + arrive_ssn + Mass + day_cap + day_syst +
    (1 | animal_id) + (1 | year),
  data = snsdep,
  family = categorical()
)


loo(fit_simp, fit_spring_z, fit_summer_z, fit_fall_z, fit_spring_summer_z, fit_spring_fall_z,
    fit_summer_fall_z)












