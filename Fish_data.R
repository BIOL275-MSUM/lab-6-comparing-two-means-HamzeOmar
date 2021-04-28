

# load packages -----------------------------------------------------------

library(tidyverse)

# read data ---------------------------------------------------------------

fish <- read_csv("chap12q19ElectricFish(1).csv")

# do stuff ----------------------------------------------------------------

glimpse(fish)

fish_long <-
  pivot_longer(
    fish,
    speciesUpstream:speciesDownstream,
    names_to = "location",
    values_to = "species"
  ) %>%
  mutate(location = str_remove(location, c("species"))) %>%
  print()

fish_long_data <- fish_long %>%
  group_by(location) %>%
  summarize(
    n = n(),
    mean = mean(species),
    sd = sd(species),
    sem = sd / sqrt(n),
    upper = mean + 1.96 * sem,
    lower = mean - 1.96 * sem
  ) %>%
  print()

t.test(formula = species ~ location, data = fish_long)

# Graph of means ----------------------------------------------------------


fish_long %>%
  ggplot(aes(x = location, y = species)) +
  geom_jitter(
    aes(color = location),
    shape = 19,
    size = 2,
    alpha = 0.2,
    width = 0.3
  ) +
  geom_errorbar(
    aes(y = mean, ymax = upper, ymin = lower),
    data = fish_long_data,
    width = .1,
    size = .8
  ) +
  geom_point(aes(y = mean),
             data = fish_long_data,
             size = 3) +
  scale_color_manual(values = c("dark blue", "red")) +
  theme_minimal() +
  guides(color = "none")

# Graph mean diff

fish_long %>%
  ggplot(aes(x = species)) +
  geom_histogram(
    aes(fill = location),
    bins = 20,
    alpha = 0.5,
    position = "identity"
  ) +
  scale_fill_manual(values = c("darkorange", "cyan4")) +
  theme_minimal()

# crab fans ---------------------------------------------------------------

crab_fans_data <- read_csv("chap15q27FiddlerCrabFans.csv") %>%
  rename(type = crabType, temperature = bodyTemperature)
crab_fans_data

# filter NA values temperature --------------------------------------------

  
  temperature_means <-
    crab_fans_data %>%
    filter(!is.na(temperature)) %>%      # remove missing values
    group_by(type) %>%
    summarize(
      mean = mean(temperature),
      sd = sd(temperature),
      n = n(),
      sem = sd / sqrt(n),
      upper = mean + 1.96 * sem,
      lower = mean - 1.96 * sem
    ) %>%
    print()     

# Plot a graph ------------------------------------------------------------

  ggplot(data = crab_fans_data, aes(x = type, y = temperature)) +
    geom_jitter(
      aes(color = type),
      width = 0.1,
      alpha = 0.7,
      show.legend = FALSE,
      na.rm = TRUE
    ) +
    geom_errorbar(
      aes(y = mean, ymin = lower, ymax = upper),
      data = temperature_means,
      width = .1,
      position = position_nudge(.3)
    ) +
    geom_point(aes(y = mean), data = temperature_means,
               position = position_nudge(.3)) +
    scale_color_manual(values = c("darkorange", "darkorchid", "cyan4", "red"))
  

# Distribution graph ------------------------------------------------------

  crab_fans_data %>% filter(!is.na(type)) %>%
    ggplot(aes(x = temperature)) +
    geom_histogram(
      aes(fill = type),
      bins = 10,
      alpha = 0.5,
      position = "identity",
      na.rm = TRUE
    ) + scale_fill_manual(values = c("darkorange", "cyan4", "darkorchid", "red")) +
    theme_minimal() +
    facet_wrap( ~ type, ncol = 1)

# Anova test for CrabFansData ---------------------------------------------

  
  aov_bill_depth_species <-
    aov(temperature ~ type, data = crab_fans_data)
  
  aov_bill_depth_species
  
  summary(aov_bill_depth_species)
  