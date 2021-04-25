
# load packages -----------------------------------------------------------

library(tidyverse)

# read data ---------------------------------------------------------------

fish <- read_csv("chap12q19ElectricFish(1).csv")

# do stuff ----------------------------------------------------------------

glimpse(fish)

fish_long <- 
  pivot_longer(fish, speciesUpstream:speciesDownstream,
               names_to = "location",
               values_to = "species") %>% 
  mutate(location = str_remove(location, c("species"))) %>% 
  print()

fish_long_data <- fish_long %>% 
  group_by(location) %>% 
  summarize(
    n = n(),
    mean = mean(species),
    sd = sd(species),
    sem = sd/sqrt(n),
    upper = mean + 1.96 * sem,
    lower = mean - 1.96 * sem
  ) %>% 
  print()
  
  t.test(formula = species ~ location, data = fish_long)

# Graph of means ----------------------------------------------------------

  
  fish_long %>% 
    ggplot(aes(x = location, y = species)) +
    geom_jitter(aes(color = location), 
                shape = 19, size = 2, 
                alpha = 0.2, width = 0.3) +
    geom_errorbar(aes(y = mean, ymax = upper, ymin = lower), 
                  data = fish_long_data, 
                  width = .1, size = .8) +
    geom_point(aes(y = mean), 
               data = fish_long_data, 
               size = 3) +
    scale_color_manual(values = c("dark blue","red")) +
    theme_minimal() +
    guides(color = "none")
    
  
    
