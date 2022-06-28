library(tidyverse)
library(purrr)
library(here)

source(here('R/funcs.R'))

data(vegdat)

# sit <- 'Big Bend - TECO'
# smp <- 1

# setup downsampling for each site, sample
sampint <- seq(0.5, 10, by = 0.5)
downsmps <- vegdat %>% 
  group_by(site, sample) %>% 
  nest() %>% 
  crossing(sampint = sampint) %>% 
  mutate(
    unimeter = map(data, function(data) data %>% pull(meter) %>% unique),
    smps = pmap(list(sampint, unimeter), function(sampint, unimeter){
      
      strlocs <- seq(min(unimeter), min(unimeter) + sampint, by = 0.5)
      lapply(as.list(strlocs), function(x) seq(x, max(unimeter), by = sampint)) %>% 
        enframe('rep', 'smps')
      
    })
  ) %>% 
  unnest('smps') %>% 
  filter((sampint == 0.5 & rep == 1) | sampint > 0.5) %>% 
  mutate(
    downsmp = pmap(list(data, smps), function(data, smps){
      data %>% 
        filter(meter %in% smps)
    })
  ) %>% 
  select(-data, -unimeter, -smps)

rchests <- downsmps %>%  
  mutate(
    spprch = map(downsmp, function(downsmp){
      
      downsmp %>% 
        filter(!species %in% rmv) %>% 
        pull(species) %>% 
        unique %>% 
        length
  
    })
  )

# toplo <- rchests %>% 
#   select(-smps, -downsmp) %>%
#   unnest('downest') %>% 
#   filter(!species %in% rmv) %>% 
#   group_by(sampint, site, zonefct, species) %>% 
#   summarise(est = mean(`Year 1`), .groups = 'drop')# %>% 
#   # filter(rep == 1) %>% 
#   # filter(species == 'Avicennia germinans')

# ggplot(toplo, aes(x = sampint, y = est, group = species, color = species)) + 
#   geom_line() + 
#   geom_point() + 
#   facet_wrap(~zonefct)
  
toplo <- rchests %>% 
  select(-downsmp) %>%
  unnest('spprch') %>% 
  group_by(site, sample, sampint) %>% 
  summarise(spprch = mean(spprch), .groups = 'drop')

ggplot(toplo, aes(x = sampint, y = spprch, color = sample, group = sample, fill= sample)) + 
  # geom_line() +
  geom_point() +
  facet_wrap(~site) +
  geom_smooth(se = F)
