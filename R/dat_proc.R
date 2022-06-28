library(tidyverse)
library(purrr)
library(here)

# copy data from ccha-workflow --------------------------------------------

load(url('https://github.com/tbep-tech/ccha-workflow/raw/main/data/vegdat.RData'))
load(url('https://github.com/tbep-tech/ccha-workflow/raw/main/data/tranloc.RData'))

save(tranloc, file = here('data/tranloc.RData'))
save(vegdat, file = here('data/vegdat.RData'))

# create sub samples ------------------------------------------------------

data(vegdat)

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
  group_by(sampint) %>% 
  filter(rep < max(rep)) %>% # this is important because the final overlaps with first
  mutate(
    downsmp = pmap(list(data, smps), function(data, smps){
      data %>% 
        filter(meter %in% smps)
    })
  ) %>% 
  select(-data, -unimeter, -smps)

save(downsmps, file = here('data/downsmps.RData'))
