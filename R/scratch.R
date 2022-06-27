library(tidyverse)
library(purrr)
library(here)

source(here('R/funcs.R'))

data(vegdat)

sit <- 'Big Bend - TECO'
smp <- 1
sitdat <- vegdat %>% 
  filter(site %in% sit) %>% 
  filter(sample %in% smp)

sitezonesum_fun(sitdat, sit, var = 'fo')

unimeter <- sitdat %>% 
 pull(meter) %>% 
  unique

ests <- seq(0.5, 10, by = 0.5) %>% 
  tibble(
    sampint = .
  ) %>% 
  group_by(sampint) %>% 
  nest() %>% 
  rename(smps = data) %>% 
  mutate(
    smps = map(sampint, function(sampint){
    
      strlocs <- seq(min(unimeter), min(unimeter) + sampint, by = 0.5)
      lapply(as.list(strlocs), function(x) seq(x, max(unimeter), by = sampint)) %>% 
        enframe('rep', 'smps')
      
    })
  ) %>% 
  unnest(smps) %>%
  mutate(
    downsmp = map(smps, function(smps){
      sitdat %>% 
        filter(meter %in% smps)
    }), 
    downest = map(downsmp, function(downsmp){
      NULL # use sitezonesum_fun
    })
  )


