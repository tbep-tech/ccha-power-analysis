library(tidyverse)
library(purrr)
library(here)
library(showtext)
library(grid)
library(hrbrthemes)

# get font
font_add_google("Roboto", "roboto")#, regular = 'C:/Windows/Fonts/Roboto.ttf')
fml <- "roboto"

showtext_auto()
showtext_opts(dpi = 400)

# relative reduction in effort --------------------------------------------

toplo <- tibble(
  x = seq(0.5, 10, by = 0.5)
) %>% 
  mutate(
    y = 1000 / (2 * x),
    red = 100 * ((1000 - y) / 1000)
  )

thm <- theme_ipsum(base_family = fml) +
  theme(
    panel.grid.minor = element_blank(),
    # panel.grid.major.x = element_blank(),
    axis.title.x = element_text(hjust = 0.5, size = 12),
    axis.title.y = element_text(hjust = 0.5, size = 12),
    legend.position = 'top'
  )

p <- ggplot(toplo, aes(x = x, y = red)) + 
  geom_line() + 
  geom_point(size = 4) + 
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
  scale_x_continuous(breaks = unique(toplo$x), labels = unique(toplo$x)) + 
  labs(
    x = 'Sampling distance every x meters',
    y = '% reduction in sample effort', 
    title = 'Relative reduction in sampling effort', 
    subtitle = 'Effort is reduced in half meter intervals'
  ) +
  thm

jpeg(here('figs/releff.jpg'), height = 5, width = 8, family = fml, units = 'in', res = 400)
print(p)
dev.off()

# example subset plot -----------------------------------------------------

ests <- seq(0.5, 3, by = 0.5) %>% 
  tibble(
    sampint = .
  ) %>% 
  group_by(sampint) %>% 
  nest() %>% 
  rename(smps = data) %>% 
  mutate(
    unimeter = list(seq(0.5, 29.5, by = 0.5)),
    smps = pmap(list(unimeter, sampint), function(unimeter, sampint){
      
      strlocs <- seq(min(unimeter), min(unimeter) + sampint, by = 0.5)
      lapply(as.list(strlocs), function(x) seq(x, max(unimeter), by = sampint)) %>% 
        enframe('rep', 'smps')
      
    })
  ) %>% 
  unnest(smps) %>% 
  group_by(sampint) %>% 
  filter(rep < max(rep)) %>% 
  mutate(
    unimeter = pmap(list(unimeter,smps), function(unimeter, smps){
      
      out <- tibble(
        unimeter = unimeter,
        insmp = unimeter %in% smps
      ) %>% 
        mutate(
          insmp = factor(as.numeric(insmp), levels = c('0', '1'), labels = c('no', 'yes'))
        )
      
      return(out)
      
    })
  ) %>% 
  select(-smps) %>% 
  unnest('unimeter')

expin <- 0.75
breaks_fun <- function(x, exp){
  seq(round(min(x + exp), 0), round(max(x - exp), 0), by = 1)
}
limits_fun <- function(x, exp){
  c(x[1] - exp, x[2] + exp)
}
legttl <- 'In sub-sample?'

p <- ggplot(ests, aes(x = unimeter, y = rep, group = rep)) + 
  geom_line(color = 'tomato1') + 
  geom_point(aes(shape = insmp, color = insmp, fill = insmp, size = insmp)) + 
  theme_bw() + 
  scale_shape_manual(values = c(21, 19)) +
  scale_size_manual(values = c(1, 3)) +
  scale_fill_manual(values = c('white', 'white')) + 
  scale_color_manual(values = c('tomato1', 'tomato1')) +
  scale_x_continuous(expand = c(0.01, 0.01)) +
  facet_grid(sampint ~ ., scales = 'free_y', space = 'free_y') + 
  scale_y_continuous(
    breaks = function(x, exp = expin) breaks_fun(x, exp), 
    limits = function(x, exp = expin) limits_fun(x, exp)
  ) +
  theme(
    panel.grid.minor= element_blank(),
    strip.text = element_text(size = 12),
    strip.background = element_blank(), 
    legend.position = 'top',
    plot.margin = unit(c(0,1,0,0), "lines")
  ) + 
  labs(
    x = 'Meter mark', 
    y = 'Sub-sampling replicate', 
    shape = legttl, 
    fill = legttl, 
    color = legttl,
    size = legttl
  ) 

jpeg(here('figs/subsampex.jpg'), height = 5, width = 8, family = fml, units = 'in', res = 400)
print(p)
grid.text('Sub-sample distance (m)', x = unit(0.99, "npc"), y = unit(0.5, "npc"), rot = 270)
dev.off()
