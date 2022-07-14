library(tidyverse)
library(purrr)
library(here)
library(grid)
library(hrbrthemes)
library(showtext)
library(RColorBrewer)
library(ggrepel)

# downsampled vegetation surveys
data(downsmps)

# get font
font_add_google("Roboto", "roboto")#, regular = 'C:/Windows/Fonts/Roboto.ttf')
fml <- "roboto"

showtext_auto()
showtext_opts(dpi = 400)

# to remove from analysis
rmv <- c("Unknown", "Woody Debris, none/detritus")

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

# richness estimates ------------------------------------------------------

rchests <- downsmps %>%
  mutate(
    spprch = map(downsmp, function(downsmp){

      downsmp %>%
        filter(!species %in% rmv) %>%
        pull(species) %>%
        unique %>%
        length

    })
  ) %>% 
  select(-downsmp) %>%
  unnest('spprch') %>%
  group_by(site, sample, sampint) %>%
  summarise(spprch = mean(spprch), .groups = 'drop')

toplo <- rchests %>% 
  mutate(
    sample = factor(paste('Year', sample))
  ) %>% 
  group_by(site, sample) %>% 
  mutate(
    per = 100 * spprch / max(spprch)
  ) %>% 
  ungroup()

cols <- c('#958984', '#00806E')

thm <- theme_ipsum(base_family = fml) +
  theme(
    panel.grid.minor = element_blank(),
    # panel.grid.major.x = element_blank(),
    axis.title.x = element_text(hjust = 0.5, size = 12),
    axis.title.y = element_text(hjust = 0.5, size = 12),
    legend.position = 'top'
  )

p1 <- ggplot(toplo, aes(x = sampint, y = spprch, group = sample, color = sample)) +
  geom_point(alpha = 0.6) +
  scale_x_continuous() + 
  facet_wrap(~site) +
  geom_smooth(se = F) + 
  scale_color_manual(values = cols) +
  thm + 
  labs(
    y = 'Species richness estimate', 
    x = 'Sampling distance every x meters', 
    color = NULL
  )

p2 <- ggplot(toplo, aes(x = sampint, y = per, group = sample, color = sample)) +
  geom_point(alpha = 0.6) +
  scale_x_continuous() + 
  facet_wrap(~site) +
  geom_smooth(se = F) + 
  scale_color_manual(values = cols) +
  thm + 
  labs(
    y = 'Species richness estimate, % of total', 
    x = 'Sampling distance every x meters', 
    color = NULL
  )

jpeg(here('figs/richex.jpg'), height = 7, width = 8, family = fml, units = 'in', res = 400)
print(p1)
dev.off()

jpeg(here('figs/richperex.jpg'), height = 7, width = 8, family = fml, units = 'in', res = 400)
print(p2)
dev.off()

# richness estimates by zone ------------------------------------------------------------------

colfun <- colorRampPalette(brewer.pal(8, "Accent"))

rchzonests <- downsmps %>%
  mutate(
    spprch = map(downsmp, function(downsmp){
      
      downsmp %>%
        filter(!species %in% rmv) %>%
        group_by(zone_name) %>% 
        summarise(
          spprch = length(unique(species))
        )
      
    })
  ) %>% 
  select(-downsmp) %>%
  unnest('spprch') %>%
  group_by(site, sample, sampint, zone_name) %>%
  summarise(spprch = mean(spprch), .groups = 'drop') %>% 
  unite(grp, c('site', 'sample'), remove = F)

thm <- theme_ipsum(base_family = fml) +
  theme(
    panel.grid.minor = element_blank(),
    # panel.grid.major.x = element_blank(),
    axis.title.x = element_text(hjust = 0.5, size = 12),
    axis.title.y = element_text(hjust = 0.5, size = 12),
    legend.position = 'top', 
    strip.text = element_text(size = 9)
  )

toplo <- rchzonests %>% 
  group_by(site, sample, zone_name) %>% 
  mutate(
    per = 100 * spprch / max(spprch)
  ) %>% 
  group_by(zone_name) %>% 
  nest() %>% 
  mutate(
    lofit = purrr::map(data, function(x){
      
      prddat <- x %>% 
        select(sampint) %>% 
        unique
      
      spprchloess <- loess(spprch ~ sampint, x) %>% 
        predict(newdat = prddat)
      
      perloess <- loess(per ~ sampint, x) %>% 
        predict(newdata = prddat)
      
      meanrich <- x %>% 
        group_by(sample) %>% 
        filter(sampint == 0.5) %>% 
        pull(spprch) %>% 
        mean()
      
      out <- tibble(
        meanrich = meanrich, 
        sampint = prddat$sampint,
        spprchloess = spprchloess, 
        perloess = perloess
      ) 
      
      return(out)
      
    })
  )

colvec <- colfun(length(unique(toplo$zone_name)))
toplo1 <- toplo %>% 
  select(-lofit) %>% 
  unnest('data')

toplo2 <- toplo %>% 
  select(-data) %>% 
  unnest('lofit')

# get factor levels based on greatest reduction
levs <- toplo2 %>% 
  group_by(zone_name) %>% 
  summarise(
    difv = max(perloess) - min(perloess), 
    .groups = 'drop'
  ) %>% 
  arrange(difv) %>% 
  pull(zone_name) %>% 
  rev

toplo1 <- toplo1 %>% 
  mutate(
    zone_name = factor(zone_name, levels = levs)
  )

p1 <- ggplot(toplo1, aes(x = sampint, y = spprch)) + 
  # geom_point(size = 0.5, aes(color = site)) +
  geom_smooth(aes(group = grp, color = site), se = F, size = 0.65) +
  geom_smooth(color = 'black', se = F, size = 1) +
  scale_color_manual(values = colfun(length(unique(toplo1$site)))) +
  facet_wrap(~zone_name) + 
  thm + 
  labs(
    y = 'Species richness estimate', 
    x = 'Sampling distance every x meters', 
    color = NULL
  )

p2 <- ggplot(toplo1, aes(x = sampint, y = per)) + 
  # geom_point(size = 0.5, aes(color = site)) +
  geom_smooth(aes(group = grp, color = site), se = F, size = 0.5) +
  geom_smooth(color = 'black', se = F) +
  scale_color_manual(values = colfun(length(unique(toplo1$site)))) +
  facet_wrap(~zone_name) +
  thm + 
  labs(
    y = 'Species richness estimate, % of total', 
    x = 'Sampling distance every x meters', 
    color = NULL
  )

lbs <- toplo2 %>% 
  filter(sampint == 10)

brks <- seq(min(toplo2$meanrich), max(toplo2$meanrich), by = 4)

p3 <- ggplot(toplo2, aes(x = sampint, y = perloess, group = zone_name, color = zone_name, alpha = meanrich)) + 
  scale_x_continuous(limits = c(0.5, 12.5)) + 
  geom_line(aes(size = meanrich, alpha = meanrich)) + 
  geom_text_repel(data = lbs, aes(label = zone_name), direction = 'y', xlim = c(0.5, 12.5), hjust = 0, nudge_x = 0.5, show.legend = F) + 
  thm + 
  scale_color_manual(values = colfun(length(unique(toplo2$zone_name)))) +
  scale_size(range = c(0.5, 1.8), breaks = brks) +
  scale_alpha_continuous(breaks = brks, range = c(0.6, 1)) +
  guides(color = 'none') +
  labs(
    y = 'Species richness estimate, % of total', 
    x = 'Sampling distance every x meters', 
    alpha = 'Mean richness across sites, years', 
    size = 'Mean richness across sites, years'
  )

jpeg(here('figs/richzone.jpg'), height = 11, width = 11, family = fml, units = 'in', res = 400)
print(p1)
dev.off()

jpeg(here('figs/richzoneper.jpg'), height = 11, width = 11, family = fml, units = 'in', res = 400)
print(p2)
dev.off()

jpeg(here('figs/richzonesum.jpg'), height = 8, width = 12, family = fml, units = 'in', res = 400)
print(p3)
dev.off()

