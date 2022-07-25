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
  summarise(
    spprchvar = var(spprch),
    spprch = mean(spprch), 
    .groups = 'drop'
  )

toplo <- rchests %>% 
  mutate(
    sample = factor(paste('Year', sample))
  ) %>% 
  group_by(site, sample) %>% 
  mutate(
    per = 100 * (spprch - max(spprch)) / max(spprch) 
  ) %>% 
  group_by(site) %>% 
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

toplo1 <- toplo %>% 
  select(-lofit) %>% 
  unnest('data')

toplo2 <- toplo %>% 
  select(-data) %>% 
  unnest('lofit')


# get factor levels based on greatest reduction
levs <- toplo2 %>% 
  group_by(site) %>% 
  summarise(
    difv = max(perloess) - min(perloess), 
    .groups = 'drop'
  ) %>% 
  arrange(difv) %>% 
  pull(site) %>% 
  rev

toplo1 <- toplo1 %>% 
  mutate(
    site = factor(site, levels = levs)
  )

thm <- theme_ipsum(base_family = fml) +
  theme(
    panel.grid.minor = element_blank(),
    # panel.grid.major.x = element_blank(),
    axis.title.x = element_text(hjust = 0.5, size = 12),
    axis.title.y = element_text(hjust = 0.5, size = 12),
    legend.position = 'top'
  )

cols <- c('#958984', '#00806E')

p1 <- ggplot(toplo1, aes(x = sampint, y = spprch, group = sample, color = sample)) +
  geom_point(alpha = 0.6, aes(size = spprchvar)) +
  scale_x_continuous() + 
  facet_wrap(~site) +
  geom_smooth(se = F) + 
  scale_color_manual(values = cols) +
  thm + 
  labs(
    y = 'Species richness estimate', 
    x = 'Sampling distance every x meters',  
    size = 'Variance with random subsample',
    color = NULL
  )

p2 <- ggplot(toplo1, aes(x = sampint, y = per, group = sample, color = sample)) +
  geom_point(alpha = 0.6, aes(size = spprchvar)) +
  scale_x_continuous() + 
  facet_wrap(~site) +
  geom_smooth(se = F) + 
  scale_color_manual(values = cols) +
  thm + 
  labs(
    y = 'Species richness estimate, % loss', 
    x = 'Sampling distance every x meters', 
    size = 'Variance with random subsample',
    color = NULL
  )

tmp <- rchests %>% 
  mutate(
    sample = factor(paste('Year', sample))
  ) %>% 
  group_by(site, sample) %>% 
  mutate(
    per = 100 * (spprch - max(spprch)) / max(spprch)
  ) %>% 
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
  ) %>% 
  select(-data) %>% 
  unnest('lofit') %>% 
  group_by(site, sample, meanrich) %>% 
  summarise(
    difv = min(perloess) - max(perloess), 
    .groups = 'drop'
  ) 

mod <- lm(difv ~ meanrich, data = tmp) %>% 
  summary() %>% 
  .$coefficients %>% 
  .[2, 4] %>% 
  round(., 2)

p3 <- ggplot(tmp, aes(x = meanrich, y = difv)) + 
  geom_point() + 
  stat_smooth(method = 'lm') + 
  thm + 
  labs(
    x = 'Actual species richness', 
    y = 'Total percent loss from 0.5 m to 10 m sampling',
    subtitle = 'Total species loss with reduced sampling as a function of actual richness', 
    caption = paste('Model insignificant, p = ', mod)
  )

jpeg(here('figs/richex.jpg'), height = 7, width = 8, family = fml, units = 'in', res = 400)
print(p1)
dev.off()

jpeg(here('figs/richperex.jpg'), height = 7, width = 8, family = fml, units = 'in', res = 400)
print(p2)
dev.off()

jpeg(here('figs/richloss.jpg'), height = 6, width = 6.5, family = fml, units = 'in', res = 400)
print(p3)
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
    per = 100 * (spprch - max(spprch)) / max(spprch)
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
    difv = min(perloess) - max(perloess), 
    .groups = 'drop'
  ) %>% 
  arrange(difv) %>% 
  pull(zone_name)

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
    y = 'Species richness estimate, % loss', 
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
  scale_size(range = c(0.4, 2), breaks = brks) +
  scale_alpha_continuous(breaks = brks, range = c(0.6, 1)) +
  guides(color = 'none') +
  labs(
    y = 'Species richness estimate, % loss', 
    x = 'Sampling distance every x meters', 
    alpha = 'Mean richness across sites, years', 
    size = 'Mean richness across sites, years'
  )

toplo3 <- lbs 

mod <- lm(perloess ~ meanrich, data = toplo3) %>% 
  summary() %>% 
  .$coefficients %>% 
  .[2, 4] %>% 
  format.pval(., eps = 0.05)

p4 <- ggplot(toplo3, aes(x = meanrich, y =perloess)) + 
  geom_point(aes(color = zone_name)) + 
  geom_text_repel(aes(label = zone_name, color = zone_name), size = 2.5, segment.color = NA) +
  scale_color_manual(values = colfun(length(unique(toplo2$zone_name)))) +
  geom_smooth(method = 'lm') + 
  guides(color = 'none') +
  thm + 
  labs(
    x = 'Average species richness by zone', 
    y = 'Total percent loss from 0.5 m to 10 m sampling',
    subtitle = 'Total loss with reduced sampling as a function of average richness', 
    caption = paste('Model significant, p ', mod)
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

jpeg(here('figs/richzoneloss.jpg'), height = 6, width = 6.5, family = fml, units = 'in', res = 400)
print(p4)
dev.off()

# frequency occurrence by sites -------------------------------------------

foests <- downsmps %>%
  mutate(
    foest = map(downsmp, function(downsmp){
      
      downsmp %>% 
        filter(!species %in% rmv) %>% 
        select(meter, species, pcent_basal_cover) %>% 
        mutate(
          pa = ifelse(pcent_basal_cover > 0, 1, 0)
        ) %>%
        select(-pcent_basal_cover) %>% 
        mutate(cnt = length(unique(meter))) %>% 
        unique %>%
        group_by(species) %>% 
        summarise(
          foest = sum(pa) / unique(cnt)
        )
      
    })
  ) %>% 
  select(-downsmp) %>%
  unnest('foest') %>%
  group_by(site, sample, sampint, species) %>%
  summarise(
    foestvar = var(foest),
    foest = mean(foest), 
    .groups = 'drop'
  )

toplo <- foests %>% 
  mutate(
    sample = factor(paste('Year', sample))
  ) %>% 
  unite('samplespecies', sample, species, remove = F) 
# filter(species == 'Avicennia germinans')

cols <- c('#958984', '#00806E')

thm <- theme_ipsum(base_family = fml) +
  theme(
    panel.grid.minor = element_blank(),
    # panel.grid.major.x = element_blank(),
    axis.title.x = element_text(hjust = 0.5, size = 12),
    axis.title.y = element_text(hjust = 0.5, size = 12),
    legend.position = 'top'
  )

p1 <- ggplot(toplo, aes(x = sampint, y = foest, group = samplespecies, color = sample)) +
  geom_point(alpha = 0.6, aes(size = foestvar)) +
  scale_x_continuous() + 
  facet_wrap(~site) +
  geom_line() +  
  scale_color_manual(values = cols) +
  thm + 
  labs(
    y = 'Species richness estimate', 
    x = 'Sampling distance every x meters', 
    size = 'Variance across random subsamples',
    color = NULL
  )

tmp1 <- toplo %>% 
  filter(sampint == 0.5) %>% 
  select(-sampint, -foestvar)
tmp2 <- toplo %>% 
  filter(sampint == 10) %>% 
  select(-sampint, -foest)
tmp <- inner_join(tmp1, tmp2, by = c('site', 'samplespecies', 'sample', 'species')) %>% 
  na.omit()

mod <- lm(log10(1 + foestvar) ~ log10(1 + foest), data = tmp) %>% 
  summary() %>% 
  .$coefficients %>% 
  .[2, 4] %>% 
  format.pval(., eps = 0.05)

p2 <- ggplot(tmp, aes(x = foest, y = foestvar)) + 
  geom_point() + 
  scale_y_log10() +
  scale_x_log10() +
  geom_smooth(method = 'lm') +
  thm + 
  labs(
    x = 'Initial frequency occurrence', 
    y = 'Final variance at 10 m sampling',
    subtitle = 'Variance of frequency occurrence with reduced effort',
    caption = paste('Model significant, p ', mod)
  )

jpeg(here('figs/foex.jpg'), height = 7, width = 8, family = fml, units = 'in', res = 400)
print(p1)
dev.off()

jpeg(here('figs/fovarex.jpg'), height = 6, width = 6.5, family = fml, units = 'in', res = 400)
print(p2)
dev.off()


# zone count and distance estimates -----------------------------------------------------------

zonedsts <- downsmps %>%
  filter(sample == 1) %>% 
  mutate(
    zonedst = purrr::map(downsmp, function(x){
      
      x %>% 
        select(zone_name, zone, meter) %>% 
        unique %>% 
        unite('zonefct', zone, zone_name, sep = ': ', remove = F) %>%
        group_by(zonefct, zone_name) %>% 
        filter(meter == max(meter)) %>% 
        ungroup() %>% 
        mutate(
          dist = diff(c(0, meter))
        )
      
    })
  ) %>% 
  select(-downsmp) %>% 
  unnest(zonedst)

toplo1 <- zonedsts %>% 
  group_by(site, sampint) %>% 
  summarize(
    zonecnt = n_distinct(zonefct), 
    .groups = 'drop'
  )

thm <- theme_ipsum(base_family = fml) +
  theme(
    panel.grid.minor = element_blank(),
    # panel.grid.major.x = element_blank(),
    axis.title.x = element_text(hjust = 0.5, size = 12),
    axis.title.y = element_text(hjust = 0.5, size = 12),
    legend.position = 'top'
  )

p1 <- ggplot(toplo1, aes(x = sampint, y = zonecnt)) + 
  geom_line() + 
  geom_point() + 
  facet_wrap(~site) + 
  thm + 
  labs(
    y = 'Total number of unique zones', 
    x = 'Sampling distance every x meters',  
  )

toplo2 <- zonedsts %>% 
  group_by(site, sampint, zonefct) %>% 
  summarize(
    meandist = mean(dist, na.rm = T), 
    vardist = var(dist, na.rm = T),
    .groups = 'drop'
  )

p2 <- ggplot(toplo2, aes(x = sampint, y = meandist, group = zonefct)) + 
  geom_line() + 
  geom_point(aes(size = vardist)) + 
  facet_wrap(~site) + 
  thm +
  labs(
    y = 'Average zone distance', 
    x = 'Sampling distance every x meters',  
    size = 'Variance with random subsample', 
    caption = 'Zones with zero distance have one sample point'
  )

toplo3 <- toplo2 %>% 
  filter(site == 'Big Bend - TECO')

toplo3 <- toplo3 %>% 
  filter(sampint == 0.5) %>% 
  mutate(meandist = meandist + 0.5) %>% 
  unite(zonefctlab, c("zonefct", "meandist"), remove = F, sep = ', ') %>% 
  mutate(zonefctlab = paste(zonefctlab, 'm')) %>% 
  select(-vardist, -sampint) %>% 
  full_join(toplo3, by = c('site', 'zonefct')) 

p3 <- ggplot(toplo3, aes(x = sampint, y = vardist)) + 
  geom_point() + 
  geom_line() +
  facet_wrap(~zonefctlab, scales = 'free_y') +
  thm +
  theme(strip.text = element_text(size = 8)) +
  labs(
    y = 'Variance across sub-samples', 
    x = 'Sampling distance every x meters',  
    subtitle = 'Big Bend - Teco'
  )

jpeg(here('figs/zonecnt.jpg'), height = 7, width = 8, family = fml, units = 'in', res = 400)
print(p1)
dev.off()

jpeg(here('figs/zonedst.jpg'), height = 7, width = 8, family = fml, units = 'in', res = 400)
print(p2)
dev.off()

jpeg(here('figs/zonevarex.jpg'), height = 7, width = 8, family = fml, units = 'in', res = 400)
print(p3)
dev.off()

# estimates for each site

sumdst <- zonedsts %>% 
  group_by(site, sampint, zonefct) %>% 
  summarize(
    meandist = mean(dist, na.rm = T), 
    vardist = var(dist, na.rm = T),
    .groups = 'drop'
  )

actdst <- sumdst %>% 
  filter(sampint == 0.5) 

estdst <- sumdst %>% 
  filter(sampint != 0.5) %>% 
  group_by(site, sampint) %>% 
  mutate(
    meandist = cumsum(meandist)
  )

thm <- theme_ipsum(base_family = fml) +
  theme(
    panel.grid.minor = element_blank(),
    # panel.grid.major.x = element_blank(),
    axis.title.x = element_text(hjust = 0.5, size = 12),
    axis.title.y = element_text(hjust = 0.5, size = 12),
    legend.position = 'top', 
    panel.grid.major = element_blank(), 
    legend.text = element_text(size = 8)
  )

dgd <- position_dodge2(width = 0.25)

sites <- unique(sumdst$site)
for(site in sites){
  
  toplo1 <- actdst %>% 
    filter(site == !!site) %>% 
    mutate(
      zonefct = factor(zonefct)
    ) 
  toplo2 <- estdst %>% 
    filter(site == !!site) %>% 
    mutate(zonefct = factor(zonefct))
  
  cols <- function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
  }
  levs <- levels(toplo1$zonefct)
  colin <- cols(length(levs))
  names(colin) <- levs
  
  
  p <- ggplot(toplo2, aes(x = sampint, y = meandist)) + 
    geom_bar(data = toplo1, aes(fill = zonefct, x = 5), stat = 'identity', position = position_stack(reverse = TRUE), alpha = 0.7, width = 20) +
    geom_point(position = dgd) + 
    geom_errorbar(aes(ymin = meandist - (vardist / 2), ymax = meandist + (vardist / 2)), position = dgd, width = 0.25) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_x_continuous(expand = c(0, 0), breaks = seq(1, 10, by = 0.5)) + 
    labs(
      x = 'Sampling distance every x meters', 
      fill = 'Actual\nzone', 
      y = 'Transect distance',
      subtitle = site
    ) +
    coord_flip(xlim = c(0.5, 10.5)) + 
    thm
  
  flnm <- paste0('figs/zoneest_', site, '.jpg')
  jpeg(here(flnm), height = 5, width = 8, family = fml, units = 'in', res = 400)
  print(p)
  dev.off()
  
}

# mangrove elevation distribution -------------------------------------------------------------

mangspp <- c('Avicennia germinans', 'Laguncularia racemosa', 'Rhizophora mangle')

mangests <- downsmps %>%
  filter(sample == 1) %>% 
  mutate(
    mangest = purrr::map(downsmp, function(x){
      x %>% 
        filter(species %in% mangspp) %>% 
        group_by(species) %>% 
        summarise(
          est = quantile(elevation_m, 0.95)
        )
      
    })
  ) %>% 
  select(-downsmp) %>% 
  unnest(mangest)

thm <- theme_ipsum(base_family = fml) +
  theme(
    panel.grid.minor = element_blank(),
    # panel.grid.major.x = element_blank(),
    axis.title.x = element_text(hjust = 0.5, size = 12),
    axis.title.y = element_text(hjust = 0.5, size = 12),
    legend.position = 'top', 
    strip.text = element_text(size = 9),
    legend.box="vertical"
  )

toplo <- mangests %>% 
  group_by(site, sampint, species) %>% 
  summarise(
    aveest = mean(est),
    varest = var(est), 
    .groups = 'drop'
  ) %>% 
  mutate(
    varest = ifelse(is.na(varest), 0, varest)
  ) 

p1 <- ggplot(toplo, aes(x = sampint, y = aveest, color = species)) +
  facet_wrap(~ site, scales = 'free_y') +
  geom_point(aes(size = varest)) + 
  # geom_line()  +
  geom_smooth(se = F) + 
  thm + 
  labs(
    y = 'Elevation (m) at 95% occurrence', 
    x = 'Sampling distance every x meters', 
    size = 'Variance across random subsamples',
    color = NULL
  )

maxeff <- toplo %>% 
  filter(sampint == 0.5) %>% 
  select(site, species, maxest = aveest)
toplo2 <- toplo %>% 
  left_join(maxeff, by = c('site', 'species')) %>% 
  mutate(
    aveestper = 100 * (aveest - maxest)/ abs(maxest)
  )

p2 <- ggplot(toplo2, aes(x = sampint, y = aveestper, color = species)) +
  facet_wrap(~ site, scales = 'free_y') +
  geom_point(aes(size = varest)) + 
  # geom_line()  +
  geom_smooth(se = F)+
  thm + 
  labs(
    y = 'Elevation (m) at 95% occurrence, % loss', 
    x = 'Sampling distance every x meters', 
    size = 'Variance across random subsamples',
    color = NULL
  )

allfo <- downsmps %>% 
  filter(sample == 1) %>% 
  filter(sampint == 0.5) %>% 
  mutate(
    foest = purrr::map(downsmp, function(x){
      
      x %>% 
        mutate(
          pa = ifelse(pcent_basal_cover > 0, 1, 0)
        ) %>%
        select(-pcent_basal_cover) %>% 
        mutate(cnt = length(unique(meter))) %>% 
        unique %>%
        filter(species %in% mangspp) %>% 
        group_by(species) %>% 
        summarise(
          foest = sum(pa) / unique(cnt), 
          .groups = 'drop'
        )
      
    })
  ) %>% 
  select(-downsmp) %>% 
  unnest(foest) %>% 
  ungroup() %>% 
  select(site, species, foest)

toplo3 <- toplo2 %>% 
  group_by(site, species) %>% 
  summarise(
    difave = min(aveestper) - max(aveestper),
    maxvar = max(varest),
    .groups = 'drop'
  ) %>% 
  left_join(allfo, by = c('site', 'species')) %>% 
  filter(difave > -500 & difave < 0)

mod <- lm(log10(-1 * difave) ~ foest, data = toplo3) %>% 
  summary() %>% 
  .$coefficients %>% 
  .[2, 4] %>% 
  format.pval(., eps = 0.05)

p3 <- ggplot(toplo3, aes(x = foest, y = -1 * difave)) + 
  geom_point(aes(color = species)) + 
  geom_smooth(method = 'lm' , se = T) + 
  scale_y_log10() + 
  thm + 
  labs(
    x = 'Actual frequency occurrence',
    y = 'Total % change in elevation\n0.5 m to 10 m sampling (log-transformed)', 
    color = NULL,
    subtitle = 'Change in elevation estimate with frequency occurrence',
    caption = paste('Model significant, p ', mod)
  )

mod <- lm(log10(maxvar) ~ foest, data = toplo3) %>% 
  summary() %>% 
  .$coefficients %>% 
  .[2, 4] %>% 
  format.pval(., eps = 0.05)

p4 <- ggplot(toplo3, aes(x = foest, y = maxvar)) + 
  geom_point(aes(color = species)) +
  scale_y_log10() + 
  geom_smooth(method = 'lm', se = T) + 
  thm + 
  labs(
    x = 'Actual frequency occurrence',
    y = 'Final variance at 10 m sampling (log-transformed)', 
    color = NULL,
    subtitle = 'Variance of elevation estimate with reduced effort',
    caption = paste('Model significant, p ', mod)
  )

jpeg(here('figs/elevex.jpg'), height = 7.5, width = 9, family = fml, units = 'in', res = 400)
print(p1)
dev.off()

jpeg(here('figs/elevperex.jpg'), height = 7.5, width = 9, family = fml, units = 'in', res = 400)
print(p2)
dev.off()

jpeg(here('figs/foperelevex.jpg'), height = 6, width = 6.5, family = fml, units = 'in', res = 400)
print(p3)
dev.off()

jpeg(here('figs/fovarelevex.jpg'), height = 6, width = 6.5, family = fml, units = 'in', res = 400)
print(p4)
dev.off()

