library(tidyverse)
library(googlesheets4)
library(googledrive)
library(sf)
library(mapview)
library(readxl)
library(leafem)

# deauth all so it can build with gh actions
drive_deauth()
gs4_deauth()

# polyline transect sf object
load(url('https://github.com/tbep-tech/ccha-workflow/raw/main/data/tranloc.RData'))

# transect start stop -------------------------------------------------------------------------

# transect start stop locations to identify landward/waterward
trnstrstp <- read_excel('T:/09_TECHNICAL_PROJECTS/CRITICAL_COASTAL_HABITAT_ASSESSMENT/04_BACKGROUND/OlderVersions/CCHA1_2_Transect_Start_End.xlsx', skip = 1) %>% 
  select(site = Site, matches('LAT|LONG')) %>% 
  pivot_longer(-site) %>% 
  mutate(
    site = case_when(
      site == 'Archie Creek Mosaic' ~ 'Mosaic', 
      site == 'TECO Big Bend' ~ 'Big Bend - TECO',
      T ~ site
    ),
    loc = case_when(
      grepl('4|5', name) ~ 'water', 
      grepl('6|7',  name) ~ 'land'
    ),
    loc = case_when(
      site %in% 'Fort DeSoto' & loc == 'water' ~ 'land',
      site %in% 'Fort DeSoto' & loc == 'land' ~ 'water', 
      T ~ loc
    ),
    name = gsub('\\.*|\\d', '', name)
  ) %>% 
  pivot_wider() %>% 
  st_as_sf(coords = c('LONG_DD', 'LAT_DD'), crs = 4326) %>% 
  st_transform(crs = 2882) %>% 
  mutate(
    xlocft = st_coordinates(.)[, 1],
    ylocft = st_coordinates(.)[, 2]
  )

# elevation data ------------------------------------------------------------------------------

# data from google drive
drv <- 'https://drive.google.com/drive/u/0/folders/1qZBKn2A5hJb8WXBeiKG4FeoEvIUWmRc5'
fls <- drive_ls(drv, type = 'spreadsheet') %>% 
  select(name, id) %>% 
  group_by(name, id) %>% 
  nest() %>% 
  mutate(
    data = purrr::map(id, function(id){
      
      dat <- read_sheet(id)
      
      dat <- dat[, 1:4]
      names(dat) <- c('point', 'northing', 'easting', 'elevation_m')
      
      dat$point <- as.character(dat$point)
      
      return(dat)
      
    })
  )

# rename sites to match vegdat
# remove points not on the transects or top measurements (seen in plot)
# reorder points not in order (by visual inspection)
# reverse those where start is not waterward side
# projection is NAD 83State Plane (Florida west) EPSG 2882
# estimate distance

sitlk <- list(
  `Big Bend TECO Elevation Survey` = "Big Bend - TECO", 
  `Cockroach Bay_10_5_16` = "Cockroach Bay", 
  Fort_Desoto_survey_10_17_2016 = "Fort DeSoto", 
  Harbor_Palms_Park_Oldsmar_9_28_16 = "Harbor Palms", 
  `Hidden Harbor Elevation Survey` = "Hidden Harbor", 
  `Little Manatee River Elevation Survey` = "Little Manatee River", 
  `Mosaic Elevation Survey` = "Mosaic", 
  `UTBP Elevation Survey` = "Upper Tampa Bay Park", 
  Weedon_Island_site_9_21_2016 = "Weedon Island"
  ) %>% 
  enframe(name = 'name', value = 'site')

# points to remove
ptrmv <- list(
  `Big Bend - TECO` = c('1105', '1106', '1107', '1108', '1109', '1110', '1111', '1112'),
  `Cockroach Bay` = c('co_mon2top', 'co_mon1top'),
  `Fort DeSoto` = c('fdmon1Top', 'fdmon2Top'),
  `Harbor Palms` = c('HPmon2_Top', 'HPmon1_top'),
  `Hidden Harbor` = c('1093'),
  `Little Manatee River` = NA,
  `Mosaic` = c('1', '2', '596', '597', '602', '603', '604', '605', '606'),
  `Upper Tampa Bay Park` = NA,
  `Weedon Island` = c('wiatop', 'wimon')
  ) %>% 
  enframe(name = 'site', value = 'ptrmv')

# verify start direction from map, NA for asis
ptord <- list(
  `Big Bend - TECO` = as.character(c(1000:1081, 1101, 1082, 1102, 1083, 1084, 1103, 1104, 1085:1100)),
  `Cockroach Bay` = NA,
  `Fort DeSoto` = NA,
  `Harbor Palms` = NA,
  `Hidden Harbor` = NA,
  `Little Manatee River` = NA,
  `Mosaic` = as.character(c(599, 598, 500:517, 556:558, 518:555, 559:584, 586, 585, 587:595)),
  `Upper Tampa Bay Park` = NA,
  `Weedon Island` = NA
  ) %>% 
  enframe(name = 'site', value = 'ptord')

# T indicates starting point is landward and needs to be reversd
revtyp <- list(
  `Big Bend - TECO` = T,
  `Cockroach Bay` = F,
  `Fort DeSoto` = T,
  `Harbor Palms` = F,
  `Hidden Harbor` = T,
  `Little Manatee River` = F,
  `Mosaic` = T,
  `Upper Tampa Bay Park` = F,
  `Weedon Island` = F
  ) %>% 
  enframe(name = 'site', value = 'revtyp')  

# T indicates locations are in meters, need to be converted to ft
cnvtyp <- list(
  `Big Bend - TECO` = F,
  `Cockroach Bay` = T,
  `Fort DeSoto` = T,
  `Harbor Palms` = T,
  `Hidden Harbor` = F,
  `Little Manatee River` = F,
  `Mosaic` = F,
  `Upper Tampa Bay Park` = F,
  `Weedon Island` = T
  ) %>% 
  enframe(name = 'site', value = 'cnvtyp') 

eledat <- fls %>% 
  left_join(sitlk, by = 'name') %>% 
  ungroup() %>% 
  select(site, data) %>%
  unnest('site') %>% 
  left_join(ptrmv, by = 'site') %>% 
  left_join(ptord, by = 'site') %>% 
  left_join(revtyp, by = 'site') %>% 
  left_join(cnvtyp, by = 'site') %>% 
  mutate(
    data = purrr::pmap(list(data, ptrmv, ptord, revtyp,  cnvtyp), function(data, ptrmv, ptord, revtyp, cnvtyp){
  
        out <- data

        # remove points
        if(!anyNA(ptrmv))
          out <- out %>% 
            filter(!point %in% ptrmv)
        
        # reorder
        if(!anyNA(ptord))
          out <- out[rank(ptord, out$point), ]
        
        # reverse
        if(revtyp)
          out <- out[nrow(out):1, ]
        
        # convert m to ft
        if(cnvtyp){
          out$easting <- 3.28084 * out$easting
          out$northing <- 3.28084 * out$northing
        }
        
        # get distances
        streast <- out$easting[1]
        strnort <- out$northing[1]
        out$distance_m <- sqrt((out$northing - strnort)^2 + (out$easting - streast)^2) / 3.28084
        
        return(out)
        
    })
  ) %>% 
  select(site, data) %>% 
  unnest('data') %>% 
  st_as_sf(coords = c('easting', 'northing'), crs = 2882) %>% 
  mutate(
    xlocft = st_coordinates(.)[, 1],
    ylocft = st_coordinates(.)[, 2]
  )

# m <- mapview(tranloc) + mapview(trnstrstp, zcol = 'loc') + mapview(eledat, zcol = 'site')
# m
# # m %>% addStaticLabels(data = eledat, label = eledat$point)
# 
# ggplot(eledat, aes(x = distance_m, y = elevation_m)) + 
#   geom_line() + 
#   geom_point() + 
#   facet_wrap(~site, scales = 'free')
