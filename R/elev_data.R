library(tidyverse)
library(googlesheets4)
library(googledrive)
library(sf)
library(mapview)
library(readxl)
library(ggrepel)

load(url('https://github.com/tbep-tech/ccha-workflow/raw/main/data/tranloc.RData'))

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

# deauth all so it can build with gh actions
drive_deauth()
gs4_deauth()

drv <- 'https://drive.google.com/drive/u/0/folders/1qZBKn2A5hJb8WXBeiKG4FeoEvIUWmRc5'

# csv files must be opened/saved as spreadsheet in google sheets
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

nms <- c("Big Bend TECO Elevation Survey", "Cockroach Bay_10_5_16",  "Fort_Desoto_survey_10_17_2016",
         "Harbor_Palms_Park_Oldsmar_9_28_16", "Hidden Harbor Elevation Survey", 
         "Little Manatee River Elevation Survey", "Mosaic Elevation Survey", 
         "UTBP Elevation Survey", "Weedon_Island_site_9_21_2016" 
)
site <- c("Big Bend - TECO", "Cockroach Bay", "Fort DeSoto", "Harbor Palms", 
          "Hidden Harbor", "Little Manatee River", "Mosaic", "Upper Tampa Bay Park", 
          "Weedon Island")

sitlk <- tibble(
  name = nms, 
  site = site
)

# ungroup
# remove points not on the transects (seen in plot)
# projection is NAD 83State Plane (Florida west) EPSG 2882
# estimate distance

# these sites have locations in meters, need to convert to feet
tocnv <- c('Harbor Palms', 'Weedon Island', 'Fort DeSoto', 'Cockroach Bay')

eledat <- fls %>% 
  left_join(sitlk, by = 'name') %>% 
  ungroup() %>% 
  select(site, data) %>%
  unnest('data') %>% 
  filter(
    !(site == 'Big Bend - TECO' & point %in% c('1105', '1106', '1107', '1108', '1109', '1110', '1111', '1112') |
    site == 'Mosaic' & point %in% c('1', '2', '596', '597', '602', '603', '604', '605', '606'))
  ) %>% 
  # group_by(site) %>% 
  mutate(
    # distance_m = c(0, sqrt(diff(northing)^2 + diff(easting)^2)), 
    # distance_m = cumsum(distance_m),
    easting = case_when(
      site %in% tocnv ~ 3.28084 * easting, 
      T ~ easting
    ), 
    northing = case_when(
      site %in% tocnv ~ 3.28084 * northing, 
      T ~ northing
    )
  ) %>% 
  st_as_sf(coords = c('easting', 'northing'), crs = 2882) %>% 
  mutate(
    xlocft = st_coordinates(.)[, 1],
    ylocft = st_coordinates(.)[, 2]
  )

mapview(tranloc) + mapview(trnstrstp, zcol = 'loc') + mapview(eledat, zcol = 'site')
# # verify start direction from map
# ptord <- list(
#   `Big Bend - TECO` = c(), 
#   `Cockroach Bay` = c(),
#   `Fort DeSoto` = c(), 
#   `Harbor Palms` = c(), 
#   `Hidden Harbor` = c(), 
#   `Little Manatee River`, 
#   `Mosaic` (599, 598, 500:595), 
#   `Upper Tampa Bay Park` = c(), 
#   `Weedon Island` = c()
# )


mapview(tranloc) + mapview(trnstrstp, zcol = 'loc') + mapview(eledat, zcol = 'site')


pdf('~/Desktop/cchalocs.pdf', height = 10, width = 10)

for(stsel in site){
  
  toplo <- eledat %>% 
    filter(site == stsel)
  toplo2 <- trnstrstp %>% 
    filter(site == stsel)
  p <- ggplot(toplo, aes(x = xlocft, y = ylocft)) + 
    geom_line(color = 'black') +
    geom_text(aes(label = point), size = 2, color = 'red') + 
    geom_point(data = toplo2, aes(color = loc)) + 
    labs(
      subtitle = stsel
    )
  
  print(p)
  
}

dev.off()

