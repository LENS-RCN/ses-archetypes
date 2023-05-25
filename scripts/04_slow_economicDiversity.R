library(tidyverse)
library(tigris)

#download cpb2010
temp <- tempfile()
download.file("https://www2.census.gov/programs-surveys/cbp/datasets/2010/cbp10co.zip",temp)
unzip(temp, exdir = here::here("data/original/slow/"))
unlink(temp)


#read in county business patterns 2010 dataset
cbp2010 <- read_delim(here::here("data/original/slow/cbp10co.txt"), delim = ",") %>% 
  filter(., grepl('(\\d+)(----)', naics)) %>% 
  mutate(., GEOID = paste0(fipstate, fipscty)) 

total.emp <- cbp2010 %>% 
  group_by(GEOID) %>% 
  summarise(., total_est = sum(est)) 

prop.emp <- cbp2010 %>% 
  left_join(total.emp) %>% 
  mutate(., prop_est = est/total_est)

cbp.d.2010 <- prop.emp %>% 
  group_by(GEOID) %>% 
  summarise(D = (sum(prop_est * prop_est)),
            DI = 1 - D)

not.conus <- c("AK", "HI", "PR", "VI", "GU", "AS", "MP", "UM")
conus.fips <- fips_codes %>% 
  filter(., !state %in% not.conus) %>% 
  select(., state) 
conus.fips.abbv <- unique(conus.fips$state)



#download cbp 2021
temp <- tempfile()
download.file("https://www2.census.gov/programs-surveys/cbp/datasets/2021/cbp21co.zip",temp)
unzip(temp, exdir = here::here("data/original/slow/"))
unlink(temp)


cbp2021 <- read_delim(here::here("data/original/slow/cbp21co.txt"), delim = ",") %>% 
  filter(., grepl('(\\d+)(----)', naics)) %>% 
  mutate(., GEOID = paste0(fipstate, fipscty))

total.emp <- cbp2021 %>% 
  group_by(GEOID) %>% 
  summarise(., total_est = sum(est)) 

prop.emp <- cbp2021 %>% 
  left_join(total.emp) %>% 
  mutate(., prop_est = est/total_est)

cbp.d.2021 <- prop.emp %>% 
  group_by(GEOID) %>% 
  summarise(D = (sum(prop_est * prop_est)),
            DI = 1 - D)

library(terra)
library(sf)
template.rast <- rast(here::here("data/processed/processed_state/us_rough.tif"))
cty.shps.2010 <- counties(conus.fips.abbv, year=2010) %>% 
  left_join(., cbp.d.2010, by = c("GEOID10" = "GEOID")) %>% 
  st_transform(., crs=crs(template.rast))

cty.shps.2021 <- counties(conus.fips.abbv, year=2021) %>% 
  left_join(., cbp.d.2021) %>% 
  st_transform(., crs=crs(template.rast))


cbp.d.2010.rast <- rasterize(vect(cty.shps.2010), template.rast, field = "DI")
cbp.d.2021.rast <- rasterize(vect(cty.shps.2021), template.rast, field = "DI")


delta.DI.rast <- cbp.d.2021.rast - cbp.d.2010.rast

writeRaster(delta.DI.rast, filename = here::here("data/processed/processed_slow/chg_econdiv.tif"))
