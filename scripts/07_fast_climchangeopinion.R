##load Yale climate opinion data
library(readxl)
library(tidyverse)
library(tigris)
library(terra)

not.conus <- c("AK", "HI", "PR", "VI", "GU", "AS", "MP", "UM")
conus.fips <- fips_codes %>% 
  filter(., !state %in% not.conus) %>% 
  select(., state) 
conus.fips.abbv <- unique(conus.fips$state)

climate.opinions <- read_xlsx(here::here("data/original/fast/ycom.coot.2008-2021.xlsx"), sheet = 3) %>% 
  filter(., GeoType == "County") 

globalwarming.happening <- climate.opinions %>% 
  filter(., varname == "happening")

colnames(globalwarming.happening)[5:17] <- paste0("year", colnames(globalwarming.happening)[5:17])
globalwarming.happening[,5:17] <- lapply(globalwarming.happening[,5:17], function(x) as.numeric(x))


globalwarming.chg <- globalwarming.happening %>% 
  mutate(percchange = (year2021 - year2016)/year2016,
         GEOIDfx = str_pad(GEOID, width= 5, side="left", pad=0))

counties <- counties(conus.fips.abbv) %>% 
  left_join(., globalwarming.chg, by=c("GEOID" ="GEOIDfx"))

template.rast <- rast(here::here("data/processed/processed_state/us_rough.tif"))

counties.proj <- vect(counties %>% st_transform(., crs=crs(template.rast)))

globalwarming.chg.rast <- rasterize(counties.proj, template.rast, field = "percchange")

writeRaster(globalwarming.chg.rast, filename = here::here("data/processed/processed_fast/clim_chg_opinion_chg.tif"))
