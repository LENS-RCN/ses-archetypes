library(tidyverse)
library(tidycensus)
library(terra)
library(sf)

not.conus <- c("AK", "HI", "PR", "VI", "GU", "AS", "MP", "UM")
conus.fips <- fips_codes %>% 
  filter(., !state %in% not.conus) %>% 
  select(., state) 
conus.fips.abbv <- unique(conus.fips$state)


pop.2010 <- map_df(conus.fips.abbv, function(x)
                   get_decennial(geography = "block group", variables = "P001001", year=2010, geometry = TRUE, state=x))
  

pop.2020 <- map_df(conus.fips.abbv, function(x)
  get_decennial(geography = "block group", variables = "P1_001N", year=2020, geometry = TRUE, state=x))

template.rast <- rast(here::here("data/processed/processed_state/us_rough.tif"))

pop.2010.proj <- vect(pop.2010 %>% st_transform(., crs(template.rast)))
pop.2020.proj <- vect(pop.2020 %>% st_transform(., crs(template.rast)))

pop.2010.rst <- rasterize(pop.2010.proj, template.rast, field = "value")
pop.2020.rst <- rasterize(pop.2020.proj, template.rast, field = "value")

pop.chg <- ((pop.2020.rst+1) - (pop.2010.rst+1))/(pop.2010.rst+1)
ann.pop.chg <- pop.chg/10

writeRaster(ann.pop.chg, filename = here::here("data/processed/processed_fast/pop_change_20102020.tif"), overwrite=TRUE)
