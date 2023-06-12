library(tigris)
library(sf)
library(terra)
library(tidyverse)
temp <- tempfile()
download.file("https://hazards.fema.gov/nri/Content/StaticDocuments/DataDownload//NRI_GDB_CensusTracts/NRI_GDB_CensusTracts.zip",temp)
unzip(temp, exdir = here::here("data/original/future/"))
unlink(temp)
not.conus <- c("AK", "HI", "PR", "VI", "GU", "AS", "MP", "UM")
conus.fips <- fips_codes %>% 
  filter(., !state %in% not.conus) %>% 
  dplyr::select(., state) 
conus.fips.abbv <- unique(conus.fips$state)

hazards <- st_read(here::here("data/original/future/NRI_GDB_CensusTracts.gdb/"), layer = "NRI_CensusTracts") %>% 
  filter(., STATEABBRV %in% conus.fips.abbv)

template.rast <- rast(here::here("data/processed/processed_state/us_rough.tif"))

hazards.proj <- vect(hazards %>% st_transform(., crs=crs(template.rast)))

alrag.rst <- rasterize(hazards.proj, template.rast, field="ALR_VALA")
alrpop.rst <- rasterize(hazards.proj, template.rast, field="ALR_VALP")
alrbldg.rst <- rasterize(hazards.proj, template.rast, field="ALR_VALB")


writeRaster(alrag.rst, filename = here::here("data/processed/processed_future/alr_ag.tif"), overwrite=TRUE)
writeRaster(alrpop.rst, filename = here::here("data/processed/processed_future/alr_pop.tif"), overwrite=TRUE)
writeRaster(alrbldg.rst, filename = here::here("data/processed/processed_future/alr_bldg.tif"), overwrite=TRUE)
