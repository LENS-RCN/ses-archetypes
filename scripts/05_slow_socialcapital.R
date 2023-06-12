library(tidyverse)
library(tigris)
library(terra)
library(sf)
library(units)
library(tidycensus)
options(tigris_use_cache = TRUE)

not.conus <- c("AK", "HI", "PR", "VI", "GU", "AS", "MP", "UM")
conus.fips <- fips_codes %>% 
  filter(., !state %in% not.conus) %>% 
  select(., state) 
conus.fips.abbv <- unique(conus.fips$state)
  
  
# download IRS business master files --------------------------------------

bmf.2010 <- read_csv("https://nccs-data.urban.org/dl.php?f=bmf/2010/bmf.bm1001.csv")


bmf.2010.summary <- bmf.2010 %>% 
  group_by(FIPS) %>%
  summarise(org.count = n())


density.2010 <- tidycensus::get_decennial(geography = "county", variables = "P001001", year=2010, geometry = TRUE, state=conus.fips.abbv) %>% 
  left_join(., bmf.2010.summary, by = c("GEOID" = "FIPS"))%>% 
  mutate(org.count = replace_na(org.count, 0),
         org.per.cap = org.count/value)
  


bmf.2020 <- read_csv("https://nccs-data.urban.org/dl.php?f=bmf/2020/bmf.bm2004.csv", col_types = list(FIPS = "c", NTMAJ12 = "c")) %>% 
  mutate(FIPSpad = str_pad(FIPS, 7, "left", 0),
         FIPSfx = str_extract(FIPSpad, ".*(?=\\.)"))

bmf.2020.summary <- bmf.2020 %>% 
  group_by(FIPSfx) %>%
  summarise(org.count = n())   


density.2020 <- get_decennial(geography = "county", year=2020, variables= "P1_001N", state = conus.fips.abbv, geometry = TRUE) %>% 
  left_join(., bmf.2020.summary, by=c("GEOID"="FIPSfx")) %>% 
  mutate(org.count = replace_na(org.count, 0),
           org.per.cap = org.count/value)

template.rast <- rast(here::here("data/processed/processed_state/us_rough.tif"))

density.2010.proj <- vect(density.2010 %>% st_transform(., crs=crs(template.rast)))
density.2020.proj <- vect(density.2020 %>% st_transform(., crs=crs(template.rast)))

density.2010.rast <- rasterize(density.2010.proj, template.rast, field="org.per.cap")
density.2020.rast <- rasterize(density.2020.proj, template.rast, field="org.per.cap")

perc.chg.rast <- ((((density.2020.rast+1) - (density.2010.rast+1))/(density.2010.rast+1))*100)/10

writeRaster(perc.chg.rast, filename = here::here("data/processed/processed_slow/social_cap_chg.tif"), overwrite=TRUE)
