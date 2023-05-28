library(tidyverse)
library(tigris)
library(terra)
library(sf)
library(googledrive)
library(furrr)

#load forest service land use change maps from https://apps.fs.usda.gov/lcms-viewer/

file <- "https://drive.google.com/drive/folders/1ABoVnzSO_R233iUo20_HTO65YOtx2m2z"
folder <- drive_get(as_id(file))
gdrive_files <- drive_ls(folder)

lapply(gdrive_files$id, function(x) drive_download(as_id(x), 
                                                   path = paste0(here::here("data/original/fast/"), gdrive_files[gdrive_files$id==x,]$name), overwrite = TRUE))

not.conus <- c("AK", "HI", "PR", "VI", "GU", "AS", "MP", "UM")
conus.fips <- fips_codes %>% 
  filter(., !state %in% not.conus) %>% 
  select(., state) 
conus.fips.abbv <- unique(conus.fips$state)


bg.2020 <- map_df(conus.fips.abbv, function(x)
  block_groups(state=x))



land.use.2000 <- rast(here::here("data/original/fast/LCMS_CONUS_v2021-7_Land_Use_2000.tif"))
land.use.2020 <- rast(here::here("data/original/fast/LCMS_CONUS_v2021-7_Land_Use_2020.tif"))


extract_proto <- function(shp, rst){
  shp.proj <- vect(shp %>% st_transform(., crs=crs(rst)))
  rst.ext <- terra::extract(rst, shp.proj, fun=NULL, ID=FALSE, raw=FALSE)
}

land.use.ext.2020 <- lapply(1:nrow(bg.2020), function(x) extract_proto(bg.2020[x,], land.use.2000))
land.use.ext.2020b <- lapply(1:nrow(bg.2020), function(x) extract_proto(bg.2020[x,], land.use.2020))

land.use.2020.df <- map_df(1:length(land.use.ext.2020b), function(x) 
  land.use.ext.2020b[[x]] %>% 
    group_by(., category) %>% 
    summarise(., numpix=n()) %>% 
    mutate(., GEOID = bg.2020$GEOID[x],
              yr = 2020))

land.use.2020.wide <- land.use.2020.df %>% 
  pivot_wider(., names_from = category,
              values_from = numpix) %>%
  mutate_at(c(3:10), ~replace_na(., 0)) %>% 
  mutate(totalpix = rowSums(dplyr::select(., c(3:10))),
         percAg2020 = Agriculture/totalpix,
         percFor2020 = Forest/totalpix,
         percDev2020 = Developed/totalpix)
            
  


land.use.2000.df <- map_df(1:length(land.use.ext.2020), function(x) 
  land.use.ext.2020[[x]] %>% 
    group_by(., category) %>% 
    summarise(., numpix=n()) %>% 
    mutate(., GEOID = bg.2020$GEOID[x],
           yr = 2000))

land.use.2000.wide <- land.use.2000.df %>% 
  pivot_wider(., names_from = category,
              values_from = numpix) %>%
  mutate_at(c(3:10), ~replace_na(., 0)) %>% 
  mutate(totalpix = rowSums(dplyr::select(., c(3:10))),
         percAg2000 = Agriculture/totalpix,
         percFor2000 = Forest/totalpix,
         percDev2000 = Developed/totalpix)

chg.bg.join <- bg.2020 %>% 
  left_join(land.use.2000.wide[,c(1,12:14)]) %>% 
  left_join(land.use.2020.wide[,c(1,12:14)]) %>%
  mutate_at(vars(starts_with("perc")), ~.+1) %>% 
  mutate(., chgAg = ((percAg2020 - percAg2000)/percAg2000)*100,
            chgFor = ((percFor2020 - percFor2000)/percFor2000)*100,
         chgDev = ((percDev2020 - percDev2000)/percDev2000)*100)

template.rast <- rast(here::here("data/processed/processed_state/us_rough.tif"))

chg.bg.proj <- vect(chg.bg.join %>% st_transform(., crs=crs(template.rast)))
agchg.rst <- rasterize(chg.bg.proj, template.rast, field="chgAg")
forchg.rst <- rasterize(chg.bg.proj, template.rast, field="chgFor")
devchg.rst <- rasterize(chg.bg.proj, template.rast, field="chgDev")

writeRaster(agchg.rst, filename = here::here("data/processed/processed_fast/agchg.tif"))
writeRaster(forchg.rst, filename = here::here("data/processed/processed_fast/forchg.tif"), overwrite=TRUE)
writeRaster(devchg.rst, filename = here::here("data/processed/processed_fast/devchg.tif"))
