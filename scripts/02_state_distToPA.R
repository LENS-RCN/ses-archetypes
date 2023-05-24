library(sf)

library(tidyverse)
#download the PADUS dataset
temp <- tempfile()
download.file("https://prod-is-usgs-sb-prod-content.s3.amazonaws.com/61794fc2d34ea58c3c6f9f69/PADUS3_0Geodatabase.zip?AWSAccessKeyId=AKIAI7K4IX6D4QLARINA&Expires=1684943672&Signature=66zU8Uyt1RMWPtC8QqQwKZ3dafQ%3D",temp)
unzip(temp, exdir = here::here("data/original/state/"))
unlink(temp)

st_layers(here::here("data/original/state/PAD_US3_0.gdb/"))
padus.gdb <- st_read(here::here("data/original/state/PAD_US3_0.gdb/"), layer =  "PADUS3_0Combined_Proclamation_Marine_Fee_Designation_Easement")

#filter out for open acess and non-marine and conus
not.conus <- c("AK", "HI", "PR", "VI", "GU", "UNKF")
pvt <- c("PVT", "UNK", "UNKL", "TRIB") #removing lands where ownership is private or restricted

open.access <- padus.gdb %>% 
  filter(Pub_Access == "OA") %>% 
  filter(FeatClass != "Marine") %>% 
  filter(!State_Nm %in% not.conus) %>% 
  filter(!Mang_Name %in% pvt)

#make the polygons valid
library(gdalUtilities)

ensure_multipolygons <- function(X) {
  tmp1 <- tempfile(fileext = ".gpkg")
  tmp2 <- tempfile(fileext = ".gpkg")
  st_write(X, tmp1)
  ogr2ogr(tmp1, tmp2, f = "GPKG", nlt = "MULTIPOLYGON")
  Y <- st_read(tmp2)
  st_sf(st_drop_geometry(X), geom = st_geometry(Y))
}

open.access.mpg <- ensure_multipolygons(open.access) 
open.access.valid <- st_make_valid(open.access.mpg)

open.access.vect <- open.access.valid %>% 
  st_transform(., crs(template.rast)) %>% 
  st_combine(.) %>% 
  st_collection_extract(., "POLYGON")

st_write(open.access.vect, here::here("data/processed/processed_state/open_access_valid.shp"), append=FALSE)

## had to run distance to padus in earth engine so downloading product here
library(googledrive)
file <- "https://drive.google.com/drive/u/0/folders/17YW_9JvWX4RZZv18stL6DZolIxTLgnUR"
folder <- drive_get(as_id(file))
gdrive_files <- drive_ls(folder)

drive_download(as_id(gdrive_files$id[1]),  path = paste0(here::here("data/processed/processed_state/"), gdrive_files[1, "name"]), overwrite = TRUE)

library(terra)
template.rast <- rast(here::here("data/processed/processed_state/us_rough.tif"))
padus.dist.raw <- rast(here::here("data/processed/processed_state/distPADUS30.tif"))

padus.resamp <- resample(padus.dist.raw, template.rast)
padus.mask <- crop(padus.resamp, template.rast, mask=TRUE)                     

writeRaster(padus.mask, filename = here::here("data/processed/processed_state/dist_to_pa_msk.tif"))
