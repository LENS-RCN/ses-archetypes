##download per pixel rate of change in mean temp and precip
library(googledrive)

#Burch Fisher generated annual estimates of the rate of change for each pixel from 1981-2022 using PRISM data. Files are in a google drive
file <- "https://drive.google.com/drive/folders/11G74DCOHv8DgV1w36KdktlzguEeCP9Yg"
folder <- drive_get(as_id(file))
gdrive_files <- drive_ls(folder)

drive_download(as_id(gdrive_files$id[3]),  path = paste0(here::here("data/processed/processed_slow/"), gdrive_files[3, "name"]), overwrite = TRUE)
drive_download(as_id(gdrive_files$id[4]),  path = paste0(here::here("data/processed/processed_slow/"), gdrive_files[4, "name"]), overwrite = TRUE)

#process to align with existing data

tmean.chg <- rast(here::here("data/processed/processed_slow/tmean_C_per_year_slope.tif"))
ppt.chg <- rast(here::here("data/processed/processed_slow/ppt_mm_per_year_slope.tif"))

template.rast <- rast(here::here("data/processed/processed_state/us_rough.tif"))

temp.chg.proj <- project(tmean.chg, template.rast)
ppt.chg.proj <- project(ppt.chg, template.rast)


writeRaster(temp.chg.proj, filename = here::here("data/processed/processed_slow/tmean_chg_resamp.tif"), overwrite=TRUE)
writeRaster(ppt.chg.proj, filename = here::here("data/processed/processed_slow/ppt_chg_resamp.tif"), overwrite=TRUE)
