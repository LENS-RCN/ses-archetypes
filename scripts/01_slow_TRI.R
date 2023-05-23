#download 1km topography data, crop to us,  process into topographic roughness, save
library(terra)
library(geodata)
temp <- tempfile()
download.file("https://www.sciencebase.gov/catalog/file/get/4fb5495ee4b04cb937751d6d?f=__disk__08%2Fbe%2F97%2F08be978091dd8d3229f74b8f4a7243006973686d",temp)
unzip(temp, exdir = here::here("data/original/slow/"))

elev.na <- elevation_30s(country="USA", path=here::here("data/original/slow/"))
us.rough <- terrain(elev.na, v="TRI")


writeRaster(us.rough, filename = here::here("data/processed/processed_slow/us_rough.tif"))
