#download 1km topography data, crop to us,  process into topographic roughness, save
library(terra)
library(geodata)


elev.na <- elevation_30s(country="USA", path=here::here("data/original/state/"))
us.rough <- terrain(elev.na, v="TRI")


writeRaster(us.rough, filename = here::here("data/processed/processed_state/us_rough.tif"))
