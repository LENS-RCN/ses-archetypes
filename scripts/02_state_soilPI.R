##Download USDA soil productivity index from https://www.fs.usda.gov/foresthealth/applied-sciences/mapping-reporting/soil-drainage.shtml
temp <- tempfile()
download.file("https://www.fs.usda.gov/foresthealth/docs/PI_240m_2020_for_L48.zip",temp)
unzip(temp, exdir = here::here("data/original/state/L48_PI/"))
unlink(temp)

library(terra)
soil.pi <- terra::rast(here::here("data/original/state/L48_PI/L48_2020_PI_240m.tif"), NA)
soil.pi.reclass <- classify(soil.pi, cbind(19, Inf, NA))
soil.pi.agg <- aggregate(soil.pi.reclass, fact = 4, fun=modal, na.rm=TRUE, cores=20)

template.rast <- terra::rast(here::here("data/processed/processed_state/us_rough.tif"))

soil.pi.proj <- project(soil.pi.agg, template.rast)
soil.pi.resamp <- resample(soil.pi.proj, template.rast, method="near")


writeRaster(soil.pi.resamp, filename = here::here("data/processed/processed_state/L48_soilPI_resamp.tif"))
