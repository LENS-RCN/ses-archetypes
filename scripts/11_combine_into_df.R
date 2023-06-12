library(terra)
library(rassta)
file.list <- list.files(here::here("data/processed/"), pattern = "*.tif", recursive = TRUE, full.names = TRUE)[c(1:10,12,14:15, 17:18)]


all.rsts <- rast(file.list)
alr.scl <- scale(all.rsts)


alr.som <- som_gap(
  alr.scl,
  xdim = 20,
  ydim = 20,
  topo = "hexagonal",
  neighbourhood.fct = "gaussian",
  rlen = 600,
  dist.fcts = c("sumofsquares", "manhattan"),
  mode = "pbatch",
  K.max = 20,
  stand = FALSE,
  B = 500,
  d.power = 2,
  spaceH0 = "original",
  method = "globalSEmax",
  SE.factor = 1
)

alr.pam <- som_pam(alr.scl, alr.som$SOM, alr.som$Kopt, metric = "manhattan", stand = FALSE)

alr.pam.rst <- alr.pam$sompam.rast

alr.pamsompam.rst <- alr.pam.rst$SOMPAM

writeRaster(alr.pamsompam.rst, filename = here::here("data/processed/sompam.tif"))

saveRDS(alr.som, file=here::here("data/processed/allsom.rds"))
saveRDS(alr.pam, file=here::here("data/processed/allpom.rds"))


