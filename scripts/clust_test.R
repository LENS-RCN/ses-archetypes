library(terra)
library(sf)

alldata <- st_read("/Users/mattwilliamson/Downloads/LENS_data_4_Matt.gpkg")

base.rst <- rast(xmin=st_bbox(alldata)[[1]], ymin=st_bbox(alldata)[[2]], xmax=st_bbox(alldata)[[3]], ymax=st_bbox(alldata)[[4]], crs = "epsg:6350", res=2000)

tst <- rasterize(alldata, base.rst, field="drought_mean")

idx <- 1:length(alldata)
idx <- idx[-length(alldata)]

rstr.conv <- lapply(1:length(idx), function(x) rasterize(alldata, base.rst, field=colnames(alldata)[x]))
rstr.stk <- do.call(c, rstr.conv)
rstr.scl <- scale(rstr.stk)


# converting the RasterBrick to a simple list of SpatRaster
dataset <- lapply(names(rstr.scl), function(n){
  aband <- rstr.scl[[n]]
  return(aband)
})

# giving a name to each band
names(dataset) <- names(rstr.scl)
library(geocmeans)
# finding an appropriate k and m values (using a multicore plan)
future::plan(future::multisession(workers = 8))
FCMvalues <- select_parameters.mc(algo = "FCM", data = dataset, 
                                  k = 5:20, m = seq(1.1,2,0.2), spconsist = FALSE, 
                                  indices = c("XieBeni.index", "Explained.inertia",
                                              "Negentropy.index", "Silhouette.index"),
                                  verbose = TRUE)
library(ggplot2)
library(viridis)
p1 <- ggplot(FCMvalues) + 
  geom_raster(aes(x = m, y = k, fill = Silhouette.index)) + 
  geom_text(aes(x = m, y = k, label = round(Silhouette.index,2)), size = 2)+
  scale_fill_viridis() +
  coord_fixed(ratio=0.125) 

p2 <- ggplot(FCMvalues) + 
  geom_raster(aes(x = m, y = k, fill = Explained.inertia)) + 
  geom_text(aes(x = m, y = k, label = round(Explained.inertia,2)), size = 2)+
  scale_fill_viridis() +
  coord_fixed(ratio=0.125)

 ggplot(FCMvalues) + 
  geom_raster(aes(x = m, y = k, fill = Negentropy.index)) + 
  geom_text(aes(x = m, y = k, label = round(Negentropy.index,2)), size = 2)+
  scale_fill_viridis() +
  coord_fixed(ratio=0.125)

library(patchwork)
p3 <- p1 + p2

ggsave("diagnostics20cluster.png", plot = p3, width = 10, units = "in")

CM_result <- CMeans(dataset, k = 12, m = 1.5, standardize = TRUE,
                    verbose = FALSE, seed = 789, tol = 0.001, init = "kpp")

maps1 <- mapClusters(object = CM_result, undecided = 0.10)
# plotting the most likely categories

CM_result3 <- CMeans(dataset, k = 6, m = 1.5, standardize = TRUE,
                    verbose = FALSE, seed = 789, tol = 0.001, init = "kpp")

maps3 <- mapClusters(object = CM_result, undecided = 0.10)
c13 <- c(
  "dodgerblue2", "#E31A1C", # red
  "green4",
  "#6A3D9A", # purple
  "#FF7F00", # orange
  "black", "gold1",
  "skyblue2", 
  "maroon", "orchid1", "deeppink1", "blue1", "lightgray"
)

map1 <- maps1$ClusterPlot + theme(legend.position = "bottom") + scale_fill_manual(values = c13)
ggsave("clustermap12.png", plot = map1, width = 10, units = "in")

FCMvalues2 <- select_parameters.mc(algo = "FCM", data = dataset, 
                                  k = 20:30, m = seq(1.1,2,0.2), spconsist = FALSE, 
                                  indices = c("XieBeni.index", "Explained.inertia",
                                              "Negentropy.index", "Silhouette.index"),
                                  verbose = TRUE)

p4 <- ggplot(FCMvalues2) + 
  geom_raster(aes(x = m, y = k, fill = Silhouette.index)) + 
  geom_text(aes(x = m, y = k, label = round(Silhouette.index,2)), size = 2)+
  scale_fill_viridis() +
  coord_fixed(ratio=0.125) 

p5 <- ggplot(FCMvalues2) + 
  geom_raster(aes(x = m, y = k, fill = Explained.inertia)) + 
  geom_text(aes(x = m, y = k, label = round(Explained.inertia,2)), size = 2)+
  scale_fill_viridis() +
  coord_fixed(ratio=0.125)
p6 <- p4 + p5

ggsave("diagnostics30cluster.png", plot = p6, width = 10, units = "in")

FCM_result2 <- CMeans(dataset, k = 23, m = 1.5, standardize = TRUE,
                    verbose = FALSE, seed = 789, tol = 0.001, init = "kpp")

maps2 <- mapClusters(object = FCM_result2, undecided = 0.05)
c25 <- c(
  "dodgerblue2", "#E31A1C", # red
  "green4",
  "#6A3D9A", # purple
  "#FF7F00", # orange
  "black", "gold1",
  "skyblue2", "#FB9A99", # lt pink
  "palegreen2",
  "#CAB2D6", # lt purple
  "#FDBF6F", # lt orange
  "gray70", "khaki2",
  "maroon", "orchid1", "deeppink1", "blue1", "steelblue4",
  "darkturquoise", "green1", "yellow4", "yellow3",
  "darkorange4", "brown"
)
map2 <- maps2$ClusterPlot + theme(legend.position = "bottom") + scale_fill_manual(values = c25)
ggsave("clustermap23.png", plot = map2, width = 10, units = "in")

map1rast <- CM_result$rasters[['Groups']]
names(map1rast) <- "Groups"
map1combine <- c(map1rast, rstr.scl)
vals.map1 <- values(map1combine, na.rm=TRUE, data.frame=TRUE)
library(tidyverse)
vals.map1.df <- as.data.frame(vals.map1) %>% 
  pivot_longer(., drought_mean:NED30m_slope_mean, names_to = "variable", values_to = "val")
library(tidybayes)
vals.df.sum.map1 <- vals.map1.df %>% 
  group_by(variable, Groups) %>% 
  median_qi(.width = c(.5, .8, .95)) %>% 
  filter(., !(.lower <= 0 & .upper >= 0))


map1.hist <- ggplot(data= filter(vals.df.sum.map1, Groups==1), mapping=aes(
  x = fct_rev(variable), y = val, ymin = .lower, ymax = .upper,
  # size = -.width means smaller probability interval => thicker line
  # this can be omitted, geom_pointinterval includes it automatically
  # if a .width column is in the input data.
  linewidth = -.width, color=as.factor(Groups), alpha=-.width
)) +  
  geom_pointinterval(show.legend = FALSE) +
  scale_color_manual(values = c13) + 
  scale_alpha_manual(values=c(1,0.7, 0.4), aesthetic="interval_alpha") +
  scale_y_continuous(scales::modulus_trans(2))+
  scale_x_discrete()+
  ylab("Standardized Value") +
  xlab("Variable") +
  theme_bw(base_size = 10) +
  theme(legend.position = "none", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), aspect.ratio = 0.5)


+
  facet_wrap(vars(Groups), scales="free_y", ncol=1, strip.position = "left")


rstr.01 <- (rstr.stk - global(rstr.stk, "min", na.rm=TRUE)[,1])/(global(rstr.stk, "max",na.rm=TRUE)[,1] - global(rstr.stk, "min", na.rm=TRUE)[,1])

dataset.01 <- lapply(names(rstr.01), function(n){
  aband <- rstr.01[[n]]
  return(aband)
})
names(dataset.01) <- names(rstr.01)
library(geocmeans)

c7 <- c(
  "dodgerblue2", "#E31A1C", # red
  "green4",
  "#6A3D9A", # purple
  "#FF7F00", # orange
  "black", "gold1"
)

CM_result.01 <- CMeans(dataset.01, k = 6, m = 1.5, standardize = FALSE,
                    verbose = FALSE, seed = 789, tol = 0.001, init = "kpp")
maps01 <- mapClusters(object = CM_result.01, undecided = 0.30)
p.01 <- maps01$ClusterPlot + theme(legend.position = "bottom") + scale_fill_manual(values = c7)

CM_result.scl <- CMeans(dataset, k = 6, m = 1.5, standardize = FALSE,
                       verbose = FALSE, seed = 789, tol = 0.001, init = "kpp")
maps.scl <- mapClusters(object = CM_result.scl, undecided = 0.30)
p.scl30 <- maps.scl$ClusterPlot + theme(legend.position = "bottom") + scale_fill_manual(values = c7)

maps.scl <- mapClusters(object = CM_result.scl, undecided = 0.10)
p.scl10 <- maps.scl$ClusterPlot + theme(legend.position = "bottom") + scale_fill_manual(values = c7)
library(patchwork)
p.comb <- p.01 + p.scl30 + plot_layout(ncol=2, nrow=1, guides="collect")&
  theme(legend.position='bottom')
ggsave("minmaxscale.png", p.comb, width=10, height = 8, units="in")
ggsave("scalelowerthresh.png", p.scl10, width=10, height = 8, units="in")
