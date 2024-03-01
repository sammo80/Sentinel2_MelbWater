library(randomForest)
library(terra)
library(sf)
library(tmap)
library(ggplot2)

# load in modelling points
pnts <- st_read('shapes/Modelling Points/points_with_vv_fixed.gpkg')

# load in vegetation mask
r <- rast('rasters/output/ndvi_19_20_gr402_stream.tif')

# extract points
# this section is to filter the dataset to only include forest and woody veg pnts that intersect with the veg mask
e <- terra::extract(r, pnts)

pnts$layer <- e$layer

xx <- subset(pnts, layer==1)
xx <- subset(xx, Class!='Grass')
xx <- xx[,c(1,5,6,8,9,18)]
xx$Total <- xx$vv_A_struc+xx$vv_B_richn+xx$vv_D_patch+xx$vv_E_regen

files <- list.files('rasters','tif$', full.names = T)
files <- files[grep('sr_2022_median_summer', files)]

rvars <- vrt(files)
names(rvars) <- c('B2','B3','B4','B5','B6','B7','B8','B8A','B11','B12')

rM <- rast('rasters/nbr_mean_2019_2023_summer.tif')
rSD <- rast('rasters/nbr_sd_2019_2023_summer.tif')
rSlope <- rast('rasters/output/theil_sen_slope_masked.tif')
names(rSlope) <- 'Slope'
rSlope[is.na(rSlope)] <- 0

# predictor variables as raster stack
rvars <- c(rvars, rM, rSD, rSlope)

# extract point locations for modelling
xvars <- terra::extract(rvars, xx)
xvars$Score <- xx$Total

set.seed(7)
rf <- randomForest(y=xvars$Score, x=xvars[,2:14], importance = T)

xImp <- as.data.frame(rf$importance)
xImp$Variable <- row.names(xImp)
xImp <- xImp[order(xImp$`%IncMSE`),]
xImp$Variable <- factor(xImp$Variable, levels = xImp$Variable)
gg <- ggplot(xImp, aes(x=Variable, y=`%IncMSE`)) + geom_bar(width=0.8, stat = 'identity', color = "black", fill = "white") + 
  coord_flip() + ggtitle('A') +
  theme_bw(base_size = 15)

ggsave('plots/rf_importance.png', gg, width=5, height=6)

xr <- predict(rvars, rf)

writeRaster(xr, 'rasters/output/predicted.tif')
mask <- rast('rasters/output/ndvi_19_20_gr402_stream.tif')

x2 <- xr*mask
x2[x2==0] <- NA
writeRaster(x2, 'rasters/output/predicted_masked.tif', gdal="COMPRESS=DEFLATE")

xvals <- x2[]
xvals <- subset(xvals, !is.na(xvals))
xvals <- as.data.frame(xvals)
gg <- ggplot(xvals, aes(x=lyr1)) + geom_histogram(color = "black", fill = "white") + 
  ggtitle('B') + theme_bw(base_size = 15) + xlab('Predicted Condition Score') + ylab('Relative Frequency') +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

ggsave('plots/rf_predicted_histogram.png', gg, width=5, height=6)




