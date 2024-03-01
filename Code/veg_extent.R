library(sf)
library(terra)
library(dplyr)
library(tidyr)
library(ggplot2)
library(pROC)

# field plot locations manually moved using QGIS / ArcGIS
pnts <- st_read('shapes/Modelling Points/points_with_vv_fixed.gpkg')

# 10th percentile ndvi from 2019_2020
r <- rast('rasters/ndvi_2019_20_p10_sr.tif')

e <- terra::extract(r, pnts)

pnts$ndvi <- e[,2]

x <- pnts
st_geometry(pnts) <- NULL

ggplot(x, aes(x=ndvi, colour=Class)) + geom_density()

x$Class <- case_when(x$Class=='Grass' ~ 'Grass',
                      TRUE ~ 'Woody')

gg <- ggplot(x, aes(x=ndvi, colour=Class)) + geom_density(linewidth=1) + scale_colour_manual(values = c('#d69404','#12d604')) + xlab('NDVI') +ylab('Density')
gg
# ggsave('plots/density.png', gg, width=8, height=6)

# To create the ROC curve, you can use the roc() function
roc_object <- roc(x$Class, x$ndvi) 

# To plot the ROC curve, you can use the plot() function
plot(roc_object)

gmeans = sqrt(roc_object$sensitivities * roc_object$specificities)
roc_object$thresholds[which(gmeans==max(gmeans))] # threshold value = 401.5

max(gmeans) # 0.91

xG <- subset(x, Class=='Grass')
nrow(subset(xG, ndvi>401.5)) # 9/62 0.145 > 0.85 - accuracy of grass class

xF <- subset(x, Class!='Grass')
nrow(subset(xF, ndvi<401.5)) # 16/444 0.03603604 > 0.96 - accuracy of woody veg class

# create threshold raster
r[r<402] <- 0
r[r>401] <- 1
writeRaster(r, 'rasters/output/ndvi_19_20_gr402.tif', datatype='INT1U')

# r <- rast('rasters/ndvi_19_20_gr402.tif')

# load in stream data
streams <- st_read('shapes/mwstr_v13.gpkg', 'streams')
streams <- st_transform(streams, 'EPSG:28355')
# write new layer
st_write(streams, 'shapes/streams_new.gpkg')

# layer edited in QGIS to flag large streams (rivers) - stream network data is only center lines, so large rivers given a slightly larger buffer to account for width of river
streams <- st_read('shapes/streams_new.gpkg')
streamsbig <- subset(streams, Big==1)
streamssmall <- subset(streams, is.na(Big))

streamsb1 <- st_buffer(streamssmall, 50) # apply 50m buffer to all but the largest
streamsb2 <- st_buffer(streamsbig, 80) # apply 80m buffer to large streams
streasmb <- rbind(streamsb1, streamsb2)

# rasterize buffered stream layer
sr <- rasterize(streasmb, r, values=1)
writeRaster(sr, 'rasters/output/streams_50m_buffer.tif', datatype='INT1U', overwrite=T)

# intersect with NDVI threshold layer to create persistent veg mask
r <- sr*r
writeRaster(r, 'rasters/output/ndvi_19_20_gr402_stream.tif', datatype='INT1U', overwrite=T)

# create random sample points to extract from slope and z-value rasters
r <- rast('rasters/output/ndvi_19_20_gr402_stream.tif')
r[r==0] <- NA
x <- as.points(r)
x <- x[sample(1:nrow(x), 10000)]
x <- st_as_sf(x)
x$layer <- 1:10000
st_write(x, 'shapes/random_10000points.gpkg')





