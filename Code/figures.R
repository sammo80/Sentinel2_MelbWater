library(raster)
library(sf)
library(tmap)
library(terra)
library(dplyr)

streams <- st_read('shapes/streams_new.gpkg')
streams <- subset(streams, type != 'stream extensions')

boundary <- st_read('shapes/mwstr_v13.gpkg', 'region_boundary')
boundary <- st_transform(boundary, 'EPSG:28355')
boundary_alb <- st_transform(boundary, 'EPSG:3577')
aus <- st_read('shapes/aus_main_simplify.shp')
st_crs(aus) <- 'EPSG:3577'

points <- st_read('shapes/Modelling Points/points_with_vv.gpkg')

# study area map
inset <- tm_shape(aus) + tm_borders(lwd=0.5) +
  tm_shape(st_as_sfc(st_bbox(boundary_alb))) + tm_borders(col='red', lwd=2)


xy <- st_bbox(boundary)
asp <- (xy$ymax - xy$ymin)/(xy$xmax - xy$xmin)
xy <- st_bbox(aus)
asp2 <- (xy$xmax - xy$xmin)/(xy$ymax - xy$ymin)

files <- list.files('rasters', 'sr_2022_median_summer', full.names = T)

r <- vrt(files)
r <- r[[c(3,7,9)]]
r2 <- aggregate(r, 5, fun='mean')
r2 <- mask(r2, boundary)
r2 <- crop(r2, boundary)
r2[r2>5000] <- 5000

rh <- rast('rasters/hillshade.tif')
rh <- aggregate(rh, 10, fun='mean')
crs(rh) <- 'EPSG:28355'
rh <- mask(rh, boundary)
rh <- crop(rh, boundary)
tm_shape(rh) + tm_raster(palette = grey(1:100/100))

ssite <- tm_shape(rh, raster.downsample = F) + tm_raster(palette = grey(1:100/100),legend.show=F)+
  tm_shape(r2, raster.downsample = F) + tm_rgb(3,2,1, max.value = 5000, alpha = 0.7) +
  tm_shape(boundary) + tm_borders(lwd=0.5) +
  tm_shape(streams) + tm_lines(col='blue', lwd=0.3) +
  tm_shape(points) + tm_dots(col='yellow2') +
  tm_compass(position = c(0.92,0.08)) + tm_scale_bar(position = c(0.69,0.01))

w <- 0.23
h <- asp2*w
vp <- grid::viewport(x=0.135, y=0.149, width = w, height=h)

tmap_save(ssite, filename="plots/studyarea_map.png",
          dpi=300,
          height=8*asp, width=8,
          insets_tm = inset,
          insets_vp = vp)




################# predicted map ###################

x <- rast('rasters/output/predicted_masked.tif')
x[x==0] <- NA

# inset extent
e <- ext(c(361701.5717, 366120.8375, 5817814.0411, 5821799.6634))
# inset
x_inset <- crop(x, e)

xpoly <- st_as_sfc(st_bbox(x_inset))
xpoly <- st_buffer(xpoly, 1000)
xpoly <- st_as_sfc(st_bbox(xpoly))

x2 <- aggregate(x, 3, fun='mean', na.rm=T)
x2 <- crop(x2, boundary)

inset <- tm_shape(x_inset) + tm_raster(style = 'cont', palette=viridisLite::magma(20), legend.show = F, colorNA = 'grey')+
  tm_layout(inner.margins = c(0,0,0,0))

ssite <- tm_shape(boundary) + tm_fill(col='grey') +
  tm_shape(x2, raster.downsample = F) + tm_raster(style = 'cont', palette=viridisLite::magma(20), title='Condition Score') +
  tm_shape(xpoly) + tm_borders(col='black') +
  tm_shape(boundary) + tm_borders(lwd=0.5) +
  tm_layout(legend.position = c('right','top'))

xy <- st_bbox(boundary)
asp <- (xy$ymax - xy$ymin)/(xy$xmax - xy$xmin)
xy <- st_bbox(x)
asp2 <- (xy$xmax - xy$xmin)/(xy$ymax - xy$ymin)

w <- 0.28
h <- asp2*w
vp <- grid::viewport(x=0.16, y=0.18, width = w, height=h)

tmap_save(ssite, filename="plots/rf_predicted_map.png",
          dpi=300,
          height=8*asp, width=8,
          insets_tm = inset,
          insets_vp = vp)

