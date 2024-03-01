library(terra)
library(sf)
library(tmap)
library(tidyr)
library(ggplot2)


# These rasters were created with Google Earth Engine

rMean <- rast('rasters/nbr_mean_2019_2023_summer.tif')
rSD <- rast('rasters/nbr_sd_2019_2023_summer.tif')

rlist <- list.files('rasters','median_summer_sr.tif',full.names = T)

# persistent vegetation mask
mask <- rast('rasters/output/ndvi_19_20_gr402_stream.tif')

for (yr in 2019:2023) {
  r <- rast(rlist[grep(yr, rlist)]) # GEE
  rz <- (r-rMean)/rSD
  rz <- rz*mask
  writeRaster(rz, paste0('rasters/output/nbr_z_',yr,'_summer.tif'))
}

# boxplot figure
x <- st_read('shapes/random_10000points.gpkg')
files <- list.files('rasters/output','nbr_z',full.names = T)

r <- rast(files)
names(r) <- 2019:2023

e <- terra::extract(r, x)
df <- as.data.frame(e)
df <- pivot_longer(df, cols = `2019`:`2023`)

gg <- ggplot(df, aes(y=value, x=name))+geom_boxplot(width=0.6, outlier.shape = NA, position = position_dodge(width = 0.8)) +
  xlab('Year') + ylab('Z-value') + theme_bw(base_size = 15) + ggtitle('   A') +
  theme(legend.position = "none", plot.title = element_text(margin = margin(t=30, b=-36)))

ggsave('plots/boxplot_zvalue.png', gg, width=5, height=6)

# create rasters for z values less than -1
r1 <- r[[1]] # 2019
r1[r1>(-1)] <- 0
writeRaster(r1, 'rasters/output/z_2019_lt_1.tif', gdal="COMPRESS=DEFLATE")
r1 <- r[[5]] # 2023
r1[r1>(-1)] <- 0
writeRaster(r1, 'rasters/output/z_2023_lt_1.tif', gdal="COMPRESS=DEFLATE")



anomalies <- st_read('shapes/anomalies.gpkg')
xx <- st_buffer(anomalies, 1500)

files <- list.files('rasters/output','nbr_z',full.names = T)

r <- rast(files)
names(r) <- 2019:2023

rc19 <- crop(r[[1]], st_buffer(anomalies[2,], 2500))

zz1 <- tm_shape(rc19) + tm_raster(style = 'cont', breaks=c(-2,0,2), palette="RdYlGn", title='Z-value 2019')+
  tm_credits('B', position = c('left', 'top'), size = 1.3) + tm_scale_bar(position = c('right','bottom')) +
  tm_layout(inner.margins = c(0,0,0,0), legend.position = c('right','top'), legend.bg.color = 'white')


rc23 <- crop(r[[5]], st_buffer(anomalies[1,], 1000))

zz2 <- tm_shape(rc23) + tm_raster(style = 'cont', breaks=c(-2,0,2), palette="RdYlGn", title='Z-value 2023')+
  tm_credits('C', position = c('left', 'top'), size = 1.3) + tm_scale_bar(position = c('right','bottom')) +
  tm_layout(inner.margins = c(0,0,0,0), legend.position = c('right','top'), legend.bg.color = 'white')

zz <- tmap_arrange(zz1, zz2, outer.margins = c(0.01,0.01,0.01,0.01), ncol=2)


tmap_save(zz, filename = 'plots/z-value_map.png', height = 5, width = 10)




