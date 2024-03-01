# Create the Theil-Sen slope with summer median NBR layers (from GEE)

library(MASS)
library(RobustLinearReg)
library(terra)
library(ggplot2)
library(tmap)

# load rasters created from GEE
r <- rast(c('rasters/nbr_2019_median_summer_sr.tif','rasters/nbr_2020_median_summer_sr.tif','rasters/nbr_2021_median_summer_sr.tif','rasters/nbr_2022_median_summer_sr.tif','rasters/nbr_2023_median_summer_sr.tif'))

f <- function(y) {
  x <- 1:5
  xx <- RobustLinearReg::theil_sen_regression(y~x)
  return(xx$coefficients[2])
}
  
s <- app(r, f, cores=4)

writeRaster(s, 'rasters/output/theil_sen_slope.tif')


s <- rast('rasters/output/theil_sen_slope.tif')
sr <- rast('rasters/output/ndvi_19_20_gr402_stream.tif')
sr[sr==0] <- NA

s <- s*sr
writeRaster(s, 'rasters/output/theil_sen_slope_masked.tif', overwrite=T)

######## Create map figure and histogram ##########
x <- st_read('shapes/random_10000points.gpkg')
r <- rast('rasters/theil_sen_slope_masked.tif')
df <- as.data.frame(terra::extract(r, x))

gg <- ggplot(df, aes(x=lyr.1)) + geom_histogram(bins = 30, color = "black", fill = "white") + 
  scale_x_continuous(limits = c(-150, 150)) + 
  xlab('Slope') + ggtitle('   A') + theme_bw(base_size = 15) +
  theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(), 
        panel.grid = element_blank(),
        plot.title = element_text(margin = margin(t=30, b=-36))) 

ggsave('plots/slope_histogram.png', gg, width=6, height=6)


anomalies <- st_read('shapes/anomalies.gpkg')

r2 <- crop(r, st_buffer(anomalies[3,],1500))

zz <- tm_shape(r2) + tm_raster(style = 'cont', breaks=c(-100,0,100), palette="RdYlGn", title='Slope 2019-2023') +
  tm_credits('B', position = c('left', 'top'), size = 1.3) + tm_scale_bar(position = c('right','bottom')) +
  tm_layout(inner.margins = c(0,0,0,0), outer.margins = c(0.01,0.01,0.01,0.01), legend.position = c('right','top'), legend.bg.color = 'white')

tmap_save(zz, filename = 'plots/slope_map_example.png', height = 5, width = 5)


