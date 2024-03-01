# Creates Figure 2 in paper from Google Earth Engine point extracts of Sentinel-2 NDVI

library(dplyr)
library(ggplot2)

x <- read.csv('data/ee-chart_ndvi_geom3_forest.csv')
y <- read.csv('data/ee-chart_ndvi_geom4_crop.csv')
z <- read.csv('data/ee-chart_ndvi_geom2_grass.csv')
w <- read.csv('data/ee-chart_ndvi_geom1_riparian.csv')

w$Date <- as.Date(w$system.time_start, "%b %d, %Y")
w$NDVI <- as.numeric(w$NDVI)
w <- filter(w, !is.na(NDVI))
w$Type <- 'Riparian'

x$Date <- as.Date(x$system.time_start, "%b %d, %Y")
x <- filter(x, !is.na(NDVI))
x$Type <- 'Forest'

y$Date <- as.Date(y$system.time_start, "%b %d, %Y")
y <- filter(y, !is.na(NDVI))
y$Type <- 'Crop'

z$Date <- as.Date(z$system.time_start, "%b %d, %Y")
z <- filter(z, !is.na(NDVI))
z$Type <- 'Grass'

xx <- bind_rows(w, x, y, z)
xx$NDVI <- xx$NDVI/1000
xx <- filter(xx, Date < '2023-04-15')

xx$Year <- substr(xx$Date, 1, 4)
xx$Month <- substr(xx$Date, 6, 7)

xm <- xx %>% group_by(Type, Year, Month) %>% summarise(NDVI_med=median(NDVI))
xm$Date <- as.Date(paste0(xm$Year,'-',xm$Month,'-',15))

xp10 <- xx %>% group_by(Type, Year) %>% summarise(NDVI_p10=quantile(NDVI,0.1))
xp10$Date <- as.Date(paste0(xp10$Year,'-06-30'))
xp10 <- filter(xp10, Year > 2018 & Year < 2023)

xS <- filter(xx, Month %in% c('12','01','02'))
xS$Year <- as.numeric(xS$Year)
xS$Year <- case_when(xS$Month=='12' ~ xS$Year+1,
                      TRUE ~ xS$Year)
xS <- xS %>% group_by(Type, Year) %>% summarise(NDVI_med=median(NDVI))
xS$Date <- as.Date(paste0(xS$Year,'-01-',15))

gg <- ggplot(xS, aes(x=Date, y=NDVI_med)) +
  geom_point(data=xx, aes(x=Date, y=NDVI), colour='darkgrey', size=0.8) +
  geom_line(data=xm, aes(x=Date, y=NDVI_med)) +
  geom_point(colour='black', shape=18, size=4) + facet_wrap(vars(Type)) +
  stat_smooth(method = "lm", formula = y ~ x, geom = "smooth", se = FALSE, colour='black', linetype='dashed') +
  geom_point(data=xp10, aes(x=Date, y=NDVI_p10), colour='black', shape=2, size=3) +
  ylab('NDVI')+xlab('Year')+theme_bw(base_size = 16)

ggsave('plots/ndvi_ts_example_figure.png', gg, width=12, height = 9)

