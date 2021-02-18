# load libraries
library(sf)
library(dplyr)
library(ggplot2)

dir <- 'C:/Users/Lena/Documents/Uni_Marburg/WS_2021/Globaler Wandel/GIS/CIESIN2016/sdei-global-uhi-2013-shp/shp'
cities_UHI <- st_read(file.path(dir, "sdei-global-uhi-2013.shp" ))
climate <- st_read("C:/Users/Lena/Documents/Uni_Marburg/WS_2021/Globaler Wandel/GIS/koeppen_geiger_climate/c1976_2000.shp")
climate$agg <- substr(climate$GRIDCODE, 1, 1)

climate <- st_transform(climate, crs = st_crs(cities_UHI))

cities_UHI <- st_make_valid(cities_UHI)
climate <- st_make_valid(climate)

cities_UHI <- st_join(cities_UHI, climate, join = st_intersects, largest = TRUE)

cities_UHI_filter <- filter(cities_UHI, ES00POP > 0, 
                            ES90POP > 0,
                            ES95POP > 0,
                            SQKM_FINAL > 0)

plot(cities_UHI_50$ES00POP ~ cities_UHI_50$D_T_DIFF)

hist(cities_UHI_50$ES95POP)
boxplot(cities_UHI_50$ES95POP)



ggplot(data = cities_UHI_filter, aes(x = ES00POP, y = D_T_DIFF))+
  scale_x_continuous(trans = "log2")+
  geom_point(size = 0.5) +
  #geom_smooth(aes(color = agg))
  facet_wrap(~agg)

ggplot(data = cities_UHI_filter, aes(x = agg, y = D_T_DIFF))+
  geom_boxplot()
