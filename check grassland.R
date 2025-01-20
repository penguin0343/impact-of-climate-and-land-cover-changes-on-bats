library(raster)
library(rgdal)
library(dplyr)
library(xlsx)
library(gdalUtils)
library(ggplot2)

#######calculate proportion layer within grassland
lc.li <- raster('D:/GIS data/Global landcover/Global simulation/modis landcover latlong.tif')
lc.li <- crop(lc.li, c(100, 110, 6,30))
lc.li <- mask(lc.li, vn)
grassland.li <- lc.li==3
forest.li <- lc.li==2
grassland.li[grassland.li==0] <- NA
forest.li[forest.li==0] <- NA

lc.layer <- c('urban','rice','other_crop','grass_shrub','orchard','evergreen_broadleaf_forest',
              'confiferous_forest','decidous_forest','plantation_forest','mangrove','water')
grassland.lc.pro <- data.frame(lc.layer)
grassland.lc.pro$proportion <- 0
for(layer in c('urban','rice','other_crop','grass_shrub','orchard','evergreen_broadleaf_forest',
               'confiferous_forest','decidous_forest','plantation_forest','mangrove','water')){
 lc <-  raster(paste0('D:/GIS data/VN_land-cover_jaxa/ver2006_VT_HRLULC_10m/proportion_cover/ver2006VT_HRLULC_',layer,'.tif'))
 lc <- lc > 36
 lc <- resample(lc, grassland.li, method='ngb')
 grassland.lc.pro$proportion[grassland.lc.pro$lc.layer == layer] <- cellStats(lc*grassland.li,'sum')/cellStats(grassland.li,'sum')*100
 print(layer)
 }

figure <- grassland.lc.pro%>%ggplot(aes(x="", y=proportion, 
                                        fill=factor(lc.layer, levels = c('water','urban','rice','other_crop','orchard','plantation_forest','grass_shrub','mangrove',
                                                                        'confiferous_forest','decidous_forest','evergreen_broadleaf_forest')))) +
  geom_bar(stat="identity", width=1) + theme_classic()+
  labs(x='',y='', title = 'Proportion of layer within grassland')+
  coord_polar("y", start=0)+
  scale_fill_brewer(palette="PiYG")+
  theme(legend.title = element_blank())
ggsave(figure, filename = 'E:/Working/2018/PhD thesis/SDM output R/19-7-21/interactive_effects/new/figures/grassland_proportion.tiff',
       width = 15, height = 15, units = 'cm')
#######calculate proportion layer within grassland
forest.lc.pro <- data.frame(lc.layer)
forest.lc.pro$proportion <- 0
for(layer in c('urban','rice','other_crop','grass_shrub','orchard','evergreen_broadleaf_forest',
               'confiferous_forest','decidous_forest','plantation_forest','mangrove','water')){
  lc <-  raster(paste0('D:/GIS data/VN_land-cover_jaxa/ver2006_VT_HRLULC_10m/proportion_cover/ver2006VT_HRLULC_',layer,'.tif'))
  lc <- lc > 34
  lc <- resample(lc, forest.li, method='ngb')
  forest.lc.pro$proportion[forest.lc.pro$lc.layer == layer] <- cellStats(lc*forest.li,'sum')/cellStats(forest.li,'sum')*100
  print(layer)
}

figure <- forest.lc.pro%>%ggplot(aes(x="", y=proportion, 
                                     fill=factor(lc.layer, levels = c('water','urban','rice','other_crop','orchard','plantation_forest','grass_shrub','mangrove',
                                                                      'confiferous_forest','decidous_forest','evergreen_broadleaf_forest')))) +
  geom_bar(stat="identity", width=1) + theme_classic()+
  labs(x='',y='', title = 'Proportion of layer within forest')+
  coord_polar("y", start=0)+
  scale_fill_brewer(palette="PiYG")+
  theme(legend.title = element_blank())
ggsave(figure, filename = 'E:/Working/2018/PhD thesis/SDM output R/19-7-21/interactive_effects/new/figures/forest_proportion.tiff',
       width = 15, height = 15, units = 'cm')

#check 2015
lc.layer <- c('water','urban','forest','paddy','orchard','barren','crop')
grassland.lc.pro <- data.frame(lc.layer)
grassland.lc.pro$proportion <- 0
for(layer in c('water','urban','forest','paddy','orchard','barren','crop')){
  lc <-  raster(paste0('D:/GIS data/VN_land-cover_jaxa/ver1908_VT_Anuual_LULC_50m/2015/proportion/',layer,'.tif'))
  lc <- lc > 40
  lc <- resample(lc, grassland.li, method='ngb')
  grassland.lc.pro$proportion[grassland.lc.pro$lc.layer == layer] <- cellStats(lc*grassland.li,'sum')/cellStats(grassland.li,'sum')*100
  print(layer)
}

##########check proportion of land-cover modis
lc.modis.2010 <- raster('D:/GIS data/MODIS/Landcover_yearly_500m/LandCover_Type_Yearly_500m_v6/LC2/MCD12Q1_LC2_2010_001.tif')
lc.modis.2010 <- crop(lc.modis.2010, c(100,110,5,30))
vn <- getData('GADM', country='VN', level = 0)
lc.modis.vn <- mask(lc.modis.2010, vn)
summary(lc.modis.vn)
hist(lc.modis.vn$MCD12Q1_LC2_2010_001)
proportion.modis <- freq(lc.modis.vn)
