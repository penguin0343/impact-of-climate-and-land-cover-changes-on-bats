library(rgeos)
library(raster)
library(rgdal)
library(sp)
library(rJava)
library(readxl)
library(ggplot2)
library(devtools)
library(rgeos)
library(xlsx)
library(dplyr)
library(tmap)
library(dismo)
library(ggpubr)
library(RColorBrewer)
library(cowplot)
library(mapview)

latlong <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
SR.ORG8287 <- CRS('+proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs') 
extent.asia.sr <- extent(11000000, 13000000, 900000, 2600000)
vn.raster <- raster('F:/Working/2018/PhD_research/SDM output R/Map/vn raster.tif') #import Vietnam
#import list of species
species.list <- read_excel('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/list of species.xlsx')
species.list <- species.list%>%filter(Num_occurence_all > 19)

#generate current species richness
sr.current <- lapply(species.list$Scientific_name, function(sp){
  raster(paste0("F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/output_maps/",sp," current.tif"))*vn.raster
})
batsr.current <- overlay(stack(sr.current),fun=sum)
writeRaster(batsr.current, filename = 'F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_current.tif', format="GTiff", overwrite=T)

#species richness for individual GCMs
#combined model
for(gcm in c('ac', 'bc','ca','cm','cn','cs','gf','gi','ip','mi')){
  sr.2050.rcp45.both <- lapply(species.list$Scientific_name, function(sp){
    raster(paste0("F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/output_maps/",sp," 2050 rcp45 both ",gcm,".tif"))*vn.raster
  })
  batsr.2050.rcp45.both <- overlay(stack(sr.2050.rcp45.both),fun=sum)
  writeRaster(batsr.2050.rcp45.both, filename = paste0('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_2050_rcp45_both_',gcm,'.tif'), format="GTiff", overwrite=T)
  print(gcm)
}

#climate change only model
for(gcm in c('ac', 'bc','ca','cm','cn','cs','gf','gi','ip','mi')){
  sr.2050.rcp45.cl <- lapply(species.list$Scientific_name, function(sp){
    raster(paste0("F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/output_maps/",sp," 2050 rcp45 cl ",gcm,".tif"))*vn.raster
  })
  batsr.2050.rcp45.cl <- overlay(stack(sr.2050.rcp45.cl),fun=sum)
  writeRaster(batsr.2050.rcp45.cl, filename = paste0('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_2050_rcp45_cl_',gcm,'.tif'), format="GTiff", overwrite=T)
  print(gcm)
}

#land-cover change only models
sr.2050.rcp45.lc <- lapply(species.list$Scientific_name, function(sp){
  raster(paste0("F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/output_maps/",sp," 2050 rcp45 lc.tif"))*vn.raster
})
batsr.2050.rcp45.lc <- overlay(stack(sr.2050.rcp45.lc),fun=sum)
writeRaster(batsr.2050.rcp45.lc, filename = 'F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_2050_rcp45_lc.tif', format="GTiff", overwrite=T)

#consensus future prediction rcp45
gcm <- c('ac', 'bc','ca','cm','cn','cs','gf','gi','ip','mi')
sr.2050.both <- lapply(gcm, function(x){
  raster(paste0('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_2050_rcp45_both_',x,'.tif'))
})
batsr.2050.both.median <- overlay(stack(sr.2050.both), fun=median)
sr.2050.cl <- lapply(gcm, function(x){
  raster(paste0('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_2050_rcp45_cl_',x,'.tif'))
})
batsr.2050.cl.median <- overlay(stack(sr.2050.cl), fun=median)

writeRaster(batsr.2050.both.median, filename = paste0('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_2050_rcp45_both_median.tif'), format="GTiff", overwrite=T)
writeRaster(batsr.2050.cl.median, filename = paste0('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_2050_rcp45_cl_median.tif'), format="GTiff", overwrite=T)

#Plot area of landcover types
forest.current.vn <- raster('F:/Working/2018/PhD_research/enviromental_variables/land-cover/forest_current_vn.tif')
grassland.current.vn <- raster('F:/Working/2018/PhD_research/enviromental_variables/land-cover/grassland_current_vn.tif')
farmland.current.vn <- raster('F:/Working/2018/PhD_research/enviromental_variables/land-cover/farmland_current_vn.tif')
urban.current.vn <- raster('F:/Working/2018/PhD_research/enviromental_variables/land-cover/urban_current_vn.tif')

#calculate area of land-cover types
vn.raster <- raster('F:/Working/2018/PhD_research/SDM output R/Map/vn raster.tif')
forest.current <- raster('F:/Working/2018/PhD_research/enviromental_variables/current/forest.tif')*vn.raster
grassland.current <- raster('F:/Working/2018/PhD_research/enviromental_variables/current/grassland.tif')*vn.raster
farmland.current <- raster('F:/Working/2018/PhD_research/enviromental_variables/current/farmland.tif')*vn.raster
urban.current <- raster('F:/Working/2018/PhD_research/enviromental_variables/current/urban.tif')*vn.raster
vn.lc.current <- raster('F:/Working/2018/PhD_research/enviromental_variables/land-cover/MODISLandcover_2010_srorg8287.tif')*vn.raster
water.current <- vn.lc.current == 1
barren.current <- vn.lc.current == 6
other.current <-  water.current + barren.current

forest.2050.rcp45 <- raster('F:/Working/2018/PhD_research/enviromental_variables/2050/rcp45/land-cover/forest.tif')*vn.raster
grassland.2050.rcp45 <- raster('F:/Working/2018/PhD_research/enviromental_variables/2050/rcp45/land-cover/grassland.tif')*vn.raster
farmland.2050.rcp45 <- raster('F:/Working/2018/PhD_research/enviromental_variables/2050/rcp45/land-cover/farmland.tif')*vn.raster
urban.2050.rcp45 <- raster('F:/Working/2018/PhD_research/enviromental_variables/2050/rcp45/land-cover/urban.tif')*vn.raster
vn.lc.2050.rcp45 <- raster('F:/Working/2018/PhD_research/enviromental_variables/land-cover/MODISLandcover_2050_rcp45_srorg8287.tif')*vn.raster
water.2050.rcp45 <- vn.lc.2050.rcp45 == 1
barren.2050.rcp45 <- vn.lc.2050.rcp45 == 6
other.2050.rcp45 <- water.2050.rcp45 + barren.2050.rcp45

forest.2050.rcp85 <- raster('F:/Working/2018/PhD_research/enviromental_variables/2050/rcp85/land-cover/forest.tif')*vn.raster
grassland.2050.rcp85 <- raster('F:/Working/2018/PhD_research/enviromental_variables/2050/rcp85/land-cover/grassland.tif')*vn.raster
farmland.2050.rcp85 <- raster('F:/Working/2018/PhD_research/enviromental_variables/2050/rcp85/land-cover/farmland.tif')*vn.raster
urban.2050.rcp85 <- raster('F:/Working/2018/PhD_research/enviromental_variables/2050/rcp85/land-cover/urban.tif')*vn.raster
vn.lc.2050.rcp85 <- raster('F:/Working/2018/PhD_research/enviromental_variables/land-cover/MODISLandcover_2050_rcp85_srorg8287.tif')*vn.raster
water.2050.rcp85 <- vn.lc.2050.rcp85 == 1
barren.2050.rcp85 <- vn.lc.2050.rcp85 == 6
other.2050.rcp85 <- water.2050.rcp85 + barren.2050.rcp85

lc <- c("Forest",'Grassland',"Farmland",'Urban','Other')
area.lc.current <- c(cellStats(forest.current,'sum')/cellStats(vn.raster,'sum')*100,
             cellStats(grassland.current,'sum')/cellStats(vn.raster,'sum')*100,
             cellStats(farmland.current,'sum')/cellStats(vn.raster,'sum')*100,
             cellStats(urban.current,'sum')/cellStats(vn.raster,'sum')*100,
             cellStats(other.current,'sum')/cellStats(vn.raster,'sum')*100)
area.lc.rcp45 <- c(cellStats(forest.2050.rcp45,'sum')/cellStats(vn.raster,'sum')*100,
                     cellStats(grassland.2050.rcp45,'sum')/cellStats(vn.raster,'sum')*100,
                     cellStats(farmland.2050.rcp45,'sum')/cellStats(vn.raster,'sum')*100,
                     cellStats(urban.2050.rcp45,'sum')/cellStats(vn.raster,'sum')*100,
                     cellStats(other.2050.rcp45,'sum')/cellStats(vn.raster,'sum')*100)
area.lc.rcp85 <- c(cellStats(forest.2050.rcp85,'sum')/cellStats(vn.raster,'sum')*100,
                   cellStats(grassland.2050.rcp85,'sum')/cellStats(vn.raster,'sum')*100,
                   cellStats(farmland.2050.rcp85,'sum')/cellStats(vn.raster,'sum')*100,
                   cellStats(urban.2050.rcp85,'sum')/cellStats(vn.raster,'sum')*100,
                   cellStats(other.2050.rcp85,'sum')/cellStats(vn.raster,'sum')*100)
lc <- rep(c("Forest",'Grassland',"Farmland",'Urban','Other'),3)
time <- c(rep('Current',5), rep('2050 Moderate',5), rep('2050 Extreme', 5))
area <- c(area.lc.current, area.lc.rcp45, area.lc.rcp85)
sum.area <- data.frame(lc, time, area)
p <- sum.area%>%ggplot(aes(x=factor(lc, levels = c('Forest', 'Grassland','Farmland','Urban','Other')), y=area))+
  geom_bar(aes(fill=factor(time, levels = c('Current','2050 Moderate','2050 Extreme'))), position = 'dodge', stat = 'identity')+
  scale_fill_manual('',values = c('grey','dark orange','brown'))+
  theme_classic()+
  labs(x='',y='Percentage area')+
  theme(text = element_text(size = 11))
ggsave(p, filename = 'F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/figures/area land-cover.tiff', width = 13, height = 10, units = 'cm',dpi = 300)
vn <- spTransform(getData('GADM', country='VN', level=0), SR.ORG8287)


#species richness across habitats
forest.current.vn <- raster('F:/Working/2018/PhD_research/enviromental_variables/land-cover/forest_current_vn.tif')
grassland.current.vn <- raster('F:/Working/2018/PhD_research/enviromental_variables/land-cover/grassland_current_vn.tif')
farmland.current.vn <- raster('F:/Working/2018/PhD_research/enviromental_variables/land-cover/farmland_current_vn.tif')
urban.current.vn <- raster('F:/Working/2018/PhD_research/enviromental_variables/land-cover/urban_current_vn.tif')
batsr.current <- raster('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_current.tif')

batsr.current.forest <- batsr.current*forest.current.vn
batsr.current.grassland <- batsr.current*grassland.current.vn
batsr.current.farmland <- batsr.current*farmland.current.vn
batsr.current.urban <- batsr.current*urban.current.vn

batsr.current.forest <- as.data.frame(batsr.current.forest, na.rm=T)
batsr.current.grassland <- as.data.frame(batsr.current.grassland, na.rm=T)
batsr.current.farmland <- as.data.frame(batsr.current.farmland, na.rm=T)
batsr.current.urban <- as.data.frame(batsr.current.urban, na.rm=T)

summary(batsr.current.forest$layer)
summary(batsr.current.grassland$layer)
summary(batsr.current.farmland$layer)
summary(batsr.current.urban$layer)
sd(batsr.current.forest$layer)
sd(batsr.current.grassland$layer)
sd(batsr.current.farmland$layer)
sd(batsr.current.urban$layer)
lc <- c(rep('Forest', length(batsr.current.forest$layer)), rep('Grassland', length(batsr.current.grassland$layer)),
        rep('Farmland', length(batsr.current.farmland$layer)), rep('Urban', length(batsr.current.urban$layer)))
batsr <- c(batsr.current.forest$layer, batsr.current.grassland$layer, batsr.current.farmland$layer, batsr.current.urban$layer)
batsr.current.lc <- data.frame(lc, batsr)

figure <- batsr.current.lc%>%ggplot(aes(x= factor(lc, levels = c('Forest','Grassland','Farmland','Urban')), y= batsr)) +
  # geom_boxplot(outlier.size = 0.5)+
  stat_summary(geom = "boxplot", 
               fun.data = function(x) setNames(quantile(x, c(0.0, 0.25, 0.5, 0.75, 1)), c("ymin", "lower", "middle", "upper", "ymax")), 
               position = "dodge")+
  theme_classic()+
  xlab('Land-cover type') + ylab('Species richness')
ggsave(figure, filename = 'F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/figures/SR_land-cover.tiff', units = 'cm', width = 10, height = 10, dpi = 300 )


#plot maps species richness change
batsr.current <- raster('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_current.tif')
batsr.2050.both <- raster('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_2050_rcp45_both_median.tif')
batsr.2050.cl <- raster('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_2050_rcp45_cl_median.tif')
batsr.2050.lc <- raster('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_2050_rcp45_lc.tif')
SR.ORG8287 <- CRS('+proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs') 
vn <- spTransform(getData('GADM', country='VN', level=0), SR.ORG8287)
paracell.islands <- spTransform(readOGR('F:/Working/2018/PhD_research/SDM output R/Map/paracel islands.shp'), SR.ORG8287)
spratly.islands <- spTransform(readOGR('F:/Working/2018/PhD_research/SDM output R/Map/spratly islands.shp'), SR.ORG8287)
# pas <- readOGR('F:/Working/2018/PhD_research/SDM output R/Map/vn_pas.shp')

batsr.cl.current <- (batsr.2050.cl - batsr.current)/batsr.current*100
batsr.lc.current <- (batsr.2050.lc - batsr.current)/batsr.current*100
batsr.both.current <- (batsr.2050.both - batsr.current)/batsr.current*100

#Plot interaction maps
gcm <- c('ac', 'bc','ca','cm','cn','cs','gf','gi','ip','mi')
sr.interaction <- lapply(gcm, function(x){
  raster(paste0('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/interaction_types_rcp45_',x,'.tif'))
})
select.major <- function(x){
  modal(x, na.rm=T, freq=F, ties='random')
}
sr.interaction.ensemble <- overlay(stack(sr.interaction), fun=select.major)#select major interaction types across GCMs

plot.srchange.rcp45.both <-  tm_shape(batsr.both.current, bbox = c(11380000, 930000,12450000,  2517140)) + 
  tm_raster('layer',breaks = c(-Inf,-30,-0.0001,0.0001,30,Inf),palette = c('red','orange','grey70','lightblue','blue'), 
            title = '',labels = c('< -30','-30~0','0','0~30','> 30'))+
  tm_shape(paracell.islands) + tm_lines(col = 'grey')+
  tm_shape(spratly.islands) + tm_lines(col = 'grey')+
  # tm_shape(pas) + tm_polygons(col = 'green', alpha = 0,border.col = 'darkgreen')+
  tm_shape(vn)+ tm_polygons(border.col = 'dark gray', alpha = 0, lwd = 0.5)+
  tm_layout(legend.outside = F, title = '', title.size = 1, legend.position = c(0, 0.25),
            title.position = c(0,1), frame = F, legend.text.size = 0.7, legend.title.size = 1, legend.show = F) 
plot.srchange.rcp45.cl <-  tm_shape(batsr.cl.current, bbox = c(11380000, 930000,12450000,  2517140)) + 
  tm_raster('layer',breaks = c(-Inf,-25,-0.0001,0.0001,25,Inf),palette = c('red','orange','grey70','lightblue','blue'), 
            title = '% SR change')+
  tm_shape(paracell.islands) + tm_lines(col = 'grey')+
  tm_shape(spratly.islands) + tm_lines(col = 'grey')+
  # tm_shape(pas) + tm_polygons(col = 'green', alpha = 0,border.col = 'darkgreen')+
  tm_shape(vn)+ tm_polygons(border.col = 'gray', alpha = 0, lwd = 0.5)+
  tm_layout(title = '', title.size = 1,title.position = c('left','top'), frame = F,legend.show = F) 
plot.srchange.rcp45.lc <-  tm_shape(batsr.lc.current, bbox = c(11380000, 930000,12450000,  2517140)) + 
  tm_raster('layer',breaks = c(-Inf,-25,-0.0001,0.0001,25,Inf),palette = c('red','orange','grey70','lightblue','blue'), 
            title = '% SR change')+
  tm_shape(paracell.islands) + tm_lines(col = 'grey')+
  tm_shape(spratly.islands) + tm_lines(col = 'grey')+
  # tm_shape(pas) + tm_polygons(col = 'green', alpha = 0,border.col = 'darkgreen')+
  tm_shape(vn)+ tm_polygons(border.col = 'dark gray', alpha = 0, lwd = 0.5)+
  tm_layout(title = '', title.size = 1, title.position = c('left','top'), frame = F, legend.show = F)
plot.interaction <- tm_shape(sr.interaction.ensemble,bbox = c(11380000, 930000,12450000,  2517140)) +
  tm_raster('layer', breaks = c(-0.5,0.5,1.5,2.5,3.5,4.5,5.5),palette = c('white','purple4','purple','grey70','aquamarine','aquamarine4'),
            labels = c("","-S", "+A", "AD","-A","+S"),title = '')+
  tm_shape(vn)+tm_polygons(col = 'white', alpha = 0, border.col = 'grey')+
  tm_shape(paracell.islands) + tm_lines(col = 'grey')+
  tm_shape(spratly.islands) + tm_lines(col = 'grey')+
  # tm_shape(pas) + tm_polygons(col = 'green', alpha = 0,border.col = 'darkgreen')+
  tm_layout(title = '',title.position = c(0,0.95),legend.position = c(0.1,0.5), frame = F,legend.show = F)

figure <- tmap_arrange(plot.srchange.rcp45.both,plot.srchange.rcp45.cl,plot.srchange.rcp45.lc,plot.interaction, nrow = 1)
tmap_save(figure, filename = 'F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/figures/srchange_interaction_rcp45.tiff',
          units = 'cm', width = 30, height = 11, dpi=300)

#plot species richness in current and in 2050s
batsr.current <- raster('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_current.tif')
batsr.2050.both <- raster('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_2050_rcp45_both_median.tif')
batsr.2050.cl <- raster('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_2050_rcp45_cl_median.tif')
batsr.2050.lc <- raster('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_2050_rcp45_lc.tif')
SR.ORG8287 <- CRS('+proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs') 
vn <- spTransform(getData('GADM', country='VN', level=0), SR.ORG8287)
paracell.islands <- spTransform(readOGR('F:/Working/2018/PhD_research/SDM output R/Map/paracel islands.shp'), SR.ORG8287)
spratly.islands <- spTransform(readOGR('F:/Working/2018/PhD_research/SDM output R/Map/spratly islands.shp'), SR.ORG8287)
pas <- readOGR('F:/Working/2018/PhD_research/SDM output R/Map/vn_pas.shp')

plot.current<- tm_shape(batsr.current, bbox = c(11380000, 930000,12450000,  2517140)) + 
  tm_raster('layer',breaks = c(0,2,4,6,8,10,15,20,25,30,35,40,50,65), palette = 'Greens', style = 'fixed',
            title = 'Species richness')+
  tm_shape(paracell.islands) + tm_lines(col = 'grey30')+
  tm_shape(spratly.islands) + tm_lines(col = 'grey30')+
  tm_shape(pas) + tm_polygons(alpha=0, border.col = 'violetred')+
  tm_shape(vn)+ tm_polygons(border.col = 'dark gray', alpha = 0, lwd = 0.5)+
  tm_layout(legend.outside = F, title = '',title.position = c('left','top'),title.size = 1,
            legend.text.size = 0.5, legend.title.size = 1,legend.position = c(0,0.3), frame = F, legend.show = F)
plot.both<- tm_shape(batsr.2050.both, bbox = c(11380000, 930000,12450000,  2517140)) + 
  tm_raster('layer',breaks = c(0,2,4,6,8,10,15,20,25,30,35,40,50,65), palette = 'Greens', style = 'fixed',
  # tm_raster('layer',breaks = c(0,2,4,6,8,10,12,14,16,18,20,25,30,35,40,45,50,55,60,65,70), palette = 'Greens', style = 'fixed',
            title = 'Species richness')+
  tm_shape(paracell.islands) + tm_lines(col = 'grey30')+
  tm_shape(spratly.islands) + tm_lines(col = 'grey30')+
  # tm_shape(pas) + tm_polygons(col = 'green', alpha = 0,border.col = 'darkgreen')+
  tm_shape(vn)+ tm_polygons(border.col = 'dark gray', alpha = 0, lwd = 0.5)+
  tm_layout(legend.outside = F, title = '',title.position = c('left','top'),title.size = 1,
            legend.text.size = 0.5, legend.title.size = 1,legend.position = c(0,0.3), frame = F, legend.show = F)



figure <- tmap_arrange(plot.current, plot.both)
tmap_save(figure, filename = 'F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/figures/current and 2050_rcp45.tiff',
          units = 'cm', width = 15, height = 14, dpi=300)


##############################################################################
#percent change in species richness bar chart
vn.raster <- raster('F:/Working/2018/PhD_research/SDM output R/Map/vn raster.tif') #import Vietnam
batsr.current <- raster('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_current.tif')
batsr.lc.rcp45 <- raster('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_2050_rcp45_lc.tif')
batsr.lc.rcp45.current <- (batsr.lc.rcp45 - batsr.current)/batsr.current*100
pro.lc.rcp45 <- c(cellStats(batsr.lc.rcp45.current>=30,'sum')/cellStats(vn.raster,'sum')*100, 
                  cellStats(batsr.lc.rcp45.current < 30 & batsr.lc.rcp45.current > 0,'sum')/cellStats(vn.raster,'sum')*100,
                  cellStats(batsr.lc.rcp45.current == 0,'sum')/cellStats(vn.raster,'sum')*100,
                  cellStats(batsr.lc.rcp45.current < 0 & batsr.lc.rcp45.current > -30,'sum')/cellStats(vn.raster,'sum')*100,
                  cellStats(batsr.lc.rcp45.current <= -30,'sum')/cellStats(vn.raster,'sum')*100)
#effect of combined change
for(gcm in c('ac', 'bc','ca','cm','cn','cs','gf','gi','ip','mi')){
  batsr.both.rcp45 <- raster(paste0('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_2050_rcp45_both_',gcm,'.tif'))
  batsr.both.rcp45.current <- (batsr.both.rcp45 - batsr.current)/batsr.current*100
  pro.both.rcp45 <- c(cellStats(batsr.both.rcp45.current>=30,'sum')/cellStats(vn.raster,'sum')*100,
                      cellStats(batsr.both.rcp45.current < 30 & batsr.both.rcp45.current > 0,'sum')/cellStats(vn.raster,'sum')*100,
                      cellStats(batsr.both.rcp45.current == 0,'sum')/cellStats(vn.raster,'sum')*100,
                      cellStats(batsr.both.rcp45.current < 0 & batsr.both.rcp45.current > -30,'sum')/cellStats(vn.raster,'sum')*100,
                      cellStats(batsr.both.rcp45.current <= -30,'sum')/cellStats(vn.raster,'sum')*100,
                      cellStats(batsr.both.rcp45.current < 0, 'sum')/cellStats(vn.raster,'sum')*100,
                      cellStats(batsr.both.rcp45.current > 0, 'sum')/cellStats(vn.raster,'sum')*100)
  assign(paste0('pro.both.rcp45.',gcm),pro.both.rcp45)
  print(gcm)
}

pro.both.all <- data.frame(pro.both.rcp45.ac,pro.both.rcp45.bc,pro.both.rcp45.ca,pro.both.rcp45.cm,pro.both.rcp45.cn,
                           pro.both.rcp45.cs,pro.both.rcp45.gf,pro.both.rcp45.gi,pro.both.rcp45.ip,pro.both.rcp45.mi)
#effect of climate change only
for(gcm in c('ac', 'bc','ca','cm','cn','cs','gf','gi','ip','mi')){
  batsr.cl.rcp45 <- raster(paste0('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_2050_rcp45_cl_',gcm,'.tif'))
  batsr.cl.rcp45.current <- (batsr.cl.rcp45 - batsr.current)/batsr.current*100
  pro.cl.rcp45 <- c(cellStats(batsr.cl.rcp45.current>=30,'sum')/cellStats(vn.raster,'sum')*100,
                    cellStats(batsr.cl.rcp45.current < 30 & batsr.cl.rcp45.current > 0,'sum')/cellStats(vn.raster,'sum')*100,
                    cellStats(batsr.cl.rcp45.current == 0,'sum')/cellStats(vn.raster,'sum')*100,
                    cellStats(batsr.cl.rcp45.current < 0 & batsr.cl.rcp45.current > -30,'sum')/cellStats(vn.raster,'sum')*100,
                    cellStats(batsr.cl.rcp45.current <= -30,'sum')/cellStats(vn.raster,'sum')*100,
                    cellStats(batsr.cl.rcp45.current < 0, 'sum')/cellStats(vn.raster,'sum')*100,
                    cellStats(batsr.cl.rcp45.current > 0, 'sum')/cellStats(vn.raster,'sum')*100)
  assign(paste0('pro.cl.rcp45.',gcm),pro.cl.rcp45)
  print(gcm)
}
pro.cl.all <- data.frame(pro.cl.rcp45.ac, pro.cl.rcp45.bc, pro.cl.rcp45.ca, pro.cl.rcp45.cm, pro.cl.rcp45.cn,
                         pro.cl.rcp45.cs, pro.cl.rcp45.gf, pro.cl.rcp45.gi, pro.cl.rcp45.ip, pro.cl.rcp45.mi)
pro.both.all <- pro.both.all
pro.cl.all <- pro.cl.all
pro.both.median <- apply(pro.both.all,1,median)
pro.both.min <- apply(pro.both.all,1,min)
pro.both.max <- apply(pro.both.all,1,max)

pro.cl.median <- apply(pro.cl.all,1,median)
pro.cl.min <- apply(pro.cl.all,1,min)
pro.cl.max <- apply(pro.cl.all,1,max)

class <- c('>30','0~30','0','-30~0','<-30')
sum.both.sr <- data.frame(class,pro.both.median[1:5],pro.both.min[1:5], pro.both.max[1:5])
sum.cl.sr <- data.frame(class,pro.cl.median[1:5], pro.cl.min[1:5], pro.cl.max[1:5])
sum.lc.sr <- data.frame(class,pro.lc.rcp45)
sum.both.sr$class <- factor(sum.both.sr$class, levels = c('<-30','-30~0','0','0~30','>30'))
sum.cl.sr$class <- factor(sum.cl.sr$class, levels = c('<-30','-30~0','0','0~30','>30'))
sum.lc.sr$class <- factor(sum.lc.sr$class, levels = c('<-30','-30~0','0','0~30','>30'))

write.xlsx(sum.both.sr,'F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/srchange_both_rcp45.xlsx')
write.xlsx(sum.cl.sr,'F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/srchange_cl_rcp45.xlsx')
write.xlsx(sum.lc.sr,'F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/srchange_lc_rcp45.xlsx')

# predict future additive distribution
species.list <- read_excel('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/list of species.xlsx')
species.list <- species.list%>%filter(Num_occurence_all > 19)
# Current environmental layers
list.predictors <- list.files("F:/Working/2018/PhD_research/enviromental_variables/current", full.names = T, pattern = ".tif")
list.predictors <- stack(list.predictors)
climate.current <- subset(list.predictors, c("bio2","bio10","bio11","bio12","bio18","bio19"))
land.cover.current <- subset(list.predictors, c("forest","grassland","farmland","urban"))

land.cover.2050 <- list.files("F:/Working/2018/PhD_research/enviromental_variables/2050/rcp45/land-cover", full.names = T, pattern = ".tif")
land.cover.2050 <- stack(land.cover.2050)
karst <- raster('F:/Working/2018/PhD_research/enviromental_variables/current/karst.tif')
predictors.current <- subset(list.predictors, c("bio2","bio10","bio11","bio12","bio18","bio19","karst","forest","grassland","farmland","urban")) #select variables
predictors.lconly <- stack(climate.current, land.cover.2050, karst)
#predict additive distribution
for(sp in species.list$Scientific_name){
  load(paste0('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/response/',sp,'.RData')) 
  maxent.result <- read.csv(paste0('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/response/',sp,'/maxentResults.csv'), header=T, sep=',')
  px.2050.lc <- predict(predictors.lconly, xm, ext=extent(11000000, 13000000, 900000, 2600000))
  px.current <- predict(predictors.current, xm, ext=extent(11000000, 13000000, 900000, 2600000))
  for(gcm in c('ac', 'bc','ca','cm','cn','cs','gf','gi','ip','mi')){
    climate.2050 <- list.files(paste0("F:/Working/2018/PhD_research/enviromental_variables/2050/rcp45/",gcm), full.names = T, pattern = ".tif")
    climate.2050 <- stack(climate.2050)
    climate.2050 <- subset(climate.2050, c("bio2","bio10","bio11","bio12","bio18","bio19"))
    predictors.clonly <- stack(climate.2050, land.cover.current, karst)
    px.2050.cl <- predict(predictors.clonly, xm, ext=extent(11000000, 13000000, 900000, 2600000))
    px.2050.add <- px.2050.cl + px.2050.lc - px.current
    species.2050.add <- px.2050.add > maxent.result$X10.percentile.training.presence.Cloglog.threshold
    writeRaster(species.2050.add, filename = paste0('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/',sp,' 2050 rcp45 additive ',gcm,'.tif'),format='GTiff', overwrite=T)
    print(gcm)
  }
  print(sp)
}

#additive species richness for each GCMs
species.list <- read_excel('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/list of species.xlsx')
species.list <- species.list%>%filter(Num_occurence_all > 19)
extent.asia.sr <- extent(11000000, 13000000, 900000, 2600000)
vn.raster <- raster('F:/Working/2018/PhD_research/SDM output R/Map/vn raster.tif') #import Vietnam
for(gcm in c('ac', 'bc','ca','cm','cn','cs','gf','gi','ip','mi')){
  sp.2050.rcp45.add <- lapply(species.list$Scientific_name, function(sp){
    raster(paste0("F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/",sp," 2050 rcp45 additive ",gcm,".tif"))*vn.raster
  })
  batsr.2050.rcp45 <- overlay(stack(sp.2050.rcp45.add),fun=sum)
  writeRaster(batsr.2050.rcp45, filename = paste0('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_2050_rcp45_add_',gcm,'.tif'), format="GTiff", overwrite=T)
  print(gcm)
}

#generate median additive species richness
gcm <- c('ac', 'bc','ca','cm','cn','cs','gf','gi','ip','mi')
sr.2050.add <- lapply(gcm, function(x){
  raster(paste0("F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_2050_rcp45_add_",x,".tif"))
})
batsr.2050.add.median <- overlay(stack(sr.2050.add), fun=median)
writeRaster(batsr.2050.add.median, filename = 'F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_2050_rcp45_add_median.tif', format="GTiff", overwrite=T)

lc.current <- crop(raster('F:/Working/2018/PhD_research/enviromental_variables/land-cover/MODISLandcover_2010_srorg8287.tif'),extent.asia.sr)*vn.raster
lc.2050.rcp45 <- crop(raster('F:/Working/2018/PhD_research/enviromental_variables/land-cover/MODISLandcover_2050_rcp45_srorg8287.tif'),extent.asia.sr)*vn.raster
# lc.change.2050.rcp45 < - lc.2050.rcp45 - lc.current
area.withlcchange <- lc.2050.rcp45 != lc.current
cellStats(area.withlcchange,'sum')/cellStats(vn.raster,'sum')
batsr.current <- raster('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_current.tif')
batsr.2050.lc <- raster('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_2050_rcp45_lc.tif')

#classify interaction effects
for(gcm in c('ac', 'bc','ca','cm','cn','cs','gf','gi','ip','mi')){  
  batsr.2050.cl <- raster(paste0('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_2050_rcp45_cl_',gcm,'.tif'))
  batsr.2050.both <- raster(paste0('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_2050_rcp45_both_',gcm,'.tif'))
  batsr.2050.add <- raster(paste0('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_2050_rcp45_add_',gcm,'.tif'))
  sr.interaction <- batsr.current*0
  #two negative and negative neutral
  sr.interaction[batsr.2050.cl <= batsr.current & batsr.2050.lc <= batsr.current & (batsr.2050.cl + batsr.2050.lc != 2* batsr.current)&
                   batsr.2050.both < batsr.2050.add] <- 1
  sr.interaction[batsr.2050.cl <= batsr.current & batsr.2050.lc <= batsr.current & (batsr.2050.cl + batsr.2050.lc != 2* batsr.current)&
                   batsr.2050.both == batsr.2050.add] <- 3
  sr.interaction[batsr.2050.cl <= batsr.current & batsr.2050.lc <= batsr.current & (batsr.2050.cl + batsr.2050.lc != 2* batsr.current)&
                   batsr.2050.both > batsr.2050.add & batsr.2050.both <= batsr.current] <- 4
  sr.interaction[batsr.2050.cl <= batsr.current & batsr.2050.lc <= batsr.current & (batsr.2050.cl + batsr.2050.lc != 2* batsr.current)&
                   batsr.2050.both > batsr.current] <- 5
  #two positive and positive neutral
  sr.interaction[batsr.2050.cl >= batsr.current & batsr.2050.lc >= batsr.current& (batsr.2050.cl + batsr.2050.lc != 2* batsr.current)&
                   batsr.2050.both < batsr.current] <- 1
  sr.interaction[batsr.2050.cl >= batsr.current & batsr.2050.lc >= batsr.current& (batsr.2050.cl + batsr.2050.lc != 2* batsr.current)&
                   batsr.2050.both >= batsr.current & batsr.2050.both < batsr.2050.add] <- 2
  sr.interaction[batsr.2050.cl >= batsr.current & batsr.2050.lc >= batsr.current& (batsr.2050.cl + batsr.2050.lc != 2* batsr.current)&
                   batsr.2050.both == batsr.2050.add] <- 3
  sr.interaction[batsr.2050.cl >= batsr.current & batsr.2050.lc >= batsr.current& (batsr.2050.cl + batsr.2050.lc != 2* batsr.current)&
                   batsr.2050.both > batsr.2050.add] <- 5
  #opposite
  sr.interaction[(batsr.2050.cl - batsr.current)*(batsr.2050.lc - batsr.current) < 0 & 
                   batsr.2050.both < min(batsr.2050.cl, batsr.2050.lc)] <- 1
  sr.interaction[(batsr.2050.cl - batsr.current)*(batsr.2050.lc - batsr.current) < 0 & 
                   batsr.2050.both >= min(batsr.2050.cl, batsr.2050.lc) & batsr.2050.both < batsr.2050.add] <- 2
  sr.interaction[(batsr.2050.cl - batsr.current)*(batsr.2050.lc - batsr.current) < 0 & 
                   batsr.2050.both == batsr.2050.add] <- 3
  sr.interaction[(batsr.2050.cl - batsr.current)*(batsr.2050.lc - batsr.current) < 0 & 
                   batsr.2050.both > batsr.2050.add & batsr.2050.both <= max(batsr.2050.cl, batsr.2050.lc)] <- 4
  sr.interaction[(batsr.2050.cl - batsr.current)*(batsr.2050.lc - batsr.current) < 0 & 
                   batsr.2050.both > max(batsr.2050.cl, batsr.2050.lc)] <- 5
  #double neutral
  sr.interaction[batsr.2050.cl == batsr.current & batsr.2050.lc == batsr.current &
                   batsr.2050.both < batsr.2050.add] <- 1
  sr.interaction[batsr.2050.cl == batsr.current & batsr.2050.lc == batsr.current &
                   batsr.2050.both == batsr.2050.add] <- 3
  sr.interaction[batsr.2050.cl == batsr.current & batsr.2050.lc == batsr.current &
                   batsr.2050.both > batsr.2050.add] <- 5
  writeRaster(sr.interaction, filename = paste0('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/interaction_types_rcp45_',gcm,'.tif'), format="GTiff", overwrite=T)
  print(gcm)
}

###########################
#compute proportion of interaction types within areas with land-cover change only
extent.asia.sr <- extent(11000000, 13000000, 900000, 2600000)
vn.raster <- raster('F:/Working/2018/PhD_research/SDM output R/Map/vn raster.tif') #import Vietnam
lc.current <- crop(raster('F:/Working/2018/PhD_research/enviromental_variables/land-cover/MODISLandcover_2010_srorg8287.tif'),extent.asia.sr)*vn.raster
lc.2050.rcp45 <- crop(raster('F:/Working/2018/PhD_research/enviromental_variables/land-cover/MODISLandcover_2050_rcp45_srorg8287.tif'),extent.asia.sr)*vn.raster
area.withlcchange <- lc.2050.rcp45 != lc.current
area.withlcchange.area <- cellStats(area.withlcchange, 'sum')
for(gcm in c('ac', 'bc','ca','cm','cn','cs','gf','gi','ip','mi')){
  sr.interaction <- raster(paste0('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/interaction_types_rcp45_',gcm,'.tif'))*area.withlcchange
  pro.interaction <- c(cellStats(sr.interaction==1,'sum')/area.withlcchange.area*100,
                       cellStats(sr.interaction==2,'sum')/area.withlcchange.area*100,
                       cellStats(sr.interaction==3,'sum')/area.withlcchange.area*100,
                       cellStats(sr.interaction==4,'sum')/area.withlcchange.area*100,
                       cellStats(sr.interaction==5,'sum')/area.withlcchange.area*100)
  assign(paste0('pro.interaction.',gcm), pro.interaction)
  print(gcm)
}
pro.interaction.all <- data.frame(pro.interaction.ac, pro.interaction.bc, pro.interaction.ca, pro.interaction.cm, pro.interaction.cn,
                                  pro.interaction.cs, pro.interaction.gf, pro.interaction.gi, pro.interaction.ip, pro.interaction.mi)
pro.interaction.median <- apply(pro.interaction.all,1,median)
pro.interaction.min <- apply(pro.interaction.all,1,min)
pro.interaction.max <- apply(pro.interaction.all,1,max)

interaction <- c('-S','+A','AD','-A','+S')
sum <- data.frame(interaction, pro.interaction.median)

sum.both.sr <- read_excel('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/srchange_both_rcp45.xlsx')[2:5]
sum.cl.sr <- read_excel('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/srchange_cl_rcp45.xlsx')[2:5]
sum.lc.sr <- read_excel('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/srchange_lc_rcp45.xlsx')[2:3]
names(sum.both.sr) <- c('class','pro.both.median','pro.both.min','pro.both.max')
names(sum.cl.sr) <- c('class','pro.cl.median','pro.cl.min','pro.cl.max')

figure.bar.both <- sum.both.sr%>%ggplot(aes(x=factor(class,levels = c('<-30','-30~0','0','0~30','>30')),y=pro.both.median[1:5],fill=class))+
  geom_bar(stat = 'identity')+ theme_classic()+
  scale_fill_manual('',values = c('>30'='blue','0~30'='lightblue','0'='grey70','-30~0'='orange','<-30'='red'))+
  xlab('')+ylab('')+labs(title = '')+  ylim(0,70)+
  geom_errorbar(aes(ymin=pro.both.min[1:5], ymax=pro.both.max[1:5]), width=.2,position=position_dodge(.9))+
  theme(
    legend.position = 'none',
        # legend.position = c(0.1,0.5),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12, angle = 0, vjust = 0.5),
        title = element_text(size = 14))
figure.bar.cl <- sum.cl.sr%>%ggplot(aes(x=factor(class,levels = c('<-30','-30~0','0','0~30','>30')),y=pro.cl.median[1:5],fill=class))+
  geom_bar(stat = 'identity')+ theme_classic()+
  scale_fill_manual('',values = c('>30'='blue','0~30'='lightblue','0'='grey70','-30~0'='orange','<-30'='red'))+
  xlab('')+ylab('')+labs(title = '')+ ylim(0,70)+
  geom_errorbar(aes(ymin=pro.cl.min[1:5], ymax=pro.cl.max[1:5]), width=.2,position=position_dodge(.9))+
  theme(legend.position = 'none', 
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12, angle = 0, vjust = 0.5),
        title = element_text(size = 14))
figure.bar.lc <- sum.lc.sr%>%ggplot(aes(x=factor(class,levels = c('<-30','-30~0','0','0~30','>30')),y=pro.lc.rcp45[1:5],fill=class))+
  geom_bar(stat = 'identity')+ theme_classic()+
  scale_fill_manual('',values = c('>30'='blue','0~30'='lightblue','0'='grey70','-30~0'='orange','<-30'='red'))+
  xlab('')+ylab('')+labs(title = '')+ ylim(0,70)+
  theme(legend.position = 'none', 
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        title = element_text(size = 14))
figure.interaction <- ggplot(sum, aes(fill=interaction, y=pro.interaction.median, x=factor(interaction,levels = c('-S','+A','AD','-A','+S')))) +
  geom_bar(position="stack", stat="identity")+  ylim(0,70)+ theme_classic()+
  scale_fill_manual('',values = c('-S'='purple4','+A'='purple','AD'='grey70', '-A'='aquamarine','+S'='aquamarine4'))+
  annotate("text", label = "", size = 4, x = 3, y = 70)+
  xlab('')+ylab('')+labs(title = '')+
  geom_errorbar(aes(ymin=pro.interaction.min, ymax=pro.interaction.max), width=.2,position=position_dodge(.9))+
  theme(text = element_text(size = 14), legend.position = 'none', 
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12))

figure <- ggarrange(figure.bar.both,figure.bar.cl,figure.bar.lc,figure.interaction,nrow = 1)
ggsave(figure, filename = 'F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/figures/srchange_interaction_bar_rcp45.tiff', 
       units = 'cm', width = 28, height = 8, dpi=300)


# 
# 
# #Plot interaction maps
# SR.ORG8287 <- CRS('+proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs')
# vn <- spTransform(getData('GADM', country='VN', level=0), SR.ORG8287)
# paracell.islands <- spTransform(readOGR('F:/Working/2018/PhD_research/SDM output R/Map/paracel islands.shp'), SR.ORG8287)
# spratly.islands <- spTransform(readOGR('F:/Working/2018/PhD_research/SDM output R/Map/spratly islands.shp'), SR.ORG8287)
# # pas <- readOGR('F:/Working/2018/PhD_research/SDM output R/Map/vn_pas.shp')
# gcm <- c('ac', 'bc','ca','cm','cn','cs','gf','gi','ip','mi')
# sr.interaction <- lapply(gcm, function(x){
#   raster(paste0('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/interaction_types_rcp45_',x,'.tif'))
# })
# select.major <- function(x){
#   modal(x, na.rm=T, freq=F, ties='random')
# }
# sr.interaction.ensemble <- overlay(stack(sr.interaction), fun=select.major)#select major interaction types across GCMs
# figure.map <- tm_shape(sr.interaction.ensemble,bbox = c(11380000, 930000,12450000,  2517140)) +
#   tm_raster('layer', breaks = c(-0.5,0.5,1.5,2.5,3.5,4.5,5.5),palette = c('white','purple4','purple','grey70','aquamarine','aquamarine4'),
#                            labels = c("","-S", "+A", "AD","-A","+S"),title = '')+
#   tm_shape(vn)+tm_polygons(col = 'white', alpha = 0, border.col = 'grey')+
#   tm_shape(paracell.islands) + tm_lines(col = 'grey')+
#   tm_shape(spratly.islands) + tm_lines(col = 'grey')+
#   # tm_shape(pas) + tm_polygons(col = 'green', alpha = 0,border.col = 'darkgreen')+
#   tm_layout(title = '',title.position = c(0,0.95),legend.position = c(0.1,0.5), frame = F)
# tmap_save(figure.map, filename = 'F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/figures/interaction_sr_rcp45.tiff',width = 8, height = 10, units = 'cm',dpi = 300)


################Interactive effect on range size
#calculate distribution range sum
vn.raster <- raster('F:/Working/2018/PhD_research/SDM output R/Map/vn raster.tif')
species.list <- read_excel('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/list of species.xlsx')
species.list <- species.list%>%filter(Num_occurence_all > 19)
area.sum <- data.frame(species.list)
area.sum[,'area.current'] <- 0
area.sum[,'area.2050.rcp45.lc'] <- 0
for(gcm in c('ac', 'bc','ca','cm','cn','cs','gf','gi','ip','mi')){
  area.sum[,paste0('area.2050.rcp45.both.',gcm)] <- 0
  area.sum[,paste0('area.2050.rcp45.cl.',gcm)] <- 0
  area.sum[,paste0('area.2050.rcp45.add.',gcm)] <- 0
}
for(i in 1:length(species.list$Scientific_name)){
  sp <- species.list$Scientific_name[i]
  prediction.current <- raster(paste0("F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/output_maps/",sp," current.tif"))*vn.raster
  prediction.lc <- raster(paste0("F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/output_maps/",sp," 2050 rcp45 lc.tif"))*vn.raster
  area.sum$area.current[i] <- cellStats(prediction.current,'sum')
  area.sum$area.2050.rcp45.lc[i] <- cellStats(prediction.lc,'sum')
  for(gcm in c('ac', 'bc','ca','cm','cn','cs','gf','gi','ip','mi')){
    prediction.both <- raster(paste0("F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/output_maps/",sp," 2050 rcp45 both ",gcm,".tif"))*vn.raster
    prediction.cl <- raster(paste0("F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/output_maps/",sp," 2050 rcp45 cl ",gcm,".tif"))*vn.raster
    prediction.add <- raster(paste0("F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/",sp," 2050 rcp45 additive ",gcm,".tif"))*vn.raster
    area.sum[,paste0('area.2050.rcp45.both.',gcm)][i] <- cellStats(prediction.both,'sum')
    area.sum[,paste0('area.2050.rcp45.cl.',gcm)][i] <- cellStats(prediction.cl,'sum')
    area.sum[,paste0('area.2050.rcp45.add.',gcm)][i] <- cellStats(prediction.add,'sum')
  }
  print(sp)
}
write.xlsx(area.sum, file= "F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/area sum rcp45.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)

#plot range current and future
area.sum <- read.xlsx("F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/area sum rcp45.xlsx", sheetName = 'Sheet1')
area.sum <- area.sum%>%mutate(area.2050.both.median = 0)
area.sum <- area.sum%>%mutate(area.2050.cl.median = 0)
area.sum$area.current <- area.sum$area.current/1000
area.sum$area.2050.rcp45.lc <- area.sum$area.2050.rcp45.lc/1000
for(i in 1:length(area.sum$Scientific_name)){
  area.sum[,'area.2050.both.median'][i] <- median(area.sum$area.2050.rcp45.both.ac[i], area.sum$area.2050.rcp45.both.bc[i], area.sum$area.2050.rcp45.both.ca[i],
                                                  area.sum$area.2050.rcp45.both.cm[i], area.sum$area.2050.rcp45.both.cn[i], area.sum$area.2050.rcp45.both.cs[i],
                                                  area.sum$area.2050.rcp45.both.gf[i], area.sum$area.2050.rcp45.both.gi[i], area.sum$area.2050.rcp45.both.ip[i],
                                                  area.sum$area.2050.rcp45.both.mi[i])/1000
  area.sum[,'area.2050.cl.median'][i] <- median(area.sum$area.2050.rcp45.cl.ac[i], area.sum$area.2050.rcp45.cl.bc[i], area.sum$area.2050.rcp45.cl.ca[i],
                                                area.sum$area.2050.rcp45.cl.cm[i], area.sum$area.2050.rcp45.cl.cn[i], area.sum$area.2050.rcp45.cl.cs[i],
                                                area.sum$area.2050.rcp45.cl.gf[i], area.sum$area.2050.rcp45.cl.gi[i], area.sum$area.2050.rcp45.cl.ip[i],
                                                area.sum$area.2050.rcp45.cl.mi[i])/1000
}
area.sum <- area.sum%>%mutate('range_size_change_both' = abs(area.2050.both.median-area.current))
area.sum <- area.sum%>%mutate('range_size_change_cl' = abs(area.2050.cl.median-area.current))
area.sum <- area.sum%>%mutate('range_size_change_lc' = abs(area.2050.rcp45.lc-area.current))
area.sum <-area.sum%>%mutate(group.both = ifelse(area.2050.both.median < area.current, "loss", "gain"))
area.sum <-area.sum%>%mutate(group.cl = ifelse(area.2050.cl.median < area.current, "loss", "gain"))
area.sum <-area.sum%>%mutate(group.lc = ifelse(area.2050.rcp45.lc < area.current, "loss", "gain"))
area.sum.both <-area.sum%>%rowwise()%>%mutate(range_size_change_both = min(area.current, area.2050.both.median),group.both = "current")
area.sum.cl <-area.sum%>%rowwise()%>%mutate(range_size_change_cl = min(area.current, area.2050.cl.median),group.cl = "current")
area.sum.lc <-area.sum%>%rowwise()%>%mutate(range_size_change_lc = min(area.current, area.2050.rcp45.lc),group.lc = "current")
area.sum.both <- rbind(area.sum,area.sum.both)
area.sum.cl <- rbind(area.sum,area.sum.cl)
area.sum.lc <- rbind(area.sum,area.sum.lc)
area.sum.both$group.both <- factor(area.sum.both$group.both, levels = c("gain", "loss", "current"))
area.sum.cl$group.cl <- factor(area.sum.cl$group.cl, levels = c("gain", "loss", "current"))
area.sum.lc$group.lc <- factor(area.sum.lc$group.lc, levels = c("gain", "loss", "current"))
figure.both <- area.sum.both%>%ggplot(aes(x = reorder(Scientific_name,area.current), y = range_size_change_both,fill = group.both))+
  geom_histogram(stat = "identity", position = "stack")+
  scale_fill_manual(values = c("blue", "brown", "white"))+
  labs(x='',y='',title = '(a) Both changes')+
  theme_bw()+
  coord_flip()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = 'none', title = element_text(size = 15),
        axis.text.y = element_blank(), axis.text.x = element_text(size = 16))
figure.cl <- area.sum.cl%>%ggplot(aes(x = reorder(Scientific_name,area.current), y = range_size_change_cl,fill = group.cl))+
  geom_histogram(stat = "identity", position = "stack")+
  scale_fill_manual(values = c("blue", "brown", "white"))+
  labs(x='',y='', title = '(b) Climate change only')+
  theme_bw()+
  coord_flip()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = 'none', title = element_text(size = 15),
        axis.text.y = element_blank(), axis.text.x = element_text(size = 16))
figure.lc <- area.sum.lc%>%ggplot(aes(x = reorder(Scientific_name,area.current), y = range_size_change_lc,fill = group.lc))+
  geom_histogram(stat = "identity", position = "stack")+
  scale_fill_manual(values = c("blue", "brown", "white"))+
  labs(x='',y='', title = '(c) Land cover change only')+
  theme_bw()+
  coord_flip()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = 'none',  title = element_text(size = 15),
        axis.text.y = element_blank(), axis.text.x = element_text(size = 16))

figure <- ggarrange(figure.both, figure.cl, figure.lc, ncol = 3)
ggsave(figure, filename = 'F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/figures/Range_size_3models_rcp45.tiff',
       width = 26, height = 35, units = 'cm', dpi = 300)


#plot species color by IUCN category
library(flextable)
#optan the majority of interaction
area.sum <- area.sum%>%dplyr::mutate('interaction.major' ='','color'='','value' = 1)
for(i in 1:nrow(area.sum)){
  area.sum$interaction.major[i] <- modal(interaction.ac[i],
                                         interaction.bc[i],
                                         interaction.ca[i],
                                         interaction.cm[i],
                                         interaction.cn[i],
                                         interaction.cs[i],
                                         interaction.gf[i],
                                         interaction.gi[i],
                                         interaction.ip[i],
                                         interaction.mi[i])
}

area.sum <- area.sum%>%arrange(desc(-area.current))
area.sum$color[area.sum$interaction.major == '1'] <- 'tomato4'
area.sum$color[area.sum$interaction.major == '2'] <- 'tomato'
area.sum$color[area.sum$interaction.major == '3'] <- 'grey70'
area.sum$color[area.sum$interaction.major == '4'] <- 'turquoise2'
area.sum$color[area.sum$interaction.major == '5'] <- 'turquoise4'
# area.sum$value <- 1
tiff('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/figures/species_color_interaction_rcp45.tiff', width = 15, height = 30, units = 'cm', res = 300)
par(mar=c(0, 0, 0, 12))
dotchart(area.sum$value, xlim=c(0,0), col=as.character(area.sum$color), pch=15, lcolor="white", xlab=colnames(area.sum$value)) # Plot the points 
for (j in 1:length(area.sum$Scientific_name)){
  axis(side=4, at=j, col.axis=as.character(area.sum$color)[j], labels=area.sum$Scientific_name[j],las=1) # Add IUCN catagory as labels, each with corresponding color, on the left margin
} 
dev.off()

#compare impact of climate & lc change on distribution range
area.sum <- read.xlsx("F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/area sum rcp45.xlsx", sheetName = 'Sheet1')
area.sum <- area.sum%>%mutate(area.change.rcp45.lc = (area.2050.rcp45.lc - area.current)/area.current*100)
no.species.increase30.rcp45.lc <- nrow(area.sum%>%filter(area.change.rcp45.lc >= 30))/nrow(area.sum)*100
no.species.increase0.rcp45.lc <- nrow(area.sum%>%filter(area.change.rcp45.lc < 30 & area.change.rcp45.lc>0))/nrow(area.sum)*100
no.species.decrease0.rcp45.lc <- nrow(area.sum%>%filter(area.change.rcp45.lc <= 0 &area.change.rcp45.lc > -30))/nrow(area.sum)*100
no.species.decrease30.rcp45.lc <- nrow(area.sum%>%filter(area.change.rcp45.lc <= -30 & area.2050.rcp45.lc > 0))/nrow(area.sum)*100

for(gcm in c('ac', 'bc','ca','cm','cn','cs','gf','gi','ip','mi')){
  area.sum[,'area.change.rcp45.both'] <- (area.sum[,paste0('area.2050.rcp45.both.',gcm)] - area.sum[,"area.current"])/area.sum[,"area.current"]*100
  area.sum[,'area.change.rcp45.cl'] <- (area.sum[,paste0('area.2050.rcp45.cl.',gcm)] - area.sum[,"area.current"])/area.sum[,"area.current"]*100
  area.sum[,'area.2050.rcp45.both'] <- area.sum[,paste0('area.2050.rcp45.both.',gcm)]
  area.sum[,'area.2050.rcp45.cl'] <- area.sum[,paste0('area.2050.rcp45.cl.',gcm)]
  no.species.increase30.rcp45.both <- nrow(area.sum%>%filter(area.change.rcp45.both>= 30))/nrow(area.sum)*100
  no.species.increase0.rcp45.both <- nrow(area.sum%>%filter(area.change.rcp45.both< 30 & area.change.rcp45.both >0))/nrow(area.sum)*100
  no.species.decrease0.rcp45.both <- nrow(area.sum%>%filter(area.change.rcp45.both <= 0 & area.change.rcp45.both > -30))/nrow(area.sum)*100
  no.species.decrease30.rcp45.both <- nrow(area.sum%>%filter(area.change.rcp45.both <= -30))/nrow(area.sum)*100
  no.species.increase30.rcp45.cl <- nrow(area.sum%>%filter(area.change.rcp45.cl>= 30))/nrow(area.sum)*100
  no.species.increase0.rcp45.cl <- nrow(area.sum%>%filter(area.change.rcp45.cl< 30 & area.change.rcp45.cl>0))/nrow(area.sum)*100
  no.species.decrease0.rcp45.cl <- nrow(area.sum%>%filter(area.change.rcp45.cl <= 0 & area.change.rcp45.cl > -30))/nrow(area.sum)*100
  no.species.decrease30.rcp45.cl <- nrow(area.sum%>%filter(area.change.rcp45.cl <= -30 & area.2050.rcp45.cl > 0))/nrow(area.sum)*100
  no.species.rcp45 <- c(no.species.increase30.rcp45.both, no.species.increase0.rcp45.both,no.species.decrease0.rcp45.both,no.species.decrease30.rcp45.both,
                        no.species.increase30.rcp45.cl, no.species.increase0.rcp45.cl,no.species.decrease0.rcp45.cl, no.species.decrease30.rcp45.cl,
                        no.species.increase30.rcp45.lc, no.species.increase0.rcp45.lc,no.species.decrease0.rcp45.lc,no.species.decrease30.rcp45.lc)
  assign(paste0('no.rcp45.',gcm), no.species.rcp45) 
  print(gcm)
}
pro <- data.frame(rbind(no.rcp45.ac, no.rcp45.bc, no.rcp45.ca, no.rcp45.cm, no.rcp45.cn,
                        no.rcp45.cs, no.rcp45.gf, no.rcp45.gi, no.rcp45.ip, no.rcp45.mi))
pro <- pro%>%mutate(in.both = X1 + X2, de.both = X3 + X4, in.cl = X5+ X6, de.cl = X7 + X8, in.lc = X9+ X10, de.lc = X11 + X12)
pro.median <- apply(pro,2,median)
pro.min <- apply(pro,2,min)
pro.max <- apply(pro,2,max)
class <- c('>30','0~30',' -30~0','< -30')
sum.both <- data.frame(class, pro.median[1:4], pro.min[1:4], pro.max[1:4])
sum.cl <- data.frame(class, pro.median[5:8], pro.min[5:8], pro.max[5.8])
sum.lc <- data.frame(class, pro.median[9:12])
sum.both$class <- factor(sum.both$class, levels = c('< -30',' -30~0','0~30','>30'))

pro.median
pro.min
pro.max

#interaction effects on distribution range size
for(gcm in c('ac', 'bc','ca','cm','cn','cs','gf','gi','ip','mi')){
  area.sum <- read_xlsx('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/area sum rcp45.xlsx')
  area.sum <- area.sum%>%select(Scientific_name,area.current, paste0('area.2050.rcp45.cl.',gcm), area.2050.rcp45.lc, paste0('area.2050.rcp45.both.', gcm),paste0('area.2050.rcp45.add.', gcm))
  colnames(area.sum) <- c('Scientific_name','area.current', 'area.2050.cl', 'area.2050.lc', 'area.2050.both', 'area.2050.add')
  area.sum <- area.sum%>%mutate(interaction = 0)
  #two negative and negative neutral
  area.sum <- area.sum%>%mutate(interaction = replace(interaction, area.2050.cl<= area.current & area.2050.lc<=area.current & area.2050.cl+area.2050.lc!=area.current &
                                                        area.2050.both < area.2050.add,1))
  area.sum <- area.sum%>%mutate(interaction = replace(interaction, area.2050.cl<= area.current & area.2050.lc<=area.current & area.2050.cl+area.2050.lc!=area.current &
                                                        area.2050.both == area.2050.add,3))
  area.sum <- area.sum%>%mutate(interaction = replace(interaction, area.2050.cl<= area.current & area.2050.lc<=area.current & area.2050.cl+area.2050.lc!=area.current &
                                                        area.2050.both > area.2050.add & area.2050.both <= area.current,4))
  area.sum <- area.sum%>%mutate(interaction = replace(interaction,area.2050.cl<= area.current & area.2050.lc<=area.current & area.2050.cl+area.2050.lc!=area.current &
                                                        area.2050.both > area.current,5))
  
  #two positive and positive neutral
  area.sum <- area.sum%>%mutate(interaction = replace(interaction, area.2050.cl>=area.current & area.2050.lc>=area.current & area.2050.cl+area.2050.lc!=2*area.current&
                                                        area.2050.both < area.current,1))
  area.sum <- area.sum%>%mutate(interaction = replace(interaction, area.2050.cl>=area.current & area.2050.lc>=area.current & area.2050.cl+area.2050.lc!=2*area.current&
                                                        area.2050.both >= area.current& area.2050.both<area.2050.add,2))
  area.sum <- area.sum%>%mutate(interaction = replace(interaction, area.2050.cl>=area.current & area.2050.lc>=area.current & area.2050.cl+area.2050.lc!=2*area.current&
                                                        area.2050.both == area.2050.add,3))
  area.sum <- area.sum%>%mutate(interaction = replace(interaction, area.2050.cl>=area.current & area.2050.lc>=area.current & area.2050.cl+area.2050.lc!=2*area.current&
                                                        area.2050.both > area.2050.add,5))
  
  #opposite
  area.sum <- area.sum%>%mutate(interaction = replace(interaction, (area.2050.cl -area.current)*(area.2050.lc-area.current)< 0 &
                                                        area.2050.both < min(area.2050.cl, area.2050.lc),1))
  area.sum <- area.sum%>%mutate(interaction = replace(interaction, (area.2050.cl -area.current)*(area.2050.lc-area.current)< 0 &
                                                        area.2050.both >= min(area.2050.cl, area.2050.lc)&area.2050.both < area.2050.add,2))
  area.sum <- area.sum%>%mutate(interaction = replace(interaction, (area.2050.cl -area.current)*(area.2050.lc-area.current)< 0 &
                                                        area.2050.both ==area.2050.add,3))
  area.sum <- area.sum%>%mutate(interaction = replace(interaction, (area.2050.cl -area.current)*(area.2050.lc-area.current)< 0 &
                                                        area.2050.both >area.2050.add & area.2050.both<= max(area.2050.cl, area.2050.lc),4))
  area.sum <- area.sum%>%mutate(interaction = replace(interaction, (area.2050.cl -area.current)*(area.2050.lc-area.current)< 0 &
                                                        area.2050.both > max(area.2050.cl, area.2050.lc),5))
  interaction.sp <- c(area.sum$interaction)
  assign(paste0('interaction.',gcm), interaction.sp)
  # area.sum <- area.sum%>%mutate(paste0('interaction.',gcm) == interaction)
  pro.range.interaction <- c(nrow(area.sum%>%filter(interaction == 1))/nrow(area.sum)*100,
                             nrow(area.sum%>%filter(interaction == 2))/nrow(area.sum)*100,
                             nrow(area.sum%>%filter(interaction == 3))/nrow(area.sum)*100,
                             nrow(area.sum%>%filter(interaction == 4))/nrow(area.sum)*100,
                             nrow(area.sum%>%filter(interaction == 5))/nrow(area.sum)*100)
  assign(paste0('pro.range.interaction.', gcm), pro.range.interaction)
  print(gcm)
}

pro.range.interaction.all <- data.frame(pro.range.interaction.ac,pro.range.interaction.bc,pro.range.interaction.ca,pro.range.interaction.cm,pro.range.interaction.cn,
                                        pro.range.interaction.cs,pro.range.interaction.gf,pro.range.interaction.gi,pro.range.interaction.ip,pro.range.interaction.mi)
pro.range.interaction.median <- apply(pro.range.interaction.all,1,median)
pro.range.interaction.min <- apply(pro.range.interaction.all,1,min)
pro.range.interaction.max <- apply(pro.range.interaction.all,1,max)

class <- c('-S','+A','AD','-A','+S')
sum.range.interaction <- data.frame(class,pro.range.interaction.median,pro.range.interaction.min, pro.range.interaction.max)
sum.range.interaction$class <- factor(sum.range.interaction$class, levels = c('-S','+A','AD','-A','+S'))


figure.both <- sum.both%>%ggplot(aes(x=factor(class, levels = c('< -30',' -30~0','0~30','>30')),y=pro.median.1.4.,fill=class)) + 
  geom_bar(stat = 'identity')+
  scale_fill_manual('',values = c('>30'='blue','0~30'='lightblue',' -30~0'='orange','< -30'='red'))+
  scale_x_discrete(limits=c('< -30',' -30~0','0~30','>30'))+ theme_classic()+ ylim(0,100)+
  labs(x='', y='', title = '')+
  geom_errorbar(aes(ymin=pro.min[1:4], ymax=pro.max[1:4]), width=.2,position=position_dodge(.9))+
  theme(text = element_text(size = 14),axis.text = element_text(size = 14), 
        plot.title = element_text(hjust = 0),legend.position = 'none',
        plot.margin = unit(c(0,0,0,0), "cm"))
figure.cl <- sum.cl%>%ggplot(aes(x=factor(class, levels = c('< -30',' -30~0','0~30','>30')),y=pro.median.5.8.,fill=class)) + 
  geom_bar(stat = 'identity')+
  scale_fill_manual('',values = c('>30'='blue','0~30'='lightblue',' -30~0'='orange','< -30'='red'))+
  scale_x_discrete(limits=c('< -30',' -30~0','0~30','>30'))+ theme_classic()+ ylim(0,100)+
  labs(x='', y='', title = '')+
  geom_errorbar(aes(ymin=pro.min[5:8], ymax=pro.max[5:8]), width=.2,position=position_dodge(.9))+
  theme(text = element_text(size = 14), axis.text = element_text(size = 14),
        plot.title = element_text(hjust = 0),legend.position = 'none',
        plot.margin = unit(c(0,0,0,0), "cm"))

figure.lc <- sum.lc%>%ggplot(aes(x=factor(class, levels = c('< -30',' -30~0','0~30','>30')), 
                                 y=pro.median.9.12.,fill=class)) + 
  geom_bar(stat = 'identity')+
  scale_fill_manual('',values = c('>30'='blue','0~30'='lightblue',' -30~0'='orange','< -30'='red'))+
  scale_x_discrete(limits=c('< -30',' -30~0','0~30','>30'))+ theme_classic()+ ylim(0,100)+
  labs(x='', y='', title = '')+
  theme(text = element_text(size = 14),axis.text = element_text(size = 14), 
        plot.title = element_text(hjust = 0),legend.position = 'none',
        plot.margin = unit(c(0,0,0,0), "cm"))

figure.interaction <- sum.range.interaction%>%ggplot(aes(x=factor(class,levels = c('-S','+A','AD','-A','+S')),y=pro.range.interaction.median,fill=class))+
  geom_bar(stat = 'identity')+ theme_classic()+
  scale_fill_manual('',values = c('-S'='purple4','+A'='purple','AD'='grey70', '-A'='aquamarine','+S'='aquamarine4'))+
  xlab('')+ylab('')+labs(title = '')+  ylim(0,100)+
  geom_errorbar(aes(ymin=pro.range.interaction.min, ymax=pro.range.interaction.max), width=.2,position=position_dodge(.9))+
  theme(text = element_text(size = 14), legend.position = 'none', axis.text = element_text(size = 14))
figure <- ggarrange(figure.both, figure.cl, figure.lc,figure.interaction,nrow = 1)
ggsave(figure, filename = 'F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/figures/rangesize_change_interaction_rcp45.tiff',
       units = 'cm', width = 28, height = 8, dpi = 300)

# 
#plot species color by interaction
library(flextable)
#optan the majority of interaction
area.sum <- area.sum%>%dplyr::mutate('interaction.major' ='','color'='','value' = 1)
for(i in 1:nrow(area.sum)){
  area.sum$interaction.major[i] <- modal(interaction.ac[i],
                                         interaction.bc[i],
                                         interaction.ca[i],
                                         interaction.cm[i],
                                         interaction.cn[i],
                                         interaction.cs[i],
                                         interaction.gf[i],
                                         interaction.gi[i],
                                         interaction.ip[i],
                                         interaction.mi[i])
}

area.sum <- area.sum%>%arrange(desc(-area.current))
area.sum$color[area.sum$interaction.major == '1'] <- 'tomato4'
area.sum$color[area.sum$interaction.major == '2'] <- 'tomato'
area.sum$color[area.sum$interaction.major == '3'] <- 'grey70'
area.sum$color[area.sum$interaction.major == '4'] <- 'turquoise2'
area.sum$color[area.sum$interaction.major == '5'] <- 'turquoise4'
# area.sum$value <- 1
tiff('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/figures/species_color_interaction_rcp45.tiff', width = 15, height = 30, units = 'cm', res = 300)
par(mar=c(0, 0, 0, 12))
dotchart(area.sum$value, xlim=c(0,0), col=as.character(area.sum$color), pch=15, lcolor="white", xlab=colnames(area.sum$value)) # Plot the points 
for (j in 1:length(area.sum$Scientific_name)){
  axis(side=4, at=j, col.axis=as.character(area.sum$color)[j], labels=area.sum$Scientific_name[j],las=1) # Add IUCN catagory as labels, each with corresponding color, on the left margin
} 
dev.off()



