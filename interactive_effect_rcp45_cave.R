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

#INTERACTIVE EFECT ON SPECIES USE CAVES
#change in species richness for species use forest only
#import list of species
species.list <- read.xlsx("F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/vnbats_habitat_traits.xlsx", sheetName = 'Sheet1')
species.list <- species.list%>%select(Scientific_name,Forest,Shrubland,Grassland,Wetlands,Caves,Artificial,Diet.Plant,Diet.Invertebrate)
species.list <- species.list%>%filter(Caves=='TRUE')
vn.raster <- raster('F:/Working/2018/PhD_research/SDM output R/Map/vn raster.tif') #import Vietnam
extent.asia.sr <- extent(11000000, 13000000, 900000, 2600000)
#current species richness
sr.current <- lapply(species.list$Scientific_name, function(sp){
  raster(paste0("F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/output_maps/",sp," current.tif"))*vn.raster
})
batsr.current <- overlay(stack(sr.current),fun=sum)
writeRaster(batsr.current,filename = 'F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_current_cave.tif',format='GTiff',overwrite=T)
#species richness for individual GCMs
#combined model
for(gcm in c('ac', 'bc','ca','cm','cn','cs','gf','gi','ip','mi')){
  sr.2050.rcp45.both <- lapply(species.list$Scientific_name, function(sp){
    raster(paste0("F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/output_maps/",sp," 2050 rcp45 both ",gcm,".tif"))*vn.raster
  })
  batsr.2050.rcp45.both <- overlay(stack(sr.2050.rcp45.both),fun=sum)
  writeRaster(batsr.2050.rcp45.both, filename = paste0('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_2050_rcp45_both_',gcm,'_cave.tif'), format="GTiff", overwrite=T)
  print(gcm)
}
#climate change only model
for(gcm in c('ac', 'bc','ca','cm','cn','cs','gf','gi','ip','mi')){
  sr.2050.rcp45.cl <- lapply(species.list$Scientific_name, function(sp){
    raster(paste0("F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/output_maps/",sp," 2050 rcp45 cl ",gcm,".tif"))*vn.raster
  })
  batsr.2050.rcp45.cl <- overlay(stack(sr.2050.rcp45.cl),fun=sum)
  writeRaster(batsr.2050.rcp45.cl, filename = paste0('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_2050_rcp45_cl_',gcm,'_cave.tif'), format="GTiff", overwrite=T)
  print(gcm)
}
#consensus future prediction rcp45
#land-cover change only models
sr.2050.rcp45.lc <- lapply(species.list$Scientific_name, function(sp){
  raster(paste0("F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/output_maps/",sp," 2050 rcp45 lc.tif"))*vn.raster
})
batsr.lc.rcp45 <- overlay(stack(sr.2050.rcp45.lc),fun=sum)

gcm <- c('ac', 'bc','ca','cm','cn','cs','gf','gi','ip','mi')
sr.2050.both <- lapply(gcm, function(x){
  raster(paste0('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_2050_rcp45_both_',x,'_cave.tif'))
})
batsr.2050.both.median <- overlay(stack(sr.2050.both), fun=median)

sr.2050.cl <- lapply(gcm, function(x){
  raster(paste0('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_2050_rcp45_cl_',x,'_cave.tif'))
})
batsr.2050.cl.median <- overlay(stack(sr.2050.cl), fun=median)
writeRaster(batsr.2050.both.median,filename = 'F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_2050_rcp45_both_median_cave.tif',format='GTiff',overwrite=T)
writeRaster(batsr.2050.cl.median,filename = 'F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_2050_rcp45_cl_median_cave.tif',format='GTiff',overwrite=T)
writeRaster(batsr.lc.rcp45,filename = 'F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_2050_rcp45_lc_cave.tif',format='GTiff',overwrite=T)

vn.raster <- raster('F:/Working/2018/PhD_research/SDM output R/Map/vn raster.tif') #import Vietnam
batsr.current <- raster('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_current_cave.tif')
batsr.lc.rcp45 <- raster('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_2050_rcp45_lc_cave.tif')
batsr.lc.rcp45.current <- (batsr.lc.rcp45 - batsr.current)/batsr.current*100
pro.lc.rcp45 <- c(cellStats(batsr.lc.rcp45.current>=30,'sum')/cellStats(vn.raster,'sum')*100, 
                  cellStats(batsr.lc.rcp45.current < 30 & batsr.lc.rcp45.current > 0,'sum')/cellStats(vn.raster,'sum')*100,
                  cellStats(batsr.lc.rcp45.current == 0,'sum')/cellStats(vn.raster,'sum')*100,
                  cellStats(batsr.lc.rcp45.current < 0 & batsr.lc.rcp45.current > -30,'sum')/cellStats(vn.raster,'sum')*100,
                  cellStats(batsr.lc.rcp45.current <= -30,'sum')/cellStats(vn.raster,'sum')*100)

#effect of combined change
for(gcm in c('ac', 'bc','ca','cm','cn','cs','gf','gi','ip','mi')){
  batsr.both.rcp45 <- raster(paste0('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_2050_rcp45_both_',gcm,'_cave.tif'))
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
  batsr.cl.rcp45 <- raster(paste0('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_2050_rcp45_cl_',gcm,'_cave.tif'))
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

write.xlsx(sum.both.sr,'F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/srchange_both_rcp45_cave.xlsx')
write.xlsx(sum.cl.sr,'F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/srchange_cl_rcp45_cave.xlsx')
write.xlsx(sum.lc.sr,'F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/srchange_lc_rcp45_cave.xlsx')

#plot maps species richness change
batsr.current <- raster('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_current_cave.tif')
batsr.2050.both <- raster('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_2050_rcp45_both_median_cave.tif')
batsr.2050.cl <- raster('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_2050_rcp45_cl_median_cave.tif')
batsr.2050.lc <- raster('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_2050_rcp45_lc_cave.tif')
SR.ORG8287 <- CRS('+proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs')
vn <- spTransform(getData('GADM', country='VN', level=0), SR.ORG8287)
paracell.islands <- spTransform(readOGR('F:/Working/2018/PhD_research/SDM output R/Map/paracel islands.shp'), SR.ORG8287)
spratly.islands <- spTransform(readOGR('F:/Working/2018/PhD_research/SDM output R/Map/spratly islands.shp'), SR.ORG8287)
# pas <- readOGR('F:/Working/2018/PhD_research/SDM output R/Map/vn_pas.shp')

batsr.both.current <- (batsr.2050.both - batsr.current)/batsr.current*100
batsr.cl.current <- (batsr.2050.cl - batsr.current)/batsr.current*100
batsr.lc.current <- (batsr.2050.lc - batsr.current)/batsr.current*100

plot.current<- tm_shape(batsr.current, bbox = c(11380000, 930000,12450000,  2517140)) + 
  tm_raster('layer',breaks = c(0,2,4,6,8,10,15,20,25,30,35,40,50,65), palette = 'Greens', style = 'fixed',
            title = 'Species richness')+
  tm_shape(paracell.islands) + tm_lines(col = 'grey')+
  tm_shape(spratly.islands) + tm_lines(col = 'grey')+
  # tm_shape(pas) + tm_polygons(col = 'green', alpha = 0,border.col = 'darkgreen')+
  tm_shape(vn)+ tm_polygons(border.col = 'dark gray', alpha = 0, lwd = 0.5)+
  tm_layout(legend.outside = F, title = 'Species richness',title.position = c('left','top'),title.size = 1,
            legend.text.size = 0.5, legend.title.size = 1,legend.position = c(0,0.3), frame = F, legend.show = F)

plot.srchange.rcp45.both <-  tm_shape(batsr.both.current, bbox = c(11380000, 930000,12450000,  2517140)) + 
  tm_raster('layer',breaks = c(-Inf,-30,-0.0001,0.0001,30,Inf),palette = c('red','orange','grey','lightblue','blue'), 
            title = 'SR change (%)',labels = c('< -30','-30~0','0','0~30','> 30'))+
  tm_shape(paracell.islands) + tm_lines(col = 'grey')+
  tm_shape(spratly.islands) + tm_lines(col = 'grey')+
  # tm_shape(pas) + tm_polygons(col = 'green', alpha = 0,border.col = 'darkgreen')+
  tm_shape(vn)+ tm_polygons(border.col = 'dark gray', alpha = 0, lwd = 0.5)+
  tm_layout(legend.outside = F, title = '(b) Combined change', title.size = 1, legend.position = c(0, 0.3),
            title.position = c('left','top'), frame = F, legend.text.size = 0.8, legend.title.size = 2, legend.show = F) 
plot.srchange.rcp45.cl <-  tm_shape(batsr.cl.current, bbox = c(11380000, 930000,12450000,  2517140)) + 
  tm_raster('layer',breaks = c(-Inf,-25,-0.0001,0.0001,25,Inf),palette = c('red','orange','grey','lightblue','blue'), 
            title = '% SR change')+
  tm_shape(paracell.islands) + tm_lines(col = 'grey')+
  tm_shape(spratly.islands) + tm_lines(col = 'grey')+
  # tm_shape(pas) + tm_polygons(col = 'green', alpha = 0,border.col = 'darkgreen')+
  tm_shape(vn)+ tm_polygons(border.col = 'gray', alpha = 0, lwd = 0.5)+
  tm_layout(title = '(c) Climate change only', title.size = 1,title.position = c('left','top'), frame = F,legend.show = F) 
plot.srchange.rcp45.lc <-  tm_shape(batsr.lc.current, bbox = c(11380000, 930000,12450000,  2517140)) + 
  tm_raster('layer',breaks = c(-Inf,-25,-0.0001,0.0001,25,Inf),palette = c('red','orange','grey','lightblue','blue'), 
            title = '% SR change')+
  tm_shape(paracell.islands) + tm_lines(col = 'grey')+
  tm_shape(spratly.islands) + tm_lines(col = 'grey')+
  # tm_shape(pas) + tm_polygons(col = 'green', alpha = 0,border.col = 'darkgreen')+
  tm_shape(vn)+ tm_polygons(border.col = 'dark gray', alpha = 0, lwd = 0.5)+
  tm_layout(title = '(d) Land cover change only', title.size = 1, title.position = c('left','top'), frame = F, legend.show = F)
figure <- tmap_arrange(plot.current,plot.srchange.rcp45.both,plot.srchange.rcp45.cl,plot.srchange.rcp45.lc, nrow = 1)
tmap_save(figure, filename = 'F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/figures/current and srchange rcp45_cave.tiff',
          units = 'cm', width = 24, height = 11, dpi=300)

#additive species richness for each GCMs
for(gcm in c('ac', 'bc','ca','cm','cn','cs','gf','gi','ip','mi')){
  sp.2050.rcp45.add <- lapply(species.list$Scientific_name, function(sp){
    raster(paste0("F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/",sp," 2050 rcp45 additive ",gcm,".tif"))*vn.raster
  })
  batsr.2050.rcp45 <- overlay(stack(sp.2050.rcp45.add),fun=sum)
  writeRaster(batsr.2050.rcp45, filename = paste0('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_2050_rcp45_add_',gcm,'_cave.tif'), format="GTiff", overwrite=T)
  print(gcm)
}

#generate median additive species richness
gcm <- c('ac', 'bc','ca','cm','cn','cs','gf','gi','ip','mi')
sr.2050.add <- lapply(gcm, function(x){
  raster(paste0("F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_2050_rcp45_add_",x,".tif"))
})
batsr.2050.add.median <- overlay(stack(sr.2050.add), fun=median)
# writeRaster(batsr.2050.add.median, filename = 'F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_2050_rcp45_add_median_forest.tif', format="GTiff", overwrite=T)

lc.current <- crop(raster('F:/Working/2018/PhD_research/enviromental_variables/land-cover/MODISLandcover_2010_srorg8287.tif'),extent.asia.sr)*vn.raster
lc.2050.rcp45 <- crop(raster('F:/Working/2018/PhD_research/enviromental_variables/land-cover/MODISLandcover_2050_rcp45_srorg8287.tif'),extent.asia.sr)*vn.raster
# lc.change.2050.rcp45 < - lc.2050.rcp45 - lc.current
area.withlcchange <- lc.2050.rcp45 != lc.current

#classify interaction effects
batsr.lc.rcp45 <- batsr.lc.rcp45
for(gcm in c('ac', 'bc','ca','cm','cn','cs','gf','gi','ip','mi')){  
  batsr.2050.cl <- raster(paste0('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_2050_rcp45_cl_',gcm,'_cave.tif'))
  batsr.2050.both <- raster(paste0('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_2050_rcp45_both_',gcm,'_cave.tif'))
  batsr.2050.add <- raster(paste0('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_2050_rcp45_add_',gcm,'_cave.tif'))
  sr.interaction <- batsr.current*0
  #two negative and negative neutral
  sr.interaction[batsr.2050.cl <= batsr.current & batsr.lc.rcp45 <= batsr.current & (batsr.2050.cl + batsr.lc.rcp45 != 2* batsr.current)&
                   batsr.2050.both < batsr.2050.add] <- 1
  sr.interaction[batsr.2050.cl <= batsr.current & batsr.lc.rcp45 <= batsr.current & (batsr.2050.cl + batsr.lc.rcp45 != 2* batsr.current)&
                   batsr.2050.both == batsr.2050.add] <- 3
  sr.interaction[batsr.2050.cl <= batsr.current & batsr.lc.rcp45 <= batsr.current & (batsr.2050.cl + batsr.lc.rcp45 != 2* batsr.current)&
                   batsr.2050.both > batsr.2050.add & batsr.2050.both <= batsr.current] <- 4
  sr.interaction[batsr.2050.cl <= batsr.current & batsr.lc.rcp45 <= batsr.current & (batsr.2050.cl + batsr.lc.rcp45 != 2* batsr.current)&
                   batsr.2050.both > batsr.current] <- 5
  #two positive and positive neutral
  sr.interaction[batsr.2050.cl >= batsr.current & batsr.lc.rcp45 >= batsr.current& (batsr.2050.cl + batsr.lc.rcp45 != 2* batsr.current)&
                   batsr.2050.both < batsr.current] <- 1
  sr.interaction[batsr.2050.cl >= batsr.current & batsr.lc.rcp45 >= batsr.current& (batsr.2050.cl + batsr.lc.rcp45 != 2* batsr.current)&
                   batsr.2050.both >= batsr.current & batsr.2050.both < batsr.2050.add] <- 2
  sr.interaction[batsr.2050.cl >= batsr.current & batsr.lc.rcp45 >= batsr.current& (batsr.2050.cl + batsr.lc.rcp45 != 2* batsr.current)&
                   batsr.2050.both == batsr.2050.add] <- 3
  sr.interaction[batsr.2050.cl >= batsr.current & batsr.lc.rcp45 >= batsr.current& (batsr.2050.cl + batsr.lc.rcp45 != 2* batsr.current)&
                   batsr.2050.both > batsr.2050.add] <- 5
  #opposite
  sr.interaction[(batsr.2050.cl - batsr.current)*(batsr.lc.rcp45 - batsr.current) < 0 & 
                   batsr.2050.both < min(batsr.2050.cl, batsr.lc.rcp45)] <- 1
  sr.interaction[(batsr.2050.cl - batsr.current)*(batsr.lc.rcp45 - batsr.current) < 0 & 
                   batsr.2050.both >= min(batsr.2050.cl, batsr.lc.rcp45) & batsr.2050.both < batsr.2050.add] <- 2
  sr.interaction[(batsr.2050.cl - batsr.current)*(batsr.lc.rcp45 - batsr.current) < 0 & 
                   batsr.2050.both == batsr.2050.add] <- 3
  sr.interaction[(batsr.2050.cl - batsr.current)*(batsr.lc.rcp45 - batsr.current) < 0 & 
                   batsr.2050.both > batsr.2050.add & batsr.2050.both <= max(batsr.2050.cl, batsr.lc.rcp45)] <- 4
  sr.interaction[(batsr.2050.cl - batsr.current)*(batsr.lc.rcp45 - batsr.current) < 0 & 
                   batsr.2050.both > max(batsr.2050.cl, batsr.lc.rcp45)] <- 5
  #double neutral
  sr.interaction[batsr.2050.cl == batsr.current & batsr.lc.rcp45 == batsr.current &
                   batsr.2050.both < batsr.2050.add] <- 1
  sr.interaction[batsr.2050.cl == batsr.current & batsr.lc.rcp45 == batsr.current &
                   batsr.2050.both == batsr.2050.add] <- 3
  sr.interaction[batsr.2050.cl == batsr.current & batsr.lc.rcp45 == batsr.current &
                   batsr.2050.both > batsr.2050.add] <- 5
  writeRaster(sr.interaction, filename = paste0('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/interaction_types_rcp45_',gcm,'_cave.tif'), format="GTiff", overwrite=T)
  print(gcm)
}

# compute proportion of interaction types within areas with land-cover change only
extent.asia.sr <- extent(11000000, 13000000, 900000, 2600000)
vn.raster <- raster('F:/Working/2018/PhD_research/SDM output R/Map/vn raster.tif') #import Vietnam
lc.current <- crop(raster('F:/Working/2018/PhD_research/enviromental_variables/land-cover/MODISLandcover_2010_srorg8287.tif'),extent.asia.sr)*vn.raster
lc.2050.rcp45 <- crop(raster('F:/Working/2018/PhD_research/enviromental_variables/land-cover/MODISLandcover_2050_rcp45_srorg8287.tif'),extent.asia.sr)*vn.raster
area.withlcchange <- lc.2050.rcp45 != lc.current
area.withlcchange.area <- cellStats(area.withlcchange, 'sum')
for(gcm in c('ac', 'bc','ca','cm','cn','cs','gf','gi','ip','mi')){
  sr.interaction <- raster(paste0('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/interaction_types_rcp45_',gcm,'_cave.tif'))*area.withlcchange
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

sum.both.sr <- read_excel('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/srchange_both_rcp45_cave.xlsx')[2:5]
sum.cl.sr <- read_excel('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/srchange_cl_rcp45_cave.xlsx')[2:5]
sum.lc.sr <- read_excel('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/srchange_lc_rcp45_cave.xlsx')[2:3]
names(sum.both.sr) <- c('class','pro.both.median','pro.both.min','pro.both.max')
names(sum.cl.sr) <- c('class','pro.cl.median','pro.cl.min','pro.cl.max')

figure.bar.both <- sum.both.sr%>%ggplot(aes(x=factor(class,levels = c('<-30','-30~0','0','0~30','>30')),y=pro.both.median[1:5],fill=class))+
  geom_bar(stat = 'identity')+ theme_classic()+
  scale_fill_manual('',values = c('>30'='blue','0~30'='lightblue','0'='grey70','-30~0'='orange','<-30'='red'))+
  xlab('')+ylab('')+labs(title = '')+  ylim(0,95)+
  geom_errorbar(aes(ymin=pro.both.min[1:5], ymax=pro.both.max[1:5]), width=.2,position=position_dodge(.9))+
  theme(legend.position = 'none', 
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12, angle = 0, vjust = 0.5),
        title = element_text(size = 14))
figure.bar.cl <- sum.cl.sr%>%ggplot(aes(x=factor(class,levels = c('<-30','-30~0','0','0~30','>30')),y=pro.cl.median[1:5],fill=class))+
  geom_bar(stat = 'identity')+ theme_classic()+
  scale_fill_manual('',values = c('>30'='blue','0~30'='lightblue','0'='grey70','-30~0'='orange','<-30'='red'))+
  xlab('')+ylab('')+labs(title = '')+ ylim(0,95)+
  geom_errorbar(aes(ymin=pro.cl.min[1:5], ymax=pro.cl.max[1:5]), width=.2,position=position_dodge(.9))+
  theme(legend.position = 'none', 
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12, angle = 0, vjust = 0.5),
        title = element_text(size = 14))
figure.bar.lc <- sum.lc.sr%>%ggplot(aes(x=factor(class,levels = c('<-30','-30~0','0','0~30','>30')),y=pro.lc.rcp45[1:5],fill=class))+
  geom_bar(stat = 'identity')+ theme_classic()+
  scale_fill_manual('',values = c('>30'='blue','0~30'='lightblue','0'='grey70','-30~0'='orange','<-30'='red'))+
  xlab('')+ylab('')+labs(title = '')+ ylim(0,95)+
  theme(legend.position = 'none', 
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        title = element_text(size = 14))
figure.interaction <- ggplot(sum, aes(fill=interaction, y=pro.interaction.median, x=factor(interaction,levels = c('-S','+A','AD','-A','+S')))) +
  geom_bar(position="stack", stat="identity")+  ylim(0,95)+ theme_classic()+
  scale_fill_manual('',values = c('-S'='purple4','+A'='purple','AD'='grey70', '-A'='aquamarine','+S'='aquamarine4'))+
  annotate("text", label = "", size = 4, x = 3, y = 70)+
  xlab('')+ylab('')+labs(title = '')+
  geom_errorbar(aes(ymin=pro.interaction.min, ymax=pro.interaction.max), width=.2,position=position_dodge(.9))+
  theme(text = element_text(size = 14), legend.position = 'none', 
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12))

figure <- ggarrange(figure.bar.both,figure.bar.cl,figure.bar.lc,figure.interaction,nrow = 1)
ggsave(figure, filename = 'F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/figures/srchange_interaction_bar_rcp45_cave.tiff', 
       units = 'cm', width = 30, height = 8, dpi=300)


#Plot interaction maps
SR.ORG8287 <- CRS('+proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs') 
vn <- spTransform(getData('GADM', country='VN', level=0), SR.ORG8287)
paracell.islands <- spTransform(readOGR('F:/Working/2018/PhD_research/SDM output R/Map/paracel islands.shp'), SR.ORG8287)
spratly.islands <- spTransform(readOGR('F:/Working/2018/PhD_research/SDM output R/Map/spratly islands.shp'), SR.ORG8287)
# pas <- readOGR('F:/Working/2018/PhD_research/SDM output R/Map/vn_pas.shp')
gcm <- c('ac', 'bc','ca','cm','cn','cs','gf','gi','ip','mi')
sr.interaction <- lapply(gcm, function(x){
  raster(paste0('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/interaction_types_rcp45_',x,'_cave.tif'))
})
select.major <- function(x){
  modal(x, na.rm=T, freq=F, ties='random')
}
sr.interaction.ensemble <- overlay(stack(sr.interaction), fun=select.major)#select major interaction types across GCMs
figure.map <- tm_shape(sr.interaction.ensemble,bbox = c(11070729, 943456.3,12186081,  2517140)) +
  tm_raster('layer', breaks = c(-0.5,0.5,1.5,2.5,3.5,4.5,5.5),palette = c('white','purple4','purple','grey70','aquamarine','aquamarine4'),
            labels = c("","-S", "+A", "AD","-A","+S"),title = '')+
  tm_shape(vn)+tm_polygons(col = 'white', alpha = 0, border.col = 'grey')+
  tm_shape(paracell.islands) + tm_lines(col = 'grey')+
  tm_shape(spratly.islands) + tm_lines(col = 'grey')+
  # tm_shape(pas) + tm_polygons(col = 'green', alpha = 0,border.col = 'darkgreen')+
  tm_layout(title = '',title.position = c(0,0.95),legend.position = c(0.1,0.5), frame = F)
tmap_save(figure.map, filename = 'F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/figures/interaction_sr_rcp45_cave.tiff',width = 8, height = 10, units = 'cm',dpi = 300)

#compare impact of climate & lc change on distribution range
species.list <- read.xlsx("F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/vnbats_habitat_traits.xlsx", sheetName = 'Sheet1')
species.list <- species.list%>%select(Scientific_name,Forest,Shrubland,Grassland,Wetlands,Caves,Artificial,Diet.Plant,Diet.Invertebrate)
species.list <- species.list%>%filter(Caves=='TRUE')
area.sum <- read.xlsx("F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/area sum rcp45.xlsx", sheetName = 'Sheet1')
area.sum <- area.sum%>%filter(Scientific_name%in%species.list$Scientific_name)
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
  area.sum <- area.sum%>%filter(Scientific_name%in%species.list$Scientific_name)
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
        plot.title = element_text(hjust = 0),legend.position = 'none')
figure.cl <- sum.cl%>%ggplot(aes(x=factor(class, levels = c('< -30',' -30~0','0~30','>30')),y=pro.median.5.8.,fill=class)) + 
  geom_bar(stat = 'identity')+
  scale_fill_manual('',values = c('>30'='blue','0~30'='lightblue',' -30~0'='orange','< -30'='red'))+
  scale_x_discrete(limits=c('< -30',' -30~0','0~30','>30'))+ theme_classic()+ ylim(0,100)+
  labs(x='', y='', title = '')+
  geom_errorbar(aes(ymin=pro.min[5:8], ymax=pro.max[5:8]), width=.2,position=position_dodge(.9))+
  theme(text = element_text(size = 14), axis.text = element_text(size = 14),
        plot.title = element_text(hjust = 0),legend.position = 'none')

figure.lc <- sum.lc%>%ggplot(aes(x=factor(class, levels = c('< -30',' -30~0','0~30','>30')), 
                                 y=pro.median.9.12.,fill=class)) + 
  geom_bar(stat = 'identity')+
  scale_fill_manual('',values = c('>30'='blue','0~30'='lightblue',' -30~0'='orange','< -30'='red'))+
  scale_x_discrete(limits=c('< -30',' -30~0','0~30','>30'))+ theme_classic()+ ylim(0,100)+
  labs(x='', y='', title = '')+
  theme(text = element_text(size = 14),axis.text = element_text(size = 14), 
        plot.title = element_text(hjust = 0),legend.position = 'none')

figure.interaction <- sum.range.interaction%>%ggplot(aes(x=factor(class,levels = c('-S','+A','AD','-A','+S')),y=pro.range.interaction.median,fill=class))+
  geom_bar(stat = 'identity')+ theme_classic()+
  scale_fill_manual('',values = c('-S'='purple4','+A'='purple','AD'='grey70', '-A'='aquamarine','+S'='aquamarine4'))+
  xlab('')+ylab('')+labs(title = '')+  ylim(0,100)+
  geom_errorbar(aes(ymin=pro.range.interaction.min, ymax=pro.range.interaction.max), width=.2,position=position_dodge(.9))+
  theme(text = element_text(size = 14), legend.position = 'none', axis.text = element_text(size = 14))
figure <- ggarrange(figure.both, figure.cl, figure.lc,figure.interaction,nrow = 1)
ggsave(figure, filename = 'F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/figures/rangesize_change_interaction_rcp45_cave.tiff',
       units = 'cm', width = 28, height = 8, dpi = 300)


#INTERACTIVE EFECT ON SPECIES USE FOREST ONLY
vn.raster <- raster('F:/Working/2018/PhD_research/SDM output R/Map/vn raster.tif') #import Vietnam
species.list <- read.xlsx("F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/vnbats_habitat_traits.xlsx", sheetName = 'Sheet1')
species.list <- species.list%>%select(Scientific_name,Forest,Shrubland,Grassland,Wetlands,Caves,Artificial,Diet.Plant,Diet.Invertebrate)
species.list <- species.list%>%filter(Caves=="FALSE")
#current species richness
sr.current <- lapply(species.list$Scientific_name, function(sp){
  raster(paste0("F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/output_maps/",sp," current.tif"))*vn.raster
})
batsr.current <- overlay(stack(sr.current),fun=sum)
writeRaster(batsr.current, filename = 'F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_current_no_cave.tif', format="GTiff", overwrite=T)
#species richness for individual GCMs
#combined model
for(gcm in c('ac', 'bc','ca','cm','cn','cs','gf','gi','ip','mi')){
  sr.2050.rcp45.both <- lapply(species.list$Scientific_name, function(sp){
    raster(paste0("F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/output_maps/",sp," 2050 rcp45 both ",gcm,".tif"))*vn.raster
  })
  batsr.2050.rcp45.both <- overlay(stack(sr.2050.rcp45.both),fun=sum)
  writeRaster(batsr.2050.rcp45.both, filename = paste0('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_2050_rcp45_both_',gcm,'_no_cave.tif'), format="GTiff", overwrite=T)
  print(gcm)
}
#climate change only model
for(gcm in c('ac', 'bc','ca','cm','cn','cs','gf','gi','ip','mi')){
  sr.2050.rcp45.cl <- lapply(species.list$Scientific_name, function(sp){
    raster(paste0("F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/output_maps/",sp," 2050 rcp45 cl ",gcm,".tif"))*vn.raster
  })
  batsr.2050.rcp45.cl <- overlay(stack(sr.2050.rcp45.cl),fun=sum)
  writeRaster(batsr.2050.rcp45.cl, filename = paste0('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_2050_rcp45_cl_',gcm,'_no_cave.tif'), format="GTiff", overwrite=T)
  print(gcm)
}
#land-cover change only models
sr.2050.rcp45.lc <- lapply(species.list$Scientific_name, function(sp){
  raster(paste0("F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/output_maps/",sp," 2050 rcp45 lc.tif"))*vn.raster
})
batsr.lc.rcp45 <- overlay(stack(sr.2050.rcp45.lc),fun=sum)
#consensus future prediction rcp45
gcm <- c('ac', 'bc','ca','cm','cn','cs','gf','gi','ip','mi')
sr.2050.both <- lapply(gcm, function(x){
  raster(paste0('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_2050_rcp45_both_',x,'_no_cave.tif'))
})
batsr.2050.both.median <- overlay(stack(sr.2050.both), fun=median)

sr.2050.cl <- lapply(gcm, function(x){
  raster(paste0('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_2050_rcp45_cl_',x,'_no_cave.tif'))
})
batsr.2050.cl.median <- overlay(stack(sr.2050.cl), fun=median)
writeRaster(batsr.2050.both.median,filename = 'F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_2050_rcp45_both_median_no_cave.tif',format='GTiff',overwrite=T)
writeRaster(batsr.2050.cl.median,filename = 'F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_2050_rcp45_cl_median_no_cave.tif',format='GTiff',overwrite=T)
writeRaster(batsr.lc.rcp45,filename = 'F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_2050_rcp45_lc_no_cave.tif',format='GTiff',overwrite=T)

vn.raster <- raster('F:/Working/2018/PhD_research/SDM output R/Map/vn raster.tif') #import Vietnam
batsr.current <- raster('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_current_no_cave.tif')
batsr.lc.rcp45 <- raster('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_2050_rcp45_lc_no_cave.tif')
batsr.lc.rcp45.current <- (batsr.lc.rcp45 - batsr.current)/batsr.current*100
pro.lc.rcp45 <- c(cellStats(batsr.lc.rcp45.current>=30,'sum')/cellStats(vn.raster,'sum')*100, 
                  cellStats(batsr.lc.rcp45.current < 30 & batsr.lc.rcp45.current > 0,'sum')/cellStats(vn.raster,'sum')*100,
                  cellStats(batsr.lc.rcp45.current == 0,'sum')/cellStats(vn.raster,'sum')*100,
                  cellStats(batsr.lc.rcp45.current < 0 & batsr.lc.rcp45.current > -30,'sum')/cellStats(vn.raster,'sum')*100,
                  cellStats(batsr.lc.rcp45.current <= -30,'sum')/cellStats(vn.raster,'sum')*100)
#effect of combined change
for(gcm in c('ac', 'bc','ca','cm','cn','cs','gf','gi','ip','mi')){
  batsr.both.rcp45 <- raster(paste0('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_2050_rcp45_both_',gcm,'_no_cave.tif'))
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
  batsr.cl.rcp45 <- raster(paste0('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_2050_rcp45_cl_',gcm,'_no_cave.tif'))
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
write.xlsx(sum.both.sr,'F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/srchange_both_rcp45_no_cave.xlsx')
write.xlsx(sum.cl.sr,'F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/srchange_cl_rcp45_no_cave.xlsx')
write.xlsx(sum.lc.sr,'F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/srchange_lc_rcp45_no_cave.xlsx')


#plot maps species richness change
batsr.current <- raster('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_current_no_cave.tif')
batsr.2050.both <- raster('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_2050_rcp45_both_median_no_cave.tif')
batsr.2050.cl <- raster('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_2050_rcp45_cl_median_no_cave.tif')
batsr.2050.lc <- raster('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_2050_rcp45_lc_no_cave.tif')
SR.ORG8287 <- CRS('+proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs')
vn <- spTransform(getData('GADM', country='VN', level=0), SR.ORG8287)
paracell.islands <- spTransform(readOGR('F:/Working/2018/PhD_research/SDM output R/Map/paracel islands.shp'), SR.ORG8287)
spratly.islands <- spTransform(readOGR('F:/Working/2018/PhD_research/SDM output R/Map/spratly islands.shp'), SR.ORG8287)
# pas <- readOGR('F:/Working/2018/PhD_research/SDM output R/Map/vn_pas.shp')

batsr.both.current <- (batsr.2050.both - batsr.current)/batsr.current*100
batsr.cl.current <- (batsr.2050.cl - batsr.current)/batsr.current*100
batsr.lc.current <- (batsr.2050.lc - batsr.current)/batsr.current*100

plot.current<- tm_shape(batsr.current, bbox = c(11380000, 930000,12450000,  2517140)) + 
  tm_raster('layer',breaks = c(0,2,4,6,8,10,15,20,25,30,35,40,50,65), palette = 'Greens', style = 'fixed',
             title = 'Species richness')+
  tm_shape(paracell.islands) + tm_lines(col = 'grey')+
  tm_shape(spratly.islands) + tm_lines(col = 'grey')+
  # tm_shape(pas) + tm_polygons(col = 'green', alpha = 0,border.col = 'darkgreen')+
  tm_shape(vn)+ tm_polygons(border.col = 'dark gray', alpha = 0, lwd = 0.5)+
  tm_layout(legend.outside = F, title = 'Species richness',title.position = c('left','top'),title.size = 1,
            legend.text.size = 0.5, legend.title.size = 1,legend.position = c(0,0.3), frame = F, legend.show = F)

plot.srchange.rcp45.both <-  tm_shape(batsr.both.current, bbox = c(11380000, 930000,12450000,  2517140)) + 
  tm_raster('layer',breaks = c(-Inf,-30,-0.0001,0.0001,30,Inf),palette = c('red','orange','grey','lightblue','blue'), 
            title = 'SR change (%)',labels = c('< -30','-30~0','0','0~30','> 30'))+
  tm_shape(paracell.islands) + tm_lines(col = 'grey')+
  tm_shape(spratly.islands) + tm_lines(col = 'grey')+
  # tm_shape(pas) + tm_polygons(col = 'green', alpha = 0,border.col = 'darkgreen')+
  tm_shape(vn)+ tm_polygons(border.col = 'dark gray', alpha = 0, lwd = 0.5)+
  tm_layout(legend.outside = F, title = '(b) Combined change', title.size = 1, legend.position = c(0, 0.3),
            title.position = c('left','top'), frame = F, legend.text.size = 0.8, legend.title.size = 2, legend.show = F) 
plot.srchange.rcp45.cl <-  tm_shape(batsr.cl.current, bbox = c(11380000, 930000,12450000,  2517140)) + 
  tm_raster('layer',breaks = c(-Inf,-25,-0.0001,0.0001,25,Inf),palette = c('red','orange','grey','lightblue','blue'), 
            title = '% SR change')+
  tm_shape(paracell.islands) + tm_lines(col = 'grey')+
  tm_shape(spratly.islands) + tm_lines(col = 'grey')+
  # tm_shape(pas) + tm_polygons(col = 'green', alpha = 0,border.col = 'darkgreen')+
  tm_shape(vn)+ tm_polygons(border.col = 'gray', alpha = 0, lwd = 0.5)+
  tm_layout(title = '(c) Climate change only', title.size = 1,title.position = c('left','top'), frame = F,legend.show = F) 
plot.srchange.rcp45.lc <-  tm_shape(batsr.lc.current, bbox = c(11380000, 930000,12450000,  2517140)) + 
  tm_raster('layer',breaks = c(-Inf,-25,-0.0001,0.0001,25,Inf),palette = c('red','orange','grey','lightblue','blue'), 
            title = '% SR change')+
  tm_shape(paracell.islands) + tm_lines(col = 'grey')+
  tm_shape(spratly.islands) + tm_lines(col = 'grey')+
  # tm_shape(pas) + tm_polygons(col = 'green', alpha = 0,border.col = 'darkgreen')+
  tm_shape(vn)+ tm_polygons(border.col = 'dark gray', alpha = 0, lwd = 0.5)+
  tm_layout(title = '(d) Land cover change only', title.size = 1, title.position = c('left','top'), frame = F, legend.show = F)
figure <- tmap_arrange(plot.current,plot.srchange.rcp45.both,plot.srchange.rcp45.cl,plot.srchange.rcp45.lc, nrow = 1)
tmap_save(figure, filename = 'F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/figures/current and srchange rcp45_no_cave.tiff',
          units = 'cm', width = 24, height = 11, dpi=300)

#classify interaction effects
#additive species richness for each GCMs
for(gcm in c('ac', 'bc','ca','cm','cn','cs','gf','gi','ip','mi')){
  sp.2050.rcp45.add <- lapply(species.list$Scientific_name, function(sp){
    raster(paste0("F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/",sp," 2050 rcp45 additive ",gcm,".tif"))*vn.raster
  })
  batsr.2050.rcp45 <- overlay(stack(sp.2050.rcp45.add),fun=sum)
  writeRaster(batsr.2050.rcp45, filename = paste0('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_2050_rcp45_add_',gcm,'_no_cave.tif'), format="GTiff", overwrite=T)
  print(gcm)
}
for(gcm in c('ac', 'bc','ca','cm','cn','cs','gf','gi','ip','mi')){  
  batsr.2050.cl <- raster(paste0('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_2050_rcp45_cl_',gcm,'_no_cave.tif'))
  batsr.2050.both <- raster(paste0('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_2050_rcp45_both_',gcm,'_no_cave.tif'))
  batsr.2050.add <- raster(paste0('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_2050_rcp45_add_',gcm,'_no_cave.tif'))
  sr.interaction <- batsr.current*0
  #two negative and negative neutral
  sr.interaction[batsr.2050.cl <= batsr.current & batsr.lc.rcp45 <= batsr.current & (batsr.2050.cl + batsr.lc.rcp45 != 2* batsr.current)&
                   batsr.2050.both < batsr.2050.add] <- 1
  sr.interaction[batsr.2050.cl <= batsr.current & batsr.lc.rcp45 <= batsr.current & (batsr.2050.cl + batsr.lc.rcp45 != 2* batsr.current)&
                   batsr.2050.both == batsr.2050.add] <- 3
  sr.interaction[batsr.2050.cl <= batsr.current & batsr.lc.rcp45 <= batsr.current & (batsr.2050.cl + batsr.lc.rcp45 != 2* batsr.current)&
                   batsr.2050.both > batsr.2050.add & batsr.2050.both <= batsr.current] <- 4
  sr.interaction[batsr.2050.cl <= batsr.current & batsr.lc.rcp45 <= batsr.current & (batsr.2050.cl + batsr.lc.rcp45 != 2* batsr.current)&
                   batsr.2050.both > batsr.current] <- 5
  #two positive and positive neutral
  sr.interaction[batsr.2050.cl >= batsr.current & batsr.lc.rcp45 >= batsr.current& (batsr.2050.cl + batsr.lc.rcp45 != 2* batsr.current)&
                   batsr.2050.both < batsr.current] <- 1
  sr.interaction[batsr.2050.cl >= batsr.current & batsr.lc.rcp45 >= batsr.current& (batsr.2050.cl + batsr.lc.rcp45 != 2* batsr.current)&
                   batsr.2050.both >= batsr.current & batsr.2050.both < batsr.2050.add] <- 2
  sr.interaction[batsr.2050.cl >= batsr.current & batsr.lc.rcp45 >= batsr.current& (batsr.2050.cl + batsr.lc.rcp45 != 2* batsr.current)&
                   batsr.2050.both == batsr.2050.add] <- 3
  sr.interaction[batsr.2050.cl >= batsr.current & batsr.lc.rcp45 >= batsr.current& (batsr.2050.cl + batsr.lc.rcp45 != 2* batsr.current)&
                   batsr.2050.both > batsr.2050.add] <- 5
  #opposite
  sr.interaction[(batsr.2050.cl - batsr.current)*(batsr.lc.rcp45 - batsr.current) < 0 & 
                   batsr.2050.both < min(batsr.2050.cl, batsr.lc.rcp45)] <- 1
  sr.interaction[(batsr.2050.cl - batsr.current)*(batsr.lc.rcp45 - batsr.current) < 0 & 
                   batsr.2050.both >= min(batsr.2050.cl, batsr.lc.rcp45) & batsr.2050.both < batsr.2050.add] <- 2
  sr.interaction[(batsr.2050.cl - batsr.current)*(batsr.lc.rcp45 - batsr.current) < 0 & 
                   batsr.2050.both == batsr.2050.add] <- 3
  sr.interaction[(batsr.2050.cl - batsr.current)*(batsr.lc.rcp45 - batsr.current) < 0 & 
                   batsr.2050.both > batsr.2050.add & batsr.2050.both <= max(batsr.2050.cl, batsr.lc.rcp45)] <- 4
  sr.interaction[(batsr.2050.cl - batsr.current)*(batsr.lc.rcp45 - batsr.current) < 0 & 
                   batsr.2050.both > max(batsr.2050.cl, batsr.lc.rcp45)] <- 5
  #double neutral
  sr.interaction[batsr.2050.cl == batsr.current & batsr.lc.rcp45 == batsr.current &
                   batsr.2050.both < batsr.2050.add] <- 1
  sr.interaction[batsr.2050.cl == batsr.current & batsr.lc.rcp45 == batsr.current &
                   batsr.2050.both == batsr.2050.add] <- 3
  sr.interaction[batsr.2050.cl == batsr.current & batsr.lc.rcp45 == batsr.current &
                   batsr.2050.both > batsr.2050.add] <- 5
  writeRaster(sr.interaction, filename = paste0('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/interaction_types_rcp45_',gcm,'_no_cave.tif'), format="GTiff", overwrite=T)
  print(gcm)
}

# compute proportion of interaction types within areas with land-cover change only
extent.asia.sr <- extent(11000000, 13000000, 900000, 2600000)
vn.raster <- raster('F:/Working/2018/PhD_research/SDM output R/Map/vn raster.tif') #import Vietnam
lc.current <- crop(raster('F:/Working/2018/PhD_research/enviromental_variables/land-cover/MODISLandcover_2010_srorg8287.tif'),extent.asia.sr)*vn.raster
lc.2050.rcp45 <- crop(raster('F:/Working/2018/PhD_research/enviromental_variables/land-cover/MODISLandcover_2050_rcp45_srorg8287.tif'),extent.asia.sr)*vn.raster
area.withlcchange <- lc.2050.rcp45 != lc.current
area.withlcchange.area <- cellStats(area.withlcchange, 'sum')
for(gcm in c('ac', 'bc','ca','cm','cn','cs','gf','gi','ip','mi')){
  sr.interaction <- raster(paste0('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/interaction_types_rcp45_',gcm,'_no_cave.tif'))*area.withlcchange
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

sum.both.sr <- read_excel('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/srchange_both_rcp45_no_cave.xlsx')[2:5]
sum.cl.sr <- read_excel('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/srchange_cl_rcp45_no_cave.xlsx')[2:5]
sum.lc.sr <- read_excel('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/srchange_lc_rcp45_no_cave.xlsx')[2:3]
names(sum.both.sr) <- c('class','pro.both.median','pro.both.min','pro.both.max')
names(sum.cl.sr) <- c('class','pro.cl.median','pro.cl.min','pro.cl.max')

figure.bar.both <- sum.both.sr%>%ggplot(aes(x=factor(class,levels = c('<-30','-30~0','0','0~30','>30')),y=pro.both.median[1:5],fill=class))+
  geom_bar(stat = 'identity')+ theme_classic()+
  scale_fill_manual('',values = c('>30'='blue','0~30'='lightblue','0'='grey70','-30~0'='orange','<-30'='red'))+
  xlab('')+ylab('')+labs(title = '')+  ylim(0,95)+
  geom_errorbar(aes(ymin=pro.both.min[1:5], ymax=pro.both.max[1:5]), width=.2,position=position_dodge(.9))+
  theme(legend.position = 'none', 
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12, angle = 0, vjust = 0.5),
        title = element_text(size = 14))
figure.bar.cl <- sum.cl.sr%>%ggplot(aes(x=factor(class,levels = c('<-30','-30~0','0','0~30','>30')),y=pro.cl.median[1:5],fill=class))+
  geom_bar(stat = 'identity')+ theme_classic()+
  scale_fill_manual('',values = c('>30'='blue','0~30'='lightblue','0'='grey70','-30~0'='orange','<-30'='red'))+
  xlab('')+ylab('')+labs(title = '')+ ylim(0,95)+
  geom_errorbar(aes(ymin=pro.cl.min[1:5], ymax=pro.cl.max[1:5]), width=.2,position=position_dodge(.9))+
  theme(legend.position = 'none', 
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12, angle = 0, vjust = 0.5),
        title = element_text(size = 14))
figure.bar.lc <- sum.lc.sr%>%ggplot(aes(x=factor(class,levels = c('<-30','-30~0','0','0~30','>30')),y=pro.lc.rcp45[1:5],fill=class))+
  geom_bar(stat = 'identity')+ theme_classic()+
  scale_fill_manual('',values = c('>30'='blue','0~30'='lightblue','0'='grey70','-30~0'='orange','<-30'='red'))+
  xlab('')+ylab('')+labs(title = '')+ ylim(0,95)+
  theme(legend.position = 'none', 
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        title = element_text(size = 14))
figure.interaction <- ggplot(sum, aes(fill=interaction, y=pro.interaction.median, x=factor(interaction,levels = c('-S','+A','AD','-A','+S')))) +
  geom_bar(position="stack", stat="identity")+  ylim(0,95)+ theme_classic()+
  scale_fill_manual('',values = c('-S'='purple4','+A'='purple','AD'='grey70', '-A'='aquamarine','+S'='aquamarine4'))+
  # annotate("text", label = "", size = 4, x = 3, y = 70)+
  xlab('')+ylab('')+labs(title = '')+
  geom_errorbar(aes(ymin=pro.interaction.min, ymax=pro.interaction.max), width=.2,position=position_dodge(.9))+
  theme(text = element_text(size = 14), legend.position = 'none', 
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12))

figure <- ggarrange(figure.bar.both,figure.bar.cl,figure.bar.lc,figure.interaction,nrow = 1)
ggsave(figure, filename = 'F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/figures/srchange_interaction_bar_rcp45_no_cave.tiff', 
       units = 'cm', width = 30, height = 8, dpi=300)


#Plot interaction maps
SR.ORG8287 <- CRS('+proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs') 
vn <- spTransform(getData('GADM', country='VN', level=0), SR.ORG8287)
paracell.islands <- spTransform(readOGR('F:/Working/2018/PhD_research/SDM output R/Map/paracel islands.shp'), SR.ORG8287)
spratly.islands <- spTransform(readOGR('F:/Working/2018/PhD_research/SDM output R/Map/spratly islands.shp'), SR.ORG8287)
# pas <- readOGR('F:/Working/2018/PhD_research/SDM output R/Map/vn_pas.shp')
gcm <- c('ac', 'bc','ca','cm','cn','cs','gf','gi','ip','mi')
sr.interaction <- lapply(gcm, function(x){
  raster(paste0('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/interaction_types_rcp45_',x,'_no_cave.tif'))
})
select.major <- function(x){
  modal(x, na.rm=T, freq=F, ties='random')
}
sr.interaction.ensemble <- overlay(stack(sr.interaction), fun=select.major)#select major interaction types across GCMs
figure.map <- tm_shape(sr.interaction.ensemble,bbox = c(11070729, 943456.3,12186081,  2517140)) +
  tm_raster('layer', breaks = c(-0.5,0.5,1.5,2.5,3.5,4.5,5.5),palette = c('white','purple4','purple','grey70','aquamarine','aquamarine4'),
  labels = c("","-S", "+A", "AD","-A","+S"),title = '')+
  tm_shape(vn)+tm_polygons(col = 'white', alpha = 0, border.col = 'grey')+
  tm_shape(paracell.islands) + tm_lines(col = 'grey')+
  tm_shape(spratly.islands) + tm_lines(col = 'grey')+
  # tm_shape(pas) + tm_polygons(col = 'green', alpha = 0,border.col = 'darkgreen')+
  tm_layout(title = '',title.position = c(0,0.95),legend.position = c(0.1,0.5), frame = F)
tmap_save(figure.map, filename = 'F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/figures/interaction_sr_rcp45_no_cave.tiff',width = 8, height = 10, units = 'cm',dpi = 300)

#impact of climate and land cover change on distribution range size
species.list <- read.xlsx("F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/vnbats_habitat_traits.xlsx", sheetName = 'Sheet1')
species.list <- species.list%>%select(Scientific_name,Forest,Shrubland,Grassland,Wetlands,Caves,Artificial,Diet.Plant,Diet.Invertebrate)
species.list <- species.list%>%filter(Caves=="FALSE")
area.sum <- read.xlsx("F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/area sum rcp45.xlsx", sheetName = 'Sheet1')
area.sum <- area.sum%>%filter(Scientific_name%in%species.list$Scientific_name)
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
  area.sum <- area.sum%>%filter(Scientific_name%in%species.list$Scientific_name)
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
        plot.title = element_text(hjust = 0),legend.position = 'none')
figure.cl <- sum.cl%>%ggplot(aes(x=factor(class, levels = c('< -30',' -30~0','0~30','>30')),y=pro.median.5.8.,fill=class)) + 
  geom_bar(stat = 'identity')+
  scale_fill_manual('',values = c('>30'='blue','0~30'='lightblue',' -30~0'='orange','< -30'='red'))+
  scale_x_discrete(limits=c('< -30',' -30~0','0~30','>30'))+ theme_classic()+ ylim(0,100)+
  labs(x='', y='', title = '')+
  geom_errorbar(aes(ymin=pro.min[5:8], ymax=pro.max[5:8]), width=.2,position=position_dodge(.9))+
  theme(text = element_text(size = 14), axis.text = element_text(size = 14),
        plot.title = element_text(hjust = 0),legend.position = 'none')

figure.lc <- sum.lc%>%ggplot(aes(x=factor(class, levels = c('< -30',' -30~0','0~30','>30')), 
                                 y=pro.median.9.12.,fill=class)) + 
  geom_bar(stat = 'identity')+
  scale_fill_manual('',values = c('>30'='blue','0~30'='lightblue',' -30~0'='orange','< -30'='red'))+
  scale_x_discrete(limits=c('< -30',' -30~0','0~30','>30'))+ theme_classic()+ ylim(0,100)+
  labs(x='', y='', title = '')+
  theme(text = element_text(size = 14),axis.text = element_text(size = 14), 
        plot.title = element_text(hjust = 0),legend.position = 'none')

figure.interaction <- sum.range.interaction%>%ggplot(aes(x=factor(class,levels = c('-S','+A','AD','-A','+S')),y=pro.range.interaction.median,fill=class))+
  geom_bar(stat = 'identity')+ theme_classic()+
  scale_fill_manual('',values = c('-S'='purple4','+A'='purple','AD'='grey70', '-A'='aquamarine','+S'='aquamarine4'))+
  xlab('')+ylab('')+labs(title = '')+  ylim(0,100)+
  geom_errorbar(aes(ymin=pro.range.interaction.min, ymax=pro.range.interaction.max), width=.2,position=position_dodge(.9))+
  theme(text = element_text(size = 14), legend.position = 'none', axis.text = element_text(size = 14))
figure <- ggarrange(figure.both, figure.cl, figure.lc,figure.interaction,nrow = 1)
ggsave(figure, filename = 'F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/figures/rangesize_change_interaction_rcp45_no_cave.tiff',
       units = 'cm', width = 28, height = 8, dpi = 300)


#test the significant on the change of distribution range size
area.sum <- read.xlsx("F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/area sum rcp45.xlsx", sheetName = 'Sheet1')
area.sum <- area.sum%>%mutate(area.2050.both.median = 0)
area.sum <- area.sum%>%mutate(area.2050.cl.median = 0)
for(i in 1:length(area.sum$Scientific_name)){
  area.sum[,'area.2050.both.median'][i] <- median(area.sum$area.2050.rcp45.both.ac[i], area.sum$area.2050.rcp45.both.bc[i], area.sum$area.2050.rcp45.both.ca[i],
                                                  area.sum$area.2050.rcp45.both.cm[i], area.sum$area.2050.rcp45.both.cn[i], area.sum$area.2050.rcp45.both.cs[i],
                                                  area.sum$area.2050.rcp45.both.gf[i], area.sum$area.2050.rcp45.both.gi[i], area.sum$area.2050.rcp45.both.ip[i],
                                                  area.sum$area.2050.rcp45.both.mi[i])
  area.sum[,'area.2050.cl.median'][i] <- median(area.sum$area.2050.rcp45.cl.ac[i], area.sum$area.2050.rcp45.cl.bc[i], area.sum$area.2050.rcp45.cl.ca[i],
                                                area.sum$area.2050.rcp45.cl.cm[i], area.sum$area.2050.rcp45.cl.cn[i], area.sum$area.2050.rcp45.cl.cs[i],
                                                area.sum$area.2050.rcp45.cl.gf[i], area.sum$area.2050.rcp45.cl.gi[i], area.sum$area.2050.rcp45.cl.ip[i],
                                                area.sum$area.2050.rcp45.cl.mi[i])
}
bats.habitat <- read.xlsx("F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/vnbats_habitat_traits.xlsx", sheetName = 'Sheet1')
bats.habitat <- bats.habitat%>%select(Scientific_name,Forest,Shrubland,Grassland,Wetlands,Caves,Artificial,Diet.Plant,Diet.Invertebrate)
area.sum <- merge(area.sum, bats.habitat, by.x='Scientific_name', by.y='Scientific_name', all.x=T)
area.sum <- area.sum%>%dplyr::mutate('area.change.both'= (area.2050.both.median-area.current)/area.current*100)
area.sum <- area.sum%>%dplyr::mutate('area.change.cl'= (area.2050.cl.median-area.current)/area.current*100)
area.sum <- area.sum%>%dplyr::mutate('area.change.lc'= (area.2050.rcp45.lc-area.current)/area.current*100)

area.sum$roosting <- ifelse(area.sum$Caves=='TRUE','Cave roosting','Others')
figure.both <- area.sum%>%ggplot(aes(x= factor(roosting, levels = c('Cave roosting','Others')), y=area.change.both))+
  stat_summary(geom = "boxplot", 
               fun.data = function(x) setNames(quantile(x, c(0.0, 0.25, 0.5, 0.75, 1)), c("ymin", "lower", "middle", "upper", "ymax")), 
               position = "dodge")+ylim(-100,200)+
  theme_classic()+
  labs(x='',y='Percent change in range size', title = '(a)')+
  theme(text = element_text(size = 12), axis.text.x = element_text(size = 12), axis.title.y = element_text(size = 14))+
  stat_compare_means(aes(label = ..p.signif..), method = 'wilcox.test',label.x = 1.5, label.y = 190,size = 10)#add Wincolson test
figure.cl <- area.sum%>%ggplot(aes(x= factor(roosting, levels = c('Cave roosting','Others')), y=area.change.cl))+
  stat_summary(geom = "boxplot", 
               fun.data = function(x) setNames(quantile(x, c(0.0, 0.25, 0.5, 0.75, 1)), c("ymin", "lower", "middle", "upper", "ymax")), 
               position = "dodge")+ylim(-100,200)+
  theme_classic()+
  labs(x='',y='', title = '(b)')+
  theme(text = element_text(size = 12), axis.text.x = element_text(size = 12))+
  stat_compare_means(aes(label = ..p.signif..), method = 'wilcox.test',label.x = 1.5, label.y = 190,size=10)#add Wincolson test
figure.lc <- area.sum%>%ggplot(aes(x= factor(roosting, levels = c('Cave roosting','Others')), y=area.change.lc))+
  stat_summary(geom = "boxplot", 
               fun.data = function(x) setNames(quantile(x, c(0.0, 0.25, 0.5, 0.75, 1)), c("ymin", "lower", "middle", "upper", "ymax")), 
               position = "dodge")+
  theme_classic()+
  labs(x='',y='', title = '(c)')+
  theme(text = element_text(size = 12), axis.text.x = element_text(size = 12))+
  stat_compare_means(aes(label = ..p.signif..), method = 'wilcox.test',label.x = 1.5, label.y = 55,size=10)#add Wincolson test
figure <- ggarrange(figure.both, figure.cl, figure.lc, nrow = 1)
ggsave(figure, filename = 'F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/figures/area change_roosting_rcp45.tiff',
       width = 24, height = 10, units = 'cm', dpi = 300)

wilcox.test(area.sum[area.sum$Caves=='TRUE',]$area.change.both, area.sum[area.sum$Caves=='FALSE',]$area.change.both,alternative = 'two.sided')
wilcox.test(area.sum[area.sum$Caves=='TRUE',]$area.change.cl, area.sum[area.sum$Caves=='FALSE',]$area.change.cl,alternative = 'two.sided')
wilcox.test(area.sum[area.sum$Caves=='TRUE',]$area.change.lc, area.sum[area.sum$Caves=='FALSE',]$area.change.lc,alternative = 'two.sided')

#test significant on species richness change
vn.raster <- raster('F:/Working/2018/PhD_research/SDM output R/Map/vn raster.tif') #import Vietnam
extent.asia.sr <- extent(11000000, 13000000, 900000, 2600000)
lc.current <- crop(raster('F:/Working/2018/PhD_research/enviromental_variables/land-cover/MODISLandcover_2010_srorg8287.tif'),extent.asia.sr)*vn.raster
lc.2050.rcp45 <- crop(raster('F:/Working/2018/PhD_research/enviromental_variables/land-cover/MODISLandcover_2050_rcp45_srorg8287.tif'),extent.asia.sr)*vn.raster
area.withlcchange <- lc.2050.rcp45 != lc.current
area.withlcchange[area.withlcchange==0] <- NA
batsr.current.cave <- raster('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_current_cave.tif')
batsr.2050.both.cave <- raster('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_2050_rcp45_both_median_cave.tif')
batsr.2050.cl.cave <- raster('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_2050_rcp45_cl_median_cave.tif')
batsr.2050.lc.cave <- raster('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_2050_rcp45_lc_cave.tif')*area.withlcchange

srchange.both.cave <- (batsr.2050.both.cave - batsr.current.cave)/batsr.current.cave*100
srchange.cl.cave <- (batsr.2050.cl.cave - batsr.current.cave)/batsr.current.cave*100
srchange.lc.cave <- (batsr.2050.lc.cave - batsr.current.cave)/batsr.current.cave*100
srchange.both.cave.df <- as.data.frame(srchange.both.cave, na.rm=T)
srchange.cl.cave.df <- as.data.frame(srchange.cl.cave, na.rm=T)
srchange.lc.cave.df <- as.data.frame(srchange.lc.cave, na.rm=T)
srchange.both.cave.df <- srchange.both.cave.df%>%mutate('Cell_number'=1:nrow(srchange.both.cave.df))
srchange.cl.cave.df <- srchange.cl.cave.df%>%mutate('Cell_number'=1:nrow(srchange.cl.cave.df))
srchange.lc.cave.df <- srchange.lc.cave.df%>%mutate('Cell_number'=1:nrow(srchange.lc.cave.df))

batsr.current.no_cave <- raster('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_current_forest_others.tif')
batsr.2050.both.no_cave <- raster('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_2050_rcp45_both_median_forest_others.tif')
batsr.2050.cl.no_cave <- raster('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_2050_rcp45_cl_median_forest_others.tif')
batsr.2050.lc.no_cave <- raster('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_2050_rcp45_lc_forest_others.tif')*area.withlcchange

srchange.both.no_cave <- (batsr.2050.both.no_cave - batsr.current.no_cave)/batsr.current.no_cave*100
srchange.cl.no_cave <- (batsr.2050.cl.no_cave - batsr.current.no_cave)/batsr.current.no_cave*100
srchange.lc.no_cave <- (batsr.2050.lc.no_cave - batsr.current.no_cave)/batsr.current.no_cave*100
srchange.both.no_cave.df <- as.data.frame(srchange.both.no_cave, na.rm=T)
srchange.cl.no_cave.df <- as.data.frame(srchange.cl.no_cave, na.rm=T)
srchange.lc.no_cave.df <- as.data.frame(srchange.lc.no_cave, na.rm=T)
srchange.both.no_cave.df <- srchange.both.no_cave.df%>%mutate('Cell_number'=1:nrow(srchange.both.no_cave.df))
srchange.cl.no_cave.df <- srchange.cl.no_cave.df%>%mutate('Cell_number'=1:nrow(srchange.cl.no_cave.df))
srchange.lc.no_cave.df <- srchange.lc.no_cave.df%>%mutate('Cell_number'=1:nrow(srchange.lc.no_cave.df))


#t-test for guilds under combined change
srchange.both.random <- replicate(1000, {
  cells <- sample(srchange.both.cave.df$Cell_number,1000)#obtain 1000 cells
  srchange.cave <- srchange.both.cave.df%>%filter(Cell_number%in%cells)#get sr change for forest specialist
  srchange.nocave <- srchange.both.no_cave.df%>%filter(Cell_number%in%cells)#get sr change for forest generalist
  mean <- list(c(mean(srchange.cave$layer),mean(srchange.nocave$layer)))
})
srchange.both.mean <- data.frame(t(sapply(srchange.both.random,c)))
names(srchange.both.mean) <- c('Cave','No_cave')
t.test(srchange.both.mean$Cave,srchange.both.mean$No_cave, paired = TRUE, alternative = "two.sided")
wilcox.test(srchange.both.mean$Cave,srchange.both.mean$No_cave, paired = TRUE, alternative = "two.sided")

#t-test for guilds under climate change
srchange.cl.random <- replicate(1000, {
  cells <- sample(srchange.cl.cave.df$Cell_number,1000)#obtain 1000 cells
  srchange.cave <- srchange.cl.cave.df%>%filter(Cell_number%in%cells)#get sr change for forest specialist
  srchange.nocave <- srchange.cl.no_cave.df%>%filter(Cell_number%in%cells)#get sr change for forest generalist
  mean <- list(c(mean(srchange.cave$layer),mean(srchange.nocave$layer)))
})
srchange.cl.mean <- data.frame(t(sapply(srchange.cl.random,c)))
names(srchange.cl.mean) <- c('Cave','No_cave')
t.test(srchange.cl.mean$Cave,srchange.cl.mean$No_cave, paired = TRUE, alternative = "two.sided")
wilcox.test(srchange.cl.mean$Cave,srchange.cl.mean$No_cave, paired = TRUE, alternative = "two.sided")

#t-test for guilds under landcover change
srchange.lc.random <- replicate(1000, {
  cells <- sample(srchange.lc.cave.df$Cell_number,1000)#obtain 1000 cells
  srchange.cave <- srchange.lc.cave.df%>%filter(Cell_number%in%cells)#get sr change for forest specialist
  srchange.nocave <- srchange.lc.no_cave.df%>%filter(Cell_number%in%cells)#get sr change for forest generalist
  mean <- list(c(mean(srchange.cave$layer),mean(srchange.nocave$layer)))
})
srchange.lc.mean <- data.frame(t(sapply(srchange.lc.random,c)))
names(srchange.lc.mean) <- c('Cave','No_cave')
t.test(srchange.lc.mean$Cave,srchange.lc.mean$No_cave, paired = TRUE, alternative = "two.sided")
wilcox.test(srchange.lc.mean$Cave,srchange.lc.mean$No_cave, paired = TRUE, alternative = "two.sided")


#Plot 
srchange <- c(srchange.both.mean$Cave,srchange.cl.mean$Cave,srchange.lc.mean$Cave,
              srchange.both.mean$No_cave,srchange.cl.mean$No_cave,srchange.lc.mean$No_cave)
habitat <- c(rep("Cave roosting",1000),rep("Cave roosting",1000),rep("Cave roosting",1000),
             rep("Others",1000),rep("Others",1000),rep("Others",1000))
models <- c(rep('Combined',1000),rep('Climate only',1000),rep('Land cover only',1000),
            rep('Combined',1000),rep('Climate only',1000),rep('Land cover only',1000))
sum <- data.frame(srchange, habitat, models)
figure <- sum%>%ggplot(aes(x= factor(habitat, levels = c('Cave roosting', 'Others')), y=srchange))+
  stat_summary(geom = "boxplot",
               fun.data = function(x) setNames(quantile(x, c(0.0, 0.25, 0.5, 0.75, 1)), c("ymin", "lower", "middle", "upper", "ymax")),
               position = "dodge")+ylim(-40,20)+
  theme_classic()+
  labs(x='',y='Percent change in range size', title = '')+
  facet_wrap(~factor(models,levels=c('Combined','Climate only','Land cover only')))+
  theme(text = element_text(size = 12), axis.text.x = element_text(size = 12), axis.title.y = element_text(size = 14))
  # stat_compare_means(aes(label = ..p.signif..), method = 'wilcox.test',label.x = 1.5, label.y = 190,size = 5)#add Wincolson test
ggsave(figure, filename = 'F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/figures/srchange_roosting_rcp45.tiff',
       width = 20, height = 10, units = 'cm', dpi = 300)


