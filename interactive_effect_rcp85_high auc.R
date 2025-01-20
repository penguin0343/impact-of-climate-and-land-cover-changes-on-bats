library(rgeos)
library(raster)
library(rgdal)#for reading shp file
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
library(ggpubr)#for assemling figures
library(RColorBrewer) # for making color

latlong <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
SR.ORG8287 <- CRS('+proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs') 
extent.asia.sr <- extent(11000000, 13000000, 900000, 2600000)
vn.raster <- raster('F:/Working/2018/PhD_research/SDM output R/Map/vn raster.tif') #import Vietnam

#import list of species
species.list <- read_excel('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/model performance_block_LQH.xlsx')
species.list <- species.list%>%filter(auc.m1.5 > 0.7)
#generate consensus species richness
sr.current <- lapply(species.list$species, function(sp){
  raster(paste0("F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/output_maps/",sp," current.tif"))*vn.raster
})
batsr.current <- overlay(stack(sr.current),fun=sum)
writeRaster(batsr.current, filename = 'F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_current_highauc.tif', format="GTiff", overwrite=T)

#species richness for individual GCMs
#combined model
for(gcm in c('ac', 'bc','ca','cm','cn','cs','gf','gi','ip','mi')){
  sr.2050.rcp85.both <- lapply(species.list$species, function(sp){
    raster(paste0("F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/output_maps/",sp," 2050 rcp85 both ",gcm,".tif"))*vn.raster
  })
  batsr.2050.rcp85.both <- overlay(stack(sr.2050.rcp85.both),fun=sum)
  writeRaster(batsr.2050.rcp85.both, filename = paste0('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_2050_rcp85_both_',gcm,'_highauc.tif'), format="GTiff", overwrite=T)
  print(gcm)
}

#climate model
for(gcm in c('ac', 'bc','ca','cm','cn','cs','gf','gi','ip','mi')){
  sr.2050.rcp85.cl <- lapply(species.list$species, function(sp){
    raster(paste0("F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/output_maps/",sp," 2050 rcp85 cl ",gcm,".tif"))*vn.raster
  })
  batsr.2050.rcp85.cl <- overlay(stack(sr.2050.rcp85.cl),fun=sum)
  writeRaster(batsr.2050.rcp85.cl, filename = paste0('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_2050_rcp85_cl_',gcm,'_highauc.tif'), format="GTiff", overwrite=T)
  print(gcm)
}

#land-cover models
sr.2050.rcp85.lc <- lapply(species.list$species, function(sp){
  raster(paste0("F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/output_maps/",sp," 2050 rcp85 lc.tif"))*vn.raster
})

batsr.2050.rcp85.lc <- overlay(stack(sr.2050.rcp85.lc),fun=sum)
writeRaster(batsr.2050.rcp85.lc, filename = 'F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_2050_rcp85_lc_highauc.tif', format="GTiff", overwrite=T)

gcm <- c('ac', 'bc','ca','cm','cn','cs','gf','gi','ip','mi')
#concensus future prediction rcp85
sr.2050.both <- lapply(gcm, function(x){
  raster(paste0('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_2050_rcp85_both_',x,'_highauc.tif'))
})
batsr.2050.both.median <- overlay(stack(sr.2050.both), fun=median)
batsr.2050.both.min <- overlay(stack(sr.2050.both), fun= min)
batsr.2050.both.max <- overlay(stack(sr.2050.both), fun= max)
sr.2050.cl <- lapply(gcm, function(x){
  raster(paste0('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_2050_rcp85_cl_',x,'_highauc.tif'))
})
batsr.2050.cl.median <- overlay(stack(sr.2050.cl), fun=median)

writeRaster(batsr.2050.both.median, filename = paste0('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_2050_rcp85_both_median_highauc.tif'), format="GTiff", overwrite=T)
writeRaster(batsr.2050.cl.median, filename = paste0('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_2050_rcp85_cl_median_highauc.tif'), format="GTiff", overwrite=T)



#plot maps species richness change
batsr.current <- raster('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_current_highauc.tif')
batsr.2050.both <- raster('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_2050_rcp85_both_median_highauc.tif')
batsr.2050.cl <- raster('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_2050_rcp85_cl_median_highauc.tif')
batsr.2050.lc <- raster('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_2050_rcp85_lc_highauc.tif')
SR.ORG8287 <- CRS('+proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs') 
vn <- spTransform(getData('GADM', country='VN', level=0), SR.ORG8287)
paracell.islands <- spTransform(readOGR('F:/Working/2018/PhD_research/SDM output R/Map/paracel islands.shp'), SR.ORG8287)
spratly.islands <- spTransform(readOGR('F:/Working/2018/PhD_research/SDM output R/Map/spratly islands.shp'), SR.ORG8287)

batsr.cl.current <- (batsr.2050.cl - batsr.current)/batsr.current*100
batsr.lc.current <- (batsr.2050.lc - batsr.current)/batsr.current*100
batsr.both.current <- (batsr.2050.both - batsr.current)/batsr.current*100

gcm <- c('ac', 'bc','ca','cm','cn','cs','gf','gi','ip','mi')
sr.interaction <- lapply(gcm, function(x){
  raster(paste0('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/interaction_types_rcp85_',x,'_highauc.tif'))
})
select.major <- function(x){
  modal(x, na.rm=T, freq=F, ties='random')
}
sr.interaction.ensemble <- overlay(stack(sr.interaction), fun=select.major)#select major interaction types across GCMs

plot.srchange.rcp85.both <-  tm_shape(batsr.both.current, bbox = c(11380000, 930000,12450000,  2517140)) + 
  tm_raster('layer',breaks = c(-Inf,-30,-0.0001,0.0001,30,Inf),palette = c('red','orange','grey70','lightblue','blue'), 
            title = '',labels = c('< -30','-30~0','0','0~30','> 30'))+
  tm_shape(vn)+ tm_polygons(border.col = 'dark gray', alpha = 0, lwd = 0.5)+
  tm_shape(paracell.islands) + tm_lines(col = 'grey')+
  tm_shape(spratly.islands) + tm_lines(col = 'grey')+
  tm_layout(legend.outside = F, title = '', title.size = 1, legend.position = c(0, 0.25),
            title.position = c(0,1), frame = F, legend.text.size = 0.7, legend.title.size = 1, legend.show = F) 
plot.srchange.rcp85.cl <-  tm_shape(batsr.cl.current, bbox = c(11380000, 930000,12450000,  2517140)) + 
  tm_raster('layer',breaks = c(-Inf,-25,-0.0001,0.0001,25,Inf),palette = c('red','orange','grey70','lightblue','blue'), 
            title = '% SR change')+
  tm_shape(vn)+ tm_polygons(border.col = 'gray', alpha = 0, lwd = 0.5)+
  tm_shape(paracell.islands) + tm_lines(col = 'grey')+
  tm_shape(spratly.islands) + tm_lines(col = 'grey')+
  tm_layout(title = '', title.size = 1,title.position = c('left','top'), frame = F,legend.show = F) 
plot.srchange.rcp85.lc <-  tm_shape(batsr.lc.current, bbox = c(11380000, 930000,12450000,  2517140)) + 
  tm_raster('layer',breaks = c(-Inf,-25,-0.0001,0.0001,25,Inf),palette = c('red','orange','grey70','lightblue','blue'), 
            title = '% SR change')+
  tm_shape(vn)+ tm_polygons(border.col = 'dark gray', alpha = 0, lwd = 0.5)+
  tm_layout(title = '', title.size = 1, title.position = c('left','top'), frame = F, legend.show = F)
plot.interaction <- tm_shape(sr.interaction.ensemble,bbox = c(11380000, 930000,12450000,  2517140)) +
  tm_raster('layer', breaks = c(-0.5,0.5,1.5,2.5,3.5,4.5,5.5),palette = c('white','purple4','purple','grey70','aquamarine','aquamarine4'),
            labels = c("","-S", "+A", "AD","-A","+S"),title = '')+
  tm_shape(vn)+tm_polygons(col = 'white', alpha = 0, border.col = 'grey')+
  tm_shape(paracell.islands) + tm_lines(col = 'grey')+
  tm_shape(spratly.islands) + tm_lines(col = 'grey')+
  tm_layout(title = '',title.position = c(0,0.95),legend.position = c(0.1,0.5), frame = F,legend.show = F)

figure <- tmap_arrange(plot.srchange.rcp85.both,plot.srchange.rcp85.cl,plot.srchange.rcp85.lc,plot.interaction, nrow = 1)
tmap_save(figure, filename = 'F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/figures/srchange_interaction_rcp85_highauc.tiff',
          units = 'cm', width = 30, height = 11, dpi=300)

#percent change in species richness bar chart
vn.raster <- raster('F:/Working/2018/PhD_research/SDM output R/Map/vn raster.tif') #import Vietnam
batsr.current <- raster('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_current_highauc.tif')
batsr.lc.rcp85 <- raster('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_2050_rcp85_lc_highauc.tif')
batsr.lc.rcp85.current <- (batsr.lc.rcp85 - batsr.current)/batsr.current*100
pro.lc.rcp85 <- c(cellStats(batsr.lc.rcp85.current>=30,'sum')/cellStats(vn.raster,'sum')*100, 
                  cellStats(batsr.lc.rcp85.current < 30 & batsr.lc.rcp85.current > 0,'sum')/cellStats(vn.raster,'sum')*100,
                  cellStats(batsr.lc.rcp85.current == 0,'sum')/cellStats(vn.raster,'sum')*100,
                  cellStats(batsr.lc.rcp85.current < 0 & batsr.lc.rcp85.current > -30,'sum')/cellStats(vn.raster,'sum')*100,
                  cellStats(batsr.lc.rcp85.current <= -30,'sum')/cellStats(vn.raster,'sum')*100)
#effect of combined change
for(gcm in c('ac', 'bc','ca','cm','cn','cs','gf','gi','ip','mi')){
  batsr.both.rcp85 <- raster(paste0('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_2050_rcp85_both_',gcm,'_highauc.tif'))
  batsr.both.rcp85.current <- (batsr.both.rcp85 - batsr.current)/batsr.current*100
  pro.both.rcp85 <- c(cellStats(batsr.both.rcp85.current>=30,'sum')/cellStats(vn.raster,'sum')*100,
                      cellStats(batsr.both.rcp85.current < 30 & batsr.both.rcp85.current > 0,'sum')/cellStats(vn.raster,'sum')*100,
                      cellStats(batsr.both.rcp85.current == 0,'sum')/cellStats(vn.raster,'sum')*100,
                      cellStats(batsr.both.rcp85.current < 0 & batsr.both.rcp85.current > -30,'sum')/cellStats(vn.raster,'sum')*100,
                      cellStats(batsr.both.rcp85.current <= -30,'sum')/cellStats(vn.raster,'sum')*100,
                      cellStats(batsr.both.rcp85.current < 0, 'sum')/cellStats(vn.raster,'sum')*100,
                      cellStats(batsr.both.rcp85.current > 0, 'sum')/cellStats(vn.raster,'sum')*100)
  assign(paste0('pro.both.rcp85.',gcm),pro.both.rcp85)
  print(gcm)
}

pro.both.all <- data.frame(pro.both.rcp85.ac,pro.both.rcp85.bc,pro.both.rcp85.ca,pro.both.rcp85.cm,pro.both.rcp85.cn,
                           pro.both.rcp85.cs,pro.both.rcp85.gf,pro.both.rcp85.gi,pro.both.rcp85.ip,pro.both.rcp85.mi)
#effect of climate change only
for(gcm in c('ac', 'bc','ca','cm','cn','cs','gf','gi','ip','mi')){
  batsr.cl.rcp85 <- raster(paste0('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_2050_rcp85_cl_',gcm,'_highauc.tif'))
  batsr.cl.rcp85.current <- (batsr.cl.rcp85 - batsr.current)/batsr.current*100
  pro.cl.rcp85 <- c(cellStats(batsr.cl.rcp85.current>=30,'sum')/cellStats(vn.raster,'sum')*100,
                    cellStats(batsr.cl.rcp85.current < 30 & batsr.cl.rcp85.current > 0,'sum')/cellStats(vn.raster,'sum')*100,
                    cellStats(batsr.cl.rcp85.current == 0,'sum')/cellStats(vn.raster,'sum')*100,
                    cellStats(batsr.cl.rcp85.current < 0 & batsr.cl.rcp85.current > -30,'sum')/cellStats(vn.raster,'sum')*100,
                    cellStats(batsr.cl.rcp85.current <= -30,'sum')/cellStats(vn.raster,'sum')*100,
                    cellStats(batsr.cl.rcp85.current < 0, 'sum')/cellStats(vn.raster,'sum')*100,
                    cellStats(batsr.cl.rcp85.current > 0, 'sum')/cellStats(vn.raster,'sum')*100)
  assign(paste0('pro.cl.rcp85.',gcm),pro.cl.rcp85)
  print(gcm)
}
pro.cl.all <- data.frame(pro.cl.rcp85.ac, pro.cl.rcp85.bc, pro.cl.rcp85.ca, pro.cl.rcp85.cm, pro.cl.rcp85.cn,
                         pro.cl.rcp85.cs, pro.cl.rcp85.gf, pro.cl.rcp85.gi, pro.cl.rcp85.ip, pro.cl.rcp85.mi)
pro.both.median <- apply(pro.both.all,1,median)
pro.both.min <- apply(pro.both.all,1,min)
pro.both.max <- apply(pro.both.all,1,max)

pro.cl.median <- apply(pro.cl.all,1,median)
pro.cl.min <- apply(pro.cl.all,1,min)
pro.cl.max <- apply(pro.cl.all,1,max)

class <- c('>30','0~30','0','-30~0','<-30')
sum.both.sr <- data.frame(class,pro.both.median[1:5],pro.both.min[1:5], pro.both.max[1:5])
sum.cl.sr <- data.frame(class,pro.cl.median[1:5], pro.cl.min[1:5], pro.cl.max[1:5])
sum.lc.sr <- data.frame(class,pro.lc.rcp85)
sum.both.sr$class <- factor(sum.both.sr$class, levels = c('<-30','-30~0','0','0~30','>30'))
sum.cl.sr$class <- factor(sum.cl.sr$class, levels = c('<-30','-30~0','0','0~30','>30'))
sum.lc.sr$class <- factor(sum.lc.sr$class, levels = c('<-30','-30~0','0','0~30','>30'))
write.xlsx(sum.both.sr,'F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/srchange_both_rcp85_highauc.xlsx')
write.xlsx(sum.cl.sr,'F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/srchange_cl_rcp85_highauc.xlsx')
write.xlsx(sum.lc.sr,'F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/srchange_lc_rcp85_highauc.xlsx')

# figure.bar.both <- sum.both.sr%>%ggplot(aes(x=factor(class,levels = c('<-30','-30~0','0','0~30','>30')),y=pro.both.median[1:5],fill=class))+
#   geom_bar(stat = 'identity')+ theme_classic()+
#   scale_fill_manual('',values = c('>30'='blue','0~30'='lightblue','0'='grey','-30~0'='orange','<-30'='brown'))+
#   xlab('')+ylab('Proportion of land in Vietnam')+labs(title = '(e)')+  ylim(0,70)+
#   geom_errorbar(aes(ymin=pro.both.min[1:5], ymax=pro.both.max[1:5]), width=.2,position=position_dodge(.9))+
#   theme(legend.position = 'none', 
#         axis.text.y = element_text(size = 10),
#         axis.text.x = element_text(size = 10, angle = 90, vjust = 0.5),
#         axis.title = element_text(size = 14),
#         title = element_text(size = 14))
# figure.bar.cl <- sum.cl.sr%>%ggplot(aes(x=factor(class,levels = c('<-30','-30~0','0','0~30','>30')),y=pro.cl.median[1:5],fill=class))+
#   geom_bar(stat = 'identity')+ theme_classic()+
#   scale_fill_manual('',values = c('>30'='blue','0~30'='lightblue','0'='grey','-30~0'='orange','<-30'='brown'))+
#   xlab('Percent change in SR')+ylab('')+labs(title = '(f)')+ ylim(0,70)+
#   geom_errorbar(aes(ymin=pro.cl.min[1:5], ymax=pro.cl.max[1:5]), width=.2,position=position_dodge(.9))+
#   theme(legend.position = 'none', 
#         axis.text.y = element_text(size = 10),
#         axis.text.x = element_text(size = 10, angle = 90, vjust = 0.5),
#         axis.title = element_text(size = 14),
#         title = element_text(size = 14))
# figure.bar.lc <- sum.lc.sr%>%ggplot(aes(x=factor(class,levels = c('<-30','-30~0','0','0~30','>30')),y=pro.lc.rcp85[1:5],fill=class))+
#   geom_bar(stat = 'identity')+ theme_classic()+
#   scale_fill_manual('',values = c('>30'='blue','0~30'='lightblue','0'='grey','-30~0'='orange','<-30'='brown'))+
#   xlab('')+ylab('')+labs(title = '(g)')+ ylim(0,70)+
#   theme(legend.position = 'none', 
#         axis.text.y = element_text(size = 10),
#         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 10),
#         title = element_text(size = 14))
# 
# figure <- ggarrange(figure.bar.both,figure.bar.cl,figure.bar.lc,nrow = 1, ncol = 3)
# ggsave(figure, filename = 'F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/figures/srchange bar rcp85 high auc.tiff', 
#        units = 'cm', width = 22, height = 8, dpi=300)


#additive species richness for each GCMs
species.list <- read_excel('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/model performance_block_LQH.xlsx')
species.list <- species.list%>%filter(auc.m1.5 > 0.7)
vn.raster <- raster('F:/Working/2018/PhD_research/SDM output R/Map/vn raster.tif') #import Vietnam
for(gcm in c('ac', 'bc','ca','cm','cn','cs','gf','gi','ip','mi')){
  sp.2050.rcp85.add <- lapply(species.list$species, function(sp){
    raster(paste0("F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/",sp," 2050 rcp85 additive ",gcm,".tif"))*vn.raster
  })
  batsr.2050.rcp85 <- overlay(stack(sp.2050.rcp85.add),fun=sum)
  writeRaster(batsr.2050.rcp85, filename = paste0('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_2050_rcp85_add_',gcm,'_highauc.tif'), format="GTiff", overwrite=T)
  print(gcm)
}

#classify interaction effects
extent.asia.sr <- extent(11000000, 13000000, 900000, 2600000)
vn.raster <- raster('F:/Working/2018/PhD_research/SDM output R/Map/vn raster.tif') #import Vietnam
lc.current <- crop(raster('F:/Working/2018/PhD_research/enviromental_variables/land-cover/MODISLandcover_2010_srorg8287.tif'),extent.asia.sr)*vn.raster
lc.2050.rcp85 <- crop(raster('F:/Working/2018/PhD_research/enviromental_variables/land-cover/MODISLandcover_2050_rcp85_srorg8287.tif'),extent.asia.sr)*vn.raster
area.withlcchange <- lc.2050.rcp85 != lc.current
cellStats(area.withlcchange,'sum')/cellStats(vn.raster,'sum')
batsr.current <- raster('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_current_highauc.tif')
batsr.2050.lc <- raster('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_2050_rcp85_lc_highauc.tif')

for(gcm in c('ac', 'bc','ca','cm','cn','cs','gf','gi','ip','mi')){  
  batsr.2050.cl <- raster(paste0('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_2050_rcp85_cl_',gcm,'_highauc.tif'))
  batsr.2050.both <- raster(paste0('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_2050_rcp85_both_',gcm,'_highauc.tif'))
  batsr.2050.add <- raster(paste0('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/batsr_2050_rcp85_add_',gcm,'_highauc.tif'))
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
  writeRaster(sr.interaction, filename = paste0('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/interaction_types_rcp85_',gcm,'_highauc.tif'), format="GTiff", overwrite=T)
  print(gcm)
}

#compute proportion of interaction types within areas with land-cover change only
extent.asia.sr <- extent(11000000, 13000000, 900000, 2600000)
vn.raster <- raster('F:/Working/2018/PhD_research/SDM output R/Map/vn raster.tif') #import Vietnam
lc.current <- crop(raster('F:/Working/2018/PhD_research/enviromental_variables/land-cover/MODISLandcover_2010_srorg8287.tif'),extent.asia.sr)*vn.raster
lc.2050.rcp85 <- crop(raster('F:/Working/2018/PhD_research/enviromental_variables/land-cover/MODISLandcover_2050_rcp85_srorg8287.tif'),extent.asia.sr)*vn.raster
area.withlcchange <- lc.2050.rcp85 != lc.current
area.withlcchange.area <- cellStats(area.withlcchange, 'sum')
for(gcm in c('ac', 'bc','ca','cm','cn','cs','gf','gi','ip','mi')){
  sr.interaction <- raster(paste0('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/interaction_types_rcp85_',gcm,'_highauc.tif'))*area.withlcchange
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

sum.both.sr <- read_excel('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/srchange_both_rcp85_highauc.xlsx')[2:5]
sum.cl.sr <- read_excel('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/srchange_cl_rcp85_highauc.xlsx')[2:5]
sum.lc.sr <- read_excel('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/srchange_lc_rcp85_highauc.xlsx')[2:3]
names(sum.both.sr) <- c('class','pro.both.median','pro.both.min','pro.both.max')
names(sum.cl.sr) <- c('class','pro.cl.median','pro.cl.min','pro.cl.max')

figure.bar.both <- sum.both.sr%>%ggplot(aes(x=factor(class,levels = c('<-30','-30~0','0','0~30','>30')),y=pro.both.median[1:5],fill=class))+
  geom_bar(stat = 'identity')+ theme_classic()+
  scale_fill_manual('',values = c('>30'='blue','0~30'='lightblue','0'='grey70','-30~0'='orange','<-30'='red'))+
  xlab('')+ylab('')+labs(title = '')+  ylim(0,70)+
  geom_errorbar(aes(ymin=pro.both.min[1:5], ymax=pro.both.max[1:5]), width=.2,position=position_dodge(.9))+
  theme(legend.position = 'none', 
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
figure.bar.lc <- sum.lc.sr%>%ggplot(aes(x=factor(class,levels = c('<-30','-30~0','0','0~30','>30')),y=pro.lc.rcp85[1:5],fill=class))+
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
ggsave(figure, filename = 'F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/figures/srchange_interaction_bar_rcp85_highauc.tiff', 
       units = 'cm', width = 28, height = 8, dpi=300)

#Plot interaction maps
SR.ORG8287 <- CRS('+proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs') 
vn <- spTransform(getData('GADM', country='VN', level=0), SR.ORG8287)
paracell.islands <- spTransform(readOGR('F:/Working/2018/PhD_research/SDM output R/Map/paracel islands.shp'), SR.ORG8287)
spratly.islands <- spTransform(readOGR('F:/Working/2018/PhD_research/SDM output R/Map/spratly islands.shp'), SR.ORG8287)
# pas <- readOGR('F:/Working/2018/PhD_research/SDM output R/Map/vn_pas.shp')
gcm <- c('ac', 'bc','ca','cm','cn','cs','gf','gi','ip','mi')
sr.interaction <- lapply(gcm, function(x){
  raster(paste0('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/interaction_types_rcp85_',x,'_highauc.tif'))
})
select.major <- function(x){
  modal(x, na.rm=T, freq=F, ties='random')
}
sr.interaction.ensemble <- overlay(stack(sr.interaction), fun=select.major)#select major interaction types across GCMs
figure.map <- tm_shape(sr.interaction.ensemble,bbox = c(11070729, 943456.3,12186081,  2517140)) +
  tm_raster('layer', breaks = c(-0.5,0.5,1.5,2.5,3.5,4.5,5.5),palette = c('white','tomato4','tomato','grey70','turquoise2','turquoise4'),
            labels = c("","-S", "+A", "AD","-A","+S"),title = '')+
  tm_shape(vn)+tm_polygons(col = 'white', alpha = 0, border.col = 'grey')+
  tm_shape(paracell.islands) + tm_lines(col = 'grey')+
  tm_shape(spratly.islands) + tm_lines(col = 'grey')+
  # tm_shape(pas) + tm_polygons(col = 'green', alpha = 0,border.col = 'darkgreen')+
  tm_layout(title = '(a)',title.position = c(0,0.95),legend.position = c(0.1,0.5), frame = F)
tmap_save(figure.map, filename = 'F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/figures/interaction_sr_rcp85_highauc.tiff',width = 8, height = 10, units = 'cm',dpi = 300)

#compare impact of climate & lc change on distribution range
species.list <- read_excel('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/model performance_block_LQH.xlsx')
species.list <- species.list%>%filter(auc.m1.5 > 0.7)
area.sum <- read.xlsx("F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/area sum rcp85.xlsx", sheetName = 'Sheet1')
area.sum <- area.sum%>%filter(Scientific_name%in%species.list$species)
area.sum <- area.sum%>%mutate(area.change.rcp85.lc = (area.2050.rcp85.lc - area.current)/area.current*100)
no.species.increase30.rcp85.lc <- nrow(area.sum%>%filter(area.change.rcp85.lc >= 30))/nrow(area.sum)*100
no.species.increase0.rcp85.lc <- nrow(area.sum%>%filter(area.change.rcp85.lc < 30 & area.change.rcp85.lc>0))/nrow(area.sum)*100
no.species.decrease0.rcp85.lc <- nrow(area.sum%>%filter(area.change.rcp85.lc <= 0 &area.change.rcp85.lc > -30))/nrow(area.sum)*100
no.species.decrease30.rcp85.lc <- nrow(area.sum%>%filter(area.change.rcp85.lc <= -30 & area.2050.rcp85.lc > 0))/nrow(area.sum)*100

for(gcm in c('ac', 'bc','ca','cm','cn','cs','gf','gi','ip','mi')){
  area.sum[,'area.change.rcp85.both'] <- (area.sum[,paste0('area.2050.rcp85.both.',gcm)] - area.sum[,"area.current"])/area.sum[,"area.current"]*100
  area.sum[,'area.change.rcp85.cl'] <- (area.sum[,paste0('area.2050.rcp85.cl.',gcm)] - area.sum[,"area.current"])/area.sum[,"area.current"]*100
  area.sum[,'area.2050.rcp85.both'] <- area.sum[,paste0('area.2050.rcp85.both.',gcm)]
  area.sum[,'area.2050.rcp85.cl'] <- area.sum[,paste0('area.2050.rcp85.cl.',gcm)]
  no.species.increase30.rcp85.both <- nrow(area.sum%>%filter(area.change.rcp85.both>= 30))/nrow(area.sum)*100
  no.species.increase0.rcp85.both <- nrow(area.sum%>%filter(area.change.rcp85.both< 30 & area.change.rcp85.both >0))/nrow(area.sum)*100
  no.species.decrease0.rcp85.both <- nrow(area.sum%>%filter(area.change.rcp85.both <= 0 & area.change.rcp85.both > -30))/nrow(area.sum)*100
  no.species.decrease30.rcp85.both <- nrow(area.sum%>%filter(area.change.rcp85.both <= -30))/nrow(area.sum)*100
  no.species.increase30.rcp85.cl <- nrow(area.sum%>%filter(area.change.rcp85.cl>= 30))/nrow(area.sum)*100
  no.species.increase0.rcp85.cl <- nrow(area.sum%>%filter(area.change.rcp85.cl< 30 & area.change.rcp85.cl>0))/nrow(area.sum)*100
  no.species.decrease0.rcp85.cl <- nrow(area.sum%>%filter(area.change.rcp85.cl <= 0 & area.change.rcp85.cl > -30))/nrow(area.sum)*100
  no.species.decrease30.rcp85.cl <- nrow(area.sum%>%filter(area.change.rcp85.cl <= -30 & area.2050.rcp85.cl > 0))/nrow(area.sum)*100
  no.species.rcp85 <- c(no.species.increase30.rcp85.both, no.species.increase0.rcp85.both,no.species.decrease0.rcp85.both,no.species.decrease30.rcp85.both,
                        no.species.increase30.rcp85.cl, no.species.increase0.rcp85.cl,no.species.decrease0.rcp85.cl, no.species.decrease30.rcp85.cl,
                        no.species.increase30.rcp85.lc, no.species.increase0.rcp85.lc,no.species.decrease0.rcp85.lc,no.species.decrease30.rcp85.lc)
  assign(paste0('no.rcp85.',gcm), no.species.rcp85) 
  print(gcm)
}
pro <- data.frame(rbind(no.rcp85.ac, no.rcp85.bc, no.rcp85.ca, no.rcp85.cm, no.rcp85.cn,
                        no.rcp85.cs, no.rcp85.gf, no.rcp85.gi, no.rcp85.ip, no.rcp85.mi))
pro <- pro%>%mutate(in.both = X1 + X2, de.both = X3 + X4, in.cl = X5+ X6, de.cl = X7 + X8, in.lc = X9+ X10, de.lc = X11 + X12)
pro.median <- apply(pro,2,median)
pro.min <- apply(pro,2,min)
pro.max <- apply(pro,2,max)
class <- c('>30','0~30',' -30~0','< -30')
sum.both <- data.frame(class, pro.median[1:4], pro.min[1:4], pro.max[1:4])
sum.cl <- data.frame(class, pro.median[5:8], pro.min[5:8], pro.max[5.8])
sum.lc <- data.frame(class, pro.median[9:12])
sum.both$class <- factor(sum.both$class, levels = c('< -30',' -30~0','0~30','>30'))


#interaction effects on distribution range size
for(gcm in c('ac', 'bc','ca','cm','cn','cs','gf','gi','ip','mi')){
  area.sum <- read_xlsx('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/area sum rcp85.xlsx')
  area.sum <- area.sum%>%filter(Scientific_name%in%species.list$species)
  area.sum <- area.sum%>%select(Scientific_name,area.current, paste0('area.2050.rcp85.cl.',gcm), area.2050.rcp85.lc, paste0('area.2050.rcp85.both.', gcm),paste0('area.2050.rcp85.add.', gcm))
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
ggsave(figure, filename = 'F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/figures/rangesize_change_interaction_rcp85_highauc.tiff',
       units = 'cm', width = 28, height = 8, dpi = 300)
