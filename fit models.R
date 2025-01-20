library(raster)
library(rgdal)
library(dismo)
library(sp)
library(rJava)
library(readxl)
library(mapview)
library(ggplot2)
library(jsonlite)
require(sf)
library(devtools)
library(xlsx)
library(dplyr)
library(ggcorrplot)
library(tmap)
library(ggpubr)
latlong <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
SR.ORG8287 <- CRS('+proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs') 
extent.asia.sr <- extent(11000000, 13000000, 900000, 2600000)
vn.raster <- raster('F:/Working/2018/PhD_research/SDM output R/Map/vn raster.tif') #import Vietnam

#check correlation between variables
list.raster.full <- list.files("F:/Working/2018/PhD_research/enviromental_variables/current", full.names = T, pattern = ".tif")
predictors.check <- stack(list.raster.full)
predictors.check <- subset(predictors.check, c('bio2','bio10','bio11','bio12','bio18','bio19'))

#import occurence data
vnbats.asia <- read_excel('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/final_occurences.xlsx') # import bat occurence data
coordinates(vnbats.asia)= ~ decimalLongitude + decimalLatitude
crs(vnbats.asia) <- latlong
vnbats.asia <- spTransform(vnbats.asia, SR.ORG8287)
#remove points from same cells
cells <- cellFromXY(predictors.check$bio2, vnbats.asia)
dups <- duplicated(cells)
vnbats.asia <- vnbats.asia[!dups,]
nrow(vnbats.asia)
#extract enviromental information from cell contains bat occurences
vnbat.asia.env = raster::extract(predictors.check, vnbats.asia, df =T)
vnbat.asia.env <- vnbat.asia.env[!is.na(as.numeric(as.character(vnbat.asia.env$bio2))),]
bat.cor <- data.frame(cor(vnbat.asia.env[,2:7], method = "spearman"))
colnames(bat.cor) <- c('Mean diurnal temperature range', 'Mean temperature of the warmest quarter','Meean temperature of the coldest quarter', 
                       'Annual Precipitation', 'Precipitation of the warmest quarter', 'Precipitation of the coldest quarter')
rownames(bat.cor) <- c('Mean diurnal temperature range', 'Mean temperature of the warmest quarter','Meean temperature of the coldest quarter', 
                       'Annual Precipitation', 'Precipitation of the warmest quarter', 'Precipitation of the coldest quarter')

p<- ggcorrplot(bat.cor,method = "square", show.legend = T, show.diag = T, lab = T, ggtheme = ggplot2::theme_classic(),
               colors = c("#6D9EC1", "white", "#E46726"),
               hc.order = F,type = 'lower',legend.title = 'Coefficient',
               sig.level = T, pch = 2, pch.col = 'red', digits = 1,
               title = "" )+
  theme(axis.text.x = element_text(size = 7), axis.text.y = element_text(size = 7), text = element_text(size = 7))
ggsave(p, filename = 'F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/figures/correlation matrix.png', width = 12, height = 12, units = 'cm', dpi = 300)

#fit the models
#import environmental variables
# Current
list.raster.full <- list.files("F:/Working/2018/PhD_research/enviromental_variables/current", full.names = T, pattern = ".tif")
predictors <- stack(list.raster.full)
predictors <- subset(predictors, c("bio2","bio10","bio11","bio12","bio18","bio19","karst","forest","grassland","farmland","urban")) #select variables

#import occurrence data
vnbats.asia <- read_excel('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/final_occurences.xlsx') # import bat occurence data
coordinates(vnbats.asia)= ~ decimalLongitude + decimalLatitude
latlong <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
crs(vnbats.asia) <- latlong
SR.ORG8287 <- CRS('+proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs') 
vnbats.asia <- spTransform(vnbats.asia, SR.ORG8287)

species.list <- read_excel('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/list of species.xlsx')
species.list <- species.list%>%filter(Num_occurence_all > 19)
nrow(species.list)

#extract environmental information
enviromental.extract <-raster::extract(predictors, vnbats.asia, df=T, cellnumbers = T)
for(sp in species.list$Scientific_name) {
  sp.presence <- vnbats.asia[vnbats.asia$species== sp,] 
  cells.presence <- data.frame(cellFromXY(predictors$karst, sp.presence))
  names(cells.presence) <- 'cellnum'
  enviromental.extract[,sp] <- ifelse(enviromental.extract$cells%in%cells.presence$cellnum, 1, 0)
}
# write.csv(enviromental.extract, file = 'E:/Working/2018/PhD thesis/SDM output R/19-7-21/enviromental extract.csv')
enviromental.extract$karst <- factor(enviromental.extract$karst)
enviromental.extract$forest <- factor(enviromental.extract$forest)
enviromental.extract$grassland <- factor(enviromental.extract$grassland)
enviromental.extract$farmland <- factor(enviromental.extract$farmland)
enviromental.extract$urban <- factor(enviromental.extract$urban)

#fit models
for(sp in species.list$Scientific_name){
  sp.presence <- vnbats.asia[vnbats.asia$species== sp,] 
  dups <- duplicated(enviromental.extract[, c('cells',sp)])
  env <- enviromental.extract[!dups,]
  xm <- maxent(x=env[,3:13], p= data.frame(env[,sp]),
               path=paste0('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/response/',sp),
               args=c('betamultiplier=1.5',
                 'linear=true',
                 'quadratic=true',
                 'hinge=true',
                 'product=false',
                 'threshold=false',
                 'threads=2',
                 'responsecurves=true',
                 'jackknife=true',
                 'askoverwrite=false',
                 'autofeature=false'))
                 # args=setMaxEntOptions(iterations = 5000))
  # save(xm, file = paste0('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/response/',sp,'.RData'))
  print(sp)
}  


#####Calculate variables contribution
#bio2","bio6","bio10","bio12","bio18","karst","forest","grassland","farmland","urban")
species.list <- read_excel('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/list of species.xlsx')
species.list <- species.list%>%filter(Num_occurence_all > 19)
nrow(species.list)
var.contri <- data.frame(species.list$Scientific_name)
var.contri$bio2 <- 0
var.contri$bio10 <- 0
var.contri$bio11 <- 0
var.contri$bio12 <- 0
var.contri$bio18 <- 0
var.contri$bio19 <- 0
var.contri$forest <- 0
var.contri$grassland <- 0
var.contri$farmland <- 0
var.contri$urban <- 0
var.contri$karst <- 0

for(i in 1:length(species.list$Scientific_name)){
  sp <- species.list$Scientific_name[i]
  contribution <- read.csv(paste0('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/response/',sp,'/maxentResults.csv'), header=T, sep=',')
  var.contri$bio2[i] <- contribution$bio2.contribution
  var.contri$bio10[i] <- contribution$bio10.contribution
  var.contri$bio11[i] <- contribution$bio11.contribution
  var.contri$bio12[i] <- contribution$bio12.contribution
  var.contri$bio18[i] <- contribution$bio18.contribution
  var.contri$bio19[i] <- contribution$bio19.contribution
  var.contri$forest[i] <- contribution$forest.contribution
  var.contri$grassland[i] <- contribution$grassland.contribution
  var.contri$farmland[i] <- contribution$farmland.contribution
  var.contri$urban[i] <- contribution$urban.contribution
  var.contri$karst[i] <- contribution$karst.contribution
  print(i)
}

contri <- c(var.contri$bio2,
                      var.contri$bio10,
                      var.contri$bio11,
                      var.contri$bio12,
                      var.contri$bio18,
                      var.contri$bio19,
                      var.contri$forest,
                      var.contri$grassland,
                      var.contri$farmland,
                      var.contri$urban,
            var.contri$karst)

var.name <- c(rep('Mean diurnal temperature range',length(species.list$Scientific_name)),
              rep('Mean temperature of the warmest quarter',length(species.list$Scientific_name)),
              rep('Mean temperature of the coldest quarter',length(species.list$Scientific_name)),
              rep('Annual precipitation',length(species.list$Scientific_name)),
              rep('Precipitation of the warmest quarter',length(species.list$Scientific_name)),
              rep('Precipitation of the coldest quarter',length(species.list$Scientific_name)),
              rep('Forest',length(species.list$Scientific_name)),
              rep('Grassland',length(species.list$Scientific_name)),
              rep('Farmland',length(species.list$Scientific_name)),
              rep('Urban',length(species.list$Scientific_name)),
              rep('Karst',length(species.list$Scientific_name))
              )
var.df <- data.frame(var.name,contri)
c
p <- ggplot(var.df, aes(x=reorder(factor(var.name),contri,median), y=contri))+
  stat_summary(geom = "boxplot", 
               fun.data = function(x) setNames(quantile(x, c(0.05, 0.25, 0.5, 0.75, 0.95)), c("ymin", "lower", "middle", "upper", "ymax")), 
               position = "dodge")+ ylim(0,106)+
  stat_summary(geom='point', 
               fun = function(x) {
                 r <- quantile(x, probs = c(0.0, 1))
                 r[[1]]<-r[[1]]-1.5*IQR(x)
                 r[[2]]<-r[[2]]+1.5*IQR(x)
                 subset(x, x < r[[1]] | r[[2]] < x)
               }, color= 'grey', size=0.5)+
  theme_classic()+   coord_flip()+ labs(title ="")+
  xlab('')+ylab('Percent contribution (%)')+
  theme(plot.title = element_text(hjust = 0.5), axis.text = element_text(size = 10), text = element_text(size = 10))
ggsave(p, filename = 'F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/figures/variable contribution.tiff', height = 10, width = 18, units = 'cm', dpi = 300)

# 
#check mess for Vietnam
list.raster.full <- list.files("F:/Working/2018/PhD_research/enviromental_variables/current", full.names = T, pattern = ".tif")
predictors <- stack(list.raster.full)
predictors <- subset(predictors, c("bio2","bio10","bio11","bio12","bio18","bio19","karst","forest","grassland","farmland","urban")) #select variables
vn.raster <- raster('F:/Working/2018/PhD_research/SDM output R/Map/vn raster.tif')
#import occurrence data
vnbats.asia <- read_excel('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/final_occurences.xlsx') # import bat occurence data
coordinates(vnbats.asia)= ~ decimalLongitude + decimalLatitude
latlong <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
crs(vnbats.asia) <- latlong
SR.ORG8287 <- CRS('+proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs') 
vnbats.asia <- spTransform(vnbats.asia, SR.ORG8287)
occEnvVal <- raster::extract(predictors, vnbats.asia)
predictors.vn <- crop(predictors,extent(11000000, 13000000, 900000, 2600000))*vn.raster
names(predictors.vn) <- names(predictors)
proj.mess<-suppressWarnings(mess(predictors.vn, occEnvVal))
writeRaster(proj.mess, filename = 'F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/mess_current.tif', format="GTiff", overwrite=T)

#generate median bioclimatic variables
for(bio in c("bio2","bio10","bio11","bio12","bio18","bio19")){
  gcm <- c('ac', 'bc','ca','cm','cn','cs','gf','gi','ip','mi')
  bio.2050.rcp45 <- lapply(gcm, function(x){
    crop(raster(paste0('F:/Working/2018/PhD_research/enviromental_variables/2050/rcp45/',x,'/',bio,'.tif')),extent(11000000, 13000000, 900000, 2600000))*vn.raster
  })
  bio.2050.rcp45.median <- overlay(stack(bio.2050.rcp45), fun=median)
  writeRaster(bio.2050.rcp45.median, filename = paste0('F:/Working/2018/PhD_research/enviromental_variables/2050/rcp45/median/',bio,' 2050 rcp45 median.tif'), format="GTiff", overwrite=T)
  print(bio)
}
#generate median bioclimatic variables
for(bio in c("bio2","bio10","bio11","bio12","bio18","bio19")){
  gcm <- c('ac', 'bc','ca','cm','cn','cs','gf','gi','ip','mi')
  bio.2050.rcp85 <- lapply(gcm, function(x){
    crop(raster(paste0('F:/Working/2018/PhD_research/enviromental_variables/2050/rcp85/',x,'/',bio,'.tif')),extent(11000000, 13000000, 900000, 2600000))*vn.raster
  })
  bio.2050.rcp85.median <- overlay(stack(bio.2050.rcp85), fun=median)
  writeRaster(bio.2050.rcp85.median, filename = paste0('F:/Working/2018/PhD_research/enviromental_variables/2050/rcp85/median/',bio,' 2050 rcp85 median.tif'), format="GTiff", overwrite=T)
  print(bio)
}

# # 2050
# #rcp45
land.cover.2050.rcp45 <- list.files("F:/Working/2018/PhD_research/enviromental_variables/2050/rcp45/land-cover", full.names = T, pattern = ".tif")
land.cover.2050.rcp45 <- stack(land.cover.2050.rcp45)
land.cover.2050.rcp45 <- subset(land.cover.2050.rcp45, c('forest','grassland','farmland','urban'))
land.cover.2050.rcp45 <- crop(land.cover.2050.rcp45, extent(11000000, 13000000, 900000, 2600000))*vn.raster
karst <- crop(raster('F:/Working/2018/PhD_research/enviromental_variables/current/karst.tif'),extent(11000000, 13000000, 900000, 2600000))*vn.raster
bio2 <-   raster('F:/Working/2018/PhD_research/enviromental_variables/2050/rcp45/median/bio2 2050 rcp45 median.tif')
bio10 <-   raster('F:/Working/2018/PhD_research/enviromental_variables/2050/rcp45/median/bio10 2050 rcp45 median.tif')
bio11 <-   raster('F:/Working/2018/PhD_research/enviromental_variables/2050/rcp45/median/bio11 2050 rcp45 median.tif')
bio12 <-   raster('F:/Working/2018/PhD_research/enviromental_variables/2050/rcp45/median/bio12 2050 rcp45 median.tif')
bio18 <-   raster('F:/Working/2018/PhD_research/enviromental_variables/2050/rcp45/median/bio18 2050 rcp45 median.tif')
bio19 <-   raster('F:/Working/2018/PhD_research/enviromental_variables/2050/rcp45/median/bio19 2050 rcp45 median.tif')
predictors.2050.rcp45 <- stack(bio2,bio10,bio11,bio12,bio18,bio19, karst, land.cover.2050.rcp45)
names(predictors.2050.rcp45) <- names(predictors)
proj.mess.rcp45<-suppressWarnings(mess(predictors.2050.rcp45, occEnvVal))
writeRaster(proj.mess.rcp45, filename = 'F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/mess_2050_rcp45.tif', format="GTiff", overwrite=T)

# #rcp85
land.cover.2050.rcp85 <- list.files("F:/Working/2018/PhD_research/enviromental_variables/2050/rcp85/land-cover", full.names = T, pattern = ".tif")
land.cover.2050.rcp85 <- stack(land.cover.2050.rcp85)
land.cover.2050.rcp85 <- subset(land.cover.2050.rcp85, c('forest','grassland','farmland','urban'))
land.cover.2050.rcp85 <- crop(land.cover.2050.rcp85, extent(11000000, 13000000, 900000, 2600000))*vn.raster
karst <- crop(raster('F:/Working/2018/PhD_research/enviromental_variables/current/karst.tif'),extent(11000000, 13000000, 900000, 2600000))*vn.raster
bio2 <-   raster('F:/Working/2018/PhD_research/enviromental_variables/2050/rcp85/median/bio2 2050 rcp85 median.tif')
bio10 <-   raster('F:/Working/2018/PhD_research/enviromental_variables/2050/rcp85/median/bio10 2050 rcp85 median.tif')
bio11 <-   raster('F:/Working/2018/PhD_research/enviromental_variables/2050/rcp85/median/bio11 2050 rcp85 median.tif')
bio12 <-   raster('F:/Working/2018/PhD_research/enviromental_variables/2050/rcp85/median/bio12 2050 rcp85 median.tif')
bio18 <-   raster('F:/Working/2018/PhD_research/enviromental_variables/2050/rcp85/median/bio18 2050 rcp85 median.tif')
bio19 <-   raster('F:/Working/2018/PhD_research/enviromental_variables/2050/rcp85/median/bio19 2050 rcp85 median.tif')
predictors.2050.rcp85 <- stack(bio2,bio10,bio11,bio12,bio18,bio19,karst,land.cover.2050.rcp85)
names(predictors.2050.rcp85) <- names(predictors)
proj.mess.rcp85<-suppressWarnings(mess(predictors.2050.rcp85, occEnvVal))
writeRaster(proj.mess.rcp85, filename = 'F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/mess_2050_rcp85.tif', format="GTiff", overwrite=T)

#plot MESS maps
mess.current <- raster('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/mess_current.tif')
mess.2050.rcp45 <- raster('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/mess_2050_rcp45.tif')
mess.2050.rcp85 <- raster('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/mess_2050_rcp85.tif')
SR.ORG8287 <- CRS('+proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs') 
vn <- spTransform(readOGR('F:/Working/2018/PhD_research/SDM output R/Map/vn.shp'), SR.ORG8287)
paracell.islands <- spTransform(readOGR('F:/Working/2018/PhD_research/SDM output R/Map/paracel islands.shp'), SR.ORG8287)
spratly.islands <- spTransform(readOGR('F:/Working/2018/PhD_research/SDM output R/Map/spratly islands.shp'), SR.ORG8287)
vn.raster <- raster('F:/Working/2018/PhD_research/SDM output R/Map/vn raster.tif')
cellStats(mess.2050.rcp45<0,stat='sum')/cellStats(vn.raster,stat = 'sum')*100
cellStats(mess.2050.rcp85<0,stat='sum')/cellStats(vn.raster,stat = 'sum')*100
cellStats(mess.current<0,stat='sum')/cellStats(vn.raster,stat = 'sum')*100
mess.2050.rcp85.extrapolation <- mess.2050.rcp85 < 0
plot.current<- tm_shape(mess.current, bbox = c(11380000, 930000,12450000,  2517140)) + 
  # tm_raster('layer',palette = c('black','grey','white'), style = 'cont',
  tm_raster('layer',breaks = c(-2,-0.01,0.01,2,Inf), palette = c('black','grey','orange','blue'),style = 'fixed',            title = 'MESS value')+
  tm_shape(paracell.islands) + tm_lines(col = 'grey')+
  tm_shape(spratly.islands) + tm_lines(col = 'grey')+
  tm_shape(vn)+ tm_polygons(border.col = 'dark gray', alpha = 0, lwd = 0.5)+
  tm_layout(legend.outside = F, title = '(a) Current',title.position = c('left','top'),title.size = 2,
            legend.text.size = 1.5, legend.title.size = 2,legend.position = c(0,0.4), frame = F, legend.show = T)

plot.2050.rcp45<- tm_shape(mess.2050.rcp45, bbox = c(11380000, 930000,12450000,  2517140)) + 
  # tm_raster('layer',palette = c('black','grey','white'), style = 'cont',
  tm_raster('layer',breaks = c(-2,-0.01,0.01,2,Inf), palette = c('black','grey','orange','blue'),style = 'fixed',
            title = '')+
  tm_shape(paracell.islands) + tm_lines(col = 'grey')+
  tm_shape(spratly.islands) + tm_lines(col = 'grey')+
  tm_shape(vn)+ tm_polygons(border.col = 'dark gray', alpha = 0, lwd = 0.5)+
  tm_layout(legend.outside = F, title = '(b) RCP4.5',title.position = c('left','top'),title.size = 2,
            legend.text.size = 1.5, legend.title.size = 2,legend.position = c(0,0.4), frame = F, legend.show = F)
plot.2050.rcp85<- tm_shape(mess.2050.rcp85, bbox = c(11380000, 930000,12450000,  2517140)) + 
  # tm_raster('layer',palette = c('black','grey','white'), style = 'cont',
    tm_raster('layer',breaks = c(-2,-0.01,0.01,2,Inf), palette = c('black','grey','orange','blue'),style = 'fixed',
            title = '')+
  tm_shape(paracell.islands) + tm_lines(col = 'grey')+
  tm_shape(spratly.islands) + tm_lines(col = 'grey')+
  tm_shape(vn)+ tm_polygons(border.col = 'dark gray', alpha = 0, lwd = 0.5)+
  tm_layout(legend.outside = F, title = '(c) RCP8.5',title.position = c('left','top'),title.size = 2,
            legend.text.size = 1.5, legend.title.size = 2,legend.position = c(0,0.4), frame = F, legend.show = F)
figure <- tmap_arrange(plot.current,plot.2050.rcp45,plot.2050.rcp85, nrow = 1)
tmap_save(figure, filename = 'F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/figures/Mess maps.tiff', height = 10, width = 15)

#predict current
list.raster.full <- list.files("F:/Working/2018/PhD_research/enviromental_variables/current", full.names = T, pattern = ".tif")
predictors <- stack(list.raster.full)
predictors <- subset(predictors, c("bio2","bio10","bio11","bio12","bio18","bio19","karst","forest","grassland","farmland","urban")) #select variables
species.list <- read_excel('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/list of species.xlsx')
species.list <- species.list%>%filter(Num_occurence_all > 19)
nrow(species.list)
for(sp in c(species.list$Scientific_name)){
  load(paste0('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/response/',sp,'.RData')) 
  maxent.result <- read.csv(paste0('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/response/',sp,'/maxentResults.csv'), header=T, sep=',')
  px <- predict(predictors, xm, ext= extent(11000000, 13000000, 900000, 2600000))
  species.current.bin <- px > maxent.result$X10.percentile.training.presence.Cloglog.threshold
  writeRaster(species.current.bin, filename = paste0('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/output_maps/',sp,' current.tif'),format='GTiff', overwrite=T)
  print(sp)
}


#compute spatial autocorrelation
#import occurrence data
library(ape)
list.raster.full <- list.files("F:/Working/2018/PhD_research/enviromental_variables/current", full.names = T, pattern = ".tif")
predictors <- stack(list.raster.full)
predictors <- subset(predictors, c("bio2","bio10","bio11","bio12","bio18","bio19","karst","forest","grassland","farmland","urban")) #select variables
latlong <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
SR.ORG8287 <- CRS('+proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs') 
vnbats.asia <- read_excel('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/final_occurences.xlsx') # import bat occurence data

species.list <- read_excel('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/list of species.xlsx')
species.list <- species.list%>%filter(Num_occurence_all > 19)
nrow(species.list)
sum.moran <- matrix(nrow = 81, ncol = 11)
colnames(sum.moran) <- c("bio2","bio10","bio11","bio12","bio18","bio19","karst","forest","grassland","farmland","urban")
row.names(sum.moran) <- species.list$Scientific_name
for(i in 1: nrow(species.list)) {
  sp <- species.list$Scientific_name[i]
  sp.presence <- vnbats.asia[vnbats.asia$species== sp,] 
  coordinates(sp.presence)= ~ decimalLongitude + decimalLatitude
  crs(sp.presence) <- latlong
  sp.presence <- spTransform(sp.presence, SR.ORG8287)
  sp.presence.env = raster::extract(predictors, sp.presence, df =T, cellnumber=T)
  sp.presence.env$checkna <- is.na(sp.presence.env$bio2)
  sp.presence.env.rmna <- sp.presence.env[!is.na(as.numeric(as.character(sp.presence.env$bio2))),]
  sp.presence$checkna <- sp.presence.env$checkna
  sp.presence <- sp.presence[sp.presence$checkna=='FALSE',]
  sp.presence.env.dists <- as.matrix(dist(cbind(sp.presence$decimalLongitude,sp.presence$decimalLatitude)))
  sp.presence.env.dists.inv <- 1/sp.presence.env.dists
  diag(sp.presence.env.dists.inv) <- 0
  sp.presence.env.dists.inv[is.infinite(sp.presence.env.dists.inv)] <- 0
  bio2.moranI <- Moran.I(sp.presence.env.rmna$bio2, sp.presence.env.dists.inv, na.rm = T,)
  bio10.moranI <- Moran.I(sp.presence.env.rmna$bio10, sp.presence.env.dists.inv, na.rm = T,)
  bio11.moranI <- Moran.I(sp.presence.env.rmna$bio11, sp.presence.env.dists.inv, na.rm = T,)
  bio12.moranI <- Moran.I(sp.presence.env.rmna$bio12, sp.presence.env.dists.inv, na.rm = T,)
  bio18.moranI <- Moran.I(sp.presence.env.rmna$bio18, sp.presence.env.dists.inv, na.rm = T,)
  bio19.moranI <- Moran.I(sp.presence.env.rmna$bio19, sp.presence.env.dists.inv, na.rm = T,)
  sum.moran[i,1] <- bio2.moranI$observed
  sum.moran[i,2] <- bio10.moranI$observed
  sum.moran[i,3] <- bio11.moranI$observed
  sum.moran[i,4] <- bio12.moranI$observed
  sum.moran[i,5] <- bio18.moranI$observed
  sum.moran[i,6] <- bio19.moranI$observed
  print(sp)
}
write.xlsx(sum.moran,'F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/vnbats_spatial_autocorrelation.xlsx')



#predict current in entire asia
list.raster.full <- list.files("F:/Working/2018/PhD_research/enviromental_variables/current", full.names = T, pattern = ".tif")
predictors <- stack(list.raster.full)
predictors <- subset(predictors, c("bio2","bio10","bio11","bio12","bio18","bio19","karst","forest","grassland","farmland","urban")) #select variables
species.list <- read_excel('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/list of species.xlsx')
species.list <- species.list%>%filter(Num_occurence_all > 19)
species.list <- species.list%>%filter(Scientific_name%in%c('Barbastella darjelingensis','Harpiola isodon','Hipposideros diadema','Kerivoula furva',
                                                           'Macroglossus minimus','Myotis formosus','Myotis rufoniger','Pipistrellus abramus',
                                                           'Pteropus hypomelanus','Rhinolophus borneensis'))
for(sp in c(species.list$Scientific_name[2:10])){
  load(paste0('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/response/',sp,'.RData')) 
  maxent.result <- read.csv(paste0('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/response/',sp,'/maxentResults.csv'), header=T, sep=',')
  px <- predict(predictors, xm)
  species.current.bin <- px > maxent.result$X10.percentile.training.presence.Cloglog.threshold
  writeRaster(species.current.bin, filename = paste0('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/output_maps/small_distribution_species/',sp,'_current_asia.tif'),format='GTiff', overwrite=T)
  print(sp)
}

