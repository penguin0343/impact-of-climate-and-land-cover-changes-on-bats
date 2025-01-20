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
options(java.parameters = "-Xmx8000m")
#latlong <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
#SR.ORG8287 <- CRS('+proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs') 
#extent.asia.sr <- extent(11000000, 13000000, 900000, 2600000)

# Current
list.raster.full <- list.files("F:/Working/2018/PhD_research/enviromental_variables/current", full.names = T, pattern = ".tif")
predictors <- stack(list.raster.full)
predictors <- subset(predictors, c("bio2","bio10","bio11","bio12","bio18","bio19","karst","forest","grassland","farmland","urban")) #select variables

# 2050
#rcp85
land.cover.2050.rcp85 <- list.files("F:/Working/2018/PhD_research/enviromental_variables/2050/rcp85/land-cover", full.names = T, pattern = ".tif")
land.cover.2050.rcp85 <- stack(land.cover.2050.rcp85)
karst <- raster('F:/Working/2018/PhD_research/enviromental_variables/current/karst.tif')

#import species list
species.list <- read_excel('F:/Working/2018/PhD_research/SDM output R/17_5_23_BC/list of species.xlsx')
species.list <- species.list%>%filter(Num_occurence_all > 19)
nrow(species.list)

#predict
for(sp in species.list$Scientific_name){
  load(paste0('F:/Working/2018/PhD_research/SDM output R/17_5_23_BC/response/',sp,'.RData')) 
  maxent.result <- read.csv(paste0('F:/Working/2018/PhD_research/SDM output R/17_5_23_BC/response/',sp,'/maxentResults.csv'), header=T, sep=',')
  for(gcm in c('ac', 'bc','ca','cm','cn','cs','gf','gi','ip','mi')){
    climate.2050.rcp85 <- list.files(paste0("F:/Working/2018/PhD_research/enviromental_variables/2050/rcp85/",gcm), full.names = T, pattern = ".tif")
    climate.2050.rcp85 <- stack(climate.2050.rcp85)
    climate.2050.rcp85 <- subset(climate.2050.rcp85, c("bio2","bio10","bio11","bio12","bio18","bio19"))
    predictors.2050.rcp85 <- stack(climate.2050.rcp85, land.cover.2050.rcp85, karst)
    px.2050.rcp85.both <- predict(predictors.2050.rcp85, xm, ext=extent(11000000, 13000000, 900000, 2600000))
    species.2050.rcp85.both.bin <- px.2050.rcp85.both > maxent.result$X10.percentile.training.presence.Cloglog.threshold
    writeRaster(species.2050.rcp85.both.bin, filename = paste0('F:/Working/2018/PhD_research/SDM output R/17_5_23_BC/output_maps/',sp,' 2050 rcp85 both ',gcm,'.tif'),format='GTiff', overwrite=T)
    print(gcm)
  }
  print(sp)
}

