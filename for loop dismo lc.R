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

# options(java.parameters = "-Xmx8000m")
#latlong <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
#SR.ORG8287 <- CRS('+proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs') 
#extent.asia.sr <- extent(11000000, 13000000, 900000, 2600000)

# Current
list.raster.full <- list.files("F:/Working/2018/PhD_research/enviromental_variables/current", full.names = T, pattern = ".tif")
climate.current <- stack(list.raster.full)
climate.current <- subset(climate.current, c("bio2","bio10","bio11","bio12","bio18","bio19")) #select variables
karst <- raster('F:/Working/2018/PhD_research/enviromental_variables/current/karst.tif')
# 2050
#rcp45
land.cover.2050.rcp45 <- list.files("F:/Working/2018/PhD_research/enviromental_variables/2050/rcp45/land-cover", full.names = T, pattern = ".tif")
land.cover.2050.rcp45 <- stack(land.cover.2050.rcp45)

#rcp85
land.cover.2050.rcp85 <- list.files("F:/Working/2018/PhD_research/enviromental_variables/2050/rcp85/land-cover", full.names = T, pattern = ".tif")
land.cover.2050.rcp85 <- stack(land.cover.2050.rcp85)

#import species list
species.list <- read_excel('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/list of species.xlsx')
species.list <- species.list%>%filter(Num_occurence_all > 19)
nrow(species.list)

#predict
for(sp in species.list$Scientific_name){
  load(paste0('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/response/',sp,'.RData')) 
  maxent.result <- read.csv(paste0('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/response/',sp,'/maxentResults.csv'), header=T, sep=',')
  predictors.2050.rcp45 <- stack(climate.current, land.cover.2050.rcp45, karst)
  px.2050.rcp45.both <- predict(predictors.2050.rcp45, xm, ext=extent(11000000, 13000000, 900000, 2600000))
  species.2050.rcp45.both.bin <- px.2050.rcp45.both > maxent.result$X10.percentile.training.presence.Cloglog.threshold
  writeRaster(species.2050.rcp45.both.bin, filename = paste0('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/output_maps/',sp,' 2050 rcp45 lc.tif'),format='GTiff', overwrite=T)
  print(sp)
}

for(sp in species.list$Scientific_name){
  load(paste0('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/response/',sp,'.RData')) 
  maxent.result <- read.csv(paste0('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/response/',sp,'/maxentResults.csv'), header=T, sep=',')
  predictors.2050.rcp85 <- stack(climate.current, land.cover.2050.rcp85, karst)
  px.2050.rcp85.both <- predict(predictors.2050.rcp85, xm, ext=extent(11000000, 13000000, 900000, 2600000))
  species.2050.rcp85.both.bin <- px.2050.rcp85.both > maxent.result$X10.percentile.training.presence.Cloglog.threshold
  writeRaster(species.2050.rcp85.both.bin, filename = paste0('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/output_maps/',sp,' 2050 rcp85 lc.tif'),format='GTiff', overwrite=T)
  print(sp)
  }

