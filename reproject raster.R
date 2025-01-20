library(maptools)
library(rgeos)
library(raster)
library(sp)
library(rgdal)
# options(java.parameters = "-Xmx8000m")#setup memory size
memory.limit(size = 1000000)#setup memorry limit

latlong <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
SR.ORG8287 <- CRS('+proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs') 
#asia <- readOGR('E:/Working/2018/PhD thesis/SDM output R/8-6/Map/asia country.shp')
#asia <- spTransform(asia, SR.ORG8287)
extent.asia.latlon <- extent(c(25, 152, -10, 56))
#extent.asia.sr <- extent(c(2777972, 16925972, -1105514, 5273486))

dir.create(file.path('E:/Working/2018/PhD thesis/enviromental_variables/','2050'))
dir.create(file.path('E:/Working/2018/PhD thesis/enviromental_variables/2050','rcp45'))
dir.create(file.path('E:/Working/2018/PhD thesis/enviromental_variables/2050','rcp85'))
for(gcm in c('ac', 'bc','ca','cm','cn','cs','gf','gi','ip','mi')){
  dir.create(file.path('E:/Working/2018/PhD thesis/enviromental_variables/2050/rcp45/',gcm))
}
for(gcm in c('ac', 'bc','ca','cm','cn','cs','gf','gi','ip','mi')){
  dir.create(file.path('E:/Working/2018/PhD thesis/enviromental_variables/2050/rcp85/',gcm))
}
#Reproject current climate
climatic.current <- list.files("F:/GIS data/Chelsa/current", full.names = T, pattern = ".tif")
climatic.current <- stack(climatic.current)
crs(climatic.current) <- latlong
climatic.current <- crop(climatic.current, extent.asia.latlon)
climatic.current <- projectRaster(climatic.current, crs = SR.ORG8287, res = 1000, method = 'bilinear')
for(i in 1:9){
  writeRaster(subset(climatic.current,paste0('CHELSA_bio10_0',i)), filename = paste0('E:/Working/2018/PhD thesis/enviromental_variables/current/bio',i,'.tif'), format="GTiff", overwrite=T)
}
for(i in 13:19){
  writeRaster(subset(climatic.current,paste0('CHELSA_bio10_',i)), filename = paste0('E:/Working/2018/PhD thesis/enviromental_variables/current/bio',i,'.tif'), format="GTiff", overwrite=T)
}

#reproject land-cover
lc.modis <- raster('E:/Working/2018/PhD thesis/enviromental_variables/land-cover/MODISLandcover_2010_reclass_1km.tif')
bio1 <- raster('E:/Working/2018/PhD thesis/enviromental_variables/current/bio1.tif')
lc.modis.reproj <- projectRaster(lc.modis, bio1, res = 1000, method = 'ngb')
writeRaster(lc.modis.reproj, filename = "E:/Working/2018/PhD thesis/enviromental_variables/land-cover/MODISLandcover_2010_srorg8287.tif", format="GTiff", overwrite=T)
lc.modis.reproj <-raster('E:/Working/2018/PhD thesis/enviromental_variables/land-cover/MODISLandcover_2010_srorg8287.tif')
# lc.modis.reproj.crop <- crop(lc.modis.reproj, bio1)
forest <- lc.modis.reproj == 2 # select forest layer
grassland <- lc.modis.reproj ==3
farmland <- lc.modis.reproj ==4
urban <- lc.modis.reproj ==5
writeRaster(forest, filename = "E:/Working/2018/PhD thesis/enviromental_variables/current/forest.tif", format="GTiff", overwrite=T)
writeRaster(grassland, filename = "E:/Working/2018/PhD thesis/enviromental_variables/current/grassland.tif", format="GTiff", overwrite=T)
writeRaster(farmland, filename = "E:/Working/2018/PhD thesis/enviromental_variables/current/farmland.tif", format="GTiff", overwrite=T)
writeRaster(urban, filename = "E:/Working/2018/PhD thesis/enviromental_variables/current/urban.tif", format="GTiff", overwrite=T)

#land-cover B1/RCP45
lc.modis.rcp45 <- raster('E:/GIS data/GlobCover/Global simulation/world_B1_2050.tif')
lc.modis.rcp45.re <- projectRaster(lc.modis.rcp45, bio1, res = 1000, method = 'ngb')
writeRaster(lc.modis.rcp45.re, filename = "E:/Working/2018/PhD thesis/enviromental_variables/land-cover/MODISLandcover_2050_rcp45_srorg8287.tif", format="GTiff", overwrite=T)
lc.modis.rcp45.re <- raster('E:/Working/2018/PhD thesis/enviromental_variables/land-cover/MODISLandcover_2050_rcp45_srorg8287.tif')
lc.modis.rcp45.crop <- crop(lc.modis.rcp45.re, bio1)
forest.rcp45 <- lc.modis.rcp45.crop == 2
grassland.rcp45 <- lc.modis.rcp45.crop ==3
farmland.rcp45 <- lc.modis.rcp45.crop ==4
urban.rcp45 <- lc.modis.rcp45.crop ==5
writeRaster(forest.rcp45, filename = "E:/Working/2018/PhD thesis/enviromental_variables/2050/rcp45/land-cover/forest.tif", format="GTiff", overwrite=T)
writeRaster(grassland.rcp45, filename = "E:/Working/2018/PhD thesis/enviromental_variables/2050/rcp45/land-cover/grassland.tif", format="GTiff", overwrite=T)
writeRaster(farmland.rcp45, filename = "E:/Working/2018/PhD thesis/enviromental_variables/2050/rcp45/land-cover/farmland.tif", format="GTiff", overwrite=T)
writeRaster(urban.rcp45, filename = "E:/Working/2018/PhD thesis/enviromental_variables/2050/rcp45/land-cover/urban.tif", format="GTiff", overwrite=T)

#land-cover A2/RCP85
lc.modis.rcp85 <- raster('E:/GIS data/GlobCover/Global simulation/world_A2_2050.tif')
lc.modis.rcp85.re <- projectRaster(lc.modis.rcp85, bio1, res = 1000, method = 'ngb')
writeRaster(lc.modis.rcp85.re, filename = "E:/Working/2018/PhD thesis/enviromental_variables/land-cover/MODISLandcover_2050_rcp85_srorg8287.tif", format="GTiff", overwrite=T)
lc.modis.rcp85.re <- raster('E:/Working/2018/PhD thesis/enviromental_variables/land-cover/MODISLandcover_2050_rcp85_srorg8287.tif')
lc.modis.rcp85.crop <- crop(lc.modis.rcp85.re, bio1)
forest.rcp85 <- lc.modis.rcp85.crop == 2
grassland.rcp85 <- lc.modis.rcp85.crop ==3
farmland.rcp85 <- lc.modis.rcp85.crop ==4
urban.rcp85 <- lc.modis.rcp85.crop ==5
writeRaster(forest.rcp85, filename = "E:/Working/2018/PhD thesis/enviromental_variables/2050/rcp85/land-cover/forest.tif", format="GTiff", overwrite=T)
writeRaster(grassland.rcp85, filename = "E:/Working/2018/PhD thesis/enviromental_variables/2050/rcp85/land-cover/grassland.tif", format="GTiff", overwrite=T)
writeRaster(farmland.rcp85, filename = "E:/Working/2018/PhD thesis/enviromental_variables/2050/rcp85/land-cover/farmland.tif", format="GTiff", overwrite=T)
writeRaster(urban.rcp85, filename = "E:/Working/2018/PhD thesis/enviromental_variables/2050/rcp85/land-cover/urban.tif", format="GTiff", overwrite=T)

#reproject karst
karst <- readOGR('F:/GIS data/enviromental/karst/karst_wgs.shp')
karst <- spTransform(karst, SR.ORG8287)
karst <- crop(karst, bio1)
karst.raster <-rasterize(karst, bio1,field = 1, background = 0)
writeRaster(karst.raster, filename = "E:/Working/2018/PhD thesis/enviromental_variables/current/karst.tif", format="GTiff", overwrite=T)

#reproject future climate
#rcp45
for(i in 1:19){
  climate <- raster(paste0('F:/GIS data/Chelsa/2050/rcp45/ac/CHELSA_bio_mon_ACCESS1-0_rcp45_r1i1p1_g025.nc_',i,'_2041-2060_V1.2.tif'))
  crs(climate) <- latlong
  climate <- crop(climate, extent.asia.latlon)
  climate <- projectRaster(climate, crs = SR.ORG8287, res = 1000, method = 'bilinear')
  writeRaster(climate, filename = paste0('E:/Working/2018/PhD thesis/enviromental_variables/2050/rcp45/ac/bio',i,'.tif'), format="GTiff", overwrite=T)
}

for(i in 4:19){
  climate <- raster(paste0('F:/GIS data/Chelsa/2050/rcp45/bc/CHELSA_bio_mon_bcc-csm1-1_rcp45_r1i1p1_g025.nc_',i,'_2041-2060_V1.2.tif'))
  crs(climate) <- latlong
  climate <- crop(climate, extent.asia.latlon)
  climate <- projectRaster(climate, crs = SR.ORG8287, res = 1000, method = 'bilinear')
  writeRaster(climate, filename = paste0('E:/Working/2018/PhD thesis/enviromental_variables/2050/rcp45/bc/bio',i,'.tif'), format="GTiff", overwrite=T)
}

for(i in 1:19){
  climate <- raster(paste0('F:/GIS data/Chelsa/2050/rcp45/ca/CHELSA_bio_mon_CanESM2_rcp45_r1i1p1_g025.nc_',i,'_2041-2060_V1.2.tif'))
  crs(climate) <- latlong
  climate <- crop(climate, extent.asia.latlon)
  climate <- projectRaster(climate, crs = SR.ORG8287, res = 1000, method = 'bilinear')
  writeRaster(climate, filename = paste0('E:/Working/2018/PhD thesis/enviromental_variables/2050/rcp45/ca/bio',i,'.tif'), format="GTiff", overwrite=T)
}

for(i in 1:19){
  climate <- raster(paste0('F:/GIS data/Chelsa/2050/rcp45/cm/CHELSA_bio_mon_CMCC-CM_rcp45_r1i1p1_g025.nc_',i,'_2041-2060_V1.2.tif'))
  crs(climate) <- latlong
  climate <- crop(climate, extent.asia.latlon)
  climate <- projectRaster(climate, crs = SR.ORG8287, res = 1000, method = 'bilinear')
  writeRaster(climate, filename = paste0('E:/Working/2018/PhD thesis/enviromental_variables/2050/rcp45/cm/bio',i,'.tif'), format="GTiff", overwrite=T)
}

for(i in 1:19){
  climate <- raster(paste0('F:/GIS data/Chelsa/2050/rcp45/cn/CHELSA_bio_mon_CNRM-CM5_rcp45_r1i1p1_g025.nc_',i,'_2041-2060_V1.2.tif'))
  crs(climate) <- latlong
  climate <- crop(climate, extent.asia.latlon)
  climate <- projectRaster(climate, crs = SR.ORG8287, res = 1000, method = 'bilinear')
  writeRaster(climate, filename = paste0('E:/Working/2018/PhD thesis/enviromental_variables/2050/rcp45/cn/bio',i,'.tif'), format="GTiff", overwrite=T)
}

for(i in 1:19){
  climate <- raster(paste0('F:/GIS data/Chelsa/2050/rcp45/cs/CHELSA_bio_mon_CSIRO-Mk3-6-0_rcp45_r1i1p1_g025.nc_',i,'_2041-2060_V1.2.tif'))
  crs(climate) <- latlong
  climate <- crop(climate, extent.asia.latlon)
  climate <- projectRaster(climate, crs = SR.ORG8287, res = 1000, method = 'bilinear')
  writeRaster(climate, filename = paste0('E:/Working/2018/PhD thesis/enviromental_variables/2050/rcp45/cs/bio',i,'.tif'), format="GTiff", overwrite=T)
}

for(i in 1:19){
  climate <- raster(paste0('F:/GIS data/Chelsa/2050/rcp45/gf/CHELSA_bio_mon_GFDL-CM3_rcp45_r1i1p1_g025.nc_',i,'_2041-2060_V1.2.tif'))
  crs(climate) <- latlong
  climate <- crop(climate, extent.asia.latlon)
  climate <- projectRaster(climate, crs = SR.ORG8287, res = 1000, method = 'bilinear')
  writeRaster(climate, filename = paste0('E:/Working/2018/PhD thesis/enviromental_variables/2050/rcp45/gf/bio',i,'.tif'), format="GTiff", overwrite=T)
}

for(i in 1:19){
  climate <- raster(paste0('F:/GIS data/Chelsa/2050/rcp45/gi/CHELSA_bio_mon_GISS-E2-R-CC_rcp45_r1i1p1_g025.nc_',i,'_2041-2060_V1.2.tif'))
  crs(climate) <- latlong
  climate <- crop(climate, extent.asia.latlon)
  climate <- projectRaster(climate, crs = SR.ORG8287, res = 1000, method = 'bilinear')
  writeRaster(climate, filename = paste0('E:/Working/2018/PhD thesis/enviromental_variables/2050/rcp45/gi/bio',i,'.tif'), format="GTiff", overwrite=T)
}

for(i in 1:19){
  climate <- raster(paste0('F:/GIS data/Chelsa/2050/rcp45/ip/CHELSA_bio_mon_IPSL-CM5A-LR_rcp45_r1i1p1_g025.nc_',i,'_2041-2060_V1.2.tif'))
  crs(climate) <- latlong
  climate <- crop(climate, extent.asia.latlon)
  climate <- projectRaster(climate, crs = SR.ORG8287, res = 1000, method = 'bilinear')
  writeRaster(climate, filename = paste0('E:/Working/2018/PhD thesis/enviromental_variables/2050/rcp45/ip/bio',i,'.tif'), format="GTiff", overwrite=T)
}

for(i in 1:19){
  climate <- raster(paste0('F:/GIS data/Chelsa/2050/rcp45/mi/CHELSA_bio_mon_MIROC-ESM_rcp45_r1i1p1_g025.nc_',i,'_2041-2060_V1.2.tif'))
  crs(climate) <- latlong
  climate <- crop(climate, extent.asia.latlon)
  climate <- projectRaster(climate, crs = SR.ORG8287, res = 1000, method = 'bilinear')
  writeRaster(climate, filename = paste0('E:/Working/2018/PhD thesis/enviromental_variables/2050/rcp45/mi/bio',i,'.tif'), format="GTiff", overwrite=T)
}


#rcp85
for(i in 1:19){
  climate <- raster(paste0('E:/GIS data/Chelsa/2050/rcp85/access1-0/CHELSA_bio_mon_ACCESS1-0_rcp85_r1i1p1_g025.nc_',i,'_2041-2060_V1.2.tif'))
  crs(climate) <- latlong
  climate <- crop(climate, extent.asia.latlon)
  climate <- projectRaster(climate, crs = SR.ORG8287, res = 1000, method = 'bilinear')
  writeRaster(climate, filename = paste0('E:/Working/2018/PhD thesis/enviromental_variables/2050/rcp85/bc/bio',i,'.tif'), format="GTiff", overwrite=T)
}

for(i in 1:19){
  climate <- raster(paste0('E:/GIS data/Chelsa/2050/rcp85/bcc-csm1-1/CHELSA_bio_mon_bcc-csm1-1_rcp85_r1i1p1_g025.nc_',i,'_2041-2060_V1.2.tif'))
  crs(climate) <- latlong
  climate <- crop(climate, extent.asia.latlon)
  climate <- projectRaster(climate, crs = SR.ORG8287, res = 1000, method = 'bilinear')
  writeRaster(climate, filename = paste0('E:/Working/2018/PhD thesis/enviromental_variables/2050/rcp85/bc/bio',i,'.tif'), format="GTiff", overwrite=T)
}

for(i in 1:19){
  climate <- raster(paste0('E:/GIS data/Chelsa/2050/rcp85/canesm2/CHELSA_bio_mon_CanESM2_rcp85_r1i1p1_g025.nc_',i,'_2041-2060_V1.2.tif'))
  crs(climate) <- latlong
  climate <- crop(climate, extent.asia.latlon)
  climate <- projectRaster(climate, crs = SR.ORG8287, res = 1000, method = 'bilinear')
  writeRaster(climate, filename = paste0('E:/Working/2018/PhD thesis/enviromental_variables/2050/rcp85/ca/bio',i,'.tif'), format="GTiff", overwrite=T)
}

for(i in 1:19){
  climate <- raster(paste0('E:/GIS data/Chelsa/2050/rcp85/cmcc-cm/CHELSA_bio_mon_CMCC-CM_rcp85_r1i1p1_g025.nc_',i,'_2041-2060_V1.2.tif'))
  crs(climate) <- latlong
  climate <- crop(climate, extent.asia.latlon)
  climate <- projectRaster(climate, crs = SR.ORG8287, res = 1000, method = 'bilinear')
  writeRaster(climate, filename = paste0('E:/Working/2018/PhD thesis/enviromental_variables/2050/rcp85/cm/bio',i,'.tif'), format="GTiff", overwrite=T)
}

for(i in 1:19){
  climate <- raster(paste0('E:/GIS data/Chelsa/2050/rcp85/cnrm-cm5/CHELSA_bio_mon_CNRM-CM5_rcp85_r1i1p1_g025.nc_',i,'_2041-2060_V1.2.tif'))
  crs(climate) <- latlong
  climate <- crop(climate, extent.asia.latlon)
  climate <- projectRaster(climate, crs = SR.ORG8287, res = 1000, method = 'bilinear')
  writeRaster(climate, filename = paste0('E:/Working/2018/PhD thesis/enviromental_variables/2050/rcp85/cn/bio',i,'.tif'), format="GTiff", overwrite=T)
}

for(i in 1:19){
  climate <- raster(paste0('E:/GIS data/Chelsa/2050/rcp85/csiro-mk3-6-0/CHELSA_bio_mon_CSIRO-Mk3-6-0_rcp85_r1i1p1_g025.nc_',i,'_2041-2060_V1.2.tif'))
  crs(climate) <- latlong
  climate <- crop(climate, extent.asia.latlon)
  climate <- projectRaster(climate, crs = SR.ORG8287, res = 1000, method = 'bilinear')
  writeRaster(climate, filename = paste0('E:/Working/2018/PhD thesis/enviromental_variables/2050/rcp85/cs/bio',i,'.tif'), format="GTiff", overwrite=T)
}

for(i in 1:19){
  climate <- raster(paste0('E:/GIS data/Chelsa/2050/rcp85/gfdl-cm3/CHELSA_bio_mon_GFDL-CM3_rcp85_r1i1p1_g025.nc_',i,'_2041-2060_V1.2.tif'))
  crs(climate) <- latlong
  climate <- crop(climate, extent.asia.latlon)
  climate <- projectRaster(climate, crs = SR.ORG8287, res = 1000, method = 'bilinear')
  writeRaster(climate, filename = paste0('E:/Working/2018/PhD thesis/enviromental_variables/2050/rcp85/gf/bio',i,'.tif'), format="GTiff", overwrite=T)
}

for(i in 1:19){
  climate <- raster(paste0('E:/GIS data/Chelsa/2050/rcp85/giss-e2-r/CHELSA_bio_mon_GISS-E2-R-CC_rcp85_r1i1p1_g025.nc_',i,'_2041-2060_V1.2.tif'))
  crs(climate) <- latlong
  climate <- crop(climate, extent.asia.latlon)
  climate <- projectRaster(climate, crs = SR.ORG8287, res = 1000, method = 'bilinear')
  writeRaster(climate, filename = paste0('E:/Working/2018/PhD thesis/enviromental_variables/2050/rcp85/gi/bio',i,'.tif'), format="GTiff", overwrite=T)
}

for(i in 1:19){
  climate <- raster(paste0('E:/GIS data/Chelsa/2050/rcp85/ipsl-cm5a-lr/CHELSA_bio_mon_IPSL-CM5A-LR_rcp85_r1i1p1_g025.nc_',i,'_2041-2060_V1.2.tif'))
  crs(climate) <- latlong
  climate <- crop(climate, extent.asia.latlon)
  climate <- projectRaster(climate, crs = SR.ORG8287, res = 1000, method = 'bilinear')
  writeRaster(climate, filename = paste0('E:/Working/2018/PhD thesis/enviromental_variables/2050/rcp85/ip/bio',i,'.tif'), format="GTiff", overwrite=T)
}

for(i in 1:19){
  climate <- raster(paste0('E:/GIS data/Chelsa/2050/rcp85/miroc-esm/CHELSA_bio_mon_MIROC-ESM_rcp85_r1i1p1_g025.nc_',i,'_2041-2060_V1.2.tif'))
  crs(climate) <- latlong
  climate <- crop(climate, extent.asia.latlon)
  climate <- projectRaster(climate, crs = SR.ORG8287, res = 1000, method = 'bilinear')
  writeRaster(climate, filename = paste0('E:/Working/2018/PhD thesis/enviromental_variables/2050/rcp85/mi/bio',i,'.tif'), format="GTiff", overwrite=T)
}


#reproject population data
population.2020 <- raster('D:/GIS data/Global_population/SSP2_1km/ssp2_total_2020.tif')
population.2020 <- crop(population.2020, extent.asia.latlon)
population.2020.reproject <- projectRaster(population.2020, crs = SR.ORG8287, res=1000, method = 'bilinear')
writeRaster(population.2020.reproject, filename = 'E:/Working/2018/PhD thesis/enviromental_variables/current/population_size.tif')


population.2050.ssp2 <- raster('D:/GIS data/Global_population/SSP2_1km/ssp2_total_2050.tif')
population.2050.ssp2 <- crop(population.2050.ssp2, extent.asia.latlon)
population.2050.ssp2 <- projectRaster(population.2050.ssp2, crs = SR.ORG8287, res=1000, method = 'bilinear')
writeRaster(population.2050.ssp2, filename = 'E:/Working/2018/PhD thesis/enviromental_variables/2050/rcp45/population_size.tif')
