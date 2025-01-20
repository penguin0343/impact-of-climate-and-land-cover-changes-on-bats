options(java.parameters = "-Xmx16g")
library(maptools)
library(raster)
library(dismo)
library(sp)
library(rJava)
library(readxl)
library(mapview)
library(ggplot2)
library(xlsx)
library(readxl)
library(ENMeval)
library(gridExtra)
library(dplyr)
library(ggpubr)
library(blockCV)


#Identify block size
latlong <- CRS("+proj=longlat +datum=WGS84")
SR.ORG8287 <- CRS('+proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs') 
#import environmental variables
# Current
list.raster.full <- list.files("F:/Working/2018/PhD_research/enviromental_variables/current", full.names = T, pattern = ".tif")
predictors <- stack(list.raster.full)
predictors <- subset(predictors, c("bio2","bio10","bio11","bio12","bio18",'bio19')) #select the important variables

#import occurrence data
vnbats.asia <- read_excel('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/final_occurences.xlsx', sheet = "Sheet1") # import bat occurence data
coordinates(vnbats.asia)= ~ decimalLongitude + decimalLatitude
crs(vnbats.asia) <- latlong
vnbats.asia <- spTransform(vnbats.asia, SR.ORG8287)

vnbats.asia.env <- raster::extract(predictors, vnbats.asia, df =T, na.rm=T, cellnumber =T)
vnbats.asia.env_2 <- vnbats.asia.env[!is.na(as.character(vnbats.asia.env$bio2)),]
vnbats.asia.2 <- vnbats.asia[vnbats.asia$cell_number%in%vnbats.asia.env_2$cells,]#remove records without environmental information
vnbats_range_2 <- cv_spatial_autocor(predictors, num_sample = 100000, plot = T, progress = T)


#validate model performance using spatial partition
latlong <- CRS("+proj=longlat +datum=WGS84")
SR.ORG8287 <- CRS('+proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs') 
#import environmental variables
# Current
list.raster.full <- list.files("F:/Working/2018/PhD_research/enviromental_variables/current", full.names = T, pattern = ".tif")
predictors <- stack(list.raster.full)
predictors <- subset(predictors, c("bio2","bio10","bio11","bio12","bio18",'bio19',"karst","forest","grassland","farmland","urban")) #select the important variables

#import occurrence data
vnbats.asia <- read_excel('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/final_occurences.xlsx', sheet = "Sheet1") # import bat occurence data
coordinates(vnbats.asia)= ~ decimalLongitude + decimalLatitude
crs(vnbats.asia) <- latlong
vnbats.asia <- spTransform(vnbats.asia, SR.ORG8287)

#import species list and select species have >= 20 records
species.list <- read_excel('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/list of species.xlsx')
species.list <- species.list%>%filter(Num_occurence_all > 19)
nrow(species.list)

model.perfome <- data.frame(species.list$Scientific_name)
colnames(model.perfome) <- 'species'
model.perfome <- model.perfome%>%mutate(auc.m0.5=0,auc.m1=0,auc.m1.5=0,auc.m2=0,auc.m2.5=0,auc.m3=0,auc.m3.5=0,auc.m4=0,auc.m4.5=0,auc.m5=0)
vnbats.asia.env <- raster::extract(predictors, vnbats.asia, df =T, na.rm=T, cellnumber =T)
vnbats.asia.env_2 <- vnbats.asia.env[!is.na(as.character(vnbats.asia.env$bio2)),]
vnbats.asia.env_2 <- vnbats.asia.env_2[!is.na(as.character(vnbats.asia.env_2$karst)),]
vnbats.asia.env_2 <- vnbats.asia.env_2[!is.na(as.character(vnbats.asia.env_2$forest)),]
vnbats.asia.2 <- vnbats.asia[vnbats.asia$cell_number%in%vnbats.asia.env_2$cells,]#remove records without enviromental information

for(i in 1:length(species.list$Scientific_name)){
  sp <- species.list$Scientific_name[i]
  vnbats.asia.2$sp_validate <- ifelse(vnbats.asia.2$species==sp,1,0)
  sp.block <- cv_spatial(x = vnbats.asia.2,
                         column = 'sp_validate',
                        r = predictors,
                        size = 50000,
                        k = 5,
                        selection = "random",
                        iteration = 100)
    vnbats.asia.2$foldID <- sp.block$folds_ids
    sp.presence <- vnbats.asia.2[vnbats.asia.2$species==sp,]
    sp.absence <- vnbats.asia.2[vnbats.asia.2$species!=sp,]
    user.grp <- list(occs.grp = sp.presence$foldID, 
                     bg.grp = sp.absence$foldID)
    enviromental.presence <- raster::extract(predictors,sp.presence)
    enviromental.absence <- raster::extract(predictors,sp.absence)
    res <- ENMevaluate(occ = enviromental.presence, RMvalues = seq(0.5,5,0.5),tune.args = list(fc = c('LQH')),
                     partitions = 'user',user.grp = user.grp, bg.coords = enviromental.absence, 
                     parallel = T, numCores = 8, algorithm = "maxent.jar", clamp = F)
    model.perfome$auc.m0.5[i] <- res@results[res@results$rm==0.5,]$auc.val.avg
    model.perfome$auc.m1[i] <- res@results[res@results$rm==1,]$auc.val.avg
    model.perfome$auc.m1.5[i] <- res@results[res@results$rm==1.5,]$auc.val.avg
    model.perfome$auc.m2[i] <- res@results[res@results$rm==2,]$auc.val.avg
    model.perfome$auc.m2.5[i] <- res@results[res@results$rm==2.5,]$auc.val.avg
    model.perfome$auc.m3[i] <- res@results[res@results$rm==3,]$auc.val.avg
    model.perfome$auc.m3.5[i] <- res@results[res@results$rm==3.5,]$auc.val.avg
    model.perfome$auc.m4[i] <- res@results[res@results$rm==4,]$auc.val.avg
    model.perfome$auc.m4.5[i] <- res@results[res@results$rm==4.5,]$auc.val.avg
    model.perfome$auc.m5[i] <- res@results[res@results$rm==5,]$auc.val.avg
    print(sp)
}

write.xlsx(model.perfome, file = "F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/model performance_block_LQH.xlsx")

#model performance with LQ
model.perfome <- data.frame(species.list$Scientific_name)
colnames(model.perfome) <- 'species'
model.perfome <- model.perfome%>%mutate(auc.m0.5=0,auc.m1=0,auc.m1.5=0,auc.m2=0,auc.m2.5=0,auc.m3=0,auc.m3.5=0,auc.m4=0,auc.m4.5=0,auc.m5=0)

for(i in 1:length(species.list$Scientific_name)){
  sp <- species.list$Scientific_name[i]
  vnbats.asia.2$sp_validate <- ifelse(vnbats.asia.2$species==sp,1,0)
  sp.block <- cv_spatial(x = vnbats.asia.2,
                         column = 'sp_validate',
                         r = predictors,
                         size = 50000,
                         k = 5,
                         selection = "random",
                         iteration = 100)
  vnbats.asia.2$foldID <- sp.block$folds_ids
  sp.presence <- vnbats.asia.2[vnbats.asia.2$species==sp,]
  sp.absence <- vnbats.asia.2[vnbats.asia.2$species!=sp,]
  user.grp <- list(occs.grp = sp.presence$foldID, 
                   bg.grp = sp.absence$foldID)
  enviromental.presence <- raster::extract(predictors,sp.presence)
  enviromental.absence <- raster::extract(predictors,sp.absence)
  res <- ENMevaluate(occ = enviromental.presence, RMvalues = seq(0.5,5,0.5),tune.args = list(fc = c('LQ')),
                     partitions = 'user',user.grp = user.grp, bg.coords = enviromental.absence, 
                     parallel = T, numCores = 8, algorithm = "maxent.jar", clamp = F)
  model.perfome$auc.m0.5[i] <- res@results[res@results$rm==0.5,]$auc.val.avg
  model.perfome$auc.m1[i] <- res@results[res@results$rm==1,]$auc.val.avg
  model.perfome$auc.m1.5[i] <- res@results[res@results$rm==1.5,]$auc.val.avg
  model.perfome$auc.m2[i] <- res@results[res@results$rm==2,]$auc.val.avg
  model.perfome$auc.m2.5[i] <- res@results[res@results$rm==2.5,]$auc.val.avg
  model.perfome$auc.m3[i] <- res@results[res@results$rm==3,]$auc.val.avg
  model.perfome$auc.m3.5[i] <- res@results[res@results$rm==3.5,]$auc.val.avg
  model.perfome$auc.m4[i] <- res@results[res@results$rm==4,]$auc.val.avg
  model.perfome$auc.m4.5[i] <- res@results[res@results$rm==4.5,]$auc.val.avg
  model.perfome$auc.m5[i] <- res@results[res@results$rm==5,]$auc.val.avg
  print(sp)
}

write.xlsx(model.perfome, file = "F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/model performance_block_LQ.xlsx")

#plot model performance with different features types and regularization values
sum.LQ <- read_xlsx("F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/model performance_block_LQ.xlsx")
sum.LQH <- read_xlsx("F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/model performance_block_LQH.xlsx")
k <- length(sum.LQH$species)
auc <- c(sum.LQ$auc.m0.5,
         sum.LQ$auc.m1,
         sum.LQ$auc.m1.5,
         sum.LQ$auc.m2,
         sum.LQ$auc.m2.5,
         sum.LQ$auc.m3,
         sum.LQ$auc.m3.5,
         sum.LQ$auc.m4,
         sum.LQ$auc.m4.5,
         sum.LQ$auc.m5,
         sum.LQH$auc.m0.5,
         sum.LQH$auc.m1,
         sum.LQH$auc.m1.5,
         sum.LQH$auc.m2,
         sum.LQH$auc.m2.5,
         sum.LQH$auc.m3,
         sum.LQH$auc.m3.5,
         sum.LQH$auc.m4,
         sum.LQH$auc.m4.5,
         sum.LQH$auc.m5)
parameters <- c(rep('LQ_0.5',k),
                rep('LQ_1',k),
                rep('LQ_1.5',k),
                rep('LQ_2',k),
                rep('LQ_2.5',k),
                rep('LQ_3',k),
                rep('LQ_3.5',k),
                rep('LQ_4',k),
                rep('LQ_4.5',k),
                rep('LQ_5',k),
                rep('LQH_0.5',k),
                rep('LQH_1',k),
                rep('LQH_1.5',k),
                rep('LQH_2',k),
                rep('LQH_2.5',k),
                rep('LQH_3',k),
                rep('LQH_3.5',k),
                rep('LQH_4',k),
                rep('LQH_4.5',k),
                rep('LQH_5',k))
sum.perfome <- data.frame(parameters,auc)
auc.plot <- ggplot(sum.perfome, aes(x= reorder(parameters, auc, median), y=auc)) + 
  stat_summary(geom = "boxplot", 
               fun.data = function(x) setNames(quantile(x, c(0.0, 0.25, 0.5, 0.75, 1)), c("ymin", "lower", "middle", "upper", "ymax")), 
               position = "dodge")+
  theme_classic()+
  coord_flip()+ 
  xlab('Feature types and Regularization values')+ylab("AUC")+
  theme(text = element_text(size = 14), axis.text = element_text(size = 14), plot.title = element_text(hjust = 0))+
  geom_hline(yintercept=median(sum.LQH$auc.m1.5), linetype="dashed", color = "red")
ggsave(auc.plot, filename = 'F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/figures/AUC_regularization values.tiff',
       height = 20, width = 12, dpi = 300, units = 'cm')

#histogram AUC
figure.auc <- ggplot(sum.LQH, aes(x=auc.m1.5)) +
  geom_histogram(col='black', binwidth = 0.04)+
  labs(title="",x="AUC", y = "Number of species")+ theme_classic()+
  theme(axis.text=element_text(size=14),axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))+
  geom_vline(xintercept = 0.7, color = 'red')

ggsave(figure.auc, filename = 'F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/figures/AUC_histogram.tiff', width = 10, height = 8, units = 'cm')

nrow(sum.LQH%>%filter(auc.m1.5 >= 0.7))


