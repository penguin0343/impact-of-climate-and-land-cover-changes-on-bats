library(rgeos)
library(raster)
library(rgdal)
library(sp)
library(rgbif)
library(readxl)
library(ggplot2)
library(jsonlite)
require(sf)
library(devtools)
library(xlsx)
library(bold)
library(dplyr)
library(tmap)
library(maptools)
library(rredlist)

#obtain synonyms
vnbats.list <- read.csv('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/IUCN_6_2_2023/taxonomy.csv')
vnbats.list <- vnbats.list%>%select(scientificName,familyName,genusName)
vnbats.list <- vnbats.list %>% arrange(familyName, scientificName)
vnbats.list <- vnbats.list%>%mutate('Synonyms'=0)
vnbats_taxon_keys <- c(vnbats.list$scientificName)%>%
  name_backbone_checklist()  %>% # match to backbone
  filter(!matchType == "NONE") %>% # get matched names
  pull(usageKey) # get the gbif taxonkeys

for(i in 1:length(vnbats.list$scientificName)){
  sp <- vnbats.list$scientificName[i]
  names_gbif <- name_usage(key=vnbats_taxon_keys[i],data="synonyms")$data
  names_gbif <- names_gbif%>%filter(rank=='SPECIES')
  vnbats.list$Synonyms[i] <- paste(unique(names_gbif$canonicalName), collapse = ',')
  print(sp)
}
write.xlsx(vnbats.list, file = 'F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/vnbats_list.xlsx')

#download record from GBIF
user = 'penguin0343'
pwd = 'quangminh142'
email = 'penguin0343@gmail.com'
vnbats.list <- read.csv('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/IUCN_6_2_2023/taxonomy.csv')
vnbats.list <- vnbats.list%>%select(scientificName,familyName,genusName)
vnbats.list <- vnbats.list %>% arrange(familyName, scientificName)
vnbats_taxon_keys <- c(vnbats.list$scientificName)%>%
  name_backbone_checklist()  %>% # match to backbone
  filter(!matchType == "NONE") %>% # get matched names
  pull(usageKey) # get the gbif taxonkeys

occ_download(
  pred_in("taxonKey", vnbats_taxon_keys),
  pred_in("basisOfRecord", c('PRESERVED_SPECIMEN','HUMAN_OBSERVATION','OBSERVATION','MACHINE_OBSERVATION')),
  pred("hasCoordinate", TRUE),
  pred("hasGeospatialIssue", FALSE),
  pred_gte("year", 1990), # pred_lte("year", 2022),
  format = "SIMPLE_CSV",
  user=user,pwd=pwd,email=email
)
occ_download_wait('0007101-230530130749713')
vnbats.gbif <- occ_download_get('0007101-230530130749713', overwrite = T) %>%
  occ_download_import()
vnbats.gbif <- vnbats.gbif%>%filter(species%in%c(vnbats.list$scientificName))
dups <- duplicated(vnbats.gbif%>%select(species, decimalLongitude,decimalLatitude))
vnbats.gbif <- vnbats.gbif[!dups, ]
write.xlsx(vnbats.gbif, file = "F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/vnbats_gbif_6_3_23.xlsx")

###########
#Combining data
#data from GBIF
vnbats.gbif <- read_excel("F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/vnbats_gbif_6_3_23.xlsx") 
vnbats.gbif$decimalLongitude <- as.numeric(as.character(vnbats.gbif$decimalLongitude))
vnbats.gbif$decimalLatitude <- as.numeric(as.character(vnbats.gbif$decimalLatitude))
vnbats.gbif <- vnbats.gbif[ ! is.na( vnbats.gbif$decimalLongitude),]
vnbats.gbif <- vnbats.gbif[ ! is.na( vnbats.gbif$year),]
vnbats.gbif$species <- as.character(vnbats.gbif$species)
vnbats.gbif$sources <- 'GBIF'
vnbats.gbif$species.origional <- vnbats.gbif$species
vnbats.gbif <- vnbats.gbif[c('species.origional','species', 'decimalLongitude','decimalLatitude',"gbifID","elevation" ,
                                   "eventDate","year","locality","family","genus",'sources',"countryCode")]

bats.asia.nonpublish <- read_excel('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/vnbats_literature_coauthors.xlsx')
bats.asia.nonpublish <- bats.asia.nonpublish[4:16]

bats.synonyms <- read_excel('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/vnbats_synonyms.xlsx')
#combined data
names(vnbats.gbif) <- colnames(bats.asia.nonpublish)
names(bats.synonyms) <- colnames(bats.asia.nonpublish)
bats.asia <- rbind(bats.asia.nonpublish,vnbats.gbif, bats.synonyms)
bats.asia$decimalLongitude <- as.numeric(as.character(bats.asia$decimalLongitude))
bats.asia$decimalLatitude <- as.numeric(as.character(bats.asia$decimalLatitude))
bats.asia <- bats.asia[ ! is.na( bats.asia$decimalLatitude),]
bats.asia <- bats.asia[ ! is.na( bats.asia$year),]
bats.asia$`species-original` <- as.character(bats.asia$`species-original`)
nrow(bats.asia)

#select bat in Vietnam
vnbats.list <- read_excel('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/vnbats_list.xlsx')
vnbats.all <- bats.asia%>%filter(species%in%vnbats.list$scientificName)
vnbats.all <- vnbats.all[vnbats.all$year>1989,]
karst <- raster('F:/Working/2018/PhD_research/enviromental_variables/current/karst.tif')
latlong <- CRS("+proj=longlat +datum=WGS84")
SR.ORG8287 <- CRS('+proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs') 
coordinates(vnbats.all)= ~ decimalLongitude + decimalLatitude
crs(vnbats.all) <- latlong
vnbats.all <- spTransform(vnbats.all, SR.ORG8287)
cells.all <- cellFromXY(karst, vnbats.all)
vnbats.all@data$cell_number <- cells.all
vnbats.all.df <- as.data.frame(spTransform(vnbats.all, latlong))
dups <- duplicated(vnbats.all.df[, c('species','cell_number')])#check species within same cell
sum(dups)
vnbats.all.df <- vnbats.all.df[!dups, ]
nrow(vnbats.all.df)
write.xlsx(vnbats.all.df, file = "F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/final_occurences.xlsx")

#species name and number of record
vnbats.all <- read_excel('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/final_occurences.xlsx')
coordinates(vnbats.all)= ~ decimalLongitude + decimalLatitude
latlong <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
crs(vnbats.all) <- latlong
SR.ORG8287 <- CRS('+proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs') 
vnbats.all <- spTransform(vnbats.all, SR.ORG8287)
vnbats.list <- read_excel('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/vnbats_list.xlsx')
vnbats.list[,'Num_occurence_all'] <- 0
for(i in 1:length(vnbats.list$scientificName)){
  sp <- vnbats.list$scientificName[i]
  species <- subset(vnbats.all, vnbats.all$species==sp)
  cells <- cellFromXY(raster('F:/Working/2018/PhD_research/enviromental_variables/current/karst.tif'), species)
  dups <- duplicated(cells)
  species <- species[!dups,]
  number <- nrow(species)
  vnbats.list$Num_occurence_all[i] = number
}

nrow(vnbats.list%>%filter(Num_occurence_all>19))
vnbats.list <- vnbats.list%>%filter(Num_occurence_all > 0)
summary(vnbats.list%>%filter(Num_occurence_all > 0)%>%select(Num_occurence_all))
sd(vnbats.list$Num_occurence_all)

#check number of occurrence inside Vietnam
vn <- spTransform(getData('GADM', country = 'VN', level = 0), SR.ORG8287)
vnbats.vn <- raster::intersect(vnbats.all, vn)
# vnbats.vn <- as.data.frame(vnbats.vn)
vnbats.list[,'Num_occurence_insideVN'] <- 0
for(i in 1:length(vnbats.list$scientificName)){
  sp <- vnbats.list$scientificName[i]
  species <- subset(vnbats.vn, vnbats.vn$species==sp)
  cells <- cellFromXY(raster('F:/Working/2018/PhD_research/SDM output R/Map/vn raster.tif'), species)
  dups <- duplicated(cells)
  species <- species[!dups,]
  number <- nrow(species)
  vnbats.list$Num_occurence_insideVN[i] = number
}
iucn.cat <- read.csv('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/IUCN_6_2_2023/assessments.csv')
iucn.cat <- iucn.cat%>%select(scientificName, redlistCategory)
species.list.merge <- merge(vnbats.list[2:7], iucn.cat, by.x ='scientificName', by.y='scientificName', all.x = T)
species.list.merge <-species.list.merge%>%rename('Scientific_name' = scientificName)
species.list.merge <- species.list.merge%>%relocate(familyName, Scientific_name, redlistCategory, Num_occurence_all, Num_occurence_insideVN)
species.list.merge$modelling <- ifelse(species.list.merge$Num_occurence_all > 19,'Yes','No')
write.xlsx(species.list.merge, file = "F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/list of species.xlsx")
#count number of points
vnbats.all <- read_excel('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/final_occurences.xlsx')
coordinates(vnbats.all)= ~ decimalLongitude + decimalLatitude
latlong <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
crs(vnbats.all) <- latlong
SR.ORG8287 <- CRS('+proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs') 
vnbats.all <- spTransform(vnbats.all, SR.ORG8287)
asia.country <- readOGR('F:/Working/2018/PhD_research/SDM output R/Map/asia country.shp')
asia.raster = aggregate(raster('F:/Working/2018/PhD_research/enviromental_variables/current/bio1.tif'), fact=100)
counts = table(cellFromXY(asia.raster,vnbats.all))
count.point = asia.raster
count.point[] = 0
count.point[as.numeric(names(counts))] = counts
count.point[count.point ==0] <- NA

figure <- tm_shape(count.point) + 
  tm_raster('bio1',breaks = c(0,20,40,80,100,200,350) , palette = c('grey','orange','brown'),
                                            title = 'Number or records', labels = c('1~20','20~40','40~80','80~100','100~200','>200'))+
  tm_shape(asia.country)+tm_polygons(col = 'white', border.col = 'black',alpha = 0)+ 
  tm_layout(legend.position = c("left","bottom"),legend.frame = T, legend.bg.color = 'white', 
            legend.text.size = 0.5, legend.title.size = 0.7)
tmap_save(figure,filename = 'F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/figures/count point.tiff',width = 15,height = 8,units = 'cm',dpi = 300)


#check number of records for data sources
vnbats.all <- read_excel('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/final_occurences.xlsx')
nrow(vnbats.all%>%filter(source=='GBIF'))
nrow(vnbats.all%>%filter(source=='IEBR museum'))
nrow(vnbats.all%>%filter(source=='Hungarian museum'))
# nrow(vnbats.all%>%filter(source=='Bold'))
# nrow(vnbats.all%>%filter(source=='iNaturalist'))
nrow(vnbats.all%>%filter(source %in% c('Dr.Son','D.Tu','HT Thanh','Tuanmu SouthVN', 'Tuanmu NorthVN','Tuanmu')))
nrow(vnbats.all%>%filter(source %in% c('Dr.Son')))
nrow(vnbats.all%>%filter(source %in% c('Dr.Tu')))
nrow(vnbats.all%>%filter(source %in% c('HT Thanh')))
nrow(vnbats.all%>%filter(source %in% c('Tuanmu SouthVN', 'Tuanmu NorthVN','Tuanmu')))

nrow(vnbats.all%>%filter(!source%in%c('GBIF','IEBR museum','Dr.Son','D.Tu','HT Thanh',
                                       'Tuanmu SouthVN', 'Tuanmu NorthVN','Tuanmu','Hungarian museum')))
#check number of species in each data source
vnbats.all%>%select(species)%>%unique()%>%summarise(n())
vnbats.all%>%filter(source=='GBIF')%>%select(species)%>%unique()%>%summarise(n())
vnbats.all%>%filter(source=='IEBR museum')%>%select(species)%>%unique()%>%summarise(n())
vnbats.all%>%filter(source=='Hungarian museum')%>%select(species)%>%unique()%>%summarise(n())
# vnbats.all%>%filter(source=='Bold')%>%select(species)%>%unique()%>%summarise(n())
# vnbats.all%>%filter(source=='iNaturalist')%>%select(species)%>%unique()%>%summarise(n())
vnbats.all%>%filter(source%in% c('Dr.Son','D.Tu','HT Thanh','Tuanmu SouthVN', 'Tuanmu NorthVN','Tuanmu'))%>%select(species)%>%unique()%>%summarise(n())
vnbats.all%>%filter(!source%in%c('GBIF','IEBR museum','Tuanmu SouthVN', 'Tuanmu NorthVN','Tuanmu',
                                 'Dr.Son','D.Tu','HT Thanh','Hungarian museum'))%>%select(species)%>%unique()%>%summarise(n())

#count number of records within Vietnam
vnbats.all <- read_excel('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/final occurences.xlsx')
vnbats.all.df <- as.data.frame(vnbats.all)
coordinates(vnbats.all)= ~ decimalLongitude + decimalLatitude
crs(vnbats.all) <- latlong
vnbats.all <- spTransform(vnbats.all, SR.ORG8287)
vn <- spTransform(readOGR('F:/Working/2018/PhD_research/SDM output R/Map/vn.shp'), SR.ORG8287)
bat.insidevn <- raster::intersect(vnbats.all, vn)
bat.insidevn.df <- as.data.frame(bat.insidevn)
nrow(bat.insidevn)/nrow(vnbats.all.df)*100


#obtain habitat from IUCN
library(rredlist)
species.list <- read_excel('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/list of species.xlsx')
species.list <- species.list%>%filter(Num_occurence_all > 19)
nrow(species.list)

species.list <- species.list%>%mutate(habitat = 0)
for(i in 1: length(species.list$Scientific_name)) {
  sp <- species.list$Scientific_name[i]
  hb <- rl_habitats(sp, key = 'e0ed4100be4f97eadca0ff19e65fc82c2594c91d97cf1a3c6958d7a8be41acf1')
  species.list$habitat[i] <- paste(hb$result$habitat, collapse = ',')
  print(sp)
}

#Habitat use highest level
species.list <- species.list%>%mutate(Forest = grepl('Forest', habitat))
species.list <- species.list%>%mutate(Shrubland = grepl('Shrubland', habitat))
species.list <- species.list%>%mutate(Grassland = grepl('Grassland', habitat))
species.list <- species.list%>%mutate(Wetlands = grepl('Wetlands', habitat))
species.list <- species.list%>%mutate(Caves = grepl('Caves', habitat))
species.list <- species.list%>%mutate(Artificial = grepl('Artificial', habitat))

#optain dietary
species_traits <- read.csv('F:/Working/2018/PhD_research/Trait and phylogeny data/DataS1/Trait_data.csv')
species_traits <-species_traits%>%mutate(Scientific_name=paste0(Genus.1.2,' ',Species.1.2))
species_traits <- species_traits%>%select(Scientific_name,Diet.Plant,Diet.Vertebrate,Diet.Invertebrate)
species.list <- merge(species.list,species_traits,by='Scientific_name',all.x=T)
write.xlsx(species.list, file = "F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/vnbats_habitat_traits.xlsx")

# species.list <- species.list%>%filter(Num_occurence_all>19)
#proportion of modelled species among family
species.list <- read_excel('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/list of species.xlsx')
family.species <- species.list%>%group_by(familyName)%>%summarise(n=n())
family.species.modelled <- species.list%>%filter(Num_occurence_all > 19)%>%group_by(familyName)%>%summarise(n=n())
family.species.combined <- merge(family.species, family.species.modelled, by='familyName')
family.species.combined <- family.species.combined%>%mutate('percent' = n.y/n.x*100)
mean(family.species.combined$percent)
sd(family.species.combined$percent)


#check number of species in redlist category
vnbats.list <- read.csv('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/IUCN_6_2_2023/assessments.csv')
vnbats.list%>%group_by(redlistCategory)%>%summarise(n())

                                                   
#check elevation of species in different guilds
library(elevatr)
vnbats.all <- read_excel('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/final_occurences.xlsx')
ll_proj <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
elev <- elevatr::get_elev_point(vnbats.all%>%select(decimalLongitude,decimalLatitude), prj = ll_proj, src = 'aws')
vnbats.all$ele_aws <- elev$elevation
write.xlsx(vnbats.all,'F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/final_occurences_elevation.xlsx')

#compare elevation with ecological traits
vnbats.ele <- read_excel('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/final_occurences_elevation.xlsx')
# vnbats.traits <- read_excel('F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/vnbats_habitat_traits.xlsx')
species.list <- read.xlsx("F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/vnbats_habitat_traits.xlsx", sheetName = 'Sheet1')
species.list <- species.list%>%select(familyName,Scientific_name,Forest,Shrubland,Grassland,Wetlands,Caves,Artificial,Diet.Plant,Diet.Invertebrate)
species.forest.only <- species.list%>%filter(Artificial=='FALSE'&Shrubland=='FALSE'&Grassland=='FALSE')
species.forest.others <- species.list%>%filter(Artificial=='TRUE'|Shrubland=='TRUE'|Grassland=='TRUE')
species.forest.only.ele <- vnbats.ele%>%filter(species%in%species.forest.only$Scientific_name)
species.forest.others.ele <- vnbats.ele%>%filter(species%in%species.forest.others$Scientific_name)

ele.habitat <- c(species.forest.only.ele$ele_aws,species.forest.others.ele$ele_aws)
habitats <- c(rep("Forest specialist",nrow(species.forest.only.ele)),rep("Habitat generalist",nrow(species.forest.others.ele)))
sum.habitat <- data.frame(ele.habitat,habitats)

figure.habitat <- sum.habitat%>%ggplot(aes(x= factor(habitats, levels = c('Forest specialist', 'Habitat generalist')), y=ele.habitat))+
  stat_summary(geom = "boxplot", 
               fun.data = function(x) setNames(quantile(x, c(0.0, 0.25, 0.5, 0.75, 1)), c("ymin", "lower", "middle", "upper", "ymax")), 
               position = "dodge")+ylim(0,5000)+
  theme_classic()+
  labs(x='',y='Elevation (m)', title = '')+
  theme(text = element_text(size = 12), axis.text.x = element_text(size = 12), axis.title.y = element_text(size = 14))+
  stat_compare_means(aes(label = ..p.signif..), method = 'wilcox.test',label.x = 1.5, label.y =4000,size = 5)#add Wincolson test


species.cave <- species.list%>%filter(Caves=='TRUE')
species.nocave <- species.list%>%filter(Caves=='FALSE')
species.cave.ele <- vnbats.ele%>%filter(species%in%species.cave$Scientific_name)
species.nocave.ele <- vnbats.ele%>%filter(species%in%species.nocave$Scientific_name)

ele.cave <- c(species.cave.ele$ele_aws,species.nocave.ele$ele_aws)
cave <- c(rep("Cave roosting",nrow(species.cave.ele)),rep('None cave roosting',nrow(species.nocave.ele)))
sum.dietary <- data.frame(ele.cave,cave)

figure.cave <- sum.dietary%>%ggplot(aes(x= factor(cave, levels = c('Cave roosting', 'None cave roosting')), y=ele.habitat))+
  stat_summary(geom = "boxplot", 
               fun.data = function(x) setNames(quantile(x, c(0.0, 0.25, 0.5, 0.75, 1)), c("ymin", "lower", "middle", "upper", "ymax")), 
               position = "dodge")+ylim(0,5000)+
  theme_classic()+
  labs(x='',y='Elevation (m)', title = '')+
  theme(text = element_text(size = 12), axis.text.x = element_text(size = 12), axis.title.y = element_text(size = 14))+
  stat_compare_means(aes(label = ..p.signif..), method = 'wilcox.test',label.x = 1.5, label.y =4000,size = 5)#add Wincolson test

figure <- ggarrange(figure.habitat,figure.dietary,figure.cave, nrow = 1)
ggsave(figure, filename = 'F:/Working/2018/PhD_research/SDM output R/2_6_23_BC_revise/interactive_effects/figures/vnbats_traits_elevation.tiff',
       width = 28, height = 12, units = 'cm', dpi = 300)
