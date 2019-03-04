library(shiny)
#library(shinyjs)
library(shinydashboard)
library(shinyIncubator)
library(ggplot2)
library(sp)
library(rgdal)
library(maptools)
library(PBSmapping)
library(foreign)
library(sqldf)
library(vegan)
library(labdsv)
library(raster)
library(leaflet)
library(rhandsontable)
library(Matrix)
library(plyr)
library(dplyr)
library(tidyr)
#library(gurobi)
library(DT)
library(Rsymphony)


### Source R scripts
source("./scripts/helper.functions.R")
source("./scripts/fit.gurobi.R")
source("./scripts/marxan.R")

### Load Workspaces
load("./RData/feature_input.RData")
load("./RData/leaflet1.RData")
#load("./RData/tree_input.RData")
#load("./RData/trees_curr.RData")
#load("./RData/trees_rcp45.RData")
#load("./RData/trees_rcp85.RData")

### Load rasters
in.raster <- raster(paste0(getwd(),"/pulayer/idx.tif"))
in.rast.val <- getValues(in.raster)
idx.r <- raster(paste0(getwd(),"/pulayer/idx.tif"))
idx.r.val <- getValues(idx.r)

### Load rasters for Leaflet map
projA <- "+proj=merc +a=6378137 +b=6356752 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs"
#projA <-"+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6378137 +b=6378137 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
#projA <-"+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6378137 +b=6380137 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

#Left Right Bottom Top
ext <- extent(-13909.22, -13697.22, 6155975, 6449775)
in.rasterL <- raster(paste0(getwd(),"/pulayer/idxL.tif"))
in.rast.valL <- getValues(in.rasterL)
idx.rL <- raster(paste0(getwd(),"/pulayer/idxL.tif"))
idx.r.valL <- getValues(idx.rL)
crs(in.rasterL) <- crs(idx.rL) <- projA


raster.on <- TRUE

nms <- c("Old Forest Birds",
         "Savannah Birds",
         "Beta Diversity",
         "Wetland Birds",
         "Shrub Birds",
         "Human Commensal Birds",
         "Inverse of Hum Com Birds",
         "Standing Carbon",
         "Carbon Sequestration Potential",
         "TEM Element Occurrence",
         "SEI Coastal Bluff",
         "SEI Herbaceous",
         "SEI Older Forest",
         "SEI Riparian",
         "SEI Second Growth Forest",
         "SEI Sparsely Vegetated",
         "SEI Woodland",
         "SEI Wetland",
         "Native Plant Species Richness",
         "Fish",
         "Herptiles",
         "California Buttercup",
         "Contorted-pod Evening Primrose",
         "Dense Flowered Lupine",
         "Dense Spike-primrose",
         "Foothill Sedge",
         "Oregan Forestsnail",
         "Maoun's Meadowfoam'",
         "While Meconella",
         "Coast Microseris",
         "Marbeld Murrulet",
         "Fragrant Popcorn",
         "Sand-verbena Moth",
         "Area")
#Setup "Edit Target" table
feat.lst <- data.frame(id=seq(1,ncol(puvsf[[1]][,-1])),
                       Percent=0,
                       name=nms,
                       #name=names(puvsf[[1]][,-1]),
                       stringsAsFactors =F)
l.feat.lst <- nrow(feat.lst)

#setup "Edit Trees" table
#tree.raw <- read.csv("./pulayer/Tongli_Species.csv")
#tree.raw[is.na(tree.raw)] <- 0
#tree.lst <- tree.raw[,c(3:4,7)]
#names(tree.lst)[3] <- "include"
#tree.lst[,3] <- as.logical(tree.lst[,3])

#setup "Edit Scenarios" table
scen <- data.frame(scenario="template",
                   #time="curr",
                   cost="dollar",
                   protected="locked",
                   maxRoadDns=0,
                   minPropSz=0,
                   maxAgrDns=0,
                   #                   FTcutoff=0,
                   stringsAsFactors =F)
scen_col <- ncol(scen)
for(kk in (scen_col+1):(scen_col+nrow(feat.lst)))
  scen[,kk] <- 0	
names(scen)[(scen_col+1):ncol(scen)] <- feat.lst$name


# Gfold = Global folder name
#globWD <- setwd("./p")

#load leaflet1 seem to set a seed, so set another one here
#set.seed(as.integer(Sys.time()))
#Gfold <- sprintf("%s",round(runif(1)*1000000))

#dir.create(sprintf("%s/%s",getwd(),Gfold))
#for (ii in 1:100000){
#  if(!file.exists(sprintf("/mnt/shiny/%s",Gfold))) {
#    system(paste("mkdir ",sprintf("/mnt/shiny/%s",Gfold)))
#    break()
#  } else {
#    Gfold <- sprintf("%s",round(runif(1)*1000000))
#  }
#}

#system(paste("cp -r ", "./files/." , sprintf("/mnt/shiny/%s",Gfold)))
#setwd(sprintf("/mnt/shiny/%s",Gfold))
#setwd("./files")




####
#Test for df
####
pu_temp <- cost[[scen$cost[1]]]

pu <- data.frame(pu_temp, status=0)
#pu <- pu[!is.na(bound.val),]

rij.id <- data.frame(pu=pu$id)
rij <- data.frame(pu=vector(),species=vector(),amount=vector)

spp <- 1
spp.nm <- nms
features.df <- puvsf[[1]][-1]

for (ii in 1:ncol(features.df)){
  
  print(nms[ii])
  flush.console()
  
  rij.tmp <- data.frame(pu=rij.id$pu,species=spp,amount=features.df[,ii])
  
  spp <- spp + 1
  
  rij.tmp <- drop_na(rij.tmp)
  rij <- rbind(rij,rij.tmp)
  rm(rij.tmp)
  
  gc()
}

rij <- rij[with(rij, order(pu, species)), ]

#so that prioritzr is tricked into thinking pu and rij have the same length in id
#need to fix that error in prioritzr
#rij <- rbind(rij,c(max(pu$id),min(rij$species),0))

feat <- data.frame(id=seq(1,spp-1),name=spp.nm)

#save.image(paste0(owd,"/problem_setup.RData"))

#tt <- problem(pu, features, rij) %>%
# add_min_set_objective() %>%
# add_binary_decisions()

#ss <- solve(tt %>% add_relative_targets(0.17))
pu.tmp <- pu
gc()

save.image("pre_global.RData")

