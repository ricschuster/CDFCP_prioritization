library(shiny)
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
library(gurobi)
#library(Rsymphony)

setwd("D:/VirtualBox/shared/UBC_VS/Shiny2/CDFCP.v0.20")
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
in.raster <- raster(paste0(getwd(),"./pulayer/idx2.tif"))
in.rast.val <- getValues(in.raster)
idx.r <- raster(paste0(getwd(),"./pulayer/idx2.tif"))
idx.r.val <- getValues(idx.r)

### Load rasters for Leaflet map
projA <- "+proj=merc +a=6378137 +b=6356752 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs"
#projA <-"+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6378137 +b=6378137 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
#projA <-"+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6378137 +b=6380137 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

#Left Right Bottom Top
ext <- extent(-13909.22, -13697.22, 6155975, 6449775)
in.rasterL <- raster(paste0(getwd(),"./pulayer/idxL3.tif"))
in.rast.valL <- getValues(in.rasterL)
idx.rL <- raster(paste0(getwd(),"./pulayer/idxL3.tif"))
idx.r.valL <- getValues(idx.rL)
crs(in.rasterL) <- crs(idx.rL) <- projA


raster.on <- TRUE

#Setup "Edit Target" table
feat.lst <- data.frame(id=seq(1,ncol(puvsf[[1]][,-1])),
                       Percent=0,
                       name=names(puvsf[[1]][,-1]),
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


#y <- runif(length(sel.fr.rast[,1]))
#x <- ifelse(y>0.9,1,0)

      sel.fr <- data.frame(id=cost[["dollar"]]$id)
      sel.fr.rast <- data.frame(id=idx.r.val[!is.na(idx.r.val)])
x<- rep(0,length(sel.fr[,1]))
x[1:200] <- 1

      #sel.fr.rast <- data.frame(id=idx.r.val[!is.na(idx.r.val)])
        tmp.fr <- data.frame(cad_id=sel.fr[,1],xx=x)

        sel.cad <- tmp.fr$cad_id[tmp.fr$xx == 1]
        sel.1ha <- unique(cad_1ha_isect$id[cad_1ha_isect$cad_id %in% sel.cad])
#        tmp.x <- rep(0,length(sel.fr.rast[,1]))
#        tmp.x[sel.fr.rast$id %in% sel.1ha] <- 1
        #tt <- data.frame(id=sel.fr.rast$id,tmp.x=rep(0,length(sel.fr.rast[,1])))

#need to change this to make work with several rasters
        sel.fr.rast$tmp.x <- 0
        sel.fr.rast$tmp.x[sel.fr.rast$id %in% sel.1ha] <- 1
        #sel.fr.rast <- join(sel.fr.rast,tt,by="id")
        #rm(tt)

        r <- in.raster
        rv <- getValues(r)
        ind.r.v <- data.frame(id=idx.r.val[!is.na(idx.r.val)])

        res <- join(ind.r.v,sel.fr.rast,by="id")

        rout <- list()
        for(ii in 2:ncol(res)){

          rv[!is.na(rv)] <- res[,ii]

          r[] <- rv
          rout[[ii-1]] <- r
        }
        rst <- stack(rout)
        names(rst) <- names(res)[-1]


    rst <- rst

    pal <- colorFactor(c('#d7191c','#2c7bb6'),domain=factor(values(rst[[1]])),
                        #pal <- colorNumeric(c("#FFFFCC", "#41B6C4", "#0C2C84"), values(r),
                        na.color = "transparent")

    outl <- leaflet() %>% addTiles() %>%
      # Base groups
      addProviderTiles("Esri.WorldStreetMap",group = "StreetMap") %>%
      addProviderTiles("Esri.WorldImagery", group = "Aerial") %>%
      addProviderTiles("Thunderforest.Landscape", group = "Terrain")# %>%


      # Overlay groups
      for(ii in 1:length(rst@layers))
        outl <- addRasterImage(outl,rst[[ii]], colors=pal, opacity = 0.9,
                               maxBytes = 8 * 1024 * 1024, group = names(rst)[ii], project = FALSE)

        outl <- addLegend(outl, pal = pal, values = factor(values(rst[[1]])),labels=c("no","yes"),
                              title = "selected") %>%
        addLayersControl(
          baseGroups = c("StreetMap", "Aerial", "Terrain"),
          overlayGroups = names(rst),
          options = layersControlOptions(collapsed = FALSE)
          )

    outl
