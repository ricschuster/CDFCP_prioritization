library(raster)
library(rgdal)
library(rgeos)
library(foreign)
library(plyr)

setwd("D:/VirtualBox/shared/UBC_VS/Shiny2/CDFCP.v0.20/pulayer_tmp/")

first=FALSE

if(first){
  bpd <- read.dbf("Bird_plant_dollar.dbf")
  ces <- read.dbf("Carb_EO_SEI.dbf")
  mast <- read.dbf("CDFCP_1ha_master.dbf")

  ces1 <- ces[,c("ID","StCAWS","SeqCAWS",
                 "SEI_CBV1","SEI_HTV1","SEI_OFV1","SEI_RIV1","SEI_SGV0","SEI_SVV1","SEI_WDV1","SEI_WNV1",
                 "EOV0")]


  mast1 <- join(mast,ces1,by="ID")
  mast2 <- mast1[,c("ID",
                    "OF_Score",
                    "SAV_score",
                    "BETA_score",
                    "WET_Score",
                    "SHR_score",
                    "HUM_score",
                    "HUM_1",
                    "StCAWS",
                    "SeqCAWS",
                    "EOV0",
                    "SEI_CBV1",
                    "SEI_HTV1",
                    "SEI_OFV1",
                    "SEI_RIV1",
                    "SEI_SGV0",
                    "SEI_SVV1",
                    "SEI_WDV1",
                    "SEI_WNV1",
                    "Pl_NatRich",
                    "DolHa_AWM",
                    "DolHa_AWS")]
  names(mast2) <- c("ID_orig",
                    "OF",
                    "SAV",
                    "BETA",
                    "WET",
                    "SHR",
                    "HUM",
                    "negHUM",
                    "StC",
                    "SeqC",
                    "EO",
                    "SEI_CB",
                    "SEI_HT",
                    "SEI_OF",
                    "SEI_RI",
                    "SEI_SG",
                    "SEI_SV",
                    "SEI_WD",
                    "SEI_WN",
                    "NatPlR",
                    "DollAWM",
                    "DollAWS")

  mast2$id <- seq(1:length(mast2[,1]))
  write.dbf(mast2,"CDFCP_1ha_master.dbf")
  write.csv(mast2,"../data/CDFCP_1ha_master.csv",row.names=F)

  #R too slow, do this in GIS
  #shp <- readOGR(".","CDFCP_1ha_Hex_habitat")
  #r <- raster(extent(shp))
  #projection(r) <- proj4string(shp)
  #res(r) <- 100
  #rp <- rasterize(shp, r, 'id')

}

setwd("../pulayer")

r.idx <- raster("idx.tif")

r <- r.idx
rv <- getValues(r)
ind.r.v <- data.frame(id=rv[!is.na(rv)])

res <- join(ind.r.v,mast2[,-1],by="id")
nms <- names(res)
setwd("rast")

for(ii in 2:ncol(res)){
  rv[!is.na(rv)] <- res[,ii]
  r[] <- rv
  writeRaster(r, filename= nms[ii], format="GTiff", overwrite=TRUE)
}














