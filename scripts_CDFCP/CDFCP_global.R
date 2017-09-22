library(shiny)
library(shinyIncubator)
library(ggplot2)
require(sp)
require(rgdal)
require(maptools)
require(PBSmapping)
require(foreign)
require(sqldf)
require(vegan)
require(labdsv)
library(leaflet)

join.sp.df <- function(x, y, xcol, ycol) {
  x$sort_id <- 1:nrow(as(x, "data.frame"))  
  x.dat <- as(x, "data.frame")  
  x.dat2 <- merge(x.dat, y, by.x = xcol, by.y = ycol)  
  x.dat2.ord <- x.dat2[order(x.dat2$sort_id), ]  
  x2 <- x[x$sort_id %in% x.dat2$sort_id, ]  
  x2.dat <- as(x2, "data.frame") 
  row.names(x.dat2.ord) <- row.names(x2.dat)  
  x2@data <- x.dat2.ord  
  return(x2)
}

# Gfold = Global folder name
globWD <- setwd("./p")
Gfold <- sprintf("%s",round(runif(1)*1000000))

#dir.create(sprintf("%s/%s",getwd(),Gfold))
for (ii in 1:100000){
  if(!file.exists(sprintf("/mnt/shiny/%s",Gfold))) {
    system(paste("mkdir ",sprintf("/mnt/shiny/%s",Gfold)))
    break()
  }
}
system(paste("cp -r ", "./files/." , sprintf("/mnt/shiny/%s",Gfold)))
setwd(sprintf("/mnt/shiny/%s",Gfold))

#sPulayer <- paste(getwd(),"/pulayer/pulayer1km.shp",sep="")
#pulayer <- readShapePoly(sPulayer)
#pulayer <<- SpatialPolygons2PolySet(pulayer)
#pu_table <<- read.dbf(paste(getwd(),"/pulayer/pulayer1km.dbf",sep=""))    

#CDFCP <- readOGR(dsn="./pulayer","Cadaster_RS_IT_non_RS_min_atr")

#setwd("./files")
