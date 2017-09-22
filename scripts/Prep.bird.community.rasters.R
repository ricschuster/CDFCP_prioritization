list.dirs <- function(path=".", pattern=NULL, all.dirs=FALSE,
  full.names=FALSE, ignore.case=FALSE) {
  # use full.names=TRUE to pass to file.info
  all <- list.files(path, pattern, all.dirs,
           full.names=TRUE, recursive=FALSE, ignore.case)
  dirs <- all[file.info(all)$isdir]
  # determine whether to return full names or just dir names
  if(isTRUE(full.names))
    return(dirs)
  else
    return(basename(dirs))
}

setwd("D:/Work/NPLCC/occu/Community_scores")

library(sp)
library(raster)
library(spatial.tools)

destwd <- "D:/Virtual_Box/shared/UBC_VS/NPLCC/NPLCC.v0.12/pulayer/raster_ebird_hab_trees"

owd <- setwd("./raster_hab_trees")

#file.remove("info/")
dirs <- list.dirs()
dirs <- dirs[dirs != "info"]

for (ii in 1:length(dirs)){
  r <- raster(dirs[ii])
  writeRaster(r, filename=sprintf("%s/%s_curr.tif",destwd,dirs[ii]), format="GTiff", overwrite=TRUE)
}


setwd("../raster_hab_trees_rcp45")
#file.remove("info/")
dirs <- list.dirs()
dirs <- dirs[dirs != "info"]

for (ii in 1:length(dirs)){
  r <- raster(dirs[ii])
  writeRaster(r, filename=sprintf("%s/%s_rcp45.tif",destwd,dirs[ii]), format="GTiff", overwrite=TRUE)
}

