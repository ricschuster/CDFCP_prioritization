library(shiny)
#library(shinyjs)
library(shinydashboard)
library(shinyIncubator)
library(ggplot2)
library(sp)
library(rgdal)
library(maptools)
#library(PBSmapping)
library(foreign)
#library(sqldf)
library(vegan)
#library(labdsv)
library(raster)
library(leaflet)
library(rhandsontable)
library(Matrix)
library(plyr)
library(dplyr)
library(tidyr)
#library(gurobi)
#library(DT)
if(!require(gurobi)){
  require(Rsymphony)
}
library(prioritizr)

load("pre_global.RData")


#setup output frames
sel.fr <- data.frame(id=cost[["dollar"]]$id)
sel.fr.rast <- data.frame(id=idx.r.val[!is.na(idx.r.val)])
res.fr <- tibble(scen=character(),
                 #time=character(),
                 cost=character(),
                 protected=character(),
                 maxRoadDns=numeric(),
                 minPropSz=numeric(),
                 maxAgrDns=numeric(),
                 # FTcutoff=integer(),
                 status=character(),
                 runtime=numeric(),
                 cost_out=numeric(),
                 cost_dollar=numeric(),
                 area=numeric())

in_col <- ncol(res.fr)
for(kk in (in_col+1):(in_col+ncol(puvsf[["curr"]])-1))
  res.fr[,kk] <- numeric()	
names(res.fr)[(in_col+1):ncol(res.fr)] <- names(puvsf[["curr"]])[-1]      

in_col <- ncol(res.fr)
for(kk in (in_col+1):(in_col+ncol(puvsf[["curr"]])-1))
  res.fr[,kk] <- numeric()
names(res.fr)[(in_col+1):ncol(res.fr)] <- paste0(names(puvsf[["curr"]])[-1],"_Tar")


ii <- 1


pu_temp <- pu

if(scen$protected[ii] == "locked"){
  pu_temp$status <- prot$Prot
  pu_temp$cost[pu_temp$status ==2] <- 0 
} else {
  pu_temp$status <- 0
}

if(scen$maxAgrDns[ii] >0 || scen$maxRoadDns[ii] > 0 || scen$minPropSz[ii] > 0){
  pu_AreaHa_RoadD_AgrD$excl <- 0
  
  if(scen$minPropSz[ii] >0) pu_AreaHa_RoadD_AgrD$excl[pu_AreaHa_RoadD_AgrD$AreaHa < scen$minPropSz[ii]] <- 3
  if(scen$maxRoadDns[ii] >0) pu_AreaHa_RoadD_AgrD$excl[pu_AreaHa_RoadD_AgrD$RoadD > scen$maxRoadDns[ii]] <- 3
  if(scen$maxAgrDns[ii] >0) pu_AreaHa_RoadD_AgrD$excl[pu_AreaHa_RoadD_AgrD$AgrD > scen$maxAgrDns[ii]] <- 3
  pu_temp$status <- ifelse(pu_temp$status == 2, 2,pu_AreaHa_RoadD_AgrD$excl )
}


prob.ta <- problem(pu.tmp, feat, rij, cost_column = "cost") %>%
  add_min_set_objective() %>%
  add_binary_decisions() %>% 
  add_relative_targets(as.numeric(unlist(scen[ii,(scen_col+1):ncol(scen)]))/100)

if(any(pu_temp$status ==2)){
  prob.ta <- prob.ta %>%
    add_locked_in_constraints(pu_temp$status ==2)
}


if(any(pu_temp$status ==3)){
  prob.ta <- prob.ta %>%
    add_locked_out_constraints(pu_temp$status ==3)
}

 result <- prioritizr::solve(prob.ta, force = TRUE)

  scnm <- paste0(substr(as.character(scen$cost[ii]),1,1),
                 substr(as.character(scen$protected[ii]),1,1))
  #                       as.numeric(scen$FTcutoff[ii]))

sel.fr <- cbind(sel.fr,result$solution_1)

#sel.fr.rast
tmp.fr <- data.frame(cad_id=sel.fr[,1],xx=result$solution_1)
sel.cad <- tmp.fr$cad_id[tmp.fr$xx == 1]
sel.1ha <- unique(cad_1ha_isect$id[cad_1ha_isect$cad_id %in% sel.cad])
tmp.x <- rep(0,length(sel.fr.rast[,1]))
tmp.x[sel.fr.rast$id %in% sel.1ha] <- 1
sel.fr.rast <- cbind(sel.fr.rast,tmp.x)
rm(tmp.x)

names(sel.fr)[ncol(sel.fr)] <- names(sel.fr.rast)[ncol(sel.fr.rast)] <- scnm

cst.tmp <- cost$dollar$cost
if(scen$protected[ii] == "locked"){
  cst.tmp[pu_temp$status ==2] <- 0
}
cst_doll <- round(sum(cst.tmp[result$x>0]),0)

## CALC HERE round(sum(cost$dollar$cost[result$x>0]),0)
res.fr[ii,] <- c(scnm, 
                 #scen$time[ii],
                 scen$cost[ii],
                 scen$protected[ii],
                 scen$maxRoadDns[ii],
                 scen$minPropSz[ii],
                 scen$maxAgrDns[ii],
                 #                         scen$FTcutoff[ii],
                 attributes(result)$status[[1]],
                 round(attributes(result)$runtime[[1]],0), 
                 round(attributes(result)$objective[[1]],0),
                 cst_doll,
                 round(sum(result$solution_1>0)/length(result$solution_1)*100,2),
                 round(colSums(features.df[result$solution_1>0,])/
                         colSums(features.df)*100,2),
                 as.numeric(unlist(scen[ii,(scen_col+1):ncol(scen)]))
) 
rm(cst.tmp,cst_doll)

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

#leaflet rasters
rL <- in.rasterL
rvL <- getValues(rL)
ind.r.vL <- data.frame(id=idx.r.valL[!is.na(idx.r.valL)])

resL <- join(ind.r.vL,sel.fr.rast,by="id")

routL <- list()
for(ii in 2:ncol(resL)){
  
  rvL[!is.na(rvL)] <- resL[,ii]
  
  rL[] <- rvL
  routL[[ii-1]] <- rL
}
rstL <- stack(routL)
names(rstL) <- names(resL)[-1]

rlist <- list(sel.fr=sel.fr,res.fr=res.fr,rst=rst,rstL=rstL)

