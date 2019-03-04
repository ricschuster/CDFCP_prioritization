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
  add_relative_targets(as.numeric(unlist(scen[ii,(scen_col+1):ncol(scen)])))

if(any(pu_temp$status ==2)){
  prob.ta <- prob.ta %>%
    add_locked_in_constraints(pu_temp$status ==2)
}

  
if(any(pu_temp$status ==3)){
  prob.ta <- prob.ta %>%
    add_locked_out_constraints(pu_temp$status ==3)
}

result <- solve(prob.ta)

