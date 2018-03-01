library(tidyr)

setwd("data")

feat <- read.csv("CDFCP_Marxan_Feature_input.csv")
tt <- spread(feat,species,amount)
tt[is.na(tt)] <- 0
spec <- read.csv("CDFCP_spec_all.csv",stringsAsFactors=F)
names(tt)[-1] <- spec$name[1:20]

write.csv(tt,"dodo.csv",row.names=F)
 

pu_AreaHa_RoadD_AgrD <- read.csv("CDFCP_AreaHa_RoadD_AgrD.csv")


cost <- list(area = read.csv("CDFCP_cost_area.csv"),
			 dollar = read.csv("CDFCP_cost_dollar.csv"),
			 human = read.csv("CDFCP_cost_human.csv"))

puvsf <- list(curr = read.csv("NPLCC_feat_input_100_curr.csv"),
			  rcp45 = read.csv("NPLCC_feat_input_100_rcp45.csv"),
			  rcp85 = NULL)
prot <- read.csv("CDFCP_prot_status.csv")

cad_1ha_isect <- read.csv("Cad_1ha_isect.csv")
save.image("../RData/feature_input.RData")
