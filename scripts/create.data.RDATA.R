library(tidyr)

setwd("data")

pu_AreaHa_RoadD_AgrD <- read.csv("CDFCP_AreaHa_RoadD_AgrD.csv")


cost <- list(area = read.csv("CDFCP_cost_area.csv"),
			 dollar = read.csv("CDFCP_cost_dollar.csv"),
			 human = read.csv("CDFCP_cost_human.csv"))

puvsf <- list(curr = read.csv("CDFCP_feat_input.csv"),
			  rcp45 = NULL,
			  rcp85 = NULL)
prot <- read.csv("CDFCP_prot_status.csv")

cad_1ha_isect <- read.csv("Cad_1ha_isect.csv")
save.image("../RData/feature_input.RData")
