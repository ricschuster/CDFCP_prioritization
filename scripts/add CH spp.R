library(foreign)

setwd("D:/VirtualBox/shared/UBC_VS/Shiny2/CDFCP.v0.22_full/pulayer_tmp")

cad <- read.dbf("Cadaster_CDFCP.dbf")
cad <- cad[order(cad$id),]
names(cad)[5:16] <- c("CButCp","CpePrRs","CfLup","DsPrRs","FthlSdg","FrstSnail",
                "MedFoam","Meconella","Microseris","MMurrlet","PopFlower","svMoth")

feat <- read.csv("../data/CDFCP_feat_input.csv")

feat1 <- data.frame(feat[,1:22], cad[,5:16],Area=feat$Area)

write.csv(feat1,"../data/CDFCP_feat_input.csv",row.names=F)


