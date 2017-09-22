load("D:\\Work\\Band_Land\\Canada\\For_Analysis\\15_11_07\\Create.summaries.from.Intersects.Canada.RData")
install.packages("ade4")
load("D:\\Work\\NPLCC\\Covariates\\NPLCC\\NPLCC_covars_raw.RData")
ls()
str(cov)
pu <- read.csv("D:/Virtual_Box/shared/UBC_VS/NPLCC/NPLCC.v0.5/p/files/input/NPLCC_1ha_dollar_100.csv")
pu2 <- read.csv("D:/Virtual_Box/shared/UBC_VS/NPLCC/NPLCC.v0.12/p/files/input/NPLCC_1ha_dollar_100.csv")
str(pu)
str(pu2)
tail(pu)
tail(pu2)
library(plyr)
join
?join
ls()
str(cov)
str(pu)
cov$id <- cov$NPLCC_ID
tt <- join(pu,cov,by="id")
str(tt)
tt2 <- tt[c("CLT_100m","RHU100","RLU100")]
tt2$rd <- tt2[,2]+tt2[,3]
tt2$id <- pu2$id
tt2 <- tt2[c("id","CLT_100m","rd")]
names(tt2)[-1] <- c("Agr","Rdl")
Ar <- 0.1*0.1*pi

tt3 <- tt2

tt3$Agr <- tt3$Agr/(1000000*Ar)
tt3$Rdl <- tt3$Rdl/(1000*Ar)
write.csv(tt3,"D:/Virtual_Box/shared/UBC_VS/NPLCC/NPLCC.v0.12/p/files/input/NPLCC_Agr_Rdl.csv",row.names=F)


setwd("D:/Virtual_Box/shared/UBC_VS/NPLCC/NPLCC.v0.12/p/files/input")

pu_ar <- read.csv("NPLCC_1ha_area_100.csv")
pu_do <- read.csv("NPLCC_1ha_dollar_100.csv")
pu_hu <- read.csv("NPLCC_1ha_human_100.csv")

pu_ar$id <- pu_do$id
pu_hu$id <- pu_do$id

write.csv(pu_ar,"NPLCC_1ha_area_100.csv",row.names=F)
write.csv(pu_hu,"NPLCC_1ha_human_100.csv",row.names=F)

 puvsfeat <- read.csv("NPLCC_comm_feature_input_100_curr.csv")

library(dplyr)
library(tidyr)

ss <- spread(puvsfeat,species,value)

feat<-read.csv("NPLCC_spec_all_100.csv")

names(ss)[-1] <- as.character(feat$name)

write.csv(ss,"NPLCC_feat_input_100_curr.csv",row.names=F)





