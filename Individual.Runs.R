setwd("/mnt/shiny/143985/output")

files <- list.files(pattern="_r")

runs <- unique(gsub("_r.*","", files))

comb <- read.csv(files[1])
count <- 2
for (jj in 1:length(runs)){
  dd <- strsplit(runs[jj],"+",fixed=T)
  temp <- grep(dd[[1]][1],files,value=T)
  for (ii in 1:length(temp)){
    tt <- read.csv(temp[ii])
    comb[,count] <- tt[,2]
    names(comb)[count] <- sprintf("%s_%03d",dd[[1]][1],ii)
    count <- count + 1
  }  
}



