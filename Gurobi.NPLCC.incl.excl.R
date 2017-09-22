# Purpose: 1) Read a Marxan dataset
#          2) Construct Gurobi OR model for the dataset
#          3) Solve the OR model with Gurobi

ptm <- proc.time()

require(gurobi)
require(Matrix)
require(sqldf)
require(plyr)
require(dplyr)
require(tidyr)

# binary means 0/1 result (ILP). not binary means continuous in (0,1) result (LP)
#fBinary <- FALSE
fBinary <- TRUE

# convert puvsfeat to use index addressing rather than id addressing for pu and species
# this speeds up generation of OR model as it saves millions of calls to "which"
fGenerateIndexMatrix <- FALSE
fUseIndexMatrix <- TRUE

if (fBinary == TRUE)
{
  sType <- "ILP"
} else {
  sType <- "LP"
}
sType

sWD <- "/media/sf_shared/UBC_VS/Gurobi_NPLCC_tiny"
#sWD <- "/Users/matt/Documents/R/ILP/marxan1"

setwd(sWD)

# read the input parameters
inputdat <- readLines("input.dat")

# get a parameter value from the input parameter file
GetParamValue <- function(sParam)
{
  iParam <- which(regexpr(sParam,inputdat)==1)
  if (length(iParam) > 0)
  {
    return(sapply(strsplit(inputdat[iParam]," "),"[",2))
  } else {
    return("")
  }
}

sINPUTDIR <- GetParamValue("INPUTDIR")
sOUTPUTDIR <- GetParamValue("OUTPUTDIR")
sSCENNAME <- GetParamValue("SCENNAME")
sSCENNAME

# read the input files
feat <- read.csv(paste0(sINPUTDIR,"/",GetParamValue("SPECNAME")))
#if (fUseIndexMatrix == TRUE)
#{
#  puvsfeat <- read.csv(paste0(sINPUTDIR,"/puorder_index.dat"))
#  names(puvsfeat)[3] <- "amount"  
#  pu <-  read.csv(paste0(sINPUTDIR,"/pu_index.dat"))
#} else {
#  puvsfeat <- read.csv(paste0(sINPUTDIR,"/",GetParamValue("PUVSPRNAME")))
#  names(puvsfeat)[3] <- "amount"
#  pu <-  read.csv(paste0(sINPUTDIR,"/",GetParamValue("PUNAME")))
#}

puvsfeat <- read.csv(paste0(sINPUTDIR,"/",GetParamValue("PUVSPRNAME")))
names(puvsfeat)[3] <- "amount"
pu <-  read.csv(paste0(sINPUTDIR,"/",GetParamValue("PUNAME")))

  #TEST prot + exclu
  #pu$status[seq(10,100000,10)] <- 2
  #pu$status[seq(15,100000,10)] <- 3

#head(pu) # id, cost, status
#head(feat) # id, prop, spf
#head(puvsfeat) # species, pu, amount

# remove PUID 151336,1.2204,1.2252,153915,154027,
#             154138,155107,155785,160066,160076,163635 from puorder.dat & sporder.dat
# because they're not present in pu.dat

# convert puvsfeat to use index addressing rather than id addressing for pu and species
if (fGenerateIndexMatrix == TRUE)
{
  puvsfeat_index <- puvsfeat
  
  iStart <- 1
  #iStart <- 1160000
  #iStart <- 1169000
  #iStart <- 1169700
  #iStart <- 1169710
  for (i in iStart:nrow(puvsfeat))
  {
    if ((i %% 10) == 100000)
    {
      cat(paste0("i ",i," date ",date(),"\n"))    
    }
    
    #i <- 1169713
    
    iPUID <- puvsfeat$pu[i]
    iSPID <- puvsfeat$species[i]
    
    iPUindex <- which(pu$id == iPUID)
    iSPindex <- which(feat$id == iSPID)
    
    #if (length(iPUindex) == 0)
    #{
    #  cat(paste0("error iPUindex not found for iPUID ",iPUID," iSPID ",iSPID," i ",i,"\n"))
    #}
    #if (length(iSPindex) == 0)
    #{
    #  cat(paste0("error iSPindex not found for iPUID ",iPUID," iSPID ",iSPID," i ",i,"\n"))
    #}
    puvsfeat_index$pu[i] <- iPUindex
    puvsfeat_index$species[i] <- iSPindex
  }
  write.csv(puvsfeat_index,paste0(sINPUTDIR,"/puorder_index.dat"),quote=FALSE,row.names=FALSE)
  puvsfeat <- read.csv(paste0(sINPUTDIR,"/puorder_index.dat"))
}

np <- nrow(pu)
nf <- nrow(feat)

cat(paste0("date ",date(),"\n"))

# Set up the sparse matrix variables - bigger vectors than needed get resized at the end
# maybe use nrow(puvsfeat) instead of 2 million
mc <- rep(0, nrow(puvsfeat)) # constraint column
mr <- rep(0, nrow(puvsfeat)) #            row
mz <- rep(0, nrow(puvsfeat)) #            value
idx <- 1

# set up vectors for the rhs and sense
sense <- rep(">=", nrow(puvsfeat))
rhs <- rep(0, nrow(puvsfeat))
idx2 <- 1

TotalArea <- rep(0,nf)

# STEP 1: set the feature targets
if (fUseIndexMatrix == TRUE)
{
  mc <- puvsfeat$pu
  mr <- puvsfeat$species
  mz <- puvsfeat$amount
  
  TotalArea <- data.frame(summarise(group_by(puvsfeat,species),area=sum(amount)))
  # set the targets for these features (the default sense is right for these):
  rhs[1:nf] <- feat$prop * TotalArea$area[match(TotalArea$species,feat$id)]
  idx <- idx + nrow(puvsfeat)

} else
{
  for (i in 1:nrow(puvsfeat))
  {
    if ((i %% 100000) == 0)
    {
      cat(paste0("i ",i," date ",date(),"\n"))    
    }
    
    iPUID <- puvsfeat$pu[i]
    iSPID <- puvsfeat$species[i]
    rAmount <- puvsfeat$amount[i]
    
    iPUindex <- which(pu$id == iPUID)
    iSPindex <- which(feat$id == iSPID)      
    
    TotalArea[iSPindex] <- TotalArea[iSPindex] + rAmount
    
    mc[idx] <- iPUindex
    mr[idx] <- iSPindex
    mz[idx] <- rAmount
    idx <- idx + 1
  }  
}

# set the targets for these features (the default sense is right for these):
rhs[1:nf] <- feat$prop * TotalArea$area[match(TotalArea$species,feat$id)]

idx2 <- nf + 1

cat(paste0("date ",date(),"\n"))

# save all objects
#save.image(file=paste0(sINPUTDIR,"/GurobiA_",sSCENNAME,"_",sType,".RData"))

# load all objects
#load(file=paste0(sINPUTDIR,"/GurobiA_",sSCENNAME,"_",sType,".RData"))

# STEP 2: lock the existing reserve PUs
# note: we are ignoring possible excluded sites here because Oscar's dataset doesn't have these
# lock all required planning units to existing reserve
iReservedCount <- nrow(pu[pu$status==2,])

if (iReservedCount > 0)
{
  mc[idx:(idx+iReservedCount-1)] <- as.numeric(rownames(pu[pu$status==2,]))
  mr[idx:(idx+iReservedCount-1)] <- idx2
  mz[idx:(idx+iReservedCount-1)] <- 1
  idx <- idx + iReservedCount
  
  sense[idx2] <- "="
  rhs[idx2] <- iReservedCount
  idx2 <- idx2 + 1
}

# STEP 2A: lock out unwanted PUs
iLockOutCount <- nrow(pu[pu$status==3,])

if (iLockOutCount > 0)
{
  mc[idx:(idx+iLockOutCount-1)] <- as.numeric(rownames(pu[pu$status==3,]))
  mr[idx:(idx+iLockOutCount-1)] <- idx2
  mz[idx:(idx+iLockOutCount-1)] <- 1
  idx <- idx + iLockOutCount
  
  sense[idx2] <- "="
  rhs[idx2] <- 0
  idx2 <- idx2 + 1
}


# STEP 3: constraints for LP, result can be continuous in 0 to 1
if (fBinary == FALSE)
{
  for (i in 1:nrow(pu))
  {
    mc[idx] <- i
    mr[idx] <- idx2
    mz[idx] <- 1
    idx <- idx + 1
    
    sense[idx2] <- "<="
    rhs[idx2] <- 1
    idx2 <- idx2 + 1  
  }
}

#sense <- "=" # note = might be problematic because strict equality might fail
#             # due to a slight rounding error
#             # best to reformulate for inequality

cat(paste0("date ",date(),"\n"))

# save all objects again
#save.image(file=paste0(sINPUTDIR,"/GurobiB_",sSCENNAME,"_",sType,".RData"))

# load all objects again
#load(file=paste0(sINPUTDIR,"/GurobiB_",sSCENNAME,"_",sType,".RData"))

# resize the vectors
mc <- mc[1:(idx-1)]
mr <- mr[1:(idx-1)]
mz <- mz[1:(idx-1)]

constr <- sparseMatrix(i=mr, j=mc, x=mz)

sense <- sense[1:(idx2-1)]
rhs <- rhs[1:(idx2-1)]

# set up Gurobi model in R
model <- list()
model$obj <- pu$cost
model$modelsense <- "min"
if (fBinary == TRUE)
{
  model$vtype <- "B"
}
model$A <- constr
model$rhs <- rhs
model$sense <- sense

# save all objects again
#save.image(file=paste0(sINPUTDIR,"/GurobiC_",sSCENNAME,"_",sType,".RData"))

# load all objects again
#load(file=paste0(sINPUTDIR,"/GurobiC_",sSCENNAME,"_",sType,".RData"))

# run Gurobi
params <- list(Presolve=2,TimeLimit=1000)
result <- gurobi(model,params)

print(result$status)

# browse the results
print(result$objval) # planning unit cost

# save the results to the output directory
write.csv(result$objval,file=paste0(sOUTPUTDIR,"/",sSCENNAME,"_",sType,"_gurobi_objval.csv"))
write.csv(result$x,file=paste0(sOUTPUTDIR,"/",sSCENNAME,"_",sType,"_gurobi_xresult.csv"))

#sum(result$x)

cat(paste0("date ",date(),"\n"))

proc.time() - ptm

#result$slack
#nf
#length(result$slack) $ # is nf-1

#(result$x) > 0
