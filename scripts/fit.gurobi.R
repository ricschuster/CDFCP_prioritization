###################################################################
####  fit gurobi optimization
###################################################################


fit.gurobi <- function(pu="",puvsfeat="",feat=""){
  ptm <- proc.time()
  
  # binary means 0/1 result (ILP). not binary means continuous in (0,1) result (LP)
  #fBinary <- FALSE
  fBinary <- TRUE
  
  # convert puvsfeat to use index addressing rather than id addressing for pu and species
  # this speeds up generation of OR model as it saves millions of calls to "which"
  fGenerateIndexMatrix <- FALSE
  fUseIndexMatrix <- TRUE
  
  if (fBinary ==TRUE)
  {
    sType <- "ILP"
  } else {
    sType <- "LP"
  }
  sType
  
  
  
  puvsfeat <- puvsfeat[,-1]
  #names(puvsfeat)[3] <- "amount"
  
  #TEST prot + exclu
#  pu$status[seq(10,100000,10)] <- 2
#  pu$status[seq(15,100000,10)] <- 3
  

  TotalArea <- colSums(puvsfeat)

  # set up vectors for the rhs and sense
  sense <- rep(">=", ncol(puvsfeat))

  # set the targets for these features (the default sense is right for these):
  feat$Percent[feat$Percent < 0] <- 0
  feat$Percent[feat$Percent > 100] <- 100
  rhs <- feat$Percent/100 * TotalArea[match(names(TotalArea),as.character(feat$name))]
  
  idx2 <- length(rhs) + 1
  # STEP 2: lock the existing reserve PUs
  # note: we are ignoring possible excluded sites here because Oscar's dataset doesn't have these
  # lock all required planning units to existing reserve
  iReservedCount <- nrow(pu[pu$status==2,])
  
  if (iReservedCount > 0)
  {
    puvsfeat$Prot <- ifelse(pu$status==2,1,0)

    sense[idx2] <- "="
    rhs[idx2] <- iReservedCount
    idx2 <- idx2 + 1
  }
  
  # STEP 2A: lock out unwanted PUs
  iLockOutCount <- nrow(pu[pu$status==3,])
  
  if (iLockOutCount > 0)
  {
    puvsfeat$Excl <- ifelse(pu$status==3,1,0)
    
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
  
  constr <- as(t(as.matrix(puvsfeat)), "sparseMatrix") 
  # resize the vectors

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
  
  # run Rsymphony on server
  #bounds <- list(lower = list(ind = seq(1:length(pu$cost)), val = rep(0,length(pu$cost))),
  #               upper = list(ind = seq(1:length(pu$cost)), val = rep(1,length(pu$cost))))
  #tm <- system.time(result <- Rsymphony_solve_LP(pu$cost, as.matrix(constr), sense, rhs, bounds, types="B",first_feasible=T,verbosity=1))
  
  #result$runtime <- tm[[3]]
  #result$x <- result$solution
  
  print(result$status)
  
  # browse the results
  print(result$objval) # planning unit cost
  
  # save the results to the output directory
  #write.csv(result$objval,file=paste0("output/NPLCC_",sType,"_gurobi_objval.csv"))
  #write.csv(result$x,file=paste0("output/NPLCC_",sType,"_gurobi_xresult.csv"))
  
  #sum(result$x)
  
  cat(paste0("date ",date(),"\n"))
  
  proc.time() - ptm
  return(result)

  
}
