###################################################################
####  marx helper
###################################################################
marx <- function(pu.temp,
                 feat.temp,
                 spec.temp,
                 bound.temp,
                 progress) {
  
  
  marxan(pu=pu.temp, 
         puvsp=feat.temp, 
         spec=spec.temp, 
         bound=bound.temp,
         progress=progress)       
  #  }  
}

###################################################################
####  marxan function
###################################################################

marxan <- function(pu="pu.dat", puvsp="puvsp2.dat", spec="spec.dat", bound="bound.dat", indir=getwd(), 
                   outdir=getwd(),session=session, ind.runs="",
                   progress=""){  
  ## Read and obtain Input.dat parameters
  if(file.exists("input.dat"))
    input <- readLines("input.dat",n=-1)
  else
    stop("input.dat file not found")
  
  #empty output directory before Marxan runs
#  wd <- getwd()
#  setwd("./output")
#  fls <- list.files()
#  if (!(length(fls) == 0) && (typeof(fls) == "character"))
#    file.remove(fls)
#  setwd(wd)
  
  #print(input.file[1])
  
  ## Set input and output directories
  txt <- "INPUTDIR"
  input[grep(txt, input)]<-paste(txt,sprintf("%s/input",indir))
  txt <- "OUTPUTDIR"
  input[grep(txt, input)]<-paste(txt,sprintf("%s/output",outdir))
  
  
  
  ## Create data frame for later use with GIS
  txt <- "PUNAME"
  input[grep(txt, input)]<-paste("PUNAME",pu) # pu file
  
  
  txt <- "PUVSPRNAME"
  input[grep(txt, input)]<-paste(txt,puvsp) # Puvsp file name in input.dat
  
  spffr <- read.csv((file=sprintf("%s/input/%s",indir,spec)))
  txt <- "SPECNAME"
  input[grep(txt, input)]<-paste(txt,spec) # Spec file name in input.dat
  
  #Boundary file
  txt <- "BOUNDNAME"
  input[grep(txt, input)]<-paste(txt,bound) # pu file
  
  
  #gsub(" ","",substr(input[grep(txt, input)],nchar(txt)+1,nchar(input[grep(txt, input)])), fixed=T)
  
  pufr <- read.csv((file=sprintf("%s/input/%s",indir,pu)))
  ssolnfr <- bestfr <- data.frame(ID= pufr$id)
  summed <- list(TimeStamp=date())
  
  
  ####################### Step 2
  ##################################################################
#  lng <- length(spf) * length(blm) * length(nreps) * length(nitns) + 1
  lng<-1
  #  progress <- Progress$new(session)
  #  withProgress(session, min=1, max=lng, expr={
  progr <- 1
  
  
  ##Loop for sequential sfp
  kk <- 2
  ## Loop for sequential BLM
  for(jj in 1:1){
    
    ## Input.dat parameters ##
    #txt <- "BLM"
    #input[grep(txt, input)]<-paste(txt,blm[jj]) # BLM in input.dat    
    
    for(ii in 1:1) {
      
      #species penalty factor needs tp be set and saved in spec file for each run
      #spffr$spf <- spf[ii]
      #write.csv(spffr, sprintf("%s/input/%s",indir,spec), row.names = FALSE)
      
      for (ll in 1:1){
        #txt <- "NUMREPS"
        #input[grep(txt, input)]<-paste(txt,nreps[ll]) # Number of runs in input.dat
        
        for (mm in 1:1){
          
          #txt <- "NUMITNS"
          #input[grep(txt, input)]<-paste(txt,sprintf("%i",round(nitns[mm]))) # Number of runs in input.dat
          
          #txt <- "SCENNAME"
          #runname <- sprintf("%sS%.2g_B%g_I%.2g",scenname,spf[ii],blm[jj],nitns[mm])
          # Below the original scenname string function          
          #          runname <- sprintf("%s_Spf%i_Blm%s_Nrep%i_Iter%i",scenname,round(spf[ii])
          #                             ,blm[jj],nreps[ll], nitns[mm])
          
          #input[grep(txt, input)]<-paste(txt,runname) # Puvsp file name in input.dat
          write(input,"input.dat")# Re-write input file at each run with the
          #corresponding parameters changed
          if(file.exists("Marxan_x64.exe")){
            system("Marxan_x64.exe",wait=T,invisible=T) # Call Marxan to execute
          }
          #            else if(file.exists("MarOpt_v243_Linux64")) {
          else if(file.exists("MarOpt_v300_Linux64")) {
            progress$set(message = 'Calculation in progress', detail = "This will take a few mins...", value = progr/lng)
            #              setProgress(message = 'Calculation in progress',
            #                          detail = 'This may take a while...',
            #                          value=progr)
            #              print(progr)
            progr <- progr + 1
            fit.gurobi(pu=pu, puvsfeat=puvsp, feat=spec)
            #system("./MarOpt_v243_Linux64")
          }
          else
            stop('No Marxan executable found in working directory')
          
          # saving results for function return
          #ssoln <- read.csv((file=sprintf("%s/output/%s_ssoln.csv",outdir,runname)))
          #best <- read.csv((file=sprintf("%s/output/%s_best.csv",outdir,runname)))
          #summ <- read.csv((file=sprintf("%s/output/%s_sum.csv",outdir,runname)))
          #ssoln<-ssoln[order(ssoln$planning_unit),]
          #probably temporary
          #names(best)[1] <- "planning_unit"     
          #names(best)[2] <- "solution"     
          
          #best<-best[order(best$planning_unit),]
          #ssolnfr <- data.frame(ssolnfr, ssoln$number)
          #bestfr <- data.frame(bestfr, best$solution)
          #summed[[kk]] <- summ
          #names(ssolnfr)[kk] <- names(bestfr)[kk] <- names(summed)[kk] <- runname
          #kk <- kk + 1
        }
      }
    }
  }
  #    paras<- data.frame(spf.ln=length(spf),blm.ln=length(blm),
  #paras<- list(spf.ln=length(spf),blm.ln=length(blm),
  #             nreps.ln=length(nreps),niter.ln=length(nitns),
  #             spf=spf, blm=blm,nreps=nreps,niter=nitns)
  #  setwd("..")
  #    system("touch ../../restart.txt")
  progress$set(value = 1)
  

  # end individual run attribute table
  ########################################
  progress$close()      
  
#  return(list(ssoln=ssolnfr,best=bestfr,sums=summed, paras=paras, ind.runs=comb))
  #  })   
}

