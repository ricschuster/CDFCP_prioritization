library(shiny)
library(shinyIncubator)
library(ggplot2)
require(sp)
require(rgdal)
require(maptools)
require(PBSmapping)
require(foreign)
require(sqldf)
require(vegan)
require(labdsv)
library(leaflet)

##################################################################
##################################################################
## R script for Marxan batch run
## returns data.frames of the best solutions, summed solutions,
## and the summary tables
##
## Parts based on code by Paulo Cardoso
## (http://lists.science.uq.edu.au/pipermail/marxan/2008-May/000319.html)
##
## Author: Richard Schuster (mail@richard-schuster.com
## 30 Dec 2014
##
## v0.5 
##    - no Marxan start right away
##    - x-axis from $ increase to % increase
##    - SEI target included (set to 0 for global target)
## v0.6
##    - ssoln + best in download attribute table
##    - individual runs downloadable
##v0.7
##    - include area as a target possibility
##    - also fixed setting target error (line 318 ff)
##v0.8
##    - make saving individual runs optional
##    - include Hectares BC layers (red+blue listed, endemic, SARA species richness)
##v0.9 
##	  - kick out HecBC
##	  - include wetland birds
##v0.10
##    - allow area to be selected as cost (drop-down menu)
##v0.11
##    - WET, SHR, HUM, HUM1 (1- HUM) included as goals
##v0.12
##    - Human score included as cost option
##v0.13
##    - Sliders to exclude properties under size X and with road densities > Y
##    - Setting sliders will dynamically calculate pu file to exclude properties that 
##    - meet these criteria
##v0.14
##    - Agriculture slider to exclude properties with more Agr than Z (in %)
##v0.16
##    - Plant layer update (covariate issues resolved)
##    - Only paved roads for slider
##v0.17.2
##    - touch restart.txt working now
##v0.17.3
##    - Example map CDFCP
##################################################################
##################################################################
marxan <- function(pu="pu.dat", puvsp="puvsp2.dat", spec="spec.dat", bound="bound.dat", spf=1,
                   blm=0, nitns=100000, nreps=10, scenname="", indir=getwd(), 
                   outdir=getwd(), session=session, ind.runs="",
                   progress=""){  
  ## Read and obtain Input.dat parameters
  if(file.exists("input.dat"))
    input <- readLines("input.dat",n=-1)
  else
    stop("input.dat file not found")
  
  #empty output directory before Marxan runs
  wd <- getwd()
  setwd("./output")
  fls <- list.files()
  if (!(length(fls) == 0) && (typeof(fls) == "character"))
    file.remove(fls)
  setwd(wd)

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
  lng <- length(spf) * length(blm) * length(nreps) * length(nitns) + 1
#  progress <- Progress$new(session)
#  withProgress(session, min=1, max=lng, expr={
    progr <- 1
    
    
    ##Loop for sequential sfp
    kk <- 2
    ## Loop for sequential BLM
    for(jj in 1:length(blm)){
      
      ## Input.dat parameters ##
      txt <- "BLM"
      input[grep(txt, input)]<-paste(txt,blm[jj]) # BLM in input.dat    
      
      for(ii in 1:length(spf)) {
      
        #species penalty factor needs tp be set and saved in spec file for each run
        spffr$spf <- spf[ii]
        write.csv(spffr, sprintf("%s/input/%s",indir,spec), row.names = FALSE)
      
        for (ll in 1:length(nreps)){
          txt <- "NUMREPS"
          input[grep(txt, input)]<-paste(txt,nreps[ll]) # Number of runs in input.dat
          
          for (mm in 1:length(nitns)){
            
            txt <- "NUMITNS"
            input[grep(txt, input)]<-paste(txt,sprintf("%i",round(nitns[mm]))) # Number of runs in input.dat
            
            txt <- "SCENNAME"
            runname <- sprintf("%sS%.2g_B%g_I%.2g",scenname,spf[ii],blm[jj],nitns[mm])
  # Below the original scenname string function          
  #          runname <- sprintf("%s_Spf%i_Blm%s_Nrep%i_Iter%i",scenname,round(spf[ii])
  #                             ,blm[jj],nreps[ll], nitns[mm])
            
            input[grep(txt, input)]<-paste(txt,runname) # Puvsp file name in input.dat
            write(input,"input.dat")# Re-write input file at each run with the
            #corresponding parameters changed
            if(file.exists("Marxan_x64.exe")){
              system("Marxan_x64.exe",wait=T,invisible=T) # Call Marxan to execute
            }
            else if(file.exists("MarOpt_v243_Linux64")) {
              progress$set(message = 'Calculation in progress', detail = "This may take a while...", value = progr/lng)
#              setProgress(message = 'Calculation in progress',
#                          detail = 'This may take a while...',
#                          value=progr)
#              print(progr)
              progr <- progr + 1

              system("./MarOpt_v243_Linux64")
            }
            else
              stop('No Marxan executable found in working directory')
            
            # saving results for function return
            ssoln <- read.csv((file=sprintf("%s/output/%s_ssoln.csv",outdir,runname)))
            best <- read.csv((file=sprintf("%s/output/%s_best.csv",outdir,runname)))
            summ <- read.csv((file=sprintf("%s/output/%s_sum.csv",outdir,runname)))
            ssoln<-ssoln[order(ssoln$planning_unit),]
            #probably temporary
            names(best)[1] <- "planning_unit"     
            names(best)[2] <- "solution"     
            
            best<-best[order(best$planning_unit),]
            ssolnfr <- data.frame(ssolnfr, ssoln$number)
            bestfr <- data.frame(bestfr, best$solution)
            summed[[kk]] <- summ
            names(ssolnfr)[kk] <- names(bestfr)[kk] <- names(summed)[kk] <- runname
            kk <- kk + 1
          }
        }
      }
    }
#    paras<- data.frame(spf.ln=length(spf),blm.ln=length(blm),
    paras<- list(spf.ln=length(spf),blm.ln=length(blm),
                       nreps.ln=length(nreps),niter.ln=length(nitns),
                       spf=spf, blm=blm,nreps=nreps,niter=nitns)
    #  setwd("..")
#    system("touch ../../restart.txt")
    progress$set(value = 1)

    ########################################
    # get individual runs attribute table
    if(ind.runs == TRUE){
      oldwd <-setwd(sprintf("%s/output",outdir))
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
          names(comb)[count] <- sprintf("%s_r%03d",runs[jj],ii)
          count <- count + 1
        }  
      } 
      setwd(oldwd)
    } else {
      comb <- 0
    }
    # end individual run attribute table
    ########################################
    progress$close()      

    return(list(ssoln=ssolnfr,best=bestfr,sums=summed, paras=paras, ind.runs=comb))
#  })   
}

###################################################################
####  FUNCTION END
###################################################################

marx <- function(min,max,niter,exponential,ITmin,ITmax,ITniter,
                 ITexponential, Nreps, session,
                 BLMmin, BLMmax, BLMniter, BLMexponential, target,
                 OFtarget,
                 SAVtarget,
                 WETtarget,
                 SHRtarget,
                 HUMtarget,
                 HUM1target,
                 BETAtarget,
                 StCtarget,
                 SeqCtarget,
                 EOtarget,
                 Planttarget,
                 SEItarget,
                 Areatarget,
                 #RD_BLtarget,
                 #ENDEMtarget,
                 #SARAtarget,
                 pu.temp,
                 bound.temp,
        				 ind.runs,
                 progress) {
  ifelse (exponential,
          spf <-min * 10^(0:(niter-1)),
          ifelse((niter-1)==0,spf <- min, spf <- seq(min,max,(max-min)/(niter-1))))
  
  ifelse (ITexponential,
          nitns <-ITmin * 10^(0:(ITniter-1)),
          ifelse((ITniter-1)==0,nitns <- ITmin, nitns <- seq(ITmin,ITmax,(ITmax-ITmin)/(ITniter-1))))

  ifelse (BLMexponential,
          blm <-BLMmin * 10^(0:(BLMniter-1)),
          ifelse((BLMniter-1)==0,blm <- BLMmin, 
                    blm <- seq(BLMmin,BLMmax,(BLMmax-BLMmin)/(BLMniter-1))))
  
  spec="CDFCP_spec_all.csv"
#  for (ii in 1:length(target)){
    spffr <- read.csv((file=sprintf("input/%s",spec)))
    spffr$prop <- target[ii] #Set target

    #####################
    #### SET TARGETS ####
    #####################
    spffr[spffr$id==100,]$prop <- OFtarget
    spffr[spffr$id==101,]$prop <- SAVtarget
    spffr[spffr$id==102,]$prop <- BETAtarget
    spffr[spffr$id==103,]$prop <- WETtarget
    spffr[spffr$id==104,]$prop <- SHRtarget
    spffr[spffr$id==105,]$prop <- HUMtarget
    spffr[spffr$id==106,]$prop <- HUM1target
    spffr[spffr$id==200,]$prop <- StCtarget
    spffr[spffr$id==201,]$prop <- SeqCtarget
    spffr[spffr$id==300,]$prop <- EOtarget
    spffr[spffr$id==500,]$prop <- Planttarget
    spffr[spffr$id>=400 & spffr$id < 500,]$prop <- SEItarget
    spffr[spffr$id==600,]$prop <- Areatarget
    #spffr[spffr$id==800,]$prop <- RD_BLtarget
    #spffr[spffr$id==801,]$prop <- ENDEMtarget
    #spffr[spffr$id==802,]$prop <- SARAtarget

    write.csv(spffr, sprintf("input/%s",spec), row.names = FALSE)
    
    marxan(pu=pu.temp, 
           puvsp="CDFCP_Marxan_Feature_input.csv", 
           spec="CDFCP_spec_all.csv", 
           bound=bound.temp,
           spf=spf,nitns=nitns,
           #scenname=target,
           nreps=Nreps,blm=blm,session=session,
           ind.runs=ind.runs,
           progress=progress)       
#  }  
}

PrepareDisplay <- function(sDir=getwd())
{
  # prepare the map: pulayer object
  sPulayer <- paste(sDir,"/pulayer/pulayer1km.shp",sep="")
  pulayer <- readShapePoly(sPulayer)
  pulayer <<- SpatialPolygons2PolySet(pulayer)
  pu_table <<- read.dbf(paste(sDir,"/pulayer/pulayer1km.dbf",sep=""))
  
  # prepare the cluster analysis objects
  #solutions_raw<-read.table(paste(sDir,"/output/output","_solutionsmatrix.csv",sep=""),header=TRUE, row.name=1, sep=",")
  #solutions <- unique(solutions_raw)
  #iUniqueSolutions <- dim(solutions)[1]
  #soldist<-vegdist(solutions,distance="bray")
  #sol.mds<<-nmds(soldist,2)
  #h<-hclust(soldist, method="complete")
}

eucdist <- function(xloc,yloc,adataframe)
  # This function handles the click event on the NMDS plot to identify the solution
  # that is nearest the x,y location clicked.
{
  mindistance <- 10000
  
  for (i in 1:dim(adataframe)[1]){
    
    x1 <- adataframe[i,][1]
    y1 <- adataframe[i,][2]
    distance <- sqrt(((x1 - xloc) ^ 2) + ((y1 - yloc) ^ 2))
    
    if (i==1){
      distances <- c(distance)
    } else {
      distances <- c(distances,distance)
    }
    
    if (distance < mindistance){
      mindistance <- distance
      closestpoint <- i
    }
  }	
  return(closestpoint)
  
}

# Define server logic 
shinyServer(function(input, output, session) {
  system(sprintf("touch %s/restart.txt",globWD))
  #  setwd("/var/shiny-server/www/examples/calib.shiny.v2/")
  my.data <- reactive({ 
    # Don't do anything until after the first button push.
    input$mrun
    # Note that just by virtue of checking the value of input$recalcButton,
    # we're now going to get called whenever it is pushed.    
    if(input$mrun == 0)
      return(NULL)    
    
    return(isolate({
      # Now do the expensive stuff
      OFtarget <- ifelse(input$OFtarget >= 0, input$OFtarget, input$Target)
      SAVtarget <- ifelse(input$SAVtarget >= 0, input$SAVtarget, input$Target)
      BETAtarget <- ifelse(input$BETAtarget >= 0, input$BETAtarget, input$Target)
      WETtarget <- ifelse(input$WETtarget >= 0, input$WETtarget, 0)
      SHRtarget <- ifelse(input$SHRtarget >= 0, input$SHRtarget, 0)
      HUMtarget <- ifelse(input$HUMtarget >= 0, input$HUMtarget, 0)
      HUM1target <- ifelse(input$HUM1target >= 0, input$HUM1target, 0)
      StCtarget <- ifelse(input$StCtarget >= 0, input$StCtarget, input$Target)
      SeqCtarget <- ifelse(input$SeqCtarget >= 0, input$SeqCtarget, input$Target)
      EOtarget <- ifelse(input$EOtarget >= 0, input$EOtarget, input$Target)
      Planttarget <- ifelse(input$Planttarget >= 0, input$Planttarget, input$Target)
      SEItarget <- ifelse(input$SEItarget >= 0, input$SEItarget, 0)
      Areatarget <- ifelse(input$Areatarget >= 0, input$Areatarget, input$Target)
      #RD_BLtarget <- ifelse(input$RD_BLtarget >= 0, input$RD_BLtarget, 0)
      #ENDEMtarget <- ifelse(input$ENDEMtarget >= 0, input$ENDEMtarget, 0)
      #SARAtarget <- ifelse(input$SARAtarget >= 0, input$SARAtarget, 0)

      cost_temp <- input$cost #ifelse(input$cost == "area", "area", ifelse(input$cost == "dollar", "cost", "human"))
      prot_temp <- ifelse(input$protected == "locked","_protected_included","")
      pu_temp1 <- sprintf("Cadaster_pu_%s_Jurisdiction_CDFCP%s.csv",cost_temp,prot_temp)

      progress <- Progress$new(session)
      progress$set(message = 'Setting up Marxan inputs', detail = "Please be patient...", value = 0.01)
      
      
      if(input$AreaHa >0 || input$RoadD > 0){
        #AreaHa + RoadD set to 3 to exclude
        pu_temp <- "Cadaster_pu_temp.csv"
        pu_temp2 <- read.csv((file=sprintf("input/%s",pu_temp1)))
        pu_AreaHa_RoadD_AgrD <- read.csv("input/pu_AreaHa_RoadD_AgrD.csv")
        pu_AreaHa_RoadD_AgrD$excl <- 0
        
        if(input$AreaHa >0) pu_AreaHa_RoadD_AgrD$excl[pu_AreaHa_RoadD_AgrD$AreaHa < input$AreaHa] <- 3 
        if(input$RoadD >0) pu_AreaHa_RoadD_AgrD$excl[pu_AreaHa_RoadD_AgrD$RoadD > input$RoadD] <- 3 
        if(input$AgrD >0) pu_AreaHa_RoadD_AgrD$excl[pu_AreaHa_RoadD_AgrD$AgrD > input$AgrD] <- 3 
        
        # if status = 2 keep it
        pu_temp2$status <- ifelse(pu_temp2$status == 2, 2,pu_AreaHa_RoadD_AgrD$excl )
        
        write.csv(pu_temp2, sprintf("input/%s",pu_temp), row.names = FALSE)
        #END AreaHa + RoadD set to 3 to exclude
      } else {
        pu_temp <- pu_temp1
      }
      
      cspf <- marx(input$min,input$max,input$iterations, input$exponential, 
                   input$ITmin,input$ITmax,input$ITiterations,input$ITexponential,
                   input$Nreps, session=session,
                   input$BLMmin,input$BLMmax,input$BLMiterations,input$BLMexponential,
                   input$Target/100,
                   OFtarget/100, 
                   SAVtarget/100,
                   WETtarget/100,
                   SHRtarget/100,
                   HUMtarget/100,
                   HUM1target/100,
                   BETAtarget/100,
                   StCtarget/100,
                   SeqCtarget/100,
                   EOtarget/100,
                   Planttarget/100,
                   SEItarget/100,
                   Areatarget/100,
                   #RD_BLtarget/100,
                   #ENDEMtarget/100,
                   #SARAtarget/100,
                   pu.temp =  pu_temp,
                   bound.temp = ifelse(input$connectivity == "yes","CDFCP_bound_max_100m_distance_perimeter.csv",""),
				   ind.runs = ifelse(input$IndRuns == TRUE, TRUE,FALSE),
           progress
      ) 
      
      Nodes <- names(cspf$sums)[-1]
      Texts <- cspf$sums[-1]
#      PrepareDisplay()
      names(Texts) <- Nodes
      list(cspf=cspf, Nodes=Nodes, Texts=Texts)
    }))
    #if (input$mrun >0) {                 
    #        input$mrun <- 0
    #    }
  })
  
  ################################################################################################  
  #################################################################################################  
  Nodes <- reactive({
    test.spf <- my.data()$cspf  
    names(test.spf$sums)[-1]
  })
      #reactive({ paste0("calib_Spf1_mv", 1:input$ntabs) })
  Texts <- reactive({
    test.spf <- my.data()$cspf
    tt <- test.spf$sums[-1]
    names(tt) <- my.data()$Nodes
    tt
  })  
  observe({
    texts <- my.data()$Texts
    nodes <- my.data()$Nodes
    if(length(input$tab0)>0){
      I <- input$tab0
      for(i in 1:length(nodes)){
        I <- input$tab0
        if(I==i) {
          tabl <- texts[[i]]
          output[[nodes[i]]] <- renderTable({tabl})
        }
      }
    }
    
    output$summary <- renderTable({ # to display in the "Summary" tab
      if(input$mrun == 0) {
        print("Run Marxan")
        return(NULL)
      }      
      test.spf <- my.data()$cspf
      #spf <- current.spf(input$range[1],input$range[2],input$iterations) 
      #test.spf <- marxan(spf=spf,puvsp="puvsp.dat")       
      ##################
      ## Get missing values, MPM
      ##################
      test.spf$sums <- test.spf$sums[names(test.spf$sums)]
      
      sumtest.spf <- data.frame(runname=I("a"), Score=0, Cost=0, PUs=0, B_length=0, Penalty=0, 
                                Shortfall=0, Missing_Values=0, MPM=0)
      Cost <- Score <- vector()
      
      for (ff in 2:(length(test.spf$sums))) {
        sumtest.spf[ff-1,1] <- as.character(names(test.spf$sums)[ff])
        
        sumtest.spf[ff-1,2] <- mean(test.spf$sums[[ff]]$Score)
        sumtest.spf[ff-1,3] <- mean(test.spf$sums[[ff]]$Cost)
        sumtest.spf[ff-1,4] <- mean(test.spf$sums[[ff]]$Planning_Units)
        sumtest.spf[ff-1,5] <- mean(test.spf$sums[[ff]]$Connectivity)
        sumtest.spf[ff-1,6] <- mean(test.spf$sums[[ff]]$Penalty)
        sumtest.spf[ff-1,7] <- mean(test.spf$sums[[ff]]$Shortfall)
        
        sumtest.spf[ff-1,8] <- sum(test.spf$sums[[ff]]$Missing_Values)
        sumtest.spf[ff-1,9] <- sum(test.spf$sums[[ff]]$MPM)
        Cost <- c(Cost,test.spf$sums[[ff]]$Cost)
        Score <- c(Score,test.spf$sums[[ff]]$Score)
        
      }
      sumtest.spf[sumtest.spf$runname!="a",]     
    })  
  }) 
  
  #UI
  output$tabsets <- renderUI({
    tabs <- list(NULL)
    texts <- my.data()$Texts
    nodes <- my.data()$Nodes
    tabs[[1]] <- tabPanel("Summary",
                            tableOutput("summary"),
                            value="summary")    
    tabs[[2]] <- tabPanel("Map",plotOutput("outputmap"))
    J <- length(nodes)
    for(i in 1:J){
      tabs[[i+2]] <- tabPanel(my.data()$Nodes[i], tableOutput(nodes[i]), value=i)
    }

    tabs$id <- "tab0"
    do.call(tabsetPanel, tabs)
  })  

  #########################################
  #########################################
  
  
  ################################################################################################  
  #################################################################################################  
  
  #  spf <- 
#  tt2 <- reactive({
#    spf< - seq(input$range[1],input$range[2],
#      (input$range[2]-input$range[1])/(input$iterations-1))
#  })
    
    
  
  # Return the formula text for printing as a caption
  output$caption <- renderText({
    "Marxan results"
  })

#  test.spf <- reactive({ 
    
#    spf <- current.spf(input$range[1],input$range[2],input$iterations) 
#    return(marxan(spf=spf,puvsp="puvsp.dat"))                         
#  })
  
  #test.spf <- marxan(spf=spf,puvsp="puvsp.dat")
#  test.spf <- marxan(spf=spf,puvsp="puvsp.dat")
  

  
  # Generate a plot of the requested variable against mpg and only 
  # include outliers if requested
  output$cfdPlot <- renderPlot({
    if(input$mrun == 0) {
      print("Run Marxan")
      return(NULL)
    }
    
    test.spf <- my.data()$cspf
#    spf <- current.spf(input$range[1],input$range[2],input$iterations) 
#    test.spf <- marxan(spf=spf,puvsp="puvsp.dat")        
    offs <- 1
    min.max <- vector()
    for (ii in 1:(length(test.spf$sums)-1)){
#      min.max <- c(min.max, test.spf$sums[[offs+ii]]$Cost-min(test.spf$sums[[offs+ii]]$Cost))
      min.max <- c(min.max, test.spf$sums[[offs+ii]]$Cost/min(test.spf$sums[[offs+ii]]$Cost)*100)
    }
    df <- data.frame(x=min.max,
                     g=gl((length(test.spf$sums)-1), length(test.spf$sums[[offs+1]]$Cost),labels = names(test.spf$sums)[-1]))
    p <- ggplot(df, aes(x, colour = g)) + stat_ecdf() +
      scale_colour_brewer(palette="Set1") +
      xlab("Solution cost (% of best solution)") +
      ylab("Cumilative # of solutions") +
      scale_y_continuous(breaks=seq(0,1,0.2),
                         labels=c(sprintf("%d",seq(0,length(test.spf$sums[[offs+1]]$Cost),length(test.spf$sums[[offs+1]]$Cost)/5)))) +
#      theme(legend.position=c(0.9, 0.2)) +
      theme(legend.title=element_blank()) + #remove legend title
      theme(legend.background = element_rect(color= "black", fill="white", size=.1, linetype="solid"),
            legend.key=element_blank(),
            legend.text.align=0) +
      theme(axis.line = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank()) +
      theme(axis.text = element_text(colour='black'))
    print (p)
  })


  output$niterPlot <- renderPlot({
    
    if(input$mrun == 0) {
      print("Run Marxan")
      return(NULL)
    }
    test.spf <- my.data()$cspf
    #    spf <- current.spf(input$range[1],input$range[2],input$iterations) 
    #    test.spf <- marxan(spf=spf,puvsp="puvsp.dat")        
    offs <- 1
    min.max <- vector()
    for (ii in 1:(length(test.spf$sums)-1)){
#      min.max <- c(min.max, test.spf$sums[[offs+ii]]$Score-min(test.spf$sums[[offs+ii]]$Score))
      min.max <- c(min.max, test.spf$sums[[offs+ii]]$Score/min(test.spf$sums[[offs+ii]]$Score)*100)
    }
    df <- data.frame(x=min.max,
                     g=gl((length(test.spf$sums)-1), length(test.spf$sums[[offs+1]]$Score),labels = names(test.spf$sums)[-1]))
    p <- ggplot(df, aes(x, colour = g)) + stat_ecdf() +
      xlab("Solution Score (% of best solution)") +
      ylab("Cumilative # of solutions") +
      scale_y_continuous(breaks=seq(0,1,0.2),
                         labels=c(sprintf("%d",seq(0,length(test.spf$sums[[offs+1]]$Score),length(test.spf$sums[[offs+1]]$Score)/5)))) +
      #      theme(legend.position=c(0.9, 0.2)) +
      theme(legend.title=element_blank()) + #remove legend title
      theme(legend.background = element_rect(color= "black", fill="white", size=.1, linetype="solid"),
            legend.key=element_blank(),
            legend.text.align=0) +
      theme(axis.line = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank()) +
      theme(axis.text = element_text(colour='black'))
    print (p)
  })

  output$BLMPlot <- renderPlot({

    if(input$mrun == 0) {
      print("Run Marxan")
      return(NULL)
    }
  ##--------------------------------###
  ## Setup table for plotting
  ##--------------------------------###
    
    test.spf <- my.data()$cspf
    test.spf$sums <- test.spf$sums[names(test.spf$sums)]
    
    sumtest.spf <- data.frame(runname=I("a"), Score=0, Cost=0, PUs=0, B_length=0, Penalty=0, 
                              Shortfall=0, Missing_Values=0, MPM=0)
    Cost <- Score <- vector()
    
    for (ff in 2:(length(test.spf$sums))) {
      sumtest.spf[ff-1,1] <- as.character(names(test.spf$sums)[ff])
      
      sumtest.spf[ff-1,2] <- mean(test.spf$sums[[ff]]$Score)
      sumtest.spf[ff-1,3] <- mean(test.spf$sums[[ff]]$Cost)
      sumtest.spf[ff-1,4] <- mean(test.spf$sums[[ff]]$Planning_Units)
      sumtest.spf[ff-1,5] <- mean(test.spf$sums[[ff]]$Connectivity)
      sumtest.spf[ff-1,6] <- mean(test.spf$sums[[ff]]$Penalty)
      sumtest.spf[ff-1,7] <- mean(test.spf$sums[[ff]]$Shortfall)
      
      sumtest.spf[ff-1,8] <- sum(test.spf$sums[[ff]]$Missing_Values)
      sumtest.spf[ff-1,9] <- sum(test.spf$sums[[ff]]$MPM)
      Cost <- c(Cost,test.spf$sums[[ff]]$Cost)
      Score <- c(Score,test.spf$sums[[ff]]$Score)
      
    }
    sumtest.spf <- sumtest.spf[sumtest.spf$runname!="a",]   
    
    sumtest.spf$BLM <-rep(test.spf$paras$blm[1:test.spf$paras$blm.ln],
                          each=(test.spf$paras$spf.ln[1]*test.spf$paras$nreps.ln*test.spf$paras$niter.ln))
    
    sumtest.spf$SPF <-rep(rep(test.spf$paras$spf[1:test.spf$paras$spf.ln],
                              each=(test.spf$paras$nreps.ln*test.spf$paras$niter.ln)),test.spf$paras$blm.ln)
    sumtest.spf$Nreps <-rep(rep(test.spf$paras$nreps[1:test.spf$paras$nreps.ln],
                                each=(test.spf$paras$niter.ln)),test.spf$paras$blm.ln*test.spf$paras$spf.ln)
    
    sumtest.spf$Niter <-rep(test.spf$paras$niter,length(sumtest.spf[,1])/test.spf$paras$niter.ln) 
    ##--------------------------------###
    ## plotting
    ##--------------------------------###
    #    paras<- data.frame(spf=length(spf),blm=length(blm),
    #                       nreps=length(nreps),niter=length(nitns))
    
    #p.spf <- test.spf$paras$blm.ln[1] * test.spf$paras$nreps.ln[1] * test.spf$paras$niter.ln[1]
    #p.nreps <- test.spf$paras$blm.ln[1] * test.spf$paras$spf.ln[1] * test.spf$paras$niter.ln[1]
    #p.niter <- test.spf$paras$blm.ln[1] * test.spf$paras$spf.ln[1] * test.spf$paras$nreps.ln[1]
    p.blm <- test.spf$paras$spf.ln * test.spf$paras$nreps.ln[1] * test.spf$paras$niter.ln
    blm.ln <- length(sumtest.spf[,1])/p.blm
    
    
    #blm=as.factor(rep(seq(1:blm.ln),each=paras)),
    #blm=as.factor(rep(seq(1:paras),blm.ln)),
    
    df2 <- data.frame(blm=sumtest.spf$BLM,
                      scen=sprintf("SPF:%s_Nreps:%s_Niter:%s",sumtest.spf$SPF,sumtest.spf$Nreps,sumtest.spf$Niter),
                      x=sumtest.spf$B_length, y=sumtest.spf$Cost, runname=sumtest.spf$runname)
    
    p<- ggplot(subset(df2), aes(x=x,y=y,color=scen)) + 
      geom_point()+
      #  geom_point(aes(x=x,y=y,color=scen)) +
      geom_text(aes(label=blm),hjust=1, vjust=1) +
      geom_path() +  
      #geom_line() +
      xlab("Boundary length") +
      ylab("Cost") +
      
      theme(legend.position=c(0.8, 0.9)) +
      theme(legend.title=element_blank()) + #remove legend title
      theme(legend.background = element_rect(color= "black", fill="white", size=.1, linetype="solid"),
            legend.key=element_blank(),
            legend.text.align=0) +
      
      theme(axis.line = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank()) +
      theme(axis.text = element_text(colour='black')) #+ 
    
    
    print (p)
  })
  
  output$outputmap <- renderPlot({
    if(input$mrun == 0) {
      print("Run Marxan")
      return(NULL)
    }    

### just for now to solve this quickly    
    test.spf <- my.data()$cspf
    test.spf$sums <- test.spf$sums[names(test.spf$sums)]
    
    sumtest.spf <- data.frame(runname=I("a"), Score=0, Cost=0, PUs=0, B_length=0, Penalty=0, 
                              Shortfall=0, Missing_Values=0, MPM=0)
    Cost <- Score <- vector()
    
    for (ff in 2:(length(test.spf$sums))) {
      sumtest.spf[ff-1,1] <- as.character(names(test.spf$sums)[ff])
      
      sumtest.spf[ff-1,2] <- mean(test.spf$sums[[ff]]$Score)
      sumtest.spf[ff-1,3] <- mean(test.spf$sums[[ff]]$Cost)
      sumtest.spf[ff-1,4] <- mean(test.spf$sums[[ff]]$Planning_Units)
      sumtest.spf[ff-1,5] <- mean(test.spf$sums[[ff]]$Connectivity)
      sumtest.spf[ff-1,6] <- mean(test.spf$sums[[ff]]$Penalty)
      sumtest.spf[ff-1,7] <- mean(test.spf$sums[[ff]]$Shortfall)
      
      sumtest.spf[ff-1,8] <- sum(test.spf$sums[[ff]]$Missing_Values)
      sumtest.spf[ff-1,9] <- sum(test.spf$sums[[ff]]$MPM)
      Cost <- c(Cost,test.spf$sums[[ff]]$Cost)
      Score <- c(Score,test.spf$sums[[ff]]$Score)
      
    }
    sumtest.spf <- sumtest.spf[sumtest.spf$runname!="a",]   
    
    sumtest.spf$BLM <-rep(test.spf$paras$blm[1:test.spf$paras$blm.ln],
                          each=(test.spf$paras$spf.ln[1]*test.spf$paras$nreps.ln*test.spf$paras$niter.ln))
    
    sumtest.spf$SPF <-rep(rep(test.spf$paras$spf[1:test.spf$paras$spf.ln],
                              each=(test.spf$paras$nreps.ln*test.spf$paras$niter.ln)),test.spf$paras$blm.ln)
    sumtest.spf$Nreps <-rep(rep(test.spf$paras$nreps[1:test.spf$paras$nreps.ln],
                                each=(test.spf$paras$niter.ln)),test.spf$paras$blm.ln*test.spf$paras$spf.ln)
    
    sumtest.spf$Niter <-rep(test.spf$paras$niter,length(sumtest.spf[,1])/test.spf$paras$niter.ln) 
    ##--------------------------------###
    ## plotting
    ##--------------------------------###
    #    paras<- data.frame(spf=length(spf),blm=length(blm),
    #                       nreps=length(nreps),niter=length(nitns))
    
    #p.spf <- test.spf$paras$blm.ln[1] * test.spf$paras$nreps.ln[1] * test.spf$paras$niter.ln[1]
    #p.nreps <- test.spf$paras$blm.ln[1] * test.spf$paras$spf.ln[1] * test.spf$paras$niter.ln[1]
    #p.niter <- test.spf$paras$blm.ln[1] * test.spf$paras$spf.ln[1] * test.spf$paras$nreps.ln[1]
    p.blm <- test.spf$paras$spf.ln * test.spf$paras$nreps.ln[1] * test.spf$paras$niter.ln
    blm.ln <- length(sumtest.spf[,1])/p.blm
    
    
    #blm=as.factor(rep(seq(1:blm.ln),each=paras)),
    #blm=as.factor(rep(seq(1:paras),blm.ln)),
    
    df2 <- data.frame(blm=sumtest.spf$BLM,
                      scen=sprintf("SPF:%s_Nreps:%s_Niter:%s",sumtest.spf$SPF,sumtest.spf$Nreps,sumtest.spf$Niter),
                      x=sumtest.spf$B_length, y=sumtest.spf$Cost, runname=sumtest.spf$runname)
### END: just for now to solve this quickly    

#    sPulayer <- paste(getwd(),"/pulayer/pulayer1km.shp",sep="")
#    pulayer <- readShapePoly(sPulayer)
#    pulayer <<- SpatialPolygons2PolySet(pulayer)
#    pu_table <<- read.dbf(paste(getwd(),"/pulayer/pulayer1km.dbf",sep=""))    
    
    click = input$clicknmds
    if (!is.null(click)){
      
      iClosestPoint <- eucdist(click$x,click$y,df2[,3:4])
#      sSolution <- row.names(sol.mds$points)[iClosestPoint]
#      iSolution <- as.integer(substr(sSolution,2,nchar(sSolution)))
      iSolution <- as.integer(unlist(test.spf$ssoln[iClosestPoint + 1]))
      
      values <- test.spf$ssoln[iClosestPoint + 1]
#      values <- sqldf(paste("SELECT SOLN",iSolution," from pu_table",sep=""))
#GREENRAMP NREP NEEDS ADJUSTMENT ONCE NREP IS VARIABLE
      greenramp <- colorRampPalette(c("white","green"))(sumtest.spf$Nreps[1]+1)
      colours <- rep(greenramp[1],nrow(values))
      for (j in 1:nrow(values))
      {
        colours[j] <- greenramp[values[j,]+1]
      }
      plotPolys(pulayer,col=colours,axes=FALSE,border=NA,cex.lab=0.1,cex.axis=0.1)
      #return(spplot(pulayer[paste("SOLN",iSolution,sep="")],col.regions= greenramp,col="transparent"))
    }
  })

  output$clickcoordsnmds = renderText({
    ### just for now to solve this quickly    
    test.spf <- my.data()$cspf
    test.spf$sums <- test.spf$sums[names(test.spf$sums)]
    
    sumtest.spf <- data.frame(runname=I("a"), Score=0, Cost=0, PUs=0, B_length=0, Penalty=0, 
                              Shortfall=0, Missing_Values=0, MPM=0)
    Cost <- Score <- vector()
    
    for (ff in 2:(length(test.spf$sums))) {
      sumtest.spf[ff-1,1] <- as.character(names(test.spf$sums)[ff])
      
      sumtest.spf[ff-1,2] <- mean(test.spf$sums[[ff]]$Score)
      sumtest.spf[ff-1,3] <- mean(test.spf$sums[[ff]]$Cost)
      sumtest.spf[ff-1,4] <- mean(test.spf$sums[[ff]]$Planning_Units)
      sumtest.spf[ff-1,5] <- mean(test.spf$sums[[ff]]$Connectivity)
      sumtest.spf[ff-1,6] <- mean(test.spf$sums[[ff]]$Penalty)
      sumtest.spf[ff-1,7] <- mean(test.spf$sums[[ff]]$Shortfall)
      
      sumtest.spf[ff-1,8] <- sum(test.spf$sums[[ff]]$Missing_Values)
      sumtest.spf[ff-1,9] <- sum(test.spf$sums[[ff]]$MPM)
      Cost <- c(Cost,test.spf$sums[[ff]]$Cost)
      Score <- c(Score,test.spf$sums[[ff]]$Score)
      
    }
    sumtest.spf <- sumtest.spf[sumtest.spf$runname!="a",]   
    
    sumtest.spf$BLM <-rep(test.spf$paras$blm[1:test.spf$paras$blm.ln],
                          each=(test.spf$paras$spf.ln[1]*test.spf$paras$nreps.ln*test.spf$paras$niter.ln))
    
    sumtest.spf$SPF <-rep(rep(test.spf$paras$spf[1:test.spf$paras$spf.ln],
                              each=(test.spf$paras$nreps.ln*test.spf$paras$niter.ln)),test.spf$paras$blm.ln)
    sumtest.spf$Nreps <-rep(rep(test.spf$paras$nreps[1:test.spf$paras$nreps.ln],
                                each=(test.spf$paras$niter.ln)),test.spf$paras$blm.ln*test.spf$paras$spf.ln)
    
    sumtest.spf$Niter <-rep(test.spf$paras$niter,length(sumtest.spf[,1])/test.spf$paras$niter.ln) 
    ##--------------------------------###
    ## plotting
    ##--------------------------------###
    #    paras<- data.frame(spf=length(spf),blm=length(blm),
    #                       nreps=length(nreps),niter=length(nitns))
    
    #p.spf <- test.spf$paras$blm.ln[1] * test.spf$paras$nreps.ln[1] * test.spf$paras$niter.ln[1]
    #p.nreps <- test.spf$paras$blm.ln[1] * test.spf$paras$spf.ln[1] * test.spf$paras$niter.ln[1]
    #p.niter <- test.spf$paras$blm.ln[1] * test.spf$paras$spf.ln[1] * test.spf$paras$nreps.ln[1]
    p.blm <- test.spf$paras$spf.ln * test.spf$paras$nreps.ln[1] * test.spf$paras$niter.ln
    blm.ln <- length(sumtest.spf[,1])/p.blm
    
    
    #blm=as.factor(rep(seq(1:blm.ln),each=paras)),
    #blm=as.factor(rep(seq(1:paras),blm.ln)),
    
    df2 <- data.frame(blm=sumtest.spf$BLM,
                      scen=sprintf("SPF:%s_Nreps:%s_Niter:%s",sumtest.spf$SPF,sumtest.spf$Nreps,sumtest.spf$Niter),
                      x=sumtest.spf$B_length, y=sumtest.spf$Cost, runname=sumtest.spf$runname)
    ### END: just for now to solve this quickly    
    
    sPulayer <- paste(getwd(),"/pulayer/pulayer1km.shp",sep="")
    pulayer <- readShapePoly(sPulayer)
    pulayer <<- SpatialPolygons2PolySet(pulayer)
    pu_table <<- read.dbf(paste(getwd(),"/pulayer/pulayer1km.dbf",sep=""))    

    
    click = input$clicknmds
    if (!is.null(click)){
#      iClosestPoint <- eucdist(click$x,click$y,df2[,3:4])
#      sprintf("solution %s", df2$runname[iClosestPoint])
      sprintf("X:%s Y:%s", round(click$x,0),round(click$y,0))
    }
  })  

  output$cadMap <- renderLeaflet({
#    if(input$mrun == 0) {
#      print("Run Marxan")
#      return(NULL)
#    }

    leaflet() %>% setView(lng = -124, lat = 49.1, zoom = 8) %>%
      addTiles() %>% #setView(-93.65, 42.0285, zoom = 4) %>%
      addWMSTiles(
        "http://192.168.1.73:8080/geoserver/CDFCP/wms",
        layers = "Cadaster_CDFCP_Marx",
        options = WMSTileOptions(format = "image/gif", transparent = TRUE),
        attribution = "Richard Schuster © 2015 Arcese Lab"
      )
  })  
#  output$BLMClick <- renderPlot({
    
    ##--------------------------------###
    ## Setup table for plotting
    ##--------------------------------###
    
#    test.spf <- my.data()$cspf
#    test.spf$sums <- test.spf$sums[names(test.spf$sums)]
    
#    sumtest.spf <- data.frame(runname=I("a"), Score=0, Cost=0, PUs=0, B_length=0, Penalty=0, 
#                              Shortfall=0, Missing_Values=0, MPM=0)
#    Cost <- Score <- vector()
    
#    for (ff in 2:(length(test.spf$sums))) {
#      sumtest.spf[ff-1,1] <- as.character(names(test.spf$sums)[ff])
      
#      sumtest.spf[ff-1,2] <- mean(test.spf$sums[[ff]]$Score)
#      sumtest.spf[ff-1,3] <- mean(test.spf$sums[[ff]]$Cost)
#      sumtest.spf[ff-1,4] <- mean(test.spf$sums[[ff]]$Planning_Units)
#      sumtest.spf[ff-1,5] <- mean(test.spf$sums[[ff]]$Connectivity)
#      sumtest.spf[ff-1,6] <- mean(test.spf$sums[[ff]]$Penalty)
#      sumtest.spf[ff-1,7] <- mean(test.spf$sums[[ff]]$Shortfall)
      
#      sumtest.spf[ff-1,8] <- sum(test.spf$sums[[ff]]$Missing_Values)
#      sumtest.spf[ff-1,9] <- sum(test.spf$sums[[ff]]$MPM)
#      Cost <- c(Cost,test.spf$sums[[ff]]$Cost)
#      Score <- c(Score,test.spf$sums[[ff]]$Score)
      
#    }
#    sumtest.spf <- sumtest.spf[sumtest.spf$runname!="a",]   
    
#    sumtest.spf$BLM <-rep(test.spf$paras$blm[1:test.spf$paras$blm.ln],
#                          each=(test.spf$paras$spf.ln[1]*test.spf$paras$nreps.ln*test.spf$paras$niter.ln))
    
#    sumtest.spf$SPF <-rep(rep(test.spf$paras$spf[1:test.spf$paras$spf.ln],
#                              each=(test.spf$paras$nreps.ln*test.spf$paras$niter.ln)),test.spf$paras$blm.ln)
#    sumtest.spf$Nreps <-rep(rep(test.spf$paras$nreps[1:test.spf$paras$nreps.ln],
#                                each=(test.spf$paras$niter.ln)),test.spf$paras$blm.ln*test.spf$paras$spf.ln)
    
#    sumtest.spf$Niter <-rep(test.spf$paras$niter,length(sumtest.spf[,1])/test.spf$paras$niter.ln) 
    ##--------------------------------###
    ## plotting
    ##--------------------------------###
    #    paras<- data.frame(spf=length(spf),blm=length(blm),
    #                       nreps=length(nreps),niter=length(nitns))
    
    #p.spf <- test.spf$paras$blm.ln[1] * test.spf$paras$nreps.ln[1] * test.spf$paras$niter.ln[1]
    #p.nreps <- test.spf$paras$blm.ln[1] * test.spf$paras$spf.ln[1] * test.spf$paras$niter.ln[1]
    #p.niter <- test.spf$paras$blm.ln[1] * test.spf$paras$spf.ln[1] * test.spf$paras$nreps.ln[1]
#    p.blm <- test.spf$paras$spf.ln * test.spf$paras$nreps.ln[1] * test.spf$paras$niter.ln
#    blm.ln <- length(sumtest.spf[,1])/p.blm
    
    
    #blm=as.factor(rep(seq(1:blm.ln),each=paras)),
    #blm=as.factor(rep(seq(1:paras),blm.ln)),
    
#    df2 <- data.frame(blm=sumtest.spf$BLM,
#                      scen=sprintf("SPF:%s_Nreps:%s_Niter:%s",sumtest.spf$SPF,sumtest.spf$Nreps,sumtest.spf$Niter),
#                      x=sumtest.spf$B_length, y=sumtest.spf$Cost, runname=sumtest.spf$runname)
    
#    colbl <- rep(rainbow(p.blm, alpha = 1),test.spf$paras$blm.ln)
#    plot(df2$y~df2$x, col=colbl,
#         xlab="Boundary length",
#         ylab="Cost")
#    for (ii in 1: length(colbl)){
#      xtemp <- df2$x[seq(ii,length(df2[,1]),p.blm)]
#      ytemp <- df2$y[seq(ii,length(df2[,1]),p.blm)]
#      lines(xtemp,ytemp,col=colbl[ii])
#    }
    
#  })

  ###############################
  # Download Results shapefile
  ###############################
  output$downloadSHP <- downloadHandler(
    
    filename = function() {
      paste('CDFCP_cadaster.zip', sep='')
    },
    content = function(file) {
      file.copy(paste(getwd(),"/pulayer/CDFCP_cadaster.zip",sep=""), file)
    }
  )

  output$download_ssoln <- downloadHandler(
    filename = function() {
      paste('Marxan_summary_results-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      ssoln <- my.data()$cspf$ssoln
      best <- my.data()$cspf$best
      names(best) <- sprintf("%s_B",names(best))
      ssoln <- merge(ssoln,best,by.x="ID",by.y="ID_B",sort=F)
      write.csv(ssoln, file, row.names=F)
    }    
  )

  output$download_detail <- downloadHandler(
    filename = function() {
      paste('Marxan_individual_runs-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      comb <- my.data()$cspf$ind.runs
      write.csv(comb, file, row.names=F)
    }    
  )

  observe ({  my.data()$cspf$ssoln
  })
})


