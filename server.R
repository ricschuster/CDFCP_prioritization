##################################################################
##################################################################
## Shiny App for CDFCP priotirization ILP
##
## Author: Richard Schuster (mail@richard-schuster.com)
## 12 Novemeber 2016
##
## v0.20
##    - based on NPLCC.v0.13
##    - included FISH and HERP
## v0.22
##    - Output map working now
##################################################################
##################################################################


# Define server logic 
shinyServer(function(input, output, session) {
  system(sprintf("touch %s/restart.txt",globWD))
  #  setwd("/var/shiny-server/www/examples/calib.shiny.v2/")
  values = reactiveValues(
    hot_feat = feat.lst,
    #hot_tree = tree.lst,
    hot_multi = scen
  )
  #setHot = function(x) values[["hot"]] <<- x
  
  calc = reactive({
    # load initial values
    df1 = values[["hot_feat"]]
    #df2 = values[["hot_tree"]]
    df3 = values[["hot_multi"]]
    
    
    list(feat = df1,
     #    tree = df2,
         multi = df3)
  })

  #######################
  ## Edit Targets
  #######################
  output$hot_feat = renderRHandsontable({
    if (!is.null(input$hot_feat)) {
      DF = hot_to_r(input$hot_feat)
      values[["hot_feat"]] = DF      
    } else if (!is.null(values[["hot_feat"]])) {
      DF = values[["hot_feat"]]
    }

    #prevent rhandson from adding rows when user drags values
    if(nrow(DF) > l.feat.lst){
      DF <- DF[1:l.feat.lst,]
      values[["hot_feat"]] = DF      
    }
    
    #setHot(DF)
    rhandsontable(DF, readOnly = T) %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
      hot_col(c("Percent"), readOnly = FALSE) %>%
      hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
      #        hot_validate_numeric(col = 2, min = 0, max = 1.0) #%>%
      hot_cols(renderer = "
         function (instance, td, row, col, prop, value, cellProperties) {
           Handsontable.renderers.TextRenderer.apply(this, arguments);
           if (col == 1 && (value > 100 || value < 0)) {
            td.style.background = 'red';
           }
         }")
  })

  #######################
  ## Edit Trees
  #######################
#  output$hot_tree = renderRHandsontable({
#    if (!is.null(input$hot_tree)) {
#      DF = hot_to_r(input$hot_tree)
#      values[["hot_tree"]] = DF
#    } else if (!is.null(values[["hot_tree"]])) {
#      DF = values[["hot_tree"]]
#    }
    
#    DF$include <- logical
    #setHot(DF)
#    rhandsontable(DF, readOnly = T) %>%
#      hot_table(highlightCol = TRUE, highlightRow = TRUE, contextMenu = FALSE) %>%
#      hot_col(c("include"), readOnly = FALSE) #%>%
      #        hot_validate_numeric(col = 2, min = 0, max = 1.0) #%>%
      #hot_cols(renderer = "
      #   function (instance, td, row, col, prop, value, cellProperties) {
      #     Handsontable.renderers.TextRenderer.apply(this, arguments);
      #     if (col == 1 && (value > 1.0 || value < 0.0)) {
      #      td.style.background = 'red';
      #     }
      #   }")
#  })

  #######################
  ## Multiple Scenarios
  #######################
  output$hot_multi = renderRHandsontable({
    if (!is.null(input$hot_multi)) {
      DF = hot_to_r(input$hot_multi)
      values[["hot_multi"]] = DF  
    } else if (!is.null(input$scen_file)){
      DF = read.csv(input$scen_file$datapath,stringsAsFactors =F)
      values[["hot_multi"]] = DF  
    } else if (!is.null(values[["hot_multi"]])) {
      DF = values[["hot_multi"]]
    }
    
    rhandsontable(DF, readOnly = F) %>%
#      hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
      hot_table() %>%
#      hot_col(col = "time", type = "dropdown", source = c("curr")) %>%
      hot_col(col = "cost", type = "dropdown", source = c("dollar","area","human")) %>%
      hot_col(col = "protected", type = "dropdown", source = c("locked","avail")) %>%
      hot_validate_numeric(col = "maxRoadDns",  min = 0, max = 20) %>%
      hot_validate_numeric(col = "minPropSz",  min = 0, max = 10) %>%
      hot_validate_numeric(col = "maxAgrDns",  min = 0, max = 1) %>%
      hot_cols(renderer = "
         function (instance, td, row, col, prop, value, cellProperties) {
           Handsontable.renderers.TextRenderer.apply(this, arguments);
           if (col > 5 && (value > 100 || value < 0)) {
            td.style.background = 'red';
           }
         }")
      #hot_col(c("include"), readOnly = FALSE) 
  })
  
  #######################
  ## Gurobi reactive
  #######################
  my.data <- reactive({ 
    # Don't do anything until after the first button push.
    input$mrun
    # Note that just by virtue of checking the value of input$recalcButton,
    # we're now going to get called whenever it is pushed.    
    if(input$mrun == 0)
      return(NULL)    
    
    return(isolate({

      if (input$MultiScen == FALSE) {
        feat.temp <- calc()$feat
        scnm <- paste0(substr(input$cost,1,1),substr(input$protected,1,1))#input$FTcutoff)
        scen[1,] <- c(scnm,input$cost,input$protected,input$RoadD,input$AreaHa,input$AgrD,feat.temp$Percent)
#        scen[1,] <- c(scnm,input$time,input$cost,input$protected,input$FTcutoff,feat.temp$Percent)
      } else {
        scen <- calc()$multi
      }
      
      #debug
      #write.csv(scen,"./output/scenarios.csv",row.names = F)
      
      progress <- Progress$new(session)
      progress$set(message = 'Setting up Analysis inputs', detail = "Please be patient...", value = 0.01)

      #setup output frames
      sel.fr <- data.frame(id=cost[["dollar"]]$id)
      sel.fr.rast <- data.frame(id=idx.r.val[!is.na(idx.r.val)])
      res.fr <- data.frame(scen=character(),
                           #time=character(),
                           cost=character(),
                           protected=character(),
                           maxRoadDns=numeric(),
                           minPropSz=numeric(),
                           maxAgrDns=numeric(),
                          # FTcutoff=integer(),
                           status=character(),runtime=numeric(),cost_out=numeric(),
                           cost_dollar=numeric(),
                           area=numeric(),stringsAsFactors=F)

      in_col <- ncol(res.fr)
      for(kk in (in_col+1):(in_col+ncol(puvsf[["curr"]])-1))
        res.fr[,kk] <- numeric()	
      names(res.fr)[(in_col+1):ncol(res.fr)] <- names(puvsf[["curr"]])[-1]      

      in_col <- ncol(res.fr)
      for(kk in (in_col+1):(in_col+ncol(puvsf[["curr"]])-1))
        res.fr[,kk] <- numeric()
      names(res.fr)[(in_col+1):ncol(res.fr)] <- paste0(names(puvsf[["curr"]])[-1],"_Tar")

      #scenario loop
      for (ii in 1:nrow(scen)){

        pu_temp <- cost[[scen$cost[ii]]]
        
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
        
        puvsfeat.temp <- puvsf[["curr"]]
        #prepare feat for cutoff
       # if (scen$FTcutoff[ii] > 0) {
          #colMax <-  sapply(puvsfeat.temp[,-1], max, na.rm = TRUE)
          #puvsfeat.temp[,-1] <- as.matrix(puvsfeat.temp[,-1]) %*% diag(1/colMax)
          #      FTcutoff <- 0.7
       #   puvsfeat.temp[puvsfeat.temp < (as.numeric(scen$FTcutoff[ii])/100)] <- 0
       # }
        
        #feat.temp <- sprintf("NPLCC_comm_feature_input_%s.csv",scale_temp)
        feat.temp <- data.frame(id=seq(1,ncol(puvsfeat.temp[,-1])),
                                Percent=as.numeric(unlist(scen[ii,(scen_col+1):ncol(scen)])),
                                name=names(puvsfeat.temp[,-1]),
                                stringsAsFactors =F)
        
        #debug
        #write.csv(feat.temp,sprintf("./output/feat.temp_%s.csv",ii),row.names = F)
        
        progress$set(message = 'Calculation in progress', detail = sprintf("Scenario %s/%s",ii,nrow(scen)), 
                     value = round(ii/nrow(scen)*0.8,1))
        
        result <- fit.gurobi(pu=pu_temp, puvsfeat=puvsfeat.temp, feat=feat.temp)      

        if (input$MultiScen == FALSE) {
          scnm <- paste0(substr(as.character(scen$cost[ii]),1,1),
                       substr(as.character(scen$protected[ii]),1,1))
#                       as.numeric(scen$FTcutoff[ii]))
        } else {
          scnm <- scen$scenario[ii]
        }  
        
        sel.fr <- cbind(sel.fr,result$x)

        #sel.fr.rast
        tmp.fr <- data.frame(cad_id=sel.fr[,1],xx=result$x)
        sel.cad <- tmp.fr$cad_id[tmp.fr$xx == 1]
        sel.1ha <- unique(cad_1ha_isect$id[cad_1ha_isect$cad_id %in% sel.cad])
        tmp.x <- rep(0,length(sel.fr.rast[,1]))
        tmp.x[sel.fr.rast$id %in% sel.1ha] <- 1
        sel.fr.rast <- cbind(sel.fr.rast,tmp.x)
        rm(tmp.x)

        names(sel.fr)[ncol(sel.fr)] <- names(sel.fr.rast)[ncol(sel.fr.rast)] <- scnm

        cst.tmp <- cost$dollar$cost
        if(scen$protected[ii] == "locked"){
          cst.tmp[pu_temp$status ==2] <- 0
        }
        cst_doll <- round(sum(cst.tmp[result$x>0]),0)

        ## CALC HERE round(sum(cost$dollar$cost[result$x>0]),0)
        res.fr[ii,] <- c(scnm, 
                         #scen$time[ii],
                         scen$cost[ii],
                         scen$protected[ii],
                         scen$maxRoadDns[ii],
                         scen$minPropSz[ii],
                         scen$maxAgrDns[ii],
#                         scen$FTcutoff[ii],
                         result$status,
                         round(result$runtime,0), 
                         round(result$objval,0),
                         cst_doll,
                         round(sum(result$x>0)/length(result$x)*100,2),
                         round(colSums(puvsfeat.temp[result$x>0,-1])/
                         colSums(puvsfeat.temp[,-1])*100,2),
                         feat.temp$Percent
                         ) 
        rm(cst.tmp,cst_doll)
      }
      
      ################################################
      ##create rasters
      ################################################
      progress$set(message = 'Post processing', detail = "This will take a few mins...", value = 0.9)
      
      if (raster.on){
        r <- in.raster
        rv <- getValues(r)
        ind.r.v <- data.frame(id=idx.r.val[!is.na(idx.r.val)])

        res <- join(ind.r.v,sel.fr.rast,by="id")

        rout <- list()
        for(ii in 2:ncol(res)){

          rv[!is.na(rv)] <- res[,ii]

          r[] <- rv
          rout[[ii-1]] <- r
        }
        rst <- stack(rout)
        names(rst) <- names(res)[-1]

        #leaflet rasters
        rL <- in.rasterL
        rvL <- getValues(rL)
        ind.r.vL <- data.frame(id=idx.r.valL[!is.na(idx.r.valL)])

        resL <- join(ind.r.vL,sel.fr.rast,by="id")

        routL <- list()
        for(ii in 2:ncol(resL)){

          rvL[!is.na(rvL)] <- resL[,ii]

          rL[] <- rvL
          routL[[ii-1]] <- rL
        }
        rstL <- stack(routL)
        names(rstL) <- names(resL)[-1]

        rlist <- list(sel.fr=sel.fr,res.fr=res.fr,rst=rst,rstL=rstL)

      } else {
        rlist <- list(sel.fr=sel.fr,res.fr=res.fr,rst=NULL,rstL=NULL)
      }
      progress$set(value = 1)
      progress$close() 
      
      return(rlist)
    }))

  })
  
  observe ({  my.data()
  })
  
  # Return the formula text for printing as a caption
  output$caption <- renderText({
    "Marxan results"
  })

  output$cadMap <- renderLeaflet({
    if(input$mrun == 0) {
      #print("Run Marxan")
      return(NULL)
    }

    
    #[[1]] for now, should allow for multiple eventually
    rst <- my.data()$rstL
    
    pal <- colorFactor(c('#d7191c','#2c7bb6'),domain=factor(values(rst[[1]])),
                        #pal <- colorNumeric(c("#FFFFCC", "#41B6C4", "#0C2C84"), values(r),
                        na.color = "transparent")
    
    outl <- leaflet() %>% addTiles() %>%
      # Base groups
      addProviderTiles("Esri.WorldStreetMap",group = "StreetMap") %>%
      addProviderTiles("Esri.WorldImagery", group = "Aerial") %>%
      addProviderTiles("Stamen.Terrain", group = "Terrain")# %>%
      

      # Overlay groups
      for(ii in 1:length(rst@layers))
        outl <- addRasterImage(outl,rst[[ii]], colors=pal, opacity = 0.9, 
                               maxBytes = 8 * 1024 * 1024, group = names(rst)[ii], project = FALSE)
    
        outl <- addLegend(outl, pal = pal, values = factor(values(rst[[1]])),labels=c("no","yes"),
                              title = "selected") %>%
        addLayersControl(
          baseGroups = c("StreetMap", "Aerial", "Terrain"),
          overlayGroups = names(rst),
          options = layersControlOptions(collapsed = FALSE)
          )

    outl    
            # Overlay groups
    #prog$set(value = 1)
    
    
    # end individual run attribute table
    ########################################
    #prog$close()     
  })  
  
  output$InMap <- renderLeaflet({
    feat.in
  })  
  
  output$TreeMap <- renderLeaflet({
    tree_curr
  })  

    
  output$TreeMapTool <- renderLeaflet({
    if(input$tree.update == 0) {
      tin_curr
    }
    
  })  

  ###############################
  # Summary Table + Download Results raster
  ###############################
  output$summary <- renderTable({ # to display in the "Summary" tab
    if(input$mrun == 0) {
      return(data.frame(Output="You need to run the prioritization first"))
    }
    
    my.data()$res.fr
    #data.frame(t(my.data()$res.fr))

  })

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
      paste('CDFCP_summary_results-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      write.csv(my.data()$res.fr, file, row.names=F)
    }
  )

  output$download_selfr <- downloadHandler(

    filename = function() {
      paste('CDFCP_property_selection-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      write.csv(my.data()$sel.fr, file, row.names=F)
    }
  )

  #Tabsets
  output$tabsets <- renderUI({
    tabs <- list(NULL)
    if(input$MultiScen == TRUE){
      tabs[[1]] <- tabPanel("Scenario List", rHandsontableOutput("hot_multi",width="100%",height="500px"))
      ii <- 1
    } else {
      tabs[[1]] <- tabPanel("Edit Target", rHandsontableOutput("hot_feat"))
      #tabs[[2]] <- tabPanel("Edit Trees", rHandsontableOutput("hot_tree"))
      ii <- 1
    }
    tabs[[ii+1]] <- tabPanel("Input Layers",leafletOutput("InMap",height=900))
    #tabs[[ii+2]] <- tabPanel("Tree Layers",leafletOutput("TreeMap",height=1000))
    #tabs[[ii+3]] <- tabPanel("Tree Community",leafletOutput("TreeMapTool",height=1000))
    tabs[[ii+2]] <- tabPanel("Results + Download",
                             helpText(HTML("<h4>Result Summary Table</h4>")),
                             tableOutput("summary"),
                             helpText(HTML("<br>")),
                             helpText(HTML("<h4>Results download (property selection):</h4>")),
                             downloadButton("download_selfr", label = "Property selection"),
                             helpText(HTML("<br>")),
                             helpText(HTML("<h4>Results download (summary of outputs):</h4>")),
                             downloadButton("download_ssoln",label = "Results download"),
                             helpText(HTML("<br>")),
                             helpText(HTML("<h4>Download the entire cadastral fabric:</h4>")),
                             downloadButton("downloadSHP",label = "Results download")
    )
    tabs[[ii+3]] <- tabPanel("Result Map",leafletOutput("cadMap",height=900))
    
    tabs$id <- "tabset2"
    do.call(tabsetPanel, tabs)
    

    
  })  
  

})


