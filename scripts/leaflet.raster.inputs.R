library(sp)
library(raster)
library(spatial.tools)
library(leaflet)

owd <- setwd("pulayer/rast")

files <- list.files(pattern="tif$")
remove <- c ("DollAWM.tif", "DollAWS.tif")
files <- files[!files %in% remove]
strng <- gsub(".tif","",files)

#### feature inputs

rr <- stack(files)
names(rr) <- strng

rr$StC[][rr$StC[] < 0] <- NA
rr$SeqC[][rr$SeqC[] < 0] <- NA


feat.in <- leaflet() %>% addTiles() %>%
  # Base groups
      addProviderTiles("Esri.WorldStreetMap",group = "StreetMap") %>%
      addProviderTiles("Esri.WorldImagery", group = "Aerial") %>%
      addProviderTiles("Stamen.Terrain", group = "Terrain")

  # Overlay groups
  for(ii in 1:length(rr@layers)) {
    pal <- colorNumeric(c('#d7191c','#fdae61','#ffffbf','#abd9e9','#2c7bb6'), rr[[ii]][],
                        na.color = "transparent")
    feat.in <- addRasterImage(feat.in,rr[[ii]], colors=pal, opacity = 0.9, maxBytes = 8 * 1024 * 1024, 
                              group = strng[ii]) %>%
      addLegend(pal = pal, values = rr[[ii]][], title = strng[ii], group = strng[ii], position = "topleft")
  }

  feat.in <-    feat.in %>%
    addLayersControl(
    baseGroups = c("OSM (default)", "Aerial", "Terrain"),
    overlayGroups = strng,
    options = layersControlOptions(collapsed = FALSE)) %>%
    hideGroup(strng) %>%
    addTiles(attribution = sprintf('<h5>Map created on %s via <a href="http://forbasin.forestry.ubc.ca/CDFCP_prioritization/" target="_blank">CDFCP conservation prioritization tool</a> developed by <a href="mailto:mail@richard-schuster.com">Richard Schuster</a> for the <a href="http://arcese.forestry.ubc.ca/marxan-tool-cdfcp/" target="_blank">Aecese Lab</a>.</h5>',Sys.Date())) 
  
setwd(owd)
save.image("RData/leaflet1.RData")
