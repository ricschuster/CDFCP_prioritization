library(sp)
library(raster)
library(spatial.tools)
library(leaflet)

setwd("D:/VirtualBox/shared/UBC_VS/Shiny2/CDFCP.v0.20/pulayer/rast")

files <- list.files(pattern="tif$")
files <- files[c(8,9,1.22,19)]
strng <- gsub(".tif","",files)



pal <- colorNumeric(c('#d7191c','#fdae61','#ffffbf','#abd9e9','#2c7bb6'), c(0,1),
  na.color = "transparent")

#### feature inputs

rr <- stack(files)
names(rr) <- strng

feat.in <- leaflet() %>% addTiles() %>%
  # Base groups
      addProviderTiles("Esri.WorldStreetMap",group = "StreetMap") %>%
      addProviderTiles("Esri.WorldImagery", group = "Aerial") %>%
      addProviderTiles("Stamen.Terrain", group = "Terrain")

  # Overlay groups
  for(ii in 1:length(rr@layers))
    feat.in <- addRasterImage(feat.in,rr[[ii]], colors=pal, opacity = 0.9, maxBytes = 8 * 1024 * 1024, group = names(rr)[ii])

  feat.in <-   addLegend(feat.in, pal = pal, values = c(0,1),
    title = "Value range") %>%

    addLayersControl(
    baseGroups = c("OSM (default)", "Aerial", "Terrain"),
    overlayGroups = names(rr),
    options = layersControlOptions(collapsed = FALSE)
  )

