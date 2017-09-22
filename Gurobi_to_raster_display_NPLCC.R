library(raster)
library(leaflet)

setwd("D:/Virtual_Box/shared/UBC_VS/Gurobi_NPLCC_tiny/pulayer")

r <- raster("NPLCC_rast.tif")
rv <- getValues(r)

setwd("../out")

res <- round(runif(sum(!is.na(rv)),0,10),0)

rv[!is.na(rv)] <- res

r[] <- rv

pal <- colorNumeric(c('#d7191c','#fdae61','#ffffbf','#abd9e9','#2c7bb6'), values(r),
#pal <- colorNumeric(c("#FFFFCC", "#41B6C4", "#0C2C84"), values(r),
  na.color = "transparent")

map <- leaflet() %>% addTiles() %>%
  # Base groups
  addTiles(group = "OSM (default)") %>%
  addProviderTiles("MapQuestOpen.Aerial", group = "Aerial") %>%
  addProviderTiles("Stamen.Terrain", group = "Terrain") %>%

  # Overlay groups
  addRasterImage(r, colors = pal, opacity = 0.9, maxBytes = 8 * 1024 * 1024, group = "score") %>%

  addLayersControl(
    baseGroups = c("OSM (default)", "Aerial", "Terrain"),
    overlayGroups = c("score"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  addLegend(pal = pal, values = values(r),
    title = "score")

map
