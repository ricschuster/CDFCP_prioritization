
purrr::walk(list.files("R", full.names = TRUE), source)
library(wheretowork)

raster::rasterOptions(tmpdir = here::here("tmpdir"))
proj <- CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs")

#get country specific target value
count_tar <- function(PU = NULL, target = NULL){
  round(cellStats(PU,"sum") / 100 * target, 0)
}

`%notin%` <- Negate(`%in%`)

dat <- "data"
out <- "out"


in.raster <- raster("pulayer/idx.tif") %>%
  projectRaster(crs = proj)

PA_shp <- st_read("pulayer/CPCAD-BDCAPC_Dec2020.gdb", "CPCAD_Dec2020") %>%
  mutate(dummy = 1) %>%
  st_transform(crs = proj)

PA <- fasterize::fasterize(PA_shp, in.raster, field = "dummy") %>%
  mask(., in.raster)
names(PA) <- "Protected_Areas"

PA.val <- PA[]
PA.val[!is.na(in.raster[])] <- ifelse(!is.na(PA.val[!is.na(in.raster[])]), 1, 0)
PA[] <- PA.val

fls <- list.files("pulayer/rast/", pattern = "*.tif$", full.names = TRUE)
rstk <- stack(fls) %>%
  projectRaster(crs = methods::as(sf::st_crs(3857), "CRS"), method = "ngb")



feat_lst <- c("OF",
              "SAV",
              "BETA",
              "SHR",
              "WET")

feat1km <- rstk[[feat_lst]]
names(feat1km) <- c("Old_Forest",
                    "Savannah",
                    "Beta_Diversity",
                    "Shrub",
                    "Wetland")

metadata <- tibble(name = names(feat1km),
                   color = c('viridis','magma','plasma','inferno','mako')
)

theme_data <- feat1km
variable_names <- metadata$name
variable_colors <- metadata$color


include_data <- PA
include_names <- "Protected Areas"
include_colors <- '#006d2c'


weight_lst <- c("DollAWS",
              "HUM",
              "StC",
              "SeqC")

weight1km <- rstk[[weight_lst]]
names(weight1km) <- c("Assessed_value_dollars",
                    "Human_association",
                    "Standing_Carbon",
                    "Carbon_Potential")

wmetadata <- tibble(name = names(weight1km),
                   color = c('inferno','viridis','viridis','viridis')
)

## prepare weight data
# weight_data <- stack(cli1km, huf1km) %>% 
#   projectRaster(crs = methods::as(sf::st_crs(3857), "CRS"), method = "ngb")
weight_data <- weight1km
weight_names <- wmetadata$name
weight_colors <- wmetadata$color


dataset <- new_dataset_from_auto(
  # raster::stack(theme_data, weight_data)
  raster::stack(theme_data, weight_data, include_data)
)



### create themes
features <- lapply(seq_len(raster::nlayers(theme_data)), function(i) {
  #### prepare variable
  if (startsWith(variable_colors[i], "#")) {
    v <- new_variable(
      dataset = dataset,
      index = names(theme_data)[i],
      units = "",
      total = raster::cellStats(theme_data[[i]], "sum"),
      legend = new_categorical_legend(
        values = c(0, 1),
        colors = c("#00000000", variable_colors[i])
      )
      
    )
  } else {
    v <- new_variable_from_auto(
      dataset = dataset,
      index = names(theme_data)[i],
      units = "",
      type = "auto",
      colors = variable_colors[i]
    )
  }
  new_feature(
    name = names(theme_data)[i],
    goal = 0.2,
    current = 0,
    limit_goal = 0,
    visible = i == 1L,
    variable = v
  )
})

theme0 <- new_theme(
  name = "Biodiversity",
  feature = features
)





### create includes
includes <- lapply(seq_len(raster::nlayers(include_data)), function(i) {
  new_include(
    name = include_names[i],
    visible = FALSE,
    variable = new_variable(
      dataset = dataset,
      index = names(include_data)[i],
      units = "",
      total = raster::cellStats(include_data[[i]], "sum"),
      legend = new_manual_legend(
        labels = c("not included", "include"),
        colors = c("#00000000", include_colors[i])
      )
    )
  )
})

### create weights
weights <- lapply(seq_len(raster::nlayers(weight_data)), function(i) {
  #### prepare variable
  if (startsWith(weight_colors[i], "#")) {
    v <- new_variable(
      dataset = dataset,
      index = names(weight_data)[i],
      units = "",
      total = raster::cellStats(weight_data[[i]], "sum"),
      legend = new_categorical_legend(
        values = c(0, 1),
        c("#00000000", weight_colors[i])
      )
    )
  } else {
    v <- new_variable_from_auto(
      dataset = dataset,
      index = names(weight_data)[i],
      units = "",
      type = "auto",
      colors = weight_colors[i]
    )
  }
  #### create weight
  new_weight(name = weight_names[i], variable = v, visible = FALSE)
})

# Exports
## create folders if needed
dir.create(
  paste0(out, "/CDFCP"), recursive = TRUE, showWarnings = FALSE
)

## save project to disk
write_project(
  x = append(theme0,
             append(includes, weights)
             ),
  dataset = dataset,
  name = "CDFCP example",
  path =
    paste0(out, "/CDFCP/CDFCP.yaml"),
  spatial_path =
    paste0(out, "/CDFCP/CDFCP_spatial.tif"),
  attribute_path =
    paste0(out, "/CDFCP/CDFCP_attribute.csv.gz"),
  boundary_path =
    paste0(out, "/CDFCP/CDFCP_boundary.csv.gz"),
  mode = "advanced"
)


# # prioritizr test
# pu <- weight_data[[3]]
# pu[pu < 1] <- NA
# feat <- stack(theme_data)
# 
# p1 <- problem(pu, features = feat) %>%
#   add_min_set_objective() %>%
#   add_relative_targets(0.2) %>%
#   add_cbc_solver()
# s1 <- solve(p1)
