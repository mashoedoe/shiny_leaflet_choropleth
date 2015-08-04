library(jsonlite)
library(rgdal)
library(rgeos)
library(jsonlite)
library(readr)

if (all.equal(dir("TopoJSON/"), c("LocalMunicipalities2011.json",
                                  "Province_New_SANeighbours.json",
                                  "Wards2011.json"))) {
  message("You are in working directory 'jcheng_choropleth3'
          & your topoJSON files are present. CONTINUE...")
} else {
  stop("The working directory is NOT 'jcheng_choropleth3' or
       subdirectory TopoJSON is empty")
}

ward_tj <- read_lines(file="TopoJSON/Wards2011.json")
town_tj <- read_lines(file = "TopoJSON/LocalMunicipalities2011.json")
province_tj <- read_lines(file = "TopoJSON/Province_New_SANeighbours.json")

ward_tj_list <- ward_tj %>% fromJSON(simplifyVector = T)
town_tj_list <- town_tj %>% fromJSON(simplifyVector = T)
province_tj_list <- province_tj %>% fromJSON(simplifyVector = T)

ward_tj_properties <- ward_tj_list$objects$Wards2011$geometries$properties
town_tj_properties <- town_tj_list$objects$LocalMunicipalities2011$geometries$properties
province_tj_properties <- province_tj_list$objects$Province_New_SANeighbours$geometries$properties

#rm(ward_tj_list, town_tj_list, province_tj_list)

ward_tj_properties$ID <- 0:4276
town_tj_properties$ID <- 0:233
province_tj_properties$ID <- 0:8

ward_tj_spd <- readOGR(ward_tj,layer="Wards2011", verbose=F)
town_tj_spd <- readOGR(town_tj,layer="LocalMunicipalities2011",verbose=F)
province_tj_spd <- readOGR(province_tj,layer="Province_New_SANeighbours",verbose=F)

ward_tj_spd <- SpatialPolygonsDataFrame(
  Sr = ward_tj_spd,
  data = ward_tj_properties,
  match.ID = F
)
town_tj_spd <- SpatialPolygonsDataFrame(
  Sr = town_tj_spd,
  data = town_tj_properties,
  match.ID = F
)
province_tj_spd <- SpatialPolygonsDataFrame(
  Sr = province_tj_spd,
  data = province_tj_properties,
  match.ID = F
)

#rm(ward_tj_properties, town_tj_properties, province_tj_properties)

source("R/topoJSON_string_style.R")

ward_density <- as.numeric(topoJSON_property_extract(
  topoJSON_string = ward_tj, property_name = "DENSITY"
))
town_density <- as.numeric(topoJSON_property_extract(
  topoJSON_string = town_tj, property_name = "DENSITY"
))

province_density <- as.numeric(topoJSON_property_extract(
  topoJSON_string = province_tj, property_name = "DENSITY"
))

# Breaks we'll use for coloring
densityBreaks <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
# Construct break ranges for displaying in the legend
densityRanges <- data.frame(
  from = head(densityBreaks, length(densityBreaks)-1),
  to = tail(densityBreaks, length(densityBreaks)-1)
)

bin_quantiles = sapply(X = densityBreaks, FUN = function(x) {length(town_density[town_density < x])/length(town_density)})
round(quantile(x = town_density, probs = bin_quantiles))

midcolor<- marray::maPalette(low="#FD8D3C", high="#FC4E2A",k=3)
colorpal <- marray::maPalette(low="#FFEDA0",mid=midcolor[2], high="#800026", k = 50)
ward_binpal <- colorBin(palette = colorpal, domain=0:max(ward_density),
                        bins = densityBreaks, pretty = FALSE, na.color = "white")
town_binpal <- colorBin(palette = colorpal, domain=0:max(town_density),
                        bins = densityBreaks, pretty = FALSE, na.color = "white")
province_binpal <- colorBin(palette = colorpal, domain=0:max(province_density),
                            bins = densityBreaks, pretty = FALSE, na.color = "white")

ward_tj <- topoJSON_fillColor(
  topoJSON_string = ward_tj,
  last_property = "DENSITY",
  fillColor = ward_binpal(ward_density)#,
  #  weight = 0,
  #  color = "#FFFFFF",
  #  opacity = 0,
  #  fillOpacity = 0.7
)
town_tj <- topoJSON_fillColor(
  topoJSON_string = town_tj,
  last_property = "DENSITY",
  fillColor = town_binpal(town_density)#,
  # weight = 0,
  # color = "#FFFFFF",
  # opacity = 0,
  # fillOpacity = 0.7
)
province_tj <- topoJSON_fillColor(
  topoJSON_string = province_tj,
  last_property = "DENSITY",
  fillColor = province_binpal(province_density)#,
  #weight = 0,
  #color = "#FFFFFF",
  #opacity = 0,
  #fillOpacity = 0.7
)
leaflet() %>% 
  setView(lng = 26, lat = -27, zoom = 6) %>% 
  addTopoJSON(town_tj, weight = 2, color = "#FFFFFF", opacity = 1, dashArray = 3)
