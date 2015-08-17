library(jsonlite)
library(rgdal)
library(rgeos)
library(jsonlite)
library(readr)
library(leaflet)

if (all.equal(dir("TopoJSON/"), c(
    "LocalMunicipalities2011.json","Province_New_SANeighbours.json",
    "Wards_0.10","Wards_0.20","Wards2011.json"
    ))) {
  message("You are in working directory 'shiny_leaflet_choropleth'
          & your topoJSON files are present. CONTINUE...")
} else {
  stop("The working directory is NOT 'shiny_leaflet_choropleth' or
       subdirectory TopoJSON is empty")

ward_tj <- read_lines(file="TopoJSON/Wards2011.json")
town_tj <- read_lines(file = "TopoJSON/LocalMunicipalities2011.json")
province_tj <- read_lines(file = "TopoJSON/Province_New_SANeighbours.json")
province_slice <- province_tj
town_slice <- town_tj

ward_tj_list <- ward_tj %>% fromJSON(simplifyVector = T)
town_tj_list <- town_tj %>% fromJSON(simplifyVector = T)
province_tj_list <- province_tj %>% fromJSON(simplifyVector = T)

ward_tj_properties <- ward_tj_list$objects$Wards2011$geometries$properties
town_tj_properties <- town_tj_list$objects$LocalMunicipalities2011$geometries$properties
province_tj_properties <- province_tj_list$objects$Province_New_SANeighbours$geometries$properties

#rm(ward_tj_list, town_tj_list, province_tj_list)

#ward_tj_properties$ID <- 0:4276
#town_tj_properties$ID <- 0:233
#province_tj_properties$ID <- 0:8

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

ward_tj_no_lines <- topoJSON_fillColor_plus(
    topoJSON_string = ward_tj,
    last_property = "DENSITY",
    fillColor = ward_binpal(ward_density),
      weight = 2,
      color = ward_binpal(ward_density),
      opacity = 0,
      fillOpacity = 0.7
)

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

provinces <- dplyr::select(.data = province_tj_spd@data, ID, PROVINCE)
provinces <- sapply(X = provinces[,2], FUN = sub, pattern = " |-", replacement = "_")

towns <- dplyr::select(.data = town_tj_spd@data, ID, MAP_TITLE)
towns <- sapply(X = towns[,2], FUN = gsub, pattern = " |-", replacement = "_")
# 
# provincial_wards <- NULL
# for (i in 1:9) {
#     provincial_wards[i] <- read_lines(
#         file=paste0("TopoJSON/Wards_0.10/",
#                     provinces[i],"_Wards2011.json")
#     )
# }
provincial_wards <- NULL
for (i in 0:8) {
    provincial_wards[i+1] <- read_lines(
        file=paste0("TopoJSON/Wards_0.10/",i,".json")
    )
}

town_wards <- NULL
for (i in 0:233) {
    town_wards[i+1] <- read_lines(
        paste0("TopoJSON/Wards_0.20/",i,".json")
    )
}

provincial_wards_density <- NULL
for (i in 0:8) {
    provincial_wards_density[[i+1]] <- as.numeric(topoJSON_property_extract(
        topoJSON_string = provincial_wards[i+1], property_name = "DENSITY"
    ))
}
town_wards_density <- NULL
for (i in 0:233) {
    town_wards_density[[i+1]] <- as.numeric(topoJSON_property_extract(
        topoJSON_string = town_wards[i+1], property_name = "DENSITY"
    ))
}

for (i in 0:8) {
    provincial_wards[i+1] <- topoJSON_fillColor(
        topoJSON_string = provincial_wards[i+1],
        last_property = "DENSITY",
        fillColor = ward_binpal(provincial_wards_density[[i+1]])
    )
}

for (i in 0:233) {
    town_wards[i+1] <- topoJSON_fillColor(
        topoJSON_string = town_wards[i+1],
        last_property = "DENSITY",
        fillColor = ward_binpal(town_wards_density[[i+1]])
    )
}

leaflet() %>% 
  setView(lng = 26, lat = -27, zoom = 6) %>% 
  addTopoJSON(ward_tj_no_lines, weight = 2, color = "#FFFFFF", opacity = 1, dashArray = 3)

leaflet() %>% addTiles() %>%
    setView(lng = 26, lat = -27, zoom = 6) %>% 
    addTopoJSON(provincial_wards[9], weight = 2, color = "#FFFFFF", opacity = 1, dashArray = 3)

leaflet() %>% addTiles() %>%
    setView(lng = 26, lat = -27, zoom = 6) %>% 
    addTopoJSON(town_wards[212+1], weight = 2, color = "#FFFFFF", opacity = 1, dashArray = 3)

# saveRDS(object = town_density, file = "R/town_density")
save(list = ls(), file = "R/v1.RData")

# create v2.RData by removing some v1 objects and renaming to ensure MUNICNAME 
# refers to the same variable across Municipal and Town levels
rm(densityRanges, densityBreaks, colorpal, i, midcolor, province_tj_list, 
   town_tj_list, ward_tj_list, provinces, towns, topoJSON_fillColor, 
   topoJSON_fillColor_plus, topoJSON_property_extract)

colnames(
    town_tj_properties
    )[which(colnames(town_tj_properties) == "MUNICNAME")] <- "MUNIC"
colnames(
    town_tj_properties
)[which(colnames(town_tj_properties) == "MAP_TITLE")] <- "MUNICNAME"
colnames(
    town_tj_spd@data
)[which(colnames(town_tj_spd@data) == "MUNICNAME")] <- "MUNIC"
colnames(
    town_tj_spd@data
)[which(colnames(town_tj_spd@data) == "MAP_TITLE")] <- "MUNICNAME"



town_slice <- gsub(x = town_slice, pattern = "MUNICNAME", replacement = "MUNIC")
town_slice <- gsub(x = town_slice, pattern = "MAP_TITLE", replacement = "MUNICNAME")
town_tj <- gsub(x = town_tj, pattern = "MUNICNAME", replacement = "MUNIC")
town_tj <- gsub(x = town_tj, pattern = "MAP_TITLE", replacement = "MUNICNAME")

all.equal(
sort(unique(ward_tj_spd@data$MUNICNAME)),
sort(town_tj_spd@data$MUNICNAME)
)

save(list = ls(), file = "R/v2.RData")
}
