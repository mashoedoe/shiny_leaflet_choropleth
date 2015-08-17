 # set working directory
 if ((stringr::str_sub(getwd(), -24) == "shiny_leaflet_choropleth") == FALSE |
     !file.exists("data_collection.R"))  {
   stop("The working directory is NOT 'shiny_leaflet_choropleth' or 
        data_collection.R is missing")
   } else {
     message("You are in the correct working directory ('shiny_leaflet_choropleth')
              to run this R script & data_collection.R can be sourced. CONTINUE...")
    
    # comment the next line out if you have already collected and saved the data from online sources
    source("data_collection.R")
    
    
    ######################################################################################
    #  Part 1 - adding & calculating data properties to shapefiles imported as spatial   #
    #  polygon dataframes                                                                #
    ######################################################################################
    
    
    
    # load libraries  
    library(dplyr)
    library(rgdal)
    
    #load Shapefiles as s-patial p-olygon d-ataframes to merge with more demographic data
    #before conversion to topoJSON
    ward_spd <- readOGR("SHP/Wards/Wards2011.shp", 
                        layer = "Wards2011", 
                        stringsAsFactors = FALSE)
    ward_spd@data$ID <- 0:(dim(ward_spd)[1]-1)
    town_spd <- readOGR("SHP/LocalMunicipalities2011.shp", 
                        layer = "LocalMunicipalities2011", 
                        stringsAsFactors = FALSE)
    town_spd@data$ID <- 0:(dim(town_spd)[1]-1)
    province_spd <- readOGR("SHP/Province_New_SANeighbours.shp", 
                            layer = "Province_New_SANeighbours", 
                            stringsAsFactors = FALSE)
    province_spd@data$ID <- 0:(dim(province_spd)[1]-1)
    
    ## Some calculations to confirm surface areas of shapes total to expected amounts for South Africa
    # SA_Area_SQkm <- sum(ward_spd@data$Area) # South Africa's surface area in sq kilometers
    # SA_Area_SQdegree_spd <- sum(ward_spd@data$Shape_Area) # South Africa's surface area in sq degrees
    # 4*pi*(180/pi)^2 # earth's total area in sq degrees (for a perfect sphere)
    # 4*pi*6371^2 # earth's total area in sq km (given a mean radius of 6371km)
    # SA_Area_SQdegree_spd/(4*pi*(180/pi)^2)*100 # SA's share of that sq degree surface in %
    # SA_Area_SQkm/510064472*100  # SA's share of earth's total surface in %
    # http://www.ngdc.noaa.gov/mgg/global/etopo1_ocean_volumes.html
    # (SA_Area_SQkm/(510064472*.291))*100 # SA's share of land surface on Earth in %
    
    # rename WARD_POP to VOTERS for regisitered voters in South Africa in 2011
    # rename WARD_ID to WARD for later merging with additional demographic data
    ward_spd@data <- rename(ward_spd@data, VOTERS = WARD_POP)
    ward_spd@data <- rename(ward_spd@data, WARD = WARD_ID)
    
    
    # get ward population from Census Data and add it to ward_spd
    ward_population <- read.csv(file = "data/Population2011.csv", header = T)
    ward_population <- rename(ward_population, POPULATION = Population)
    ward_spd <- sp::merge(x = ward_spd, y = ward_population, by = "WARD")
    # table(sapply(ward_population, class)) # class of variables
    # sum(x = ward_population$POPULATION) # total population
    
    # create munipipal population density and add it to town_spd
    town_population <- ward_spd@data %>% group_by(CAT_B) %>%
      summarise(POPULATION = sum(POPULATION)) %>%
      select(CAT_B, POPULATION)
    town_spd <- merge(x = town_spd, y = town_population, by = "CAT_B")
    # sum(x = town_spd@data$POPULATION) # total population
    # sum(x = town_spd@data$AREA) # total area
    
    # create provincial population density and add it to province_spd
    province_population <- ward_spd@data %>% group_by(PROVINCE) %>%
      summarise(POPULATION = sum(POPULATION)) %>% 
      select(PROVINCE, POPULATION)
    province_spd <- merge(x = province_spd, y = province_population, by = "PROVINCE")
    # sum(x = province_spd@data$POPULATION) # total population
    # sum(x = province_spd@data$Area) # total area
    
    # create population density variable
    ward_spd@data$DENSITY <- ward_spd@data$POPULATION/ward_spd@data$Area # per ward
    town_spd@data$DENSITY <- town_spd@data$POPULATION/town_spd@data$AREA # per municipality
    province_spd@data$DENSITY <- province_spd@data$POPULATION/province_spd@data$Area # per province
    
    # find duplicate municipality names in town_spd
    towns_town <- select(.data = town_spd@data, ID,MAP_TITLE,PROVINCE)
    length(towns_town$MAP_TITLE[duplicated(towns_town$MAP_TITLE)])
    town_duplicates <- towns_town[towns_town$MAP_TITLE == towns_town$MAP_TITLE[duplicated(towns_town$MAP_TITLE)][1] | 
                                 towns_town$MAP_TITLE == towns_town$MAP_TITLE[duplicated(towns_town$MAP_TITLE)][2],]
    # rename duplicate towns in town_spd to each be unique
    town_duplicates$new_MAP_TITLE <- paste0(town_duplicates$MAP_TITLE,
                                            "(",town_duplicates$PROVINCE,")")
    for (i in 1:length(town_duplicates$MAP_TITLE)) {
        plyr::revalue(town_spd@data$MAP_TITLE[town_spd@data$ID == town_duplicates$ID[i]] <- town_duplicates$new_MAP_TITLE[i])
    }
    length(town_spd@data$MAP_TITLE[duplicated(town_spd@data$MAP_TITLE)])
    
    # replace PROVINCE abbreviations with full PROVINCE names in Municipal Layer
    # to match other two layers
    Provinces_town <- sort(unique(town_spd@data$PROVINCE))
    Provinces_province <- sort(unique(province_spd@data$PROVINCE))
    for (i in 1:9) {
        plyr::revalue(town_spd@data$PROVINCE[town_spd@data$PROVINCE == Provinces_town[i]] <- Provinces_province[i])
    }
    
    # find duplicate municipality names in ward_spd
    length(unique(ward_spd@data$MUNICNAME))
    towns_ward <- select(.data = ward_spd@data, ID,MUNICNAME,PROVINCE)
    town_ward_duplicates <- towns_ward[towns_ward$MUNICNAME == towns_town$MAP_TITLE[duplicated(towns_town$MAP_TITLE)][1] | 
                                           towns_ward$MUNICNAME == towns_town$MAP_TITLE[duplicated(towns_town$MAP_TITLE)][2],]
    for (i in 1:9) {
        plyr::revalue(town_ward_duplicates$PROVINCE[town_ward_duplicates$PROVINCE == Provinces_province[i]] <- Provinces_town[i])
    }
    # rename duplicate towns in ward_spd to each be unique
    town_ward_duplicates$new_MUNICNAME <- paste0(town_ward_duplicates$MUNICNAME,
                                                 "(",town_ward_duplicates$PROVINCE,")")
    for (i in 1:length(town_ward_duplicates$MUNICNAME)) {
        plyr::revalue(ward_spd@data$MUNICNAME[ward_spd@data$ID == town_ward_duplicates$ID[i]] <- town_ward_duplicates$new_MUNICNAME[i])
    }
    length(unique(ward_spd@data$MUNICNAME))
    
    #check for a change in $ORDER
    all.equal(c(1,diff(ward_spd@data$ID)), rep(1, length(ward_spd@data$ID)))
    #check for a change in $ORDER
    all.equal(c(1,diff(town_spd@data$ID)), rep(1, length(town_spd@data$ID)))    
    #check for a change in $ORDER
    all.equal(c(1,diff(province_spd@data$ID)), rep(1, length(province_spd@data$ID)))
    
    
    # return to shape order as at start of script - necessary step if you want to merge 
    # topojson files derived from these same shapefiles with these s-patial p-olygon d-ataframes
    #ward_spd <- arrange(ward_spd, ORDER)
    #town_spd <- arrange(town_spd, ORDER)
    #province_spd <- arrange(province_spd, ORDER)
    
    #rm(list=setdiff(x = ls(), y = c("wdir", "tempdir", "provinces", "towns")))
        
    ######################################################################################
    #       Part 2 - converting temporary shapefiles to topoJSON and saving them         #
    ######################################################################################
    
    # toTopoJSON and geojsonio::topojson_write are wrappers for the node cli tool: topojson
    # cli instructions for topojson:
    # browseURL("https://github.com/mbostock/topojson/wiki/Command-Line-Reference")
    
    # toTopoJSON can be installed from github
    if (is.na(grep(pattern = "toTopoJSON", x = dir(.libPaths()))[1])) {
      options(unzip = "internal"); devtools::install_github("joelgombin/toTopoJSON")
    }
    
    # create dir to store topojson creation functions
    if (!dir.exists("R")) dir.create("R")
    # or you can source just toTopoJSON.R, and its help file from github
    devtools::source_url("https://raw.githubusercontent.com/joelgombin/toTopoJSON/master/R/toTopoJSON.R")
    if (!file.exists("R/toTopoJSON.Rd")) {
      sink("R/toTopoJSON.Rd")
      cat(paste(readr::read_lines("https://raw.githubusercontent.com/joelgombin/toTopoJSON/master/man/toTopoJSON.Rd"), collapse = "\n"))
      sink()
    }
    
    rstudio::previewRd("R/toTopoJSON.Rd")
    
    # topojson_write.R is a modification of toTopoJSON.R by the authors of package
    # geojsonio. Because topjson cli is dependent on node, topojson_write.R was
    # removed from geojsonio on 22 April 2015 but you can source it from versions
    # of geojsonio up to 21 April and use it if node and topojson cli are installed
    # on your system.
    devtools::source_url("https://raw.githubusercontent.com/ropensci/geojsonio/e9000460901ca18b212ec92d8548e65f9bac546a/R/topojson_write.r")
    if (!file.exists("R/topojson_write.Rd")) {
      sink("R/topojson_write.Rd")
      cat(paste(readr::read_lines("https://raw.githubusercontent.com/ropensci/geojsonio/e9000460901ca18b212ec92d8548e65f9bac546a/man/topojson_write.Rd"), collapse = "\n"))
      sink()
    }
    rstudio::previewRd("R/topojson_write.Rd")
    
    # I have had more success using MAPSHAPER cli (also dependent on node) compared
    # to topojson.cli.MAPSHAPER cli code can be tested at www.MAPSHAPER.org before
    # running it at the command line for reproducibility, as I have done below
    # cli instructions for MAPSHAPER:
    # browseURL("https://github.com/mbloch/mapshaper/wiki/Introduction-to-the-Command-Line-Tool")
    # browseURL("https://github.com/mbloch/mapshaper/wiki/Command-Reference")
    
    # create temporary directory to save temporary shapefiles in
    wdir <- getwd()
    tempdir <- tempdir()
    setwd(tempdir)
    unlink(dir(tempdir))
    
    #Store shapefiles temporarily while converting to topoJSON files
    writeOGR(obj = ward_spd, dsn = '.', layer = "Wards2011", 
             driver = "ESRI Shapefile")
    writeOGR(obj = town_spd, dsn = '.', layer = "LocalMunicipalities2011", 
             driver = "ESRI Shapefile")
    writeOGR(obj = province_spd, dsn = '.', layer = "Province_New_SANeighbours", 
             driver = "ESRI Shapefile")
    
    # convert shapefiles to topoJSON files using MAPSHAPER at the command line,
    # this can also be done manually with an online GUI at http://www.mapshaper.org/
    setwd(tempdir)
    # remove 95% of polygon points with the Douglas–Peucker algorithm
    if (!file.exists(paste0(wdir,"/TopoJSON/Wards2011.json"))) {
      try(system(command = paste0("mapshaper -i auto-snap Wards2011.shp -simplify dp 5% keep-shapes -o quantization=100000 topojson-precision=0.10 ",wdir,"/TopoJSON/Wards2011.json format=topojson"), 
                 intern = T, ignore.stdout = F, ignore.stderr = F))
    }
    if (!file.exists(paste0(wdir,"/TopoJSON/LocalMunicipalities2011.json"))) {
      try(system(command = paste0("mapshaper -i auto-snap LocalMunicipalities2011.shp -simplify dp 5% keep-shapes -o ",wdir,"/TopoJSON/LocalMunicipalities2011.json format=topojson"), 
                 intern = T, ignore.stdout = F, ignore.stderr = F))
    }
    if (!file.exists(paste0(wdir,"/TopoJSON/Province_New_SANeighbours.json"))) {
      try(system(command = paste0("mapshaper -i auto-snap Province_New_SANeighbours.shp -simplify dp 5% keep-shapes -o ",wdir,"/TopoJSON/Province_New_SANeighbours.json format=topojson"), 
                 intern = T, ignore.stdout = F, ignore.stderr = F))
    }
    
    unlink(dir(tempdir))
    
    ## create province level shape files for wards (to be converted to TopoJSON)
    ## store it in the temp directory. 
    ## Alternatively uncomment the following command to  create a sub directory 
    ## to save it with other shape files:
    #    if(!dir.exists(paste0(wdir,"/SHP/Wards/","Wards_by_Province"))) {dir.create(paste0(wdir,"/SHP/Wards/","Wards_by_Province"))}
    #    setwd(paste0(wdir,"/SHP/Wards/","Wards_by_Province"))
    provinces <- select(.data = province_spd@data, ID, PROVINCE)
    for (i in 0:8){
        writeOGR(
            obj = subset(x = ward_spd, subset = ward_spd@data$PROVINCE == provinces$PROVINCE[i+1]),
            dsn = '.',layer = i,driver = "ESRI Shapefile")
    }
#     for (i in 1:9){
#         writeOGR(
#             obj = subset(x = ward_spd, subset = ward_spd@data$PROVINCE == provinces[i]), 
#             dsn = '.', 
#             layer = paste0(
#                 sub(pattern = " |-", replacement = "_", x = provinces[i]),"_Wards2011"
#             ),
#             driver = "ESRI Shapefile")
#     }
    
#     # convert the 9 provincial ward shapefiles to topoJSON files using MAPSHAPER at the command line,
#     # removeing 95% of polygon points with the Douglas–Peucker algorithm
#     if (!dir.exists(paste0(wdir,"/TopoJSON/Wards_0.05"))) dir.create(paste0(wdir,"/TopoJSON/Wards_0.05"))
#     for (i in 1:9) {
#         if (!file.exists(paste0(wdir,"/TopoJSON/Wards_0.05/",sub(pattern = " |-", replacement = "_", x = provinces[i]),"_Wards2011.json"))) {
#             try(system(command = paste0("mapshaper -i auto-snap ",sub(pattern = " |-", replacement = "_", x = provinces[i]),
#                                         "_Wards2011.shp -simplify dp 5% keep-shapes -o quantization=100000 topojson-precision=0.10 ",
#                                         wdir,"/TopoJSON/Wards_0.05/",sub(pattern = " |-", replacement = "_", x = provinces[i]),"_Wards2011.json format=topojson"),
#                        intern = T, ignore.stdout = F, ignore.stderr = F))
#         }
#     }
    # convert the 9 provincial ward shapefiles to topoJSON files using MAPSHAPER at the command line,
    # removeing 90% of polygon points with the Douglas–Peucker algorithm
    if (!dir.exists(paste0(wdir,"/TopoJSON/Wards_0.10"))) dir.create(
        paste0(wdir,"/TopoJSON/Wards_0.10")
        )
    for (i in 0:8) {
        if (!file.exists(paste0(wdir,"/TopoJSON/Wards_0.10/",i,".json"))) {
            try(system(command = paste0(
                "mapshaper -i auto-snap ",i,
                ".shp -simplify dp 10% keep-shapes -o quantization=100000 topojson-precision=0.10 ",
                wdir,"/TopoJSON/Wards_0.10/",i,".json format=topojson"),
                intern = T, ignore.stdout = F, ignore.stderr = F))
        }
    }
#     for (i in 1:length(provinces)) {
#         if (!file.exists(paste0(
#             wdir,"/TopoJSON/Wards_0.10",
#             sub(pattern = " |-",replacement = "_",x=provinces[i]),
#             "_Wards2011.json"))) {
#             try(system(command = paste0(
#                 "mapshaper -i auto-snap ",
#                 sub(pattern = " |-",replacement = "_",x=provinces[i]),
#                 "_Wards2011.shp -simplify dp 10% keep-shapes -o quantization=100000 topojson-precision=0.10 ",
#                 wdir,"/TopoJSON/Wards_0.10/",
#                 sub(pattern = " |-", replacement = "_", x = provinces[i]),
#                 "_Wards2011.json format=topojson"),
#                 intern = T, ignore.stdout = F, ignore.stderr = F))
#         }
#     }
    
    unlink(dir(tempdir))
    
    towns <- select(.data = town_spd@data, ID,MAP_TITLE,PROVINCE)
    for (i in 0:233){
        writeOGR(
            obj = subset(x = ward_spd, subset = ward_spd@data$MUNICNAME == towns$MAP_TITLE[i+1]),
            dsn = '.',layer = i,driver = "ESRI Shapefile")
        }
#     for (i in 1:length(towns)){
#         writeOGR(
#             obj = subset(x = ward_spd, subset = ward_spd@data$MUNICNAME == towns[i]),
#             dsn = '.',
#             layer = paste0(gsub(pattern = " |-|/|\\(|\\)", replacement = "_",
#                                 x = towns[i]),"_Wards2011"),
#             driver = "ESRI Shapefile")}
    
    # convert the 9 provincial ward shapefiles to topoJSON files using MAPSHAPER at the command line,
    # removeing 80% of polygon points with the Douglas–Peucker algorithm
    if (!dir.exists(paste0(wdir,"/TopoJSON/Wards_0.20"))) dir.create(
        paste0(wdir,"/TopoJSON/Wards_0.20")
        )
    for (i in 0:233) {
        if (!file.exists(paste0(wdir,"/TopoJSON/Wards_0.20/",i,".json"))) {
            try(system(command = paste0(
                "mapshaper -i auto-snap ",i,
                ".shp -simplify dp 20% keep-shapes -o quantization=100000 topojson-precision=0.10 ",
                wdir,"/TopoJSON/Wards_0.20/",i,".json format=topojson"),
                intern = T, ignore.stdout = F, ignore.stderr = F))
        }
    }
#     for (i in 1:length(towns)) {
#         if (!file.exists(paste0(
#             wdir,"/TopoJSON/Wards_0.20",
#             gsub(pattern = " |-|/|\\(|\\)",replacement = "_",x = towns[i]),
#             "_Wards2011.json"))) {
#             try(system(command = paste0(
#                 "mapshaper -i auto-snap ",
#                 gsub(pattern = " |-|/|\\(|\\)",replacement = "_",x = towns[i]),
#                 "_Wards2011.shp -simplify dp 20% keep-shapes -o quantization=100000 topojson-precision=0.10 ",
#                 wdir,"/TopoJSON/Wards_0.20/",
#                 gsub(pattern = " |-|/|\\(|\\)",replacement = "_",x = towns[i]),
#                 "_Wards2011.json format=topojson"),
#                 intern = T, ignore.stdout = F, ignore.stderr = F))
#         }
#     }
    unlink(dir(tempdir))
    
    setwd(wdir)
    
#     file.rename(
#         from = paste0(
#             "TopoJSON/Wards_0.20/",
#             list.files(path = "TopoJSON/Wards_0.20/", 
#                        pattern = "_EC_|_FS_|_NW_|_MP_")
#             ),
#         to = stringr::str_replace_all(
#             string = paste0(
#                 "TopoJSON/Wards_0.20/",
#                 list.files(path = "TopoJSON/Wards_0.20/", 
#                            pattern = "_EC_|_FS_|_NW_|_MP_")
#             ),
#             pattern = c("_EC_"="(EC)","_FS_"="(FS)","_NW_"="(NW)","_MP_"="(MP)")
#         )
#     )
    
    #alternative with topojson_write
    #tpojson_write(shppath = paste0(tempdir,"/Wards2011.shp"), quantisation = "1e5",
    #               simplification = "0.05", filename = "TopoJSON/Wards2011_5")
    #toTopoJSON(SPDF = paste0(tempdir,"/Wards2011"), quantisation = "1e5",
    #               simplification = "0.05", filename = "TopoJSON/Wards2011_5")
    
  }

