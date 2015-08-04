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
    ward_spd <- readOGR("SHP/Wards/Wards2011.shp", layer = "Wards2011")
    #ward_spd@data$ORDER <- 0:(dim(ward_spd)[1]-1)
    town_spd <- readOGR("SHP/LocalMunicipalities2011.shp", layer = "LocalMunicipalities2011")
    #town_spd@data$ORDER <- 0:(dim(town_spd)[1]-1)
    province_spd <- readOGR("SHP/Province_New_SANeighbours.shp", layer = "Province_New_SANeighbours")
    #province_spd@data$ORDER <- 0:(dim(province_spd)[1]-1)
    
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
    
    #check for a change in $ORDER
    #all.equal(c(1,diff(ward_spd@data$ORDER)), rep(1, length(ward_spd@data$ORDER)))
    
    # create munipipal population density and add it to town_spd
    town_population <- ward_spd@data %>% group_by(CAT_B) %>%
      summarise(POPULATION = sum(POPULATION)) %>%
      select(CAT_B, POPULATION)
    town_spd <- merge(x = town_spd, y = town_population, by = "CAT_B")
    # sum(x = town_spd@data$POPULATION) # total population
    # sum(x = town_spd@data$AREA) # total area
    
    #check for a change in $ORDER
    #all.equal(c(1,diff(town_spd@data$ORDER)), rep(1, length(town_spd@data$ORDER)))
    
    # create provincial population density and add it to province_spd
    province_population <- ward_spd@data %>% group_by(PROVINCE) %>%
      summarise(POPULATION = sum(POPULATION)) %>% 
      select(PROVINCE, POPULATION)
    province_spd <- merge(x = province_spd, y = province_population, by = "PROVINCE")
    # sum(x = province_spd@data$POPULATION) # total population
    # sum(x = province_spd@data$Area) # total area
    
    #check for a change in $ORDER
    #all.equal(c(1,diff(province_spd@data$ORDER)), rep(1, length(province_spd@data$ORDER)))
    
    # create population density variable
    ward_spd@data$DENSITY <- ward_spd@data$POPULATION/ward_spd@data$Area # per ward
    town_spd@data$DENSITY <- town_spd@data$POPULATION/town_spd@data$AREA # per municipality
    province_spd@data$DENSITY <- province_spd@data$POPULATION/province_spd@data$Area # per province
    
    # return to shape order as at start of script - necessary step if you want to merge 
    # topojson files derived from these same shapefiles with these s-patial p-olygon d-ataframes
    #ward_spd <- arrange(ward_spd, ORDER)
    #town_spd <- arrange(town_spd, ORDER)
    #province_spd <- arrange(province_spd, ORDER)
    
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
    
    setwd(wdir)
    rm(list=setdiff(x = ls(), y = c("wdir", "tempdir")))
        
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
    
    setwd(wdir)
    
    #alternative with topojson_write
    #tpojson_write(shppath = paste0(tempdir,"/Wards2011.shp"), quantisation = "1e5",
    #               simplification = "0.05", filename = "TopoJSON/Wards2011_5")
    #toTopoJSON(SPDF = paste0(tempdir,"/Wards2011"), quantisation = "1e5",
    #               simplification = "0.05", filename = "TopoJSON/Wards2011_5")
    
  }

