library(shiny)
#options(unzip = "internal"); devtools::install_github("rstudio/leaflet")
library(leaflet)
library(DT)

if (!file.exists("R/v1.RData")) {
    if (all.equal(dir("TopoJSON/"),
                  c("LocalMunicipalities2011.json",
                    "Province_New_SANeighbours.json",
                    "Wards2011.json"))) {
        source("data_mapping.R")
    } else if (!all.equal(dir("TopoJSON/"),
                          c("LocalMunicipalities2011.json",
                            "Province_New_SANeighbours.json",
                            "Wards2011.json"))) {
        stop("check your working directory is correct.
             Can't find TopoJSON or .RData source files")
    }
    } else {
        load("R/v1.RData")
}


source("R/topoJSON_string_style.R")
town_density <- topoJSON_property_extract(
    topoJSON_string = town_tj, property_name = "DENSITY"
)
ward_density <- topoJSON_property_extract(
    topoJSON_string = ward_tj, property_name = "DENSITY"
)
province_density <- topoJSON_property_extract(
    topoJSON_string = province_tj, property_name = "DENSITY"
)

#sapply(slot(object = ward_tj_spd, name = "polygons"), function(x) slot(x, "ID"))
#ward_tj_spd@data$ID
ward_tj_spd@data$ID[ward_tj_spd@data$PROVINCE == "Northern Cape"]


ui <- navbarPage(
    title = "South African Demographics",
    tabPanel(
        # Add CSS to give floating Panels a style
        tags$head(tags$style(".floater { background-color: white; 
                             padding: 8px; opacity: 0.7; border-radius: 6px; 
                             box-shadow: 0 0 15px rgba(0,0,0,0.2); }")),
        
        title = "Map",
        leafletOutput(outputId = 'map1', width = "100%", height = "650px"),
        absolutePanel(
            right=25,top=80,width =260, class="floater",
            h3("SA Population Density"),
            uiOutput('hoverInfo')
        ),
        absolutePanel(
            right = 25, top = 330, width = 200, class="floater",
            radioButtons( 
                inputId='select_map_level',
                label=p(h4(strong('Select a level:')),
                        em('At high zoom, Wards'),
                        br(),em('are displayed')),
                choices=c('Province','Municipality'),
                selected='Municipality',
                inline=F
            )
        ),
        absolutePanel( # Zoom level
            left=20,top=720,width =200,
            textOutput(outputId='message3',container=span,inline=T)
        ),
        absolutePanel( # Boundary Coordinates
            left=220,top=720,width =630,
            textOutput(outputId='message4',container=span,inline=T)
        ),
        absolutePanel(   # Click coordinates
            left=850,top=720,width =330,
            textOutput(outputId='message1',container=span,inline=T)
        ),
        absolutePanel(   # Province
            left=1180,top=720,width =150,
            textOutput(outputId='message_slice',container=span,inline=T)
        ),
        absolutePanel(   # Province
            left=1180,top=720,width =150,
            textOutput(outputId='message_shp_id',container=span,inline=T)
        )#,
        #        absolutePanel(  # Province
        #            left=1330,top=720,width =150,
        #            textOutput(outputId='message_coords',container=span,inline=T)
        #        )
        ),
    
    tabPanel(
        title="Shape Data",
        selectInput(
            inputId='select_data_level', 
            label=h4(
                strong('Select a level:'),
                br(),
                span('(Ward, Municipal or Provincial)')
            ),
            choices=c('Ward','Municipal','Provincial'),
            selected='Municipality'
        ),
        # A download option for later implimentation:  
        #  p(class='text-right', downloadButton(outputId='downloadData',
        #                                         label='Download Data')),
        DT::dataTableOutput(outputId='table')
    )
        )


server <- function(session, input, output) {
    
    gis <- reactiveValues(tj=town_tj)
    gis <- reactiveValues(shp=town_tj_spd)
    gis <- reactiveValues(id=NULL)
    gis <- reactiveValues(binpal=NULL)
    gis <- reactiveValues(slice=NULL)
    gis <- reactiveValues(shp_mouseover_id=NULL)
    
    observe(if (input$select_map_level == 'Ward'){label="event3"
    gis$tj <- ward_tj_no_lines
    })
    observe(if (input$select_map_level == 'Municipality'){
        gis$tj <- town_tj
    })
    observe(if (input$select_map_level == 'Province'){
        gis$tj <- province_tj
    })
    observe(if (input$select_map_level == 'Ward'){
        gis$shp <- subset(ward_tj_spd,
                          subset = ward_tj_spd@data$PROVINCE == gis$slice)
    })
    observe(if (input$select_map_level == 'Municipality'){
        gis$shp <- town_tj_spd
    })
    observe(if (input$select_map_level == 'Province'){
        gis$shp <- province_tj_spd
    })
    observeEvent(input$map1_topojson_mouseover, label="event4",{
        if (input$select_map_level == 'Ward'){
            gis$id <- input$map1_topojson_mouseover$properties$WARD
        }
    })
    observeEvent(input$map1_topojson_mouseover, label="event5", {
        if (input$select_map_level == 'Municipality'){
            gis$id <- input$map1_topojson_mouseover$properties$MUNICNAME
        }
    })
    observeEvent(input$map1_topojson_mouseover,label="event6", {
        if (input$select_map_level == 'Province'){
            gis$id <- input$map1_topojson_mouseover$properties$PROVINCE
        }
    })
    observeEvent(input$map1_shape_mouseover, label="event7",{
        if (input$select_map_level == 'Ward'){
            gis$id <- gis$shp@data$WARD[gis$shp@data$ID == input$map1_shape_mouseover$id]
        }
    })
    observeEvent(input$map1_shape_mouseover, label="event8", {
        if (input$select_map_level == 'Municipality'){
            gis$id <- gis$shp@data$MUNICNAME[gis$shp@data$ID == input$map1_shape_mouseover$id]
        }
    })
    observeEvent(input$map1_shape_mouseover, label="event9",{
        if (input$select_map_level == 'Province'){
            gis$id <- gis$shp@data$PROVINCE[gis$shp@data$ID == input$map1_shape_mouseover$id]
        }
    })
    observeEvent(input$map1_shape_mouseout, label="event10",{
        gis$id <- NULL
    })
    observe(if (input$select_map_level == 'Ward'){label="event11"
    gis$binpal <- ward_binpal
    })
    observe(if (input$select_map_level == 'Municipality'){label="event12"
    gis$binpal <- town_binpal
    })
    observe(if (input$select_map_level == 'Province'){label="event13"
    gis$binpal <- province_binpal
    })
    observeEvent(input$map1_topojson_mouseover, label="event14",{
        gis$slice <- input$map1_topojson_mouseover$properties$PROVINCE
    })
    #    observeEvent(input$map1_topojson_mouseout, label="event15",{
    #        gis$slice <- NULL
    #    })
#    observeEvent(input$map1_shape_mouseover, label="event16",{
#        gis$slice <- gis$shp@data$PROVINCE[gis$shp@data$ID == input$map1_shape_mouseover$id]
#    })
    #    observeEvent(input$map1_shape_mouseout, label="event17",{
    #        gis$slice <- NULL
    #    })
    observeEvent(input$map1_shape_mouseover, label="event18",{
        gis$shp_mouseover_id <- input$map1_shape_mouseover$id
    })
    output$message_slice <- renderText(gis$slice)
    output$message_shp_id <- renderText(input$map1_shape_mouseover$id)
    output$message_coords <- renderText(paste(v$msg4[1]))
    
    v <- reactiveValues(msg1=NULL) # Click Latitude
    v <- reactiveValues(msg2=NULL) # Click Longitude
    v <- reactiveValues(msg3=NULL) # Zoom level
    v <- reactiveValues(lng1 = -23)
    v <- reactiveValues(lat1 = 43) 
    v <- reactiveValues(lng2 = -35)
    v <- reactiveValues(lat2 = 10)
    v <- reactiveValues(msg5=NULL) # Ward/Municipality/Province Name of mouseover object
    v <- reactiveValues(msg6=NULL) # Population Density of mouseover object
    v <- reactiveValues(msg7=NULL) # Municipality name in Ward level mouseover object
    
    observeEvent(input$map1_click, {
        v$msg1 <- input$map1_click$lat
    })
    observeEvent(input$map1_topojson_click, {
        v$msg1 <- input$map1_topojson_click$lat
    })
    observeEvent(input$map1_shape_click,{
        v$msg1 <  input$map1_shape_click$lat
        
    })
    observeEvent(input$map1_click, {
        v$msg2 <- input$map1_click$lng
    })
    observeEvent(input$map1_topojson_click, {
        v$msg2 <- input$map1_topojson_click$lng
    })
    observeEvent(input$map1_shape_click,{
        v$msg2 <  input$map1_shape_click$lng
        
    })
    observeEvent(input$map1_zoom, label="zoom_message", {
        v$msg3 <- input$map1_zoom
    })
    observeEvent(input$map1_bounds, {
        v$lng1 <- input$map1_bounds[4]
    })
    observeEvent(input$map1_bounds, {
        v$lat1 <- input$map1_bounds[3]
    })
    observeEvent(input$map1_bounds, {
        v$lng2 <- input$map1_bounds[2]
    })
    observeEvent(input$map1_bounds, {
        v$lat2 <- input$map1_bounds[1]
    })
    output$message1 <- renderText(if (!is.null(v$msg1)) {
        paste("Click coordinates: long",round(v$msg2,3),
              "lat",round(v$msg1,3))
    }) # Click Coordinates
    
    output$message3 <- renderText(paste("Zoom level is",v$msg3)) # Zoom level
    output$message4 <- renderText(
        paste(
            "View Bounds: long",substr(paste(v$lng1),start=1,stop=6),
            "lat",substr(paste(v$lat1),start=1,stop=6),"(bottomleft/SW); ",
            "long",substr(paste(v$lng2),start=1,stop=6),
            "lat",substr(paste(v$lat2),start = 1, stop = 6),"(topright/NE)"
        ) # Boundary Coordinates
    )
    
    observe(if (is.null(v$msg3)) {
        output$map1 <-renderLeaflet({
            leaflet() %>%
                setView(zoom=6,lng=26,lat=-29) %>%
                addTiles() %>%
                addTopoJSON(#group = "province_slice",
                    topojson=province_tj,stroke=T,dashArray=3,weight=2,color="white",
                    opacity=1,fill=T,smoothFactor = 0.5
                ) %>% 
                
                
                
                
                addLegend(
                    position = "bottomright", 
                    pal = town_binpal,
                    opacity = 1,
                    labFormat = labelFormat(big.mark = " "),
                    values = town_density
                )
        })
    })
    
    observeEvent(eventExpr = c(input$select_map_level, gis$slice),
                 label = "town_proxymap_event", {
        proxy1 <- leafletProxy(
            "map1", data =gis$shp
        )
        proxy1 %>%
            clearShapes() %>%
            
            addPolygons(layerId = gis$shp@data$ID,
                        stroke=T,dashArray=3,weight=2,color="white",
                        opacity=1,fill=T,smoothFactor = 0.5, fillOpacity = 0.7,
                        fillColor = gis$binpal(gis$shp@data$DENSITY)
            ) 
    })
    
    observeEvent(input$map1_zoom, label="event20",{
        if ((v$msg3 > 8) & (!is.null(gis$slice))) {
            #label="zoom_effect"
            updateRadioButtons(
                session, inputId = "select_map_level", 
                choices = c('Province', 'Municipality', 'Ward'),
                selected = 'Ward'
            )
        }
    })
    
    observeEvent(input$map1_zoom ,label="event21", {
        if ((v$msg3 <= 8) & (!is.null(gis$slice))) {
            updateRadioButtons(
                session, inputId = "select_map_level", 
                choices = c('Province', 'Municipality'),
                selected = 'Municipality'
            )
        }
    })
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    observe({
        if (input$select_data_level=='Ward') {
            output$table=DT::renderDataTable(
                ward_tj_list$objects$Wards2011$geometries$properties, server=T,
                options=list(pageLength = 50)
            )
        }
    })
    observe({
        if (input$select_data_level=='Municipal') {
            output$table=DT::renderDataTable(
                town_tj_list$objects$LocalMunicipalities2011$geometries$properties,
                server=T,options=list(pageLength=50)
            )
        }
    })
    observe({
        if (input$select_data_level=='Provincial') {
            output$table=DT::renderDataTable(
                province_tj_list$objects$Province_New_SANeighbours$geometries$properties,
                server=T
            )
        }
    })
}

shinyApp(ui,server)
