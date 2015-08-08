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
            left=20,top=720,width =120,
            textOutput(outputId='message3',container=span,inline=T)
        ),
        absolutePanel( # Boundary Coordinates
            left=140,top=720,width =600,
            textOutput(outputId='message4',container=span,inline=T)
        ),
        absolutePanel( # Click coordinates
            left=740,top=720,width =300,
            textOutput(outputId='message1',container=span,inline=T)
        ),
        absolutePanel( # Province
            left=1040,top=720,width =130,
            textOutput(outputId='message_slice1',container=span,inline=T)
        ),
        absolutePanel( # Municipality
            left=1170,top=720,width =340,
            textOutput(outputId='message_slice2',container=span,inline=T)
        ),
        absolutePanel( # spd@data$ID)
            left=1510,top=720,width =50,
            textOutput(outputId='message_shp_id',container=span,inline=T)
        ),
        absolutePanel( # gis$mouse_events
            left=1500,top=720,width =1,
            textOutput(outputId='message_events',container=span,inline=T)
        )
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
    gis <- reactiveValues(slice1=NULL)
    gis <- reactiveValues(slice2=NULL)
    gis <- reactiveValues(shp_mouseover_id=NULL)
    gis <- reactiveValues(mouse_events=0)
    observeEvent(input$map1_topojson_mouseover, {
        gis$mouse_events <- 1
    })
    
    observeEvent(input$map1_topojson_mouseout, {
        gis$mouse_events <- 0
    })
    
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
                          subset = ward_tj_spd@data$MUNICNAME == gis$slice2)
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
            gis$id <- input$map1_topojson_mouseover$properties$MAP_TITLE
        }
    })
    observeEvent(input$map1_topojson_mouseover,label="event6", {
        if (input$select_map_level == 'Province'){
            gis$id <- input$map1_topojson_mouseover$properties$PROVINCE
        }
    })
    observeEvent(input$map1_shape_mouseover, label="event7",{
        if (input$select_map_level == 'Ward'){
            gis$id <- gis$shp@data$WARD[gis$shp@data$ID == gis$shp_mouseover_id]
        }
    })
    observeEvent(input$map1_shape_mouseover, label="event8", {
        if (input$select_map_level == 'Municipality'){
            gis$id <- gis$shp@data$MAP_TITLE[gis$shp@data$ID == gis$shp_mouseover_id]
        }
    })
    observeEvent(input$map1_shape_mouseover, label="event9",{
        if (input$select_map_level == 'Province'){
            gis$id <- gis$shp@data$PROVINCE[gis$shp@data$ID == gis$sh_mouseover_id]
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
        gis$slice1 <- input$map1_topojson_mouseover$properties$PROVINCE
    })
    observeEvent(input$map1_topojson_mouseover, label="event14b",{
        gis$slice2 <- input$map1_topojson_mouseover$properties$MAP_TITLE
    })
#    observeEvent(input$map1_topojson_mouseout, label="event15",{
#        gis$slice2 <- NULL
#    })
    observeEvent(input$map1_shape_mouseover, label="event16a",{
        gis$slice1 <- gis$shp@data$PROVINCE[gis$shp@data$ID == gis$shp_mouseover_id]
    })
#     observeEvent(input$map1_shape_mouseover, label="event16b",{
#         if (input$select_mapl_level == 'Ward') {
#             gis$slice2 <- gis$shp@data$MUNICNAME[gis$shp@data$ID == gis$sh_mouseover_id]
#         }
#     })
    observe(if((!is.null(gis$shp_mouseover) & input$select_map_level == 'Municipality')){ 
        label="event16c"
        gis$slice2 <- town_tj_spd@data$MAP_TITLE[town_tj_spd@data$ID == gis$shp_mouseover_id]
    })
#    observeEvent(input$map1_shape_mouseout, label="event17",{
#        gis$slice1 <- NULL
#    })
    observeEvent(input$map1_shape_mouseover, label="event18a",{
        gis$shp_mouseover_id <- input$map1_shape_mouseover$id
    })
    observeEvent(input$map1_shape_mouseout, label="event18b",{
        gis$shp_mouseover_id <- NULL
    })
#    observeEvent(input$map1_topojson_mouseover, label="event18c",{
#        gis$shp_mouseover_id <- input$map1_topojson_mouseover$properties$ID
#    })
#    observe(if (is.null(input$map1_shape_mouseover$id) & gis$mouse_events == 0) {    
#        gis$shp_mouseover_id <- NULL
#        label="event18d"
#    })
    
    output$message_slice1 <- renderText(gis$slice1)
    output$message_slice2 <- renderText(gis$slice2)
    output$message_shp_id <- renderText(gis$shp_mouseover_id)
    output$message_id <- renderText(gis$id)
    output$message_events <- renderText(gis$mouse_events)
    
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
                addTopoJSON(#group = 'town_tj',
                    topojson=town_tj,stroke=T,dashArray=3,weight=2,color="white",
                    opacity=1,fill=T,smoothFactor = 0.5
                ) %>%
#                addTopoJSON(group = 'province_slice',
#                            topojson=province_slice,stroke=T,dashArray=3,weight=2,color="black",
#                            opacity=1,fill=F,smoothFactor = 0.5
#                ) %>% 
                addLegend(
                    position = "bottomright", 
                    pal = town_binpal,
                    opacity = 1,
                    labFormat = labelFormat(big.mark = " "),
                    values = town_density
                )
        })
    })
    
    observe(if(input$select_map_level == 'Province'){
        label = "province_proxymap_event"
        proxy <- leafletProxy(
            "map1"#, data =gis$shp
        )
        proxy %>%
            clearShapes() %>%
            clearTopoJSON() %>%
        addTopoJSON(
            topojson = gis$tj,stroke=T,dashArray=3,weight=2,color="white",
            opacity=1,fill=T,smoothFactor = 0.5
            ) %>%
        addTopoJSON(
            topojson=town_slice,stroke=F,weight=0,color="white",
            opacity=0,fill=T,fillColor="white",fillOpacity=0,smoothFactor = 1
        )
#            addPolygons(#layerId = gis$shp@data$ID,
#                        stroke=T,dashArray=3,weight=2,color="white",
#                        opacity=1,fill=T,smoothFactor = 0.5, fillOpacity = 0.5,
#                        fillColor = gis$binpal(gis$shp@data$DENSITY)
#            ) 
    })
    observe(if(input$select_map_level == 'Ward' | input$select_map_level =='Municipality'){
        label = "ward_proxymap_event"
        proxy <- leafletProxy(
            "map1"#, data =gis$shp
        )
        proxy %>%
            clearShapes() %>%
            clearTopoJSON() %>%
            addTopoJSON(
                topojson=town_tj,stroke=T,dashArray=3,weight=2,color="white",
                opacity=1,fill=T,smoothFactor = 0.5
            )
    })
    observe(if(input$select_map_level == 'Ward'){
        proxy <- leafletProxy(
            "map1", data =gis$shp
        )
        proxy %>%
            clearShapes() %>%
            addPolygons(
                layerId = gis$shp@data$ID,
                stroke=T,dashArray=3,weight=2,color="white",
                opacity=1,fill=T,smoothFactor = 0.5, fillOpacity = 0.5,
                fillColor = gis$binpal(gis$shp@data$DENSITY)
            ) 
    })
    
    observeEvent(input$map1_zoom, label="event20",{
        if ((v$msg3 > 8) & (!is.null(gis$slice1))) {
            #label="zoom_effect"
            updateRadioButtons(
                session, inputId = "select_map_level", 
                choices = c('Province', 'Municipality', 'Ward'),
                selected = 'Ward'
            )
        }
    })
    
    observeEvent(input$map1_zoom ,label="event21", {
        if ((v$msg3 <= 8) & (!is.null(gis$slice1))) {
            updateRadioButtons(
                session, inputId = "select_map_level", 
                choices = c('Province', 'Municipality'),
                selected = 'Municipality'
            )
        }
    })
    
    output$hoverInfo <- renderUI({
       if (is.null(gis$shp_mouseover_id) & gis$mouse_events == 0) {
         return(
           div(
             paste("Hover over a", input$select_map_level)
             ))
       } else if (input$select_map_level == 'Ward'){
         return(
           div(
             strong(
               sub(
                 pattern=" Local Municipality| Metropolitan Municipality|Local Municipality of ",
                 replacement="",
                 x=gis$shp@data$MUNICNAME[gis$shp@data$ID == gis$shp_mouseover_id])),
             br(),
             "Ward",
             gis$shp@data$WARDNO[gis$shp@data$ID == gis$shp_mouseover_id],
             br(),
             span(round(gis$shp@data$DENSITY[gis$shp@data$ID == gis$shp_mouseover_id],1), HTML("people/km<sup>2</sup>"))
           )
         )
         } else if (input$select_map_level == 'Municipality'){
      return(
            div(
              strong(gis$id),
              br(),
              span(round(gis$shp@data$DENSITY[gis$shp@data$MAP_TITLE == gis$id],1), HTML("people/km<sup>2</sup>"))
            )
          )
         } else if (input$select_map_level == 'Province'){
             return(
                 div(
                     strong(gis$id),
                     br(),
                     span(round(gis$shp@data$DENSITY[gis$shp@data$PROVINCE == gis$id],1), HTML("people/km<sup>2</sup>"))
                 )
             )
         }
    })
    
     observe(if (input$select_map_level == 'Ward' & (!is.null(gis$shp_mouseover_id))) {
         proxy <- leafletProxy(
             "map1", data = subset(gis$shp, gis$shp@data$ID == gis$shp_mouseover_id)
         )
#         single_data <- subset(gis$shp, gis$shp@data$ID == gis$shp_mouseover_id)
         proxy %>%
             clearGroup('single') %>%
             addPolygons(group = 'single',
                 stroke=T,weight=3,color="#555555",opacity=1,
                 smoothFactor=1,fillColor="#FFFFFF",fillOpacity=0.2
             )
     })
     
#     observeEvent(input$map1_shape_mouseout, {
#         proxy <- leafletProxy(mapId = 'map1')
#         proxy %>%
#         clearGroup('single')
#     })
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
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
