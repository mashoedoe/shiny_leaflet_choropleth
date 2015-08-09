library(shiny)
library(leaflet)
library(DT)

load("R/v1.RData")

ui <- navbarPage(
  title = "South African Demographics",
  tabPanel(
    title = "Map",
    leafletOutput(outputId = 'map1', width = "100%", height = "650px"),
    absolutePanel(
      right=0,top=70,width =275,#class="floater",
      h3("SA Population Density"),
      uiOutput('hoverInfo')
    ),
    absolutePanel(
#      right=20,top=250,width=300, #class="floater",
      right = 0, top = 400, width = 145,
      radioButtons( 
        inputId='select_data_level_tab1',
        label=h4(strong('Select a level:')),#,
        choices=c('Ward','Municipality','Province'),
        selected='Municipality',
        inline=F
      )
    ),
    absolutePanel( # Zoom level
      left=20,top=720,width =200,
      textOutput(outputId='message5',container=span,inline=T)
    ),
    absolutePanel( # Boundary Coordinates
      left=240,top=720,width =610,
      textOutput(outputId='message6',container=span,inline=T)
    ),
    absolutePanel(   # Click coordinates
      left=850,top=720,width =300,
      textOutput(outputId='message3',container=span,inline=T)
    )
    ),
  
  tabPanel(
    title="Shape Data",
    selectInput(
      inputId='select_data_level_tab2', 
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



server <- function(input,output,session) {
  
      #addProviderTiles("Thunderforest.Transport") %>%
      #addProviderTiles("Thunderforest.OpenCycleMap") %>%
      #addProviderTiles("Thunderforest.Landscape") %>%
      #addProviderTiles("Thunderforest.Outdoors") %>%
      #addProviderTiles("CartoDB.Positron") %>%
      #addProviderTiles("CartoDB.PositronNoLabels) %>%
      #addProviderTiles("CartoDB.DarkMatter") %>%
      #addProviderTiles("CartoDB.DarkMatterNoLabels) %>%
      #addProviderTiles("Acetate.all") %>%
      #addProviderTiles("Acetate.terrain") %>%
      #addProviderTiles("Acetate.basemap") %>%
      #addProviderTiles("Acetate.hillshading") %>%
      #addProviderTiles("Acetate.foreground") %>%
      #addProviderTiles("Acetate.roads") %>%
      #addProviderTiles("Acetate.labels") %>%
      #addProviderTiles("Stamen.Toner") %>%
      #addProviderTiles("Stamen.TonerLite") %>%
      #addProviderTiles("Stamen.TonerHybrid") %>%
      #addProviderTiles("Stamen.TonerLines") %>% 
      #addProviderTiles("Stamen.TonerLabels") %>%
      #addProviderTiles("Stamen.TonerBackground") %>%
      #addProviderTiles("HERE.normalNightGrey") %>%
      #addProviderTiles("HERE.satelliteDay") %>%
      #addProviderTiles("NASAGIBS.ViirsEarthAtNight2012")
  
  observe({
    if (input$select_data_level_tab1 == 'Ward') {
      v <- reactiveValues(msg1=NULL) # Ward/Municipality/Province Name
      v <- reactiveValues(msg2=NULL) # Population Density
      v <- reactiveValues(msg3=NULL) # Click Coordinates
      v <- reactiveValues(msg4=NULL) # Click coordinates
      v <- reactiveValues(msg5=NULL) # Zoom level
      v <- reactiveValues(msg6=NULL) # Boundary Coordinates
      v <- reactiveValues(msg7=NULL) # Municipality name while on Ward Map
      output$map1 <-renderLeaflet({
        leaflet() %>%
          setView(zoom=6,lng=26,lat=-29) %>%
          addTiles() %>%
          addTopoJSON(
            topojson=ward_tj,stroke=T,color="white",opacity=1,fill=T,
            dashArray=3,weight=1
          ) %>% 
          addLegend(
            position = "bottomright", 
            pal = ward_binpal,#(town_tj_spd@data$DENSITY),
            opacity = 1,
            labFormat = labelFormat(big.mark = " "),
            values = ward_tj_spd@data$DENSITY
          )
        })
      
      observeEvent(input$map1_topojson_mouseover, label = "ward_message_event",{
        v$msg1 <- input$map1_topojson_mouseover$properties$WARD
        v$msg2 <- input$map1_topojson_mouseover$properties$DENSITY
        v$msg7 <- input$map1_topojson_mouseover$properties$MUNICNAME
      })
      
      observeEvent(input$map1_topojson_mouseover, label = "ward_proxymap_event",{
        if (!is.null(input$map1_zoom) & v$msg5 >= 9) {
          proxy1 <- leafletProxy(
            "map1", data = subset(ward_tj_spd, ward_tj_spd@data$WARD == v$msg1)
            )
            single_data <- subset(ward_tj_spd@data, ward_tj_spd@data$WARD == v$msg1)
            proxy1 %>%
              clearShapes() %>%
              addPolygons(
                stroke=T,weight=3,color="#555555",opacity=1,
                smoothFactor=1,fillColor="#FFFFFF",fillOpacity=0.2
              )
        }
      })
      
      
       observeEvent(input$map1_shape_mouseout, {
         proxy <- leafletProxy(mapId = 'map1')
         proxy %>%
           clearShapes()
         })
          
      output$hoverInfo <- renderUI({
        if (is.null(v$msg2)) {
          return(
            div(
              paste("Hover over a", input$select_data_level_tab1)
            ))
        } else {
          return(
            div(
              strong(
                sub(
                  pattern=" Local Municipality| Metropolitan Municipality|Local Municipality of ",
                  replacement="",
                  #x=input$map1_topojson_mouseover$properties$MUNICNAME | 
                  x=v$msg7)),
              br(),
              "Ward",
              v$msg1,
              br(),
              span(round(v$msg2,1), HTML("people/km<sup>2</sup>"))
            )
          )
        }
      })
      
      observeEvent(input$map1_click, {
        v$msg3 <- paste(
          "Click coordinates: Lat",round(input$map1_click$lat,3),
          "Long",round(input$map1_click$lng,3))
      })
      observeEvent(input$map1_shape_click, {
        v$msg3 <- paste(
          "Click coordinates: Lat",round(input$map1_shape_click$lat,3),
          "Long",round(input$map1_shape_click$lng,3))
      })
      observeEvent(input$map1_zoom, {
        v$msg5 <- input$map1_zoom
      })
      observeEvent(input$map1_bounds, {
        v$msg6 <- paste(
          "View Bounds: lat",substr(paste(input$map1_bounds[1]),start = 1, stop = 6),"long",
          substr(paste(input$map1_bounds[4]),start=1,stop=6),"(topleft); lat",
          substr(paste(input$map1_bounds[3]),start=1,stop=6),"long",
          substr(paste(input$map1_bounds[2]),start=1,stop=6),"(bottomright)")
      })
      output$message3 <- renderText(v$msg3) # Click Coordinates
      output$message4 <- renderText(v$msg4) # Click coordinates
      output$message5 <- renderText(paste("Zoom level is",v$msg5)) # Zoom level
      output$message6 <- renderText(v$msg6) # Boundary Coordinates
    }
    })
  
  
  observe({
    if (input$select_data_level_tab1 == 'Municipality') {
      v <- reactiveValues(msg1=NULL) # Ward/Municipality/Province Name
      v <- reactiveValues(msg2=NULL) # Population Density
      v <- reactiveValues(msg3=NULL) # Click Coordinates
      v <- reactiveValues(msg4=NULL) # Click coordinates
      v <- reactiveValues(msg5=NULL) # Zoom level
      v <- reactiveValues(msg6=NULL) # Boundary Coordinates
      
      output$map1 <-renderLeaflet({
        leaflet() %>%
          setView(zoom=6,lng=26,lat=-29) %>%
          addTiles() %>%
          addTopoJSON(
            topojson=town_tj,stroke=T,color="white",opacity=1,fill=T,
            dashArray=3,weight=1
          ) %>% 
          addLegend(
            position = "bottomright", 
            pal = town_binpal,#(town_tj_spd@data$DENSITY),
            opacity = 1,
            labFormat = labelFormat(big.mark = " "),
            values = town_tj_spd@data$DENSITY
          )
      })
      
#     observe({
#       if (input$select_data_level_tab1 == 'Municipality') {
         observeEvent(input$map1_topojson_mouseover, label = "town_message_event",{
            v$msg1 <- input$map1_topojson_mouseover$properties$MUNICNAME
            v$msg2 <- input$map1_topojson_mouseover$properties$DENSITY
         })
         
         observeEvent(input$map1_topojson_mouseover, label = "town_proxymap_event", {
           proxy1 <- leafletProxy(
              "map1", data = subset(town_tj_spd, town_tj_spd@data$MUNICNAME == v$msg1)
            )
            single_data <- subset(town_tj_spd@data, town_tj_spd@data$MUNICNAME == v$msg1)
            proxy1 %>%
              clearShapes() %>%
              addPolygons(stroke=T,weight=3,color="#555555",opacity=1,
                          smoothFactor=1,fillColor="#FFFFFF",fillOpacity=0.2
              )
          })
         
      observeEvent(input$map1_shape_mouseout, {
        proxy <- leafletProxy(mapId = 'map1')
        proxy %>%
          clearShapes()
      })
          
      output$hoverInfo <- renderUI({
        if (is.null(v$msg2)) {
          return(
            div(
              paste("Hover over a", input$select_data_level_tab1)
            ))
        } else {
          return(
            div(
              strong(v$msg1),
              br(),
              span(round(v$msg2,1), HTML("people/km<sup>2</sup>"))
            )
          )
        }
      })
      
      observeEvent(input$map1_click, {
        v$msg3 <- paste(
          "Click coordinates: Lat",round(input$map1_click$lat,3),
          "Long",round(input$map1_click$lng,3))
      })
      observeEvent(input$map1_shape_click, {
        v$msg3 <- paste(
          "Click coordinates: Lat",round(input$map1_shape_click$lat,3),
          "Long",round(input$map1_shape_click$lng,3))
      })
      observeEvent(input$map1_zoom, {
        v$msg5 <- paste("Zoom level is",input$map1_zoom)
      })
      observeEvent(input$map1_bounds, {
        v$msg6 <- paste(
          "View Bounds: lat",substr(paste(input$map1_bounds[1]),start = 1, stop = 6),"long",
          substr(paste(input$map1_bounds[4]),start=1,stop=6),"(topleft); lat",
          substr(paste(input$map1_bounds[3]),start=1,stop=6),"long",
          substr(paste(input$map1_bounds[2]),start=1,stop=6),"(bottomright)")
      })
      output$message3 <- renderText(v$msg3) # Click Coordinates
      output$message4 <- renderText(v$msg4) # Click coordinates
      output$message5 <- renderText(v$msg5) # Zoom level
      output$message6 <- renderText(v$msg6) # Boundary Coordinates
    }
    })
  
  
  observe({
    if (input$select_data_level_tab1 == 'Province') {
      v <- reactiveValues(msg1=NULL) # Ward/Municipality/Province Name
      v <- reactiveValues(msg2=NULL) # Population Density
      v <- reactiveValues(msg3=NULL) # Click Coordinates
      v <- reactiveValues(msg4=NULL) # Click coordinates
      v <- reactiveValues(msg5=NULL) # Zoom level
      v <- reactiveValues(msg6=NULL) # Boundary Coordinates
      
      output$map1 <-renderLeaflet({
        leaflet() %>%
          setView(zoom=6,lng=26,lat=-29) %>%
          addTiles() %>%
          addTopoJSON(
            topojson=province_tj,stroke=T,color="white",opacity=1,fill=T,
            dashArray=3,weight=1
          ) %>% 
          addLegend(
            position = "bottomright", 
            pal = province_binpal,#(province_tj_spd@data$DENSITY),
            opacity = 1,
            labFormat = labelFormat(big.mark = " "),
            values = province_tj_spd@data$DENSITY
          )
      })
      
#     observe({
#       if (input$select_data_level_tab1 == 'Province') {
      observeEvent(input$map1_topojson_mouseover, label = "province_message_event",{ 
        v$msg1 <- input$map1_topojson_mouseover$properties$PROVINCE
        v$msg2 <- input$map1_topojson_mouseover$properties$DENSITY
      })
      
      observeEvent(input$map1_topojson_mouseover, label = "province_proxymap_event",{ 
        proxy1 <- leafletProxy(
          "map1", data = subset(province_tj_spd, province_tj_spd@data$PROVINCE == v$msg1)
        )
        single_data <- subset(province_tj_spd@data, province_tj_spd@data$PROVINCE == v$msg1)
        proxy1 %>%
          clearShapes() %>%
          addPolygons(stroke=T,weight=3,color="#555555",opacity=1,
                      smoothFactor=1,fillColor="#FFFFFF",fillOpacity=0.2
          )
      })
     
      observeEvent(input$map1_shape_mouseout, {
        proxy <- leafletProxy(mapId = 'map1')
        proxy %>%
          clearShapes()
      })
      
      output$hoverInfo <- renderUI({
        if (is.null(v$msg2)) {
          return(
            div(
              paste("Hover over a", input$select_data_level_tab1)
            ))
        } else {
          return(
            div(
              strong(v$msg1),
              br(),
              span(round(v$msg2,1), HTML("people/km<sup>2</sup>"))
            )
          )
        }
      })
      
      observeEvent(input$map1_click, {
        v$msg3 <- paste(
          "Click coordinates: Lat",round(input$map1_click$lat,3),
          "Long",round(input$map1_click$lng,3))
      })
      observeEvent(input$map1_shape_click, {
        v$msg3 <- paste(
          "Click coordinates: Lat",round(input$map1_shape_click$lat,3),
          "Long",round(input$map1_shape_click$lng,3))
      })
      observeEvent(input$map1_zoom, {
        v$msg5 <- paste("Zoom level is",input$map1_zoom)
      })
      observeEvent(input$map1_bounds, {
        v$msg6 <- paste(
          "View Bounds: lat",substr(paste(input$map1_bounds[1]),start = 1, stop = 6),"long",
          substr(paste(input$map1_bounds[4]),start=1,stop=6),"(topleft); lat",
          substr(paste(input$map1_bounds[3]),start=1,stop=6),"long",
          substr(paste(input$map1_bounds[2]),start=1,stop=6),"(bottomright)")
      })
      output$message3 <- renderText(v$msg3) # Click Coordinates
      output$message4 <- renderText(v$msg4) # Click coordinates
      output$message5 <- renderText(v$msg5) # Zoom level
      output$message6 <- renderText(v$msg6) # Boundary Coordinates
    }
    })
  
#   output$hoverInfo <- renderUI({
#     if (is.null(v$msg2)) {
#       return(
#         div(
#           paste("Hover over a", input$select_data_level_tab1)
#           ))
#     } else if (input$select_data_level_tab1 == 'Ward'){
#       return(
#         div(
#           strong(
#             sub(
#               pattern=" Local Municipality| Metropolitan Municipality|Local Municipality of ",
#               replacement="",
#               #x=input$map1_topojson_mouseover$properties$MUNICNAME | 
#                 x=input$map1_shape_mouseover$properties$MUNICNAME)),
#           br(),
#           "Ward",
#           v$msg1,
#           br(),
#           span(v$msg2, HTML("people/km<sup>2</sup>"))
#         )
#       )
#       } else {
#         return(
#           div(
#             strong(v$msg1),
#             br(),
#             span(v$msg2, HTML("people/km<sup>2</sup>"))
#           )
#         )
#       }
#   })
# #  observeEvent(input$map1_topojson_click, {
# #    v$msg3 <- paste(
# #      "Click coordinates: Lat",round(input$map1_topojson_click$lat,3),
# #      "Long",round(input$map1_topojson_click$lng,digits=3))
# #  })
#   observeEvent(input$map1_click, {
#     v$msg3 <- paste(
#       "Click coordinates: Lat",round(input$map1_click$lat,3),
#       "Long",round(input$map1_click$lng,3))
#   })
#   observeEvent(input$map1_shape_click, {
#     v$msg3 <- paste(
#       "Click coordinates: Lat",round(input$map1_shape_click$lat,3),
#       "Long",round(input$map1_shape_click$lng,3))
#   })
#   observeEvent(input$map1_zoom, {
#     v$msg5 <- paste("Zoom level is",input$map1_zoom)
#   })
#   observeEvent(input$map1_bounds, {
#     v$msg6 <- paste(
#       "View Bounds: lat",substr(paste(input$map1_bounds[1]),start = 1, stop = 6),"long",
#       substr(paste(input$map1_bounds[4]),start=1,stop=6),"(topleft); lat",
#       substr(paste(input$map1_bounds[3]),start=1,stop=6),"long",
#       substr(paste(input$map1_bounds[2]),start=1,stop=6),"(bottomright)")
#   })
#   output$message3 <- renderText(v$msg3) # Click Coordinates
#   output$message4 <- renderText(v$msg4) # Click coordinates
#   output$message5 <- renderText(v$msg5) # Zoom level
#   output$message6 <- renderText(v$msg6) # Boundary Coordinates
  
  observe({
    if (input$select_data_level_tab2=='Ward') {
      output$table=DT::renderDataTable(
        ward_tj_list$objects$Wards2011$geometries$properties, server=T,
        options=list(pageLength = 50)
        )
    }
    })
  observe({
    if (input$select_data_level_tab2=='Municipal') {
      output$table=DT::renderDataTable(
        town_tj_list$objects$LocalMunicipalities2011$geometries$properties,
        server=T,options=list(pageLength=50)
        )
    }
    })
  observe({
    if (input$select_data_level_tab2=='Provincial') {
      output$table=DT::renderDataTable(
        province_tj_list$objects$Province_New_SANeighbours$geometries$properties,
        server=T
        )
    }
    })
}


shinyApp(ui, server)

