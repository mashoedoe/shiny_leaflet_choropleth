library(shiny)
library(leaflet)
library(DT)

load("R/v1.RData")

ui <- bootstrapPage(
    # Add CSS to give floating Panels a style
    tags$head(tags$style(".floater { background-color: white;
                       padding: 8px; opacity: 0.7; border-radius: 6px; 
                       box-shadow: 0 0 15px rgba(0,0,0,0.2); }")),
    title = "South African Demographics",
    leafletOutput(outputId = 'map1', width = "100%", height = "650px"),
    absolutePanel(
        right=0,top=70,width =275, class="floater",
        h3("SA Population Density"),
        uiOutput('hoverInfo')
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
    ),
    absolutePanel(   # Click coordinates
        left=1200,top=720,width =100,
        textOutput(outputId='message7',container=span,inline=T)
    )
)
  

server <- function(input,output,session) {
    
    v <- reactiveValues(msg1=NULL) # Municipality Name
    v <- reactiveValues(msg2=NULL) # Population Density
    v <- reactiveValues(msg3=NULL) # Click Coordinates
    v <- reactiveValues(msg4=NULL) # Click coordinates
    v <- reactiveValues(msg5=NULL) # Zoom level
    v <- reactiveValues(msg6=NULL) # Boundary Coordinates
    v <- reactiveValues(msg7=0)
    
    observeEvent(input$map1_topojson_mouseover, {
        v$msg7 <- 1
    })
    observeEvent(input$map1_shape_mouseout, {
        v$msg7 <- 0
    })
    
    
    output$map1 <-renderLeaflet({
        leaflet() %>%
            setView(zoom=6,lng=26,lat=-29) %>%
            addTiles() %>%
            addTopoJSON(
                topojson=town_tj,stroke=T,color="white",opacity=1,fill=T,
                weight=1
            ) %>% 
            addLegend(
                position = "bottomright", 
                pal = town_binpal,#(town_tj_spd@data$DENSITY),
                opacity = 1,
                labFormat = labelFormat(big.mark = " "),
                values = town_tj_spd@data$DENSITY
            )
    })
    
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
            addPolygons(stroke=T,weight=4,color="#FFFFFF",opacity=1,
                        smoothFactor=1,fill=T,fillOpacity=0
            )
    })
    
    observeEvent(input$map1_shape_mouseout, {
        proxy <- leafletProxy(mapId = 'map1')
        proxy %>%
            clearShapes()
    })
    
    output$hoverInfo <- renderUI({
        if (v$msg7 == 0) {
            return(
                div(
                    paste("Hover over a Municipality")
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
    output$message7 <- renderText(paste("Actice mouseover events =", v$msg7)) # Mouse Events
}
shinyApp(ui, server)

