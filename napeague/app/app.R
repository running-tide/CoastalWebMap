library(shiny)
library(leaflet)
library(leaflet.extras)
library(leaflet.esri)
library(sf)
library(htmlwidgets)
library(raster)

#setwd("app/")

cmwl = st_read("geojson/NapeagueBay.geojson")
dep = raster("tifs/naibagbath.tif")
sed = raster("tifs/sedime.tif")


cap = c("#7e7e7e40"   ,"#7e7e7e40"   ,"#7e7e7e40"   ,"#7e7e7e40"  ,"#ffbf9740"  ,"#ffbf9740"  ,"#ffbf9740"  ,"#B4222240"  ,"#B4222240"  ,"#B4222240"  ,"#77DD7740"  ,"#77DD7740","#77DD7740" ,"#ffbf9780" ,"#B4222280" ,"#77DD7780")
cat5 = colorFactor(palette = cap, domain = values(sed), na.color = "transparent", alpha = TRUE)
cat5c = colorFactor(palette = c( "#77DD77", "#7e7e7e", "#B42222"  , "#ffbf97"), domain = c("Mix","Mud","Gravel","Sand"))


cat2 = colorNumeric(palette = c('#000000', '#43c8c8',"#ffbf97",'#77DD77', '#005aff'),
                    domain = c(-120:0), na.color = "transparent")




ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                checkboxInput("ploo", "NapeagueBay Lease", FALSE),
                checkboxInput("ptem", "Bathymetry", FALSE),
                checkboxInput("sedi", "Sediment", FALSE)
                ))


server <- shinyServer(function(input, output, session){
  local <- reactiveValues(
    rast = NULL,
    raster_value = data.frame(value = NA, long = 0, lat = 0)
  )

  
  #//////////////////////\\\\\\\\\\\\\\\\\\\\\/////////////////////////////
  #/////////////    Base map             \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
  #//////////////////////\\\\\\\\\\\\\\\\\\\\\/////////////////////////////
  
  output$map <- renderLeaflet({
    leaflet() %>% 
      addEsriBasemapLayer(esriBasemapLayers$Oceans, autoLabels = TRUE,
                          options = providerTileOptions(maxZoom = 13)
                          )%>%
      setView(-72.1, 41.0285, zoom = 12) %>% 
      addScaleBar(position = "bottomright") %>% 
      onRender(
        "function(el,x){
                    this.on('click', function(e) {
                        var lat = e.latlng.lat;
                        var lng = e.latlng.lng;
                        var coord = [lat, lng];
                        Shiny.onInputChange('hover_coordinates', coord)
                    });
                }"
      )
    
  })
  

  #/////////////    ObserverEvent          ploo  (windlease) ////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
  
  observeEvent(input$ploo,{
    if(input$ploo){
      proxy = leafletProxy("map")%>%
        clearGroup("NapeagueBay Lease") %>% 
        addPolygons(data = cmwl,  color = "green", fill = TRUE, group = "NapeagueBay Lease",
                    stroke = TRUE, fillOpacity = .2, weight = 1)%>%
        addLegend("bottomleft",layerId = "as", labels ='NapeagueBay Lease', 
                  colors = "green", group = "FOOTPRINT") }
    else{
      proxy = leafletProxy("map") %>%
        clearGroup("NapeagueBay Lease")  %>% 
        removeControl("as")
    }})
  
  
  
  
  #/////////////    ObserverEvent          ptem  (Temp) ////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
  
  observeEvent(c(input$ptem,input$monthv), {
    if(input$ptem){

      
      proxy = leafletProxy("map") %>%
        clearGroup("bath") %>% 
        addRasterImage(dep, group = "bath", colors = cat2, opacity = 0.8) %>% 
        addLegend("bottomleft", pal = cat2, values = c(-120:0), layerId = "er",
                  title = "Bathymetry (m)",opacity = .8)}
    else{
      proxy = leafletProxy("map") %>%
        clearGroup("bath") %>% 
        removeControl("er")
    }})
  
  
  #/////////////    ObserverEvent          sedi  (sediment) ////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
  
  observeEvent(input$sedi,{
    
    if(input$sedi){
      leafletProxy("map") %>%
        clearGroup("sad") %>%
        addRasterImage(x = sed, colors = cat5, opacity = 1, group = "sad") %>% 
        addLegend(pal = cat5c, values = c("Mix","Mud","Gravel","Sand"), group = "sad", 
                  position = "bottomleft", layerId = "assa", title = 'Sediment <br><small> <small> <a from href="https://portal.midatlanticocean.org/static/data_manager/metadata/html/SoftSediment_2020_metadata.html"> NAMERA survey </a> </small> </small>')}
    
    else{
      leafletProxy("map") %>%
        clearGroup("sad") %>% 
        removeControl("assa")
    }})
  

  
  #/////////////    Observer          inst  (Chlo) ////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
  
  
  observe({
    invalidateLater(30)
  })
  
  observeEvent(
    c(input$insp,input$monthv),{
      cho = (1:3)[(c("Temp","Chlor","Depth") == input$insp)]
      if (cho == 1) {
        local$rast = dep
      }
      if (cho == 2) {
        local$rast = dep
      }
      if (cho == 3) {
        local$rast = dep
      }
    }
  )
  
  observeEvent(
    input$hover_coordinates[1],{# extract raster value based on input$hover_coordinates      
      print(input$hover_coordinates[1])
      print(input$hover_coordinates[2])
      x <- raster::extract(
        local$rast,
        matrix(
          c(input$hover_coordinates[2], input$hover_coordinates[1]),
          nrow = 1
        )
      )
      
      # cursor lat/long and corresponding raster value in a reactive data.frame
      local$raster_value <- data.frame(
        value = round(x, 2),
        long = input$hover_coordinates[2],
        lat = input$hover_coordinates[1]
      )
      
      proxy = leafletProxy("map")%>%
        clearGroup("Clic")
      
      leafletProxy("map") %>%
        addCircleMarkers(
          data = local$raster_value,
          label = ~ value,
          group = "Clic",
          stroke = F,
          fill = TRUE,
          fillColor = "#00000000",
          radius = 2
        )
    }
  )
  
  
  
  
  
})

shinyApp(ui = ui, server = server)
