library(shiny)
library(leaflet)
library(leaflet.extras)
library(leaflet.esri)
library(sf)
library(htmlwidgets)
library(raster)

cmwl = st_read("geojson/NapeagueBay.geojson")
dep = raster("tifs/napeaguebaybath.tif")

cat2 = colorNumeric(palette = c('#005aff', '#43c8c8','#77DD77', '#fff700'),
                    domain = c(-11:2), na.color = "transparent")




ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                checkboxInput("ploo", "NapeagueBay Lease", TRUE),
                checkboxInput("ptem", "Bathymetry", TRUE),
                ))


server <- shinyServer(function(input, output, session){
  
  local <- reactiveValues(
    rast = NULL,
    raster_value = data.frame(value = NA, long = 0, lat = 0)
  )
  theaninum = reactive({
    (1:length(names(animal)))[names(animal) == input$aninum]
  })
  nap = reactive({
    colorBin (
      palette = c("blue","purple","red","yellow"),
      domain = animal[[theaninum()]],
      n = 7, pretty=TRUE)
  })
  temget = reactive({
    raster(paste0("tifs/MODIS_SST_2010-2021_",input$monthv[1], ".tif"))
  })
  chlorget = reactive({
    raster(paste0("tifs/MODIS_chlor_2010-2021_",input$monthv[1], ".tif"))
  })
  bathcat = reactive({
    colorNumeric(palette =  '#baaf06',
                 domain = c(input$bathint[2]:input$bathint[1]),  na.color = "transparent")
  })
  
  #//////////////////////\\\\\\\\\\\\\\\\\\\\\/////////////////////////////
  #/////////////    Base map             \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
  #//////////////////////\\\\\\\\\\\\\\\\\\\\\/////////////////////////////
  
  output$map <- renderLeaflet({
    leaflet() %>% 
      addEsriBasemapLayer(esriBasemapLayers$Oceans, autoLabels = TRUE,
                          options = providerTileOptions(maxZoom = 13)
                          )%>%
      setView(-72.1, 41.0285, zoom = 12) %>% 
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
        clearGroup("Wind Lease Area") %>% 
        addPolygons(data = cmwl,  color = "green", fill = TRUE, group = "Wind Lease Area",
                    stroke = TRUE, fillOpacity = .2, weight = 1)%>%
        addLegend("bottomleft",layerId = "as", labels ='Wind Lease Area', 
                  colors = "green", group = "Wind Lease Area") }
    else{
      proxy = leafletProxy("map") %>%
        clearGroup("Wind Lease Area")  %>% 
        removeControl("as")
    }})
  
  
  
  
  #/////////////    ObserverEvent          ptem  (Temp) ////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
  
  observeEvent(c(input$ptem,input$monthv), {
    if(input$ptem){

      
      proxy = leafletProxy("map") %>%
        clearGroup("Temp") %>% 
        addRasterImage(dep, group = "Temp", colors = cat2, opacity = 0.8) %>% 
        addLegend("bottomleft", pal = cat2, values = c(-11:2), layerId = "er",
                  title = "TEMP(c)",opacity = .8)}
    else{
      proxy = leafletProxy("map") %>%
        clearGroup("Temp") %>% 
        removeControl("er")
    }})
  
  #/////////////    ObserverEvent          pchl  (Chlo) ////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
  
  observeEvent(c(input$pchl,input$monthv),{
    if(input$pchl){
      chat = chlorget()
      
      proxy = leafletProxy("map") %>%
        clearGroup("chlo") %>% 
        addRasterImage(chat, group = "chlo", colors = cat3,opacity = 0.8) %>% 
        addLegend("bottomleft", pal = cat3, values = c(0:10), layerId = "dr",
                  title = "Chlorophil	mg/m^3",opacity = .8 )}
    else{
      proxy = leafletProxy("map") %>%
        clearGroup("chlo") %>% 
        removeControl("dr")
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
