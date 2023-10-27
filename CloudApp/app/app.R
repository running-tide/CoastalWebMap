library(shiny)
library(leaflet)
library(leaflet.extras)
library(leaflet.esri)
library(htmlwidgets)
library(raster) #need to install
library(sf)


img_dir = "images"

animal = st_read("geojson/Scallops.geojson")
cmwl = st_read("geojson/CleanMaster_Windlease.geojson")
taom = raster("tifs/MODIS_SST_2010-2021_1.tif")
ca = raster("tifs/bathymetry.tif")
cont = st_read("geojson/Contors_5_110_15r.geojson")
sed = raster("tifs/Sedimenttiff.tif")

st_drivers()

cat2 = colorNumeric(palette = c('#005aff', '#43c8c8','#77DD77', '#fff700', '#ff0000', "#B42222"),
                    domain = c(-2:30), na.color = "transparent")

cat3 = colorNumeric(palette = c('#3500a8', '#0800ba','#003fd6', '#00aca9', '#77f800', "#ff8800",
                                '#b30000', '#920000', "#880000"), domain = c(0:10),  na.color = "transparent")
vis3 = list(min= 0,max= 10,
            palette= c('#3500a8', '#0800ba','#003fd6', '#00aca9', '#77f800', "#ff8800",
                       '#b30000', '#920000', "#880000"))

pal <- colorBin (
  palette = c("blue","purple","red","yellow"), domain = NULL,n = 7, pretty=TRUE)


opu1 = .8
# need to add code to use these
opu2 = 0.3


ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                checkboxInput("bath", "Bathymetry", FALSE),
                sliderInput("bathint", "Depth", -100, 0,
                            value = c(-20, -10), step = 1),
                checkboxInput("ptem", "Tempurature", FALSE),
                checkboxInput("pchl", "Chlorophyll", FALSE),
                sliderInput("monthv", "Month", 1, 12,
                            value = 1, step = 1),
                checkboxInput("ani", "Animal Density", FALSE),
                selectInput("aninum", label = ("Select Animal Dataset"), 
                            choices = (names(animal)[c(-1,-8,-9)])),
                checkboxInput("ploo", "WindEnergy Lease", FALSE),
                selectInput("insp", label = "inspector tool (click to retrieve value)", 
                            choices = c("Temp","Chlor","Depth"))
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
      addEsriBasemapLayer(esriBasemapLayers$Oceans, autoLabels = TRUE)%>%
      setView(-72.65, 40.0285, zoom = 7) %>% 
      addRasterImage(sed, group = "Temp", colors = cat2, opacity = 0.8) %>% 
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
  
  #/////////////    ObserverEvent          ani  (animal) ////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
  
  observeEvent(c(input$ani,input$aninum),{
    
    if(input$ani){
      i = theaninum()
      leafletProxy("map") %>%
        clearGroup("Scallop Density") %>%
        addPolygons(data = animal[i],  color = ~pal(animal[[i]]),
                    fill = TRUE, group = "Scallop Density", 
                    fillOpacity = animal[[i]] / max(animal[[i]]), #maybe try quantile(animal$DepthAve)[[3]]
                    weight = 0)%>% 
        addLegend("bottomleft", pal = nap(),layerId = "qw", values = animal[[i]], # <br> 
                  title = 'Animal Abundance <br><small> <small> From <a href="https://www.northeastoceandata.org/files/metadata/Themes/Habitat/AveragePresenceAbundanceSMAST.pdf"> SMAST </a> video <br> Survey (2003-2012) </small> </small>',
                  #labFormat = labelFormat(suffix = ""),
                  opacity = .8, group = "Scallop Density")}
    else{
      leafletProxy("map") %>%
        clearGroup("Scallop Density") %>% 
        removeControl("qw")
    }})
  
  
  #/////////////    ObserverEvent          bath  (Bathem) ////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
  
  observeEvent(c(input$bath,input$bathint),{
    cat4 = bathcat()
    if(input$bath){
      leafletProxy("map") %>%
        clearGroup("bas") %>%
        addRasterImage(x = ca, colors = cat4, opacity = 0.3, group = "bas")}
    
    else{
      leafletProxy("map") %>%
        clearGroup("bas") 
    }})
  
  
  
  
  #/////////////    ObserverEvent          ploo  (windlease) ////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
  
  observeEvent(input$ploo,{
    if(input$ploo){
      proxy = leafletProxy("map")%>%
        clearGroup("Wind Lease Area") %>% 
        addPolygons(data = cmwl,  color = "green", fill = TRUE, group = "Wind Lease Area",
                    stroke = TRUE, fillOpacity = .2, weight = 1, popup = 
                      paste(sep = "<br/>",
                            c(paste0("<b>",cmwl$COMPANY,"</b>")),
                            cmwl$LEASE_NU_1,
                            c(paste0("<b>",cmwl$STATE,"</b>")),
                            cmwl$RESOURCE,
                            cmwl$PROTRACTIO_2,
                            cmwl$PROTRACT_1,
                            c(paste0("<a href='",cmwl$LEASE_DOCU,"' >Doc_1</a>")),
                            c(paste0("<a href='",cmwl$LEASE_DO_1,"' >Doc_2</a>"))))%>%
        addLegend("bottomleft",layerId = "as", labels ='Wind Lease Area  <br><small> <small> Aggregated permitted, planning <br> and operational offshore wind <br> files from <a from href="https://portal.midatlanticocean.org/data-catalog/energy/#collapse-layer-5537"> MARCO </a> </small> </small>', 
                  colors = "green", group = "Wind Lease Area") }
    else{
      proxy = leafletProxy("map") %>%
        clearGroup("Wind Lease Area")  %>% 
        removeControl("as")
    }})
  
  
  
  
  #/////////////    ObserverEvent          ptem  (Temp) ////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
  
  observeEvent(c(input$ptem,input$monthv), {
    if(input$ptem){
      dep = temget()
      
      proxy = leafletProxy("map") %>%
        clearGroup("Temp") %>% 
        addRasterImage(dep, group = "Temp", colors = cat2, opacity = 0.8) %>% 
        addLegend("bottomleft", pal = cat2, values = c(-2:24), layerId = "er",
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
        local$rast = temget()
      }
      if (cho == 2) {
        local$rast = chlorget()
      }
      if (cho == 3) {
        local$rast = ca
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
