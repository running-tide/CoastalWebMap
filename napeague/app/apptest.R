library(shiny)
library(leaflet)
library(leaflet.extras)
library(leaflet.esri)
library(rgdal)
library(htmlwidgets)
library(raster) #need to install

# animal = rgdal::readOGR("geojson/Scallops.geojson")
# cmwl = rgdal::readOGR("geojson/CleanMaster_Windlease.geojson")

temp = list()
chlo = list()

for (i in 1:12) {
  temp[[i]] = raster(paste0("tifs/MODIS_SST_2010-2021_",i,".tif"))
}



cat2 = colorNumeric(palette = c('#005aff', '#43c8c8','#77DD77', '#fff700', '#ff0000', "#B42222"),
                    domain = c(-2:30), na.color = "transparent")

cat3 = colorNumeric(palette = c('#3500a8', '#0800ba','#003fd6', '#00aca9', '#77f800', "#ff8800",
                                '#b30000', '#920000', "#880000"), domain = c(0:10),  na.color = "transparent")

vis3 = list(min= 0,max= 10,
            palette= c('#3500a8', '#0800ba','#003fd6', '#00aca9', '#77f800', "#ff8800",
                       '#b30000', '#920000', "#880000"))

pal <- colorBin (
  palette = c("blue","purple","red","yellow"), domain = NULL,n = 7, pretty=TRUE)



ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                sliderInput("range", "Year", 2011, 2021,
                            value = c(2015, 2021), step = 1),
                sliderInput("monthv", "Month", 1, 12,
                            value = 1, step = 1),
                checkboxInput("ani", "Animal Density", FALSE),
                checkboxInput("win", "WindEnergy Lease", FALSE),
                checkboxInput("ptem", "Tempurature", FALSE),
                checkboxInput("pchl", "Chlorophyll", FALSE),
                checkboxInput("insp", "Inspector", FALSE),
                selectInput("aninum", label = h3("Select Animal Dataset"), 
                            choices = (names(animal)[c(-1,-8,-9)]))))


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
      domain = animal[[(1:length(names(animal)))[names(animal) == input$aninum]]],
      n = 7, pretty=TRUE)})
  temget = reactive({
    raster(paste0("tifs/MODIS_SST_2010-2021_",input$monthv[1], ".tif"))
  })
  chlorget = reactive({
    raster(paste0("tifs/MODIS_chlor_2010-2021_",input$monthv[1], ".tif"))
  })
  
  
  #//////////////////////\\\\\\\\\\\\\\\\\\\\\/////////////////////////////
  #/////////////    Base map             \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
  #//////////////////////\\\\\\\\\\\\\\\\\\\\\/////////////////////////////
  
  output$map <- renderLeaflet({
    leaflet() %>% 
      addEsriBasemapLayer(esriBasemapLayers$Oceans, autoLabels = TRUE)%>%
      setView(-72.65, 40.0285, zoom = 7) %>% 
      onRender(
        "function(el,x){
                    this.on('mousemove', function(e) {
                        var lat = e.latlng.lat;
                        var lng = e.latlng.lng;
                        var coord = [lat, lng];
                        Shiny.onInputChange('hover_coordinates', coord)
                    });
                    this.on('mouseout', function(e) {
                        Shiny.onInputChange('hover_coordinates', null)
                    })
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
  
  
  
  #/////////////    ObserverEvent          win  (windlease) ////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
  
  observeEvent(input$win,{
    if(input$win){
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
  
  observeEvent(c(input$ptem,input$monthv), priority = 1, {
    if(input$ptem){
      proxy = leafletProxy("map") %>%
        clearGroup("Temp") %>% 
        addRasterImage(temp[[input$monthv]], group = "Temp", colors = cat2, opacity = 0.8) %>% 
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
        addRasterImage(chat,
                       group = "chlo", colors = cat3,
                       opacity = 0.7) %>% 
        addLegend("bottomleft", pal = cat3, values = c(0:10), layerId = "dr",
                  title = "Chlorophil	mg/m^3",opacity = .8 )}
    else{
      proxy = leafletProxy("map") %>%
        clearGroup("chlo") %>% 
        removeControl("dr")
    }})
  
  #/////////////    inspector code    insp   ////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
  
  
  observeEvent(
    input$hover_coordinates[1],
    {if(input$ptem){# extract raster value based on input$hover_coordinates
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
    }}
  )
  
  # Use "addCircleMarkers" to generate popup containing raster value
  observeEvent(
    local$raster_value, 
    {
      if(input$ptem){
        
        leafletProxy("map") %>%
          addCircleMarkers(
            data = local$raster_value,
            label = ~ value,
            labelOptions = labelOptions(
              style = list(
                "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                "font-size" = "12px",
                "font-weight" = "bold",
                "border-color" = "rgba(0,0,0,0.5)"
              )
            ),
            stroke = FALSE,
            fill = TRUE,
            fillColor = "#00000000",
            radius = 8
          )
      }}
  )
  
  
})

shinyApp(ui = ui, server = server)
