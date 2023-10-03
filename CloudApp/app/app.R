library(shiny)
library(leaflet)
library(leaflet.extras)
library(leaflet.esri)
library(rgdal)



img_dir = "images"

animal = rgdal::readOGR("geojson/Scallops.geojson")
cmwl = rgdal::readOGR("geojson/CleanMaster_Windlease.geojson")



cat2 = colorNumeric(palette = c(  '#005aff', '#43c8c8','#77DD77', '#fff700', '#ff0000', "#B42222"), domain = c(-2:24))

visP = list(min= -2.0,max= 24.0,
            palette= c('#005aff', '#43c8c8','#77DD77', '#fff700', '#ff0000', "#B42222"))

cat3 = colorNumeric(palette = c('#3500a8', '#0800ba','#003fd6', '#00aca9', '#77f800', "#ff8800",
                                '#b30000', '#920000', "#880000"), domain = c(0:10))

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
                checkboxInput("ploo", "WindEnergy Lease", FALSE),
                checkboxInput("ptem", "Tempurature", FALSE),
                checkboxInput("pchl", "Chlorophyll", FALSE),
                selectInput("aninum", label = h3("Select Animal Dataset"), 
                            choices = (names(animal)[c(-1,-8,-9)]))))


server <- shinyServer(function(input, output, session){
  
  
  theaninum = reactive({
    (1:length(names(animal)))[names(animal) == input$aninum]
  })
  nap = reactive({
    colorBin (
      palette = c("blue","purple","red","yellow"),
      domain = animal[[theaninum()]],
      n = 7, pretty=TRUE)})
    
  #//////////////////////\\\\\\\\\\\\\\\\\\\\\/////////////////////////////
  #/////////////    Base map             \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
  #//////////////////////\\\\\\\\\\\\\\\\\\\\\/////////////////////////////
  
  output$map <- renderLeaflet({
    leaflet() %>% 
      addEsriBasemapLayer(esriBasemapLayers$Oceans, autoLabels = TRUE)%>%
      setView(-72.65, 40.0285, zoom = 7)
  })
  
  #/////////////    ObserverEvent          ani  (animal) ////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
  
  observeEvent(c(input$ani,input$aninum),{
    if(input$ani){
      leafletProxy("map") %>%
        clearGroup("Scallop Density") %>%
        addPolygons(data = animal[theaninum()],  color = ~pal(animal[[theaninum()]]),
                    fill = TRUE, group = "Scallop Density", 
                    fillOpacity = animal[[theaninum()]] / max(animal[[theaninum()]]), #maybe try quantile(animal$DepthAve)[[3]]
                    weight = 0)%>% 
        addLegend("bottomleft", pal = nap(),layerId = "qw", values = animal[[theaninum()]], # <br> 
                  title = 'Animal Abundance <br><small> <small> From <a href="https://www.northeastoceandata.org/files/metadata/Themes/Habitat/AveragePresenceAbundanceSMAST.pdf"> SMAST </a> video <br> Survey (2003-2012) </small> </small>',
                  #labFormat = labelFormat(suffix = ""),
                  opacity = .8, group = "Scallop Density")}
    else{
      leafletProxy("map") %>%
        clearGroup("Scallop Density") %>% 
        removeControl("qw")
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
  
  
  
})

shinyApp(ui = ui, server = server)
