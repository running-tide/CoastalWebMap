library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinythemes)
library(leaflet)
library(leaflet.extras)
library(leaflet.esri)
library(rgdal)



img_dir = "images"



#animal = readOGR("geojson/Scallops.geojson")
#cmwl = readOGR("geojson/CleanMaster_Windlease.geojson")



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
                checkboxInput("ani", "Animal Density", FALSE)
  )
)


server <- shinyServer(function(input, output, session){
  
  
  # theaninum = reactive({
  #   (1:length(names(animal)))[names(animal) == input$aninum]
  # })
  # nap = reactive({
  #   colorBin (
  #     palette = c("blue","purple","red","yellow"),
  #     domain = animal[[theaninum()]],
  #     n = 7, pretty=TRUE)})
    
  
  output$map <- renderLeaflet({
    leaflet() %>% 
      addEsriBasemapLayer(esriBasemapLayers$Oceans, autoLabels = TRUE)%>%
      setView(-72.65, 40.0285, zoom = 7)
  })
  
})

shinyApp(ui = ui, server = server)
