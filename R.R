library(leaflet)
#library(sp)
library(rgdal)
#library(sf)
#library(dplyr)
#library(leaflet.extras)
library(leaflet.esri)



animal = readOGR("animallops.geojson")
windfoot = readOGR("Windlease_Footprint.geojson")
sed = readOGR("Sedi.geojson")

sed$op = NA
sed$class = "Mix"
sed$class[sed$Sediment %in% c(1000,120,150,170)] = "Sand"
sed$class[sed$Sediment %in% c(2000,210,250,260)] = "Mud"
sed$class[sed$Sediment %in% c(5000,510,520,530)] = "Rock"
sed$class = factor(sed$class)asd
sed$op[sed$Sediment %in% c(1000,2000,5000)] = 0.4
sed$op[sed$Sediment %in% c(120,150,170,201,250,260,510,520,530)] = 0.2
sed$op[is.na(sed$op)] = 0.1

pal <- colorBin (
    palette = c("blue","purple","red","yellow"),
    domain = animal$animallopAve,
    n = 7, pretty=TRUE)

cat <- colorFactor(c("#444444","#B42222","#77DD77","#ffbf97"), sed$class)

#\\\\\\\\\\\\\\\\\///////////////////////\\\\\\\\\\\\\\\//////////////////////////

m <- leaflet() %>%
  addEsriBasemapLayer(esriBasemapLayers$Oceans, autoLabels = TRUE) %>% 
  
  
  setView(-72.65, 40.0285, zoom = 7) %>% 
  
  
  #sediment
  addPolygons(data = sed,  color = cat(sed$class), fill = TRUE, group = "Windsase",
              stroke = FALSE, fillOpacity = sed$op) %>% 
  #
  addPolygons(data = animal,  color = ~pal(animal$animallopAve),
              fill = TRUE, group = "Animal Density",
              fillOpacity = animal$animallopAve / max(animal$animallopAve),
              weight = 0) %>% 
  addPolygons(data = windfoot,  color = "green", fill = TRUE, group = "Wind Lease",
              stroke = TRUE, fillOpacity = .2) %>% 
  
  
  addLegend("bottomright", pal = pal, values = animal$animallopAve, # <br> 
            title = 'animallop Abundance <br><small> <small> From <a href="https://www.r-project.org/"> SMAST </a> video <br> Survey (2003-2012) </small> </small>',
            #labFormat = labelFormat(suffix = ""),
            opacity = .8, group = "Animal Density") %>% 
  addLegend("bottomright",labels ='Wind Lease  <br><small> <small> Aggregated permitted, planning <br> and operational offshore wind <br> files from <a from href="https://portal.midatlanticocean.org/data-catalog/energy/#collapse-layer-5537"> MARCO </a> </small> </small>', 
            colors = "green", group = "Wind Lease") %>% 
  addLegend("bottomright", pal = cat, values = sed$class,
            title = "Sediment",opacity = .8, group = "Windsase") %>% 
  
  
  addLayersControl(overlayGroups = c("Animal Density", "Wind Lease","Windsase"),
                   options = layersControlOptions(collapsed = FALSE))
  



m






