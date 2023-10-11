library(leaflet)
#library(sp)
library(rgdal)
#library(sf)
#library(dplyr)
#library(leaflet.extras)
library(leaflet.esri)
library(htmltools)



#animal = readOGR("geojson/Scallops.geojson")
#windfoot = readOGR("geojson/Windlease_Footprint.geojson")
#sed = readOGR("geojson/Sediment.geojson")
#cmwl = readOGR("geojson/CleanMaster_Windlease.geojson")





sed$op = NA
sed$class = "Mix"
sed$class[sed$Sediment %in% c(1000,120,150,170)] = "Sand"
sed$class[sed$Sediment %in% c(2000,210,250,260)] = "Mud"
sed$class[sed$Sediment %in% c(5000,510,520,530)] = "Rock"
sed$class = factor(sed$class)
sed$op[sed$Sediment %in% c(1000,2000,5000)] = 0.4
sed$op[sed$Sediment %in% c(120,150,170,201,250,260,510,520,530)] = 0.2
sed$op[is.na(sed$op)] = 0.1






pal <- colorBin (
    palette = c("blue","purple","red","yellow"),
    domain = animal$ScallopAve,
    n = 7, pretty=TRUE)

cat <- colorFactor(c("#444444","#B42222","#77DD77","#ffbf97"), sed$class)

cat2 = colorNumeric(palette = c(  '#005aff', '#43c8c8','#77DD77', '#fff700', '#ff0000', "#B42222"), domain = c(-2:24))
cat3 = colorNumeric(palette = c('#3500a8', '#0800ba','#003fd6', '#00aca9', '#77f800', "#ff8800",
                                '#b30000', '#920000', "#880000"), domain = c(0:10))


#\\\\\\\\\\\\\\\\\///////////////////////\\\\\\\\\\\\\\\//////////////////////////

m <- leaflet() %>%
  addEsriBasemapLayer(esriBasemapLayers$Oceans, autoLabels = TRUE) %>% 

  addTiles(urlTemplate = ted$rgee$tokens,
    group = "FEB-mean-Temp-2011-21(MODIS)",options = leaflet::tileOptions(opacity = 0.7)) %>%
  
  addTiles(urlTemplate = tec$rgee$tokens,
           group = "FEB-mean-CHL-2011-21(MODIS)",options = leaflet::tileOptions(opacity = 0.7)) %>%
  
  setView(-72.65, 40.0285, zoom = 7) %>% 
  
  
  #sediment
  addPolygons(data = sed,  color = cat(sed$class), fill = TRUE, group = "Sediment Type",
              stroke = FALSE, fillOpacity = sed$op) %>% 
  #
  addPolygons(data = animal,  color = ~pal(animal$ScallopAve),
              fill = TRUE, group = "Scallop Density",
              fillOpacity = animal$ScallopAve / max(animal$ScallopAve),
              weight = 0) %>% 
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
                c(paste0("<a href='",cmwl$LEASE_DO_1,"' >Doc_2</a>"))
                )) %>% 

    
  
  addLegend("bottomright", pal = pal, values = animal$ScallopAve, # <br> 
            title = 'Scallop Abundance <br><small> <small> From <a href="https://www.northeastoceandata.org/files/metadata/Themes/Habitat/AveragePresenceAbundanceSMAST.pdf"> SMAST </a> video <br> Survey (2003-2012) </small> </small>',
            #labFormat = labelFormat(suffix = ""),
            opacity = .8, group = "Scallop Density") %>% 
  addLegend("bottomright",labels ='Wind Lease Area  <br><small> <small> Aggregated permitted, planning <br> and operational offshore wind <br> files from <a from href="https://portal.midatlanticocean.org/data-catalog/energy/#collapse-layer-5537"> MARCO </a> </small> </small>', 
            colors = "green", group = "Wind Lease Area") %>% 
  addLegend("bottomright", pal = cat, values = sed$class,
            title = "Sediment Type",opacity = .8, group = "Sediment Type") %>% 
  addLegend("bottomright", pal = cat2, values = c(1:24),
            title = "TEMP(c)",opacity = .8, group = "FEB-mean-Temp-2011-21(MODIS)") %>% 
  addLegend("bottomright", pal = cat3, values = c(0:10),
            title = "Chlorophil	mg/m^3",opacity = .8, group = "FEB-mean-CHL-2011-21(MODIS)") %>% 
  
  
  addLayersControl(overlayGroups = c("Scallop Density", "Wind Lease Area","Sediment Type", "FEB-mean-Temp-2011-21(MODIS)", "FEB-mean-CHL-2011-21(MODIS)"),
                   options = layersControlOptions(collapsed = FALSE))%>% 
  hideGroup(c("Scallop Density", "Wind Lease Area","Sediment Type", "FEB-mean-Temp-2011-21(MODIS)", "FEB-mean-CHL-2011-21(MODIS)"))
  



m
  





