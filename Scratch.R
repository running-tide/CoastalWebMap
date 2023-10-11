library(raster)

r <- raster("MODIS _SST _2010-2021_1.tif")
cat2 = colorNumeric(palette = c('#005aff', '#43c8c8','#77DD77', '#fff700', '#ff0000', "#B42222"),
                    domain = c(-2:24), na.color = "transparent")


leaflet() %>% addTiles() %>%
  addRasterImage(r, colors = cat2, opacity = 0.8) %>%
  addLegend(pal = cat2, values = c(-2:24),
            title = "Surface temp")



















images <- ee$ImageCollection("COPERNICUS/S2_SR")
sf <- ee$Geometry$Point(c(-122.463, 37.768))

# Expensive function to reduce the neighborhood of an image.
reduceFunction <- function(image) {
  image$reduceNeighborhood(
    reducer = ee$Reducer$mean(),
    kernel = ee$Kernel$square(4)
  )
}

bands <- list("B4", "B3", "B2")
# Select and filter first!
reasonableComputation <- images$select(bands)$
  filterBounds(sf)$
  filterDate("2018-01-01", "2019-02-01")$
  filter(ee$Filter$lt("CLOUDY_PIXEL_PERCENTAGE", 1))$
  aside(ee_print)$ # Useful for debugging.
  map(reduceFunction)$
  reduce('mean')$
  rename(bands)

viz <- list(bands = bands, min = 0, max = 10000)
Map$addLayer(reasonableComputation, viz, "resonableComputation")












sen <- ee$ImageCollection("COPERNICUS/S2")$
  filterBounds(ee$Geometry$Point(-74.56247455148491, 41.58555088395143))$
  filterDate('2019-01-01', '2019-12-31')$
  sort('CLOUDY_PIXEL_PERCENTAGE')$
  first()

# Define the visualization parameters.
vizParams <- list(
  bands = c("B4", "B3", "B2"),
  min = 0,
  max = 2000,
  gamma = c(0.95, 1.1, 1)
)

# Center the map and display the image.
Map$centerObject(sen, 7)
# You can see it
m1 <- Map$addLayer(sen, vizParams, 'first')
m1



















# Load an image.
landsat <- ee$Image('LANDSAT/LC08/C01/T1_TOA/LC08_044034_20140318')

# Define the visualization parameters.
vizParams <- list(
  bands = c('B5', 'B4', 'B3'),
  min = 0,
  max = 0.5,
  gamma = c(0.95, 1.1, 1)
)

# Center the map and display the image.
Map$setCenter(lon = -122.1899, lat = 37.5010, zoom = 10) # San Francisco Bay
m1 <- Map$addLayer(landsat, vizParams, 'false color composite')
m1$rgee


leaflet() %>% 
  addTiles() %>% 
  setView(-122.1899, 37.5010, 9) %>% 
  addTiles(
    urlTemplate = ted$rgee$tokens,
    layerId = "leaflet_false_color",
    options = leaflet::tileOptions(opacity = 1)
  )


















# old rgee code I think to convert points to geometry



#geojson::geo_write(data, "testqso")
#sil = ee_extract(data,nysb,sf = TRUE,scale = 5500, fun = ee$Reducer$mean())




data = ee_as_sf(data)
write_sf(data, "asfd.geojson")


plot(data)

mil = data %>% 
  st_make_grid(cellsize = .05) %>% 
  st_as_sf() %>% 
  st_join(data)


mil = st_make_grid(data, cellsize = 0.1)

plot(mil)

til = geojson::as.geojson(data)
geojson::geo_write(til, "testqso")


ee$batch$Export$image$
  ee.batch.Export.image.toDrive(ndviChange, **params)

params = {
  'description': 'ndviChange_to_gtiff',
  'scale': 30,
  'region': AOI.geometry().getInfo()['coordinates']
}


ee.batch.Export.image.toDrive(ndviChange, **params)

data = geetodf(data)

ggplot(data, aes(lon, lat)) +
  geom_raster(aes( fill = data[,3]))

