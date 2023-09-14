# Call the color function (colorNumeric) to create a new palette function
library(leaflet)


countries <- sf::st_read("https://rstudio.github.io/leaflet/json/countries.geojson")


map <- leaflet(countries)

# Create a continuous palette function
pal <- colorNumeric(
  palette = "Blues",
  domain = countries$gdp_md_est)
# Apply the function to provide RGB colors to addPolygons
map %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1,
              color = ~pal(gdp_md_est))















binpal <- colorBin("Blues", countries$gdp_md_est, 6, pretty = FALSE)

map %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1,
              color = ~binpal(gdp_md_est))
qpal <- colorQuantile("Blues", countries$gdp_md_est, n = 7)
map %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1,
              color = ~qpal(gdp_md_est))
# Make up some random levels. (TODO: Better example)
countries$category <- factor(sample.int(5L, nrow(countries), TRUE))

factpal <- colorFactor(topo.colors(5), countries$category)

leaflet(countries) %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1,
              color = ~factpal(category))