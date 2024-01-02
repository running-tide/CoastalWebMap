library(raster)
library(ncdf4)
library(lubridate)
library(tidyverse)
library(sf)
library(gstat)


datanet = nc_open("http://www.smast.umassd.edu:8080/thredds/dodsC/FVCOM/NECOFS/Forecasts/NECOFS_GOM3_FORECAST.nc")

datanet$var$temp$dimids

datanet$dim[[3+1]]$name
datanet$dim[[4+1]]$name
datanet$dim[[0+1]]$name

datanet$dim$node$len
datanet$dim$siglay$vals
datanet$var$lon$dimids









# ///////////\\\\\\\\\\\\//////////  gathers data 

x = ncvar_get(datanet, "lon", start = c(1), count = c(53087))
y = ncvar_get(datanet, "lat", start = c(1), count = c(53087))
temp = ncvar_get(datanet, "temp", start = c(1,1,1), count = c(53087, 1, 1))



# ///////////\\\\\\\\\\\\//////////  makes data frame

data = data.frame(x, y, temp)
data = data %>% 
  filter(y>40.3 & y<41.8) %>% 
  filter(x>-72.4 & x< -70.4)

ggplot(data, aes(x = x, y = y, color = temp)) +
  geom_point(size = 3) +
  scale_color_continuous()


# ///////////\\\\\\\\\\\\//////////  makes grid
grid <- expand.grid(
  x = seq(from = min(data$x), to = max(data$x), length.out = 300),
  y = seq(from = min(data$y), to = max(data$y), length.out = 300)
)
coordinates(grid) = ~x+y
sam = st_read("worldshape.geojson")
grid = st_as_sf(grid)
st_crs(grid) <- st_crs(sam)

grid = grid[is.na(as.numeric(st_intersects(grid, sam, sparse = T))),]

plot(grid)


# ///////////\\\\\\\\\\\\//////////  interpolates


coordinates(data) = ~x+y
st_crs(grid) <- st_crs(data)
v = variogram(temp~1, data)
m = fit.variogram(v, vgm(model =  "Sph"))

plot(v, model = m)
interdat = krige(formula = temp~1, data, grid, model = m)
interdat = interdat[,-2]

# ///////////\\\\\\\\\\\\//////////  displays results
plot(interdat$var1.pred)

ggplot(interdat) +
  geom_sf(aes(color = var1.pred), size = 4) +
  scale_color_gradient(limits = c(15, 18),low = "blue", high = "red", name = "Value")

ggplot(data) +
  geom_sf(aes(color = temp), size = 4) +
  scale_color_gradient(limits = c(15, 18),low = "blue", high = "red", name = "Value")






nras = raster(extent(st_bbox(interdat)),resolution = .01)
assa = rasterize(interdat, nras)

plot(assa$temp)

data = st_as_sf(data)

