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
  x = seq(from = min(data$x), to = max(data$x), length.out = 30),
  y = seq(from = min(data$y), to = max(data$y), length.out = 30)
)
coordinates(grid) = ~x+y
sam = st_read("worldshape.geojson")

st_as_sf(grid)

dri = st_crs(sam)



st_crs()

st_crs(grid) <- dri

st_transform(grid, st_crs(sam))


grid[st_within(grid, sam), ]

grid$temp = 1



gridded(grid) = ~x+y

1:nrow(grid)

grid$tempdis = NA
for (i in 44) {
  tempdis = rep(NA, nrow(grid))
  for (e in 1:nrow(data)) {
    tempdis[e] =  sqrt((grid$x[i] - data$x[e])^2 + (grid$y[i] - data$y[e])^2)
  }
  
  if(min(tempdis)>0.01 & sum(tempdis>0.01&tempdis<0.05)>6)
  
  grid$tempdis[i] = min(tempdis)
}

hist(tempdis)


tempdis



grid = grid %>% 
  filter(tempdis < 0.01)

# ///////////\\\\\\\\\\\\//////////  interpolates


coordinates(data) = ~x+y
v = variogram(temp~1, data)
m = fit.variogram(v, vgm(model =  "Sph"))

plot(v, model = m)
lzn.kr1 = krige(formula = temp~1, data, grid, model = m)

# ///////////\\\\\\\\\\\\//////////  displays results
plot(lzn.kr1)








plo = as.matrix(lzn.kr1$var1.pred)
image(plo)



predicted <- lzn.kr1@data$var1.pred %>%
  as.data.frame() %>%
  rename(krige_pred = 1) %>% 
  mutate(krige= exp(krige))
variance <- lzn.kr1@data$var1.var %>%
  as.data.frame() %>%
  mutate(variance = exp(variance))
  





plo[10,10] = 16

x





























data = data.frame(x, y, temp)
data = data %>% 
  filter(y>41 & y<41.2) %>% 
  filter(x>-72.5 & x< -72.2)




ggplot(data, aes(x = x, y = y, color = temp)) +
  geom_point(size = 3) +
  scale_color_continuous()  # This scale function sets up the continuous color scale






coordinates(data) = ~x+y
grid <- expand.grid(
  x = seq(from = min(data$x), to = max(data$x), length.out = 30),
  y = seq(from = min(data$y), to = max(data$y), length.out = 30)
)


ggplot(data, aes(x = x, y = y, color = temp)) +
  geom_point(size = 3) +
  scale_color_continuous() +
  geom_point(data = grid, aes(x = x, y = y), size = 1, shape = 17, color = "black")

# This scale function sets up the continuous color scale



grid$temp = NA
gridded(grid) = ~x+y
plot(grid$x, grid$y)

data = st_as_sf(data)
grid = st_as_stars(grid)
v = variogram(temp~1, data)
m = fit.variogram(v, vgm(c("Exp", "Mat", "Sph")))
plot(v, model = m)
lzn.kr1 = krige(formula = temp~1, data, grid, model = m)
plot(lzn.kr1)
plo = as.matrix(lzn.kr1$var1.pred)
































spatial_df = data.frame(df,lon,lat)
coordinates(spatial_df) <- c("lon", "lat")
raster_from_points <- raster(extent(spatial_df), resolution = 0.1) # Adjust resolution if needed
raster_from_points <- rasterize(spatial_df, raster_from_points, field = "df")
plot(raster_from_points)


unique(df)

raster_from_points  = rasterize(spatial_df, raster(), field = "df")
plot(raster_from_points)


df = df[nrow(df):1,ncol(df):1]
df = raster(df)
plot(df)


































library(gstat)
library(sp)
data(meuse)

plot(meuse$x~meuse$y)

coordinates(meuse) = ~x+y

plot(meuse)

data(meuse.grid)

plot(meuse.grid$y~meuse.grid$x)

gridded(meuse.grid) = ~x+y

plot(meuse.grid$dist)


#> Loading required package: abind
meuse = st_as_sf(meuse)
meuse.grid = st_as_stars(meuse.grid)

# ordinary kriging --------------------------------------------------------
v = variogram(log(zinc)~1, meuse)
m = fit.variogram(v, vgm(1, "Sph", 300, 1))
plot(v, model = m)


lzn.kr1 = krige(formula = zinc~1, meuse, meuse.grid, model = m)
#> [using ordinary kriging]
plot(lzn.kr1[1])

