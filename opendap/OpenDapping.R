library(raster)
library(ncdf4)
library(lubridate)
#
link = "my.cmems-du.eu/thredds/dodsC/cmems_mod_glo_bgc_my_0.25_P1D-m"


data = nc_open(paste0("https://jinnis:Innis832787@", link))

data = nc_open("http://www.smast.umassd.edu:8080/thredds/dodsC/FVCOM/NECOFS/Forecasts/NECOFS_GOM3_FORECAST.nc")

data$var$temp$dimids

data$dim[[3+1]]$name
data$dim[[4+1]]$name
data$dim[[0+1]]$name

data$dim$node$len
data$dim$siglay$vals
data$var$lon$dimids

lon = ncvar_get(data, "lon", start = c(1), count = c(53087))
lat = ncvar_get(data, "lat", start = c(1), count = c(53087))

df = ncvar_get(data, "temp", start = c(1,1,1), count = c(53087, 1, 1))
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












spatial_df <- data.frame(
  x = runif(100, min = 0, max = 10),
  y = runif(100, min = 0, max = 10),
  value = rnorm(100)
)

# Convert the data frame to a spatial points data frame
coordinates(df) <- c("x", "y")

# Convert to a raster
raster_from_points <- rasterize(df, raster(), field = "value")

# Plot the raster
plot(df)
