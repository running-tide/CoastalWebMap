library(ncdf4)
library(lubridate)

USERNAME = readline("Enter your username: ")# input your username
PASSWORD = readline("Enter your password: ")# input your password

server = "my.cmems-du.eu";                    # Copernicus Marine server
datasetID = "cmems_obs_glo_bgc3d_rep_weekly";  # datasetID

# creates the OPeNDAP url
url <- paste ("https://", USERNAME, ":", PASSWORD,"@",server,"/thredds/dodsC/",datasetID, sep = "")

# Open the connection
ds <- nc_open(url)

print(ds)



## Get dataset's dimensions 
# Longitude
lon <- ncvar_get(ds, "longitude")
nlon <- dim(lon)
# Latitude
lat <- ncvar_get(ds, "latitude")
nlat <- dim(lat)

# Check dimensions
print(c(nlon,nlat))

# Time
time<-ncvar_get(ds,"time")
nt <- dim(time)
t_units <- ncatt_get(ds, "time", "units")
t_units
# convert time -- split the time units string into fields
t_ustr <- strsplit(t_units$value, " ")
t_dstr <- strsplit(unlist(t_ustr)[3], "-")
date <- ymd(t_dstr) + dhours(time)      
date

# Depth
depth<-ncvar_get(ds,"depth")
dim(depth)

## Define the parameters and ranges for subset
#Bounding box
x <- c(65.55, 91.77)                # longitude
y <- c(-7.77, 22.19)                # latitude
t <- c("2013-07-03", "2013-07-31")  # time
z <- c(0, 35)                       # depth

# Function to get the indices from the ranges
btw <- function(data, num){
  c(min(which(num<=data)), max(which(num>=data)))
}

# Starting indices
lon_indices <- btw(data = lon, num = x)
lat_indices <- btw(data = lat, num = y)
time_indices <- btw(data = date, num = t)
depth_indices <- btw(data = depth, num = z)


# Count number of indices to extract along each dimension
lon_range <- lon_indices[-1] - lon_indices[1]+1 
lat_range <- lat_indices[-1] - lat_indices[1]+1
time_range <- time_indices[-1]+1 - time_indices[1]+1 # #Add +1 to time_indices[-1] 
depth_range <- depth_indices[-1] - depth_indices[1]+1

# Start and Count vectors
offset <- c(lon_indices[1], lat_indices[1], depth_indices[1], time_indices[1])    #lon,lat,depth,time
count <- c(lon_range, lat_range, depth_range, time_range) #dimension CHL

# Get subsetted variable   
chl <- ncvar_get(ds,"chl", start = offset, count = count)

dim(chl)


# List the values of the dimensions of the dataset ds
lat_list <- list(ds[["dim"]][["latitude"]][["vals"]])
lon_list <- list(ds[["dim"]][["longitude"]][["vals"]])
time_list <- list(ds[["dim"]][["time"]][["vals"]])
depth_list <- list(ds[["dim"]][["depth"]][["vals"]])

# Function to select a range from a list
select_range <- function(lst, start, end) {
  sub_lst <- lst[[1]]  # Get the list
  
  if (start < 1 || end > length(sub_lst)) {
    stop("Invalid range")
  }
  return(sub_lst[start:end])
}

# Extract the dimension range using the indices
range_lat <- select_range(lat_list, 299, 418) #lat_indices
range_lon <- select_range(lon_list, 983, 1086) #lon_indices
range_time <- select_range(time_list, 809, 813) #time_indices +1
range_depth <- select_range(depth_list, 1, 8) #depth_indices

# Define the dimensions
dim_lon <- ncdim_def("longitude", "degrees_east", range_lon)
dim_lat <- ncdim_def("latitude", "degrees_north", range_lat)
dim_time <- ncdim_def("time", "hours since 1950-01-01 00:00:00", range_time)
dim_depth <- ncdim_def("depth", "m", range_depth)

# Define the dimensions of the variable Chl
dimCHL <- ncdim_def( "Chl", "mg m-3", count)


# Define the chl variable by integrating the dimensions
var_chl <- ncvar_def("Chl", "mg m-3", list(dim_lon, dim_lat, dim_time, dim_depth), 99999, longname = "Total chlorophylle-a concentration",prec = "float")

# Create a new NetCDF file object
ncnew <- nc_create("Chlorophylle.nc", var_chl)

# Write the chlorophyll data
ncvar_put(ncnew, var_chl, chl, start = NA, count = NA)

# Close the netCDF to save it
nc_close(ncnew)
