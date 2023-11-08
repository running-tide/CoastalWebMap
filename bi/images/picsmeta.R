library(exiftoolr)
library(tidyverse)   # this is purely so that I can easily show the results as a tibble
library(imager)
store = exif_read('GOPR0997.JPG')


store$DateTimeOriginal


files <- list.files( recursive=TRUE, pattern="*.jpg", full.names=TRUE)
exifinfo <- read_exif(files)


resize()