library(ggplot2)
library(sf)
library(stars)

ROI = function( stri = "[117.83526716151925, 15.49233818000526],
          [117.83526716151925, 6.486174330165204],
          [129.74444684901925, 6.486174330165204],
          [129.74444684901925, 15.49233818000526]"){
  
  for (i in 1:40) {
    stri =   stringr::str_replace(stri, "\\[","c(")
    stri =     stringr::str_replace(stri,"\\]","\\)")
    stri =     stringr::str_replace(stri, "\n"," ")
  }
  
  stri =  paste0("list(",stri,")")
  
  stri = eval(parse(text = stri))
  
  ROI = rgee::ee$Geometry$Polygon(stri)
  
  return(ROI)
}





nysb = ROI("[-74.56247455148491, 41.58555088395143],
          [-74.56247455148491, 39.58335186033913],
          [-67.64108783273491, 39.58335186033913],
          [-67.64108783273491, 41.58555088395143]")


list = lubridate::ymd("20200401")
i_date = as.character(list)
f_date = as.character((list+30))

visP = list(min= -2.0,max= 24.0,
  palette= c('#005aff', '#43c8c8','#77DD77', '#fff700', '#ff0000', "#B42222")
)

data = ee$ImageCollection('NASA/OCEANDATA/MODIS-Terra/L3SMI')$
  filter(ee$Filter$calendarRange(2011,2021,'year'))$filter(ee$Filter$calendarRange(2,2,'month'))$
  select("sst")$mean()

ted = Map$addLayer(data, visP, 'false color composite')

ee_as_rast(image = data, region = nysb, dsn = "testguy")
data


visP = list(min= 0,max= 10,
            palette= c('#3500a8', '#0800ba','#003fd6', '#00aca9', '#77f800', "#ff8800",
                       '#b30000', '#920000', "#880000")
)

data = ee$ImageCollection('NASA/OCEANDATA/MODIS-Terra/L3SMI')$
  filter(ee$Filter$calendarRange(2011,2021,'year'))$filter(ee$Filter$calendarRange(2,2,'month'))$
  select("chlor_a")$mean()

tec = Map$addLayer(data, visP, 'false color composite')


