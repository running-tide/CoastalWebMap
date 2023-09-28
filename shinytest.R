library(leaflet)
library(shiny)

createTimeBand <- function(img) {
  year <- ee$Date(img$get("system:time_start"))$get("year")$subtract(1991L)
  ee$Image(year)$byte()$addBands(img)
}

collection <- ee$
  ImageCollection("NOAA/DMSP-OLS/NIGHTTIME_LIGHTS")$
  select("stable_lights")$
  map(createTimeBand)

col_reduce <- collection$reduce(ee$Reducer$linearFit())
col_reduce <- col_reduce$addBands(
  col_reduce$select("scale")
)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      numericInput(inputId = "in_latitude", label = "Latitude", value = 9.08203, min = -90, max = 90, step = 0.1),
      numericInput(inputId = "in_longitude", label = "Longitude", value = 47.39835, min = -180, max = 180, step = 0.1),
      actionButton(inputId = "reposition", label = "Reposition Map")
    ),
    mainPanel(
      leafletOutput(outputId = "map")
    )
  )
)


server <- function(input, output) {
  map <- eventReactive(input$reposition,
                       {
                         Map$setCenter(input$in_latitude, input$in_longitude, 3)
                         Map$addLayer(
                           eeObject = col_reduce,
                           visParams = list(
                             bands = c("scale", "offset", "scale"),
                             min = 0,
                             max = c(0.18, 20, -0.18)
                           ),
                           name = "stable lights trend"
                         )
                       },
                       ignoreNULL = FALSE
  )
  
  output$map <- renderLeaflet({
    map()
  })
}

shinyApp(ui = ui, server = server)
