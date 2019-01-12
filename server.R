library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)

# Leaflet bindings are a bit slow; for now we'll just sample to compensate
set.seed(100)

function(input, output, session) {

  ## Interactive Map ###########################################

  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = -13.1393366, lat = 38.7222524, zoom = 5)
  })
  
  output$scatterCollegeIncome <- renderPlot({
    # If no zipcodes are in view, don't plot
    print(xyplot(Total_Average_income ~ Fraction_Superior, data = df_2015))
  })

  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  observe({
    colorBy <- input$color
    sizeBy <- input$size

 
    pal <- colorFactor(c("#fdaa48","#cb416b","#840000"), c("PPD/PSD.CDS-PP","PS","PCP-PEV"),  ordered = TRUE)
    radius <- 5000

    leafletProxy("map", data = df_2015) %>%
      clearShapes() %>%
      addCircles(~y, ~x, radius=radius, layerId=~Municipality,
                 stroke=FALSE, fillOpacity=0.7, fillColor=pal(df_2015$Winning_Party)) %>%
      addLegend("bottomleft", pal=pal, values=df_2015$Winning_Party, title="Winning Party",
                layerId="colorLegend")
  })

  # Show a popup at the given location
  showPopup <- function(municipality, lat, lng) {
    selectedMunicipality <- df_2015[df_2015$Municipality == municipality,]
    content <- as.character(tagList(
      tags$h4("Pop:", as.integer(selectedMunicipality$Total)),
      tags$strong(HTML(sprintf("%s %s",
        selectedMunicipality$x, selectedMunicipality$y
      ))), tags$br(),
      sprintf("Median household income: %s", dollar(selectedMunicipality$Total_Average_income)), 
      tags$br()
    ))
    leafletProxy("map") %>% addPopups(selectedMunicipality$y, selectedMunicipality$x, content, layerId = selectedMunicipality$Municipality)
  }

  # When map is clicked, show a popup with city info
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()
    isolate({
      showPopup(event$id, event$lat, event$lng)
    })
  })


  ## Data Explorer ###########################################

  observe({
    Municipality <- if (is.null(input$Municipality)) character(0) else {
      filter(cleantable, Municipality %in% input$Municipality) %>%
        `$`('City') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$Municipality[input$Municipality %in% Municipality])
    updateSelectInput(session, "Municipality", choices = Municipality,
      selected = stillSelected)
  })

  output$municipTable <- DT::renderDataTable({
    df <- cleantable %>%
      filter(
        Total >= input$minScore,
        Total <= input$maxScore,
        is.null(input$municipality) | Municipality %in% input$municipality,
      ) %>%
      mutate(Action = paste('<a class="go-map" href="" data-lat="', x, '" data-long="', y, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
    action <- DT::dataTableAjax(session, df)

    DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
  })
}
