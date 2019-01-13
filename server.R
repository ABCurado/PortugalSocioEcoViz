library(leaflet)
library(leaflet.extras)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)

# Leaflet bindings are a bit slow; for now we'll just sample to compensate
set.seed(100)

colorPicker <- function(municipality){
  party = df_2015[which(df_2015$Municipality == municipality),]["Winning_Party"]
  print(municipality)
  print(party)
  if(dim(party)[1] == 0){
    return(list(fillColor="grey", color="grey"))
  }
  if (party == "PS"){
    return(list(fillColor="#cb416b", color="#cb416b"))
  }else if (party == "PPD/PSD.CDS-PP"){
    return(list(fillColor="#fdaa48", color="#fdaa48"))
  }else if (party == "PCP-PEV"){
    return(list(fillColor="#840000", color="#840000"))
  }else{
    return(list(fillColor="grey", color="grey"))
  }
}

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
    pal <- colorFactor(c("#fdaa48","#cb416b","#840000"), c("PPD/PSD.CDS-PP","PS","PCP-PEV"),  ordered = TRUE)


    geojson$features <- lapply(geojson$features,
                                  function(row) {
                                    row$properties$popup <- showPopup(row$properties$name_2)
                                    row$properties$style <- colorPicker(row$properties$name_2)
                                    return(row)
                      })

    leafletProxy("map", data = df_2015) %>%
      clearShapes() %>%
      addGeoJSONv2(geojson, 
                   layerId=~Municipality,
                   fillOpacity=1,
                   opacity =1,
                   labelProperty='name_2',
                   popupProperty='popup',
                   highlightOptions = highlightOptions(
                     weight=2, 
                     fillOpacity=0.6, opacity =1,
                     bringToFront=TRUE, sendToBack=TRUE)) %>%
      addLegend("bottomleft", pal=pal, values=df_2015$Winning_Party, title="Winning Party",
                layerId="colorLegend")
  })

  # Show a popup at the given location
  showPopup <- function(municipality) {
    selectedMunicipality <- df_2015[df_2015$Municipality == municipality,]
    content <- as.character(tagList(
      tags$h3((selectedMunicipality$Municipality)),
      tags$b(),
      sprintf("Unemployment Rate: %s %s", (selectedMunicipality$Unemployment_.Rate), ("%")),
      tags$b(), 
      tags$br(),
      sprintf("Average Income: %s ", (selectedMunicipality$Total_Average_income)),
      ("Euro"), tags$br(),
      tags$b(),
      sprintf("Uneducated Population: %s %s", round((selectedMunicipality$Fraction_Without_Education)*100, digits = 2), ("%")),
      tags$b(),
      tags$h4("Election Result:"),
      tags$b(),
      sprintf("PPD/PSD.CDS-PP: %s %s",round((selectedMunicipality$PPD.PSD.CDS.PP/selectedMunicipality$Total)*100, digits = 1) ,("%")), tags$b(),
      tags$br(),
      sprintf("PS: %s %s",round((selectedMunicipality$PS/selectedMunicipality$Total)*100, digits = 1) ,("%")), tags$b(),
      tags$br(),
      sprintf("PCP-PEV: %s %s",round((selectedMunicipality$PCP.PEV/selectedMunicipality$Total)*100, digits = 1) ,("%")), tags$b(),
      tags$br(),
      sprintf("BE: %s %s",round((selectedMunicipality$BE/selectedMunicipality$Total)*100, digits = 1) ,("%")), tags$b(),
      tags$br(),
      sprintf("Other: %s %s",round(((selectedMunicipality$Other+selectedMunicipality$NC+selectedMunicipality$PAN)/selectedMunicipality$Total)*100, digits = 1) ,("%")), tags$b()
      
    ))
    return(content)
#    leafletProxy("map") %>% addPopups(selectedMunicipality$x, selectedMunicipality$y, content, layerId = municipality)
}

  # When map is clicked, show a popup with city info
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    print(event)
    if (is.null(event))
      return()
    isolate({
      showPopup(event$id, event$lat, event$lng)
    })
  })


  ## Data Explorer ###########################################

  observe({
    Municipality <- if (is.null(input$Municipality)) character(0) else {
      filter(sociotable, Municipality %in% input$Municipality) %>%
        `$`('City') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$Municipality[input$Municipality %in% Municipality])
    updateSelectInput(session, "Municipality", choices = Municipality,
      selected = stillSelected)
  })
  
  observe({
    if (is.null(input$goto))
      return()
    isolate({
      map <- leafletProxy("map")
      map %>% clearPopups()
      dist <- 0.5
      munic <- input$goto$munic
      lat <- input$goto$lat
      lng <- input$goto$lng
      showPopup(munic)
      map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
    })
  })
  
  
  output$municipTable <- DT::renderDataTable({
    df <- sociotable %>%
      filter(
        Population >= input$minScore,
        Population <= input$maxScore,
        is.null(input$municipality) | Municipality %in% input$municipality,
      ) %>%
      mutate(Action = paste('<a class="go-map" href="" data-lat="',  '" data-long="',  '"><i class="fa fa-crosshairs"></i></a>', sep=""))
    action <- DT::dataTableAjax(session, df)

    DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
  })
}
