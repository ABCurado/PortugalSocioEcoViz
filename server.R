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
      addProviderTiles(providers$Stamen.Toner) %>%
      setView(lng = -13.1393366, lat = 38.7222524, zoom = 5) 
  })

  output$scatterSocioEco <- renderPlot({
    plot(df_2015[ , c(input$x_value,input$y_value)], pch = 20, cex = 1, col=c("green"))
  })


  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  observe({
    pal <- colorFactor(c("#fdaa48","#cb416b","#840000", "#ff000d","grey"), c("PPD/PSD.CDS-PP","PS","PCP-PEV", "BE", "Others"),  ordered = TRUE)


    geojson$features <- lapply(geojson$features,
                                  function(row) {
                                    row$properties$popup <- showPopup(row$properties$name_2)
                                    row$properties$style <- colorPicker(row$properties$name_2)
                                    return(row)
                      })

    leafletProxy("map",data = df_2015) %>%
      clearShapes() %>%
      addGeoJSONv2(geojson, 
                   layerId=df_2015$Municipality,
                   fillOpacity=0.6,
                   opacity =1,
                   labelProperty='name_2',
                   popupProperty='popup',
                   highlightOptions = highlightOptions(
                     weight=2, 
                     fillOpacity=1, opacity =1,
                     bringToFront=TRUE, sendToBack=TRUE)) %>%
      addLegend("bottomleft", pal=pal, values=c("PPD/PSD.CDS-PP","PS","PCP-PEV", "BE", "Others"), title="Party",
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
}
  showPopupAtXandY <- function(municipality) {
    selectedMunicipality <- df_2015[df_2015$Municipality == municipality,]
    dist <- 0.5
    lng <- selectedMunicipality$y
    lat <- selectedMunicipality$x
    leafletProxy("map") %>% 
      addPopups(lng, lat, showPopup(municipality), layerId = selectedMunicipality$Municipality ) %>%
      fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
  }
  
  # When map is clicked, 
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_geojson_click

    if (is.null(event))
      return()
    isolate({
      output$diffPlot <- renderPlot({
        
        barplot(height = as.matrix(df_diff[df_diff$Municipality==event$properties$name_2,c("BE","PCP", "PSD","PS", "Others")]),
                      main = "Change in % votes from 2011",
                      horiz = FALSE,
                      beside = TRUE,
                      ylab = "in Percentage Points",
                      #                  legend.text = c("BE","PCP.PEV", "PPD","PS", "Others"),
                      col = c("#ff000d", "#840000","#fdaa48","#cb416b","grey")
        )
        output$scatterSocioEco <- renderPlot({
          plot(df_2015[ , c(input$x_value,input$y_value)], pch = 20, cex = 1, col=c("green")) %>%
            points(x=df_2015[df_2015$Municipality==event$properties$name_2,input$x_value],
                   y=df_2015[df_2015$Municipality==event$properties$name_2,input$y_value], 
                   pch = 10, cex = 4, lwd = 4, col=c("red"))
        })
      })
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
      munic <- input$goto$lat
      showPopupAtXandY(munic)
      output$diffPlot <- renderPlot({
        
       barplot(height = as.matrix(df_diff[df_diff$Municipality==munic,c("BE","PCP", "PSD","PS", "Others")]),
                      main = df_diff[df_diff$Municipality==munic,"Municipality"],
                      horiz = FALSE,
                      beside = TRUE,
                      ylab = "in Percentage Points",
                      #                  legend.text = c("BE","PCP.PEV", "PPD","PS", "Others"),
                      col = c("#ff000d", "#840000","#fdaa48","#cb416b","grey")
        )
      })
      output$scatterSocioEco <- renderPlot({
        plot(df_2015[ , c(input$x_value,input$y_value)], pch = 20, cex = 1, col=c("green")) %>%
          points(x=df_2015[df_2015$Municipality==munic,input$x_value],
                 y=df_2015[df_2015$Municipality==munic,input$y_value], 
                 pch = 10, cex = 4, lwd = 4, col=c("red"))
      })
    })
  })
  
  output$municipTable <- DT::renderDataTable({
    df <- sociotable %>%
      filter(
        Population >= input$minScore,
        Population <= input$maxScore,
        is.null(input$municipality) | Municipality %in% input$municipality,
      ) %>%
      mutate(Action = paste('<a class="go-map" href="" data-lat="', Municipality ,'"><i class="fa fa-crosshairs"></i></a>', sep=""))
    action <- DT::dataTableAjax(session, df)

    DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
  })
}
