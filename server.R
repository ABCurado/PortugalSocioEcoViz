library(leaflet)
library(leaflet.extras)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)

# Leaflet bindings are a bit slow; for now we'll just sample to compensate
set.seed(100)
colorPalette <- c("#ff000d", "#840000","#fdaa48","#cb416b","grey")

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

  # Interactive Map Panel #####################################
  # Create the map
  output$map <- renderLeaflet({
    pal <- colorFactor(colorPalette, c("BE","PCP-PEV","PPD/PSD.CDS-PP", "PS", "Others"),  ordered = TRUE)
    
    
    geojson$features <- lapply(geojson$features,
                               function(row) {
                                 row$properties$popup <- showPopup(row$properties$name_2)
                                 row$properties$style <- colorPicker(row$properties$name_2)
                                 return(row)
                               })
    
    leaflet() %>%
      addProviderTiles(providers$Hydda.Base) %>%
      setView(lng = -13.1393366, lat = 38.7222524, zoom = 5) %>%
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
      ("Euro"), tags$b(),
      tags$br(),
      sprintf("Uneducated Population: %s %s", round((selectedMunicipality$Fraction_Without_Education)*100, digits = 2), ("%")),
      tags$b(),
      tags$br(),
      sprintf("Turnout: %s %s", round((selectedMunicipality$Total/selectedMunicipality$Total_Number_People_x)*100, digits = 2), ("%")),
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
  
  output$scatterSocioEco <- renderPlot({
    plot(sociotable[ , c(input$x_value,input$y_value)], pch = 20, cex = 1, col=c("black"), ann=FALSE, par(mar=c(2,2,0,0)))
  })


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
    municip <- event$properties$name_2
    if (is.null(event))
      return()
    isolate({
      output$diffPlot <- renderPlot({
        
        barplot(height = as.matrix(df_diff[df_diff$Municipality==municip,c("BE","PCP", "PSD","PS", "Others")]),
                      main = "Difference from 2011",
                      horiz = FALSE,
                      beside = TRUE,
                      ylab = "in Percentage Points",
                      col = colorPalette
        )
        output$scatterSocioEco <- renderPlot({
          plot(sociotable[ , c(input$x_value,input$y_value)], pch = 20, cex = 1, col=c("dark grey"), ann=FALSE, par(mar=c(2,2,0,0))) %>%
            points(x=sociotable[sociotable$Municipality==municip,input$x_value],
                   y=sociotable[sociotable$Municipality==municip,input$y_value], 
                   pch = 20, cex =  1, lwd = 4, col=c("red"))
        })
      })
      })
  })
  
  ## Socio-Economic Data ###########################################

  observe({
    Municipality <- if (is.null(input$Municipality)) character(0) else {
      filter(sociotable, Municipality %in% input$Municipality) %>%
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
      municip <- input$goto$lat
      showPopupAtXandY(municip)
      output$diffPlot <- renderPlot({
       barplot(height = as.matrix(df_diff[df_diff$Municipality==municip,c("BE","PCP", "PSD","PS", "Others")]),
                      main = df_diff[df_diff$Municipality==municip,"Municipality"],
                      horiz = FALSE,
                      beside = TRUE,
                      ylab = "in Percentage Points",
                      col = colorPalette
        )
      })
      output$scatterSocioEco <- renderPlot({
        plot(sociotable[ , c(input$x_value,input$y_value)], pch = 20, cex = 1, col=c("dark grey"), ann=FALSE, par(mar=c(2,2,0,0))) %>%
          points(x=sociotable[sociotable$Municipality==municip,input$x_value],
                 y=sociotable[sociotable$Municipality==municip,input$y_value], 
                 pch = 20, cex = 1, lwd = 4, col=c("red"))
      })
    })
  })
  
  output$municipTable <- DT::renderDataTable({
    df <- sociotable %>%
      filter(
        Population >= input$population[1], Population <= input$population[2],
        `Average Income` >= input$avg_income[1], `Average Income` <= input$avg_income[2],
        Turnout >= input$turnout[1], Turnout <= input$turnout[2],
        `Unemployment Rate` >= input$unemployment[1], `Unemployment Rate` <= input$unemployment[2],
        `Uneducated Population` >= input$education[1], `Uneducated Population` <= input$education[2],
        `Young Population` >= input$age[1], `Young Population` <= input$age[2],
        is.null(input$municipality) | Municipality %in% input$municipality
      ) %>%
      mutate( Location = paste('<a class="go-map" href="" data-lat="', Municipality ,'"><i class="fa fa-2x fa-crosshairs"></i></a>', sep=""))
    action <- DT::dataTableAjax(session, df)

    DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
  })

## Election Results ##########################################  
    
  output$pieplot_results <- renderPlot({
    pie(pieplot_values, 
        labels = parties, 
        main="Voting Percentages", 
        col=colorPalette,
        radius = 1)
    
  })
  
  output$results_table <- DT::renderDataTable({
    DT::datatable(results_table, options = list(dom = 't'), rownames = FALSE)
  })
}
