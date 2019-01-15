library(leaflet)

# Choices for drop-downs
vars <- c(
  "Young People in %" = "Young Population",
  "Population Size" = "Population",
  "Total Average Income" = "Average Income",
  "Unemployment Rate" = "Unemployment Rate",
  "Uneducated Population in %" = "Uneducated Population",
  "Turnout" = "Turnout"
)


navbarPage("Portugal SocioEco", id="nav",
  
  # Interactive Map Panel ######################################
  tabPanel("Interactive map",
    div(class="outer",

      tags$head(
        # Include our custom CSS
        includeCSS("styles.css"),
        includeScript("gomap.js")
      ),

      # If not using custom CSS, set height of leafletOutput to a number instead of percent
      leafletOutput("map", width="100%", height="100%"),

      # Shiny versions prior to 0.11 should use class = "modal" instead.
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
        width = 330, height = "auto",

        h2("Parliament Election 2015"),

        selectInput("x_value", "X-axis", vars),
        selectInput("y_value", "Y-axis", vars, selected = "Turnout"),

        plotOutput("scatterSocioEco", height = 200),
        plotOutput("diffPlot", height = 250)
        
      ),

      tags$div(id="cite",
        'Data compiled for IP Project, Curado et al.(2019)'
      )
    )
  ),
  
  # DataTable Panel #####################################
  tabPanel("Socio-Economic DataExplorer",
    fluidRow(
      column(6,
        selectInput("municipality", "Municipality", sociotable$Municipality, multiple=TRUE)
      ),
      column(3,
        numericInput("minScore", "Min Population", min=0, max=510000, value=0, step = 10000)
      ),
      column(3,
        numericInput("maxScore", "Max Population", min=0, max=510000, value=510000, step = 10000)
      )
    ),
    hr(),
    DT::dataTableOutput("municipTable")
  ),
  
  # Election results ######################################
  
  tabPanel("Election Results", 
           fluidRow(
             column(width = 6, plotOutput(outputId = "pieplot_results")),
             column(width = 4, DT::dataTableOutput("results_table"))
           )
  ),
  conditionalPanel("false", icon("crosshair"))
)
