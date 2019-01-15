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


navbarPage("Portugal Election 2015", id="nav",
  
  # Interactive Map Panel ######################################
  tabPanel("Interactive Map",
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
  tabPanel("Socio-Economic Data",
fluidRow(
      column(3,
             sliderInput("turnout", "Turnout:",
                         min = min(sociotable$Turnout), max = max(sociotable$Turnout),
                         value = c(min(sociotable$Turnout),max(sociotable$Turnout)), 
                         step = 1)
      ),column(3,
             sliderInput("avg_income", "Average Income:",
                         min = min(sociotable$`Average Income`), max = max(sociotable$`Average Income`),
                         value = c(min(sociotable$`Average Income`),max(sociotable$`Average Income`)), 
                         step = 10)
      ),column(3,
             sliderInput("unemployment", "Unemployment Rate:",
                         min = min(sociotable$`Unemployment Rate`), max = max(sociotable$`Unemployment Rate`),
                         value = c(min(sociotable$`Unemployment Rate`),max(sociotable$`Unemployment Rate`)), 
                         step = 1.0)
      ),column(3,
             sliderInput("education", "Uneducated Population:",
                         min = min(sociotable$`Uneducated Population`), max = max(sociotable$`Uneducated Population`),
                         value = c(min(sociotable$`Uneducated Population`),max(sociotable$`Uneducated Population`)), 
                         step = 1.0)
      )
    ),    fluidRow(
      column(6,
             selectInput("municipality", "Municipality", sociotable$Municipality, multiple=TRUE)
      ),
      column(6,
             sliderInput("population", "Population:",
                         min = min(sociotable$Population), max = max(sociotable$Population),
                         value = c(min(sociotable$Population),max(sociotable$Population)), 
                         step = 1000)
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
