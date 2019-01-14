library(leaflet)

# Choices for drop-downs
vars <- c(
  "Young people%" = "Fraction_0.14",
  "Population Size" = "Total",
  "Total Average Income" = "Total_Average_income",
  "Unemployment Rate" = "Unemployment_.Rate",
  "Uneducated Percentage" = "Fraction_Without_Education"
)


navbarPage("Portugal SocioEco", id="nav",

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

        h2("Explorer"),

        selectInput("x_value", "X-axis", vars),
        selectInput("y_value", "Y-axis", vars, selected = "Total_Average_income"),

        textOutput("textMun"),
        plotOutput("scatterSocioEco", height = 250)
      ),

      tags$div(id="cite",
        'Data compiled for IP Project, Curado et al.(2018)'
      )
    )
  ),

  tabPanel("Socio-Economic Data",
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

  conditionalPanel("false", icon("crosshair"))
)
