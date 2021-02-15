fluidPage(
  titlePanel("Meteorological data"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput("site", 
                  "UK Sites - choose up to 5", 
                  choices=sites$Site_Name, 
                  multiple=TRUE, 
                  select = sites$Site_Name[1],
                  options = list(maxItems = 5) ),
      selectInput("weather_var", "Weather Variable", choices=weather_vars[,1]),
      selectInput("agg", "Aggregation", choices=aggregation[,1] ), 
      selectInput("time", "Time - x-axis", choices=date_choice[,1] ),
      selectInput("hc", "Month for Hutton Criteria", choices=hc[,1] )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Weather Statistics",
                 
                 h4("Time plot for selected variables"),
                 plotOutput("timeplot"),
                 
                 h4("Location of selected weather stations"),
                 plotOutput("mapplot"),
                 
                 h4("Daily mean summary of all weather variables for latest week"),
                 dataTableOutput("summarytable"),
                 
                 downloadButton("download1", "Download daily means", icon="download"),
                 
                 downloadButton("downloadReportButton", "Download report", icon="download")
                 
                 ),
        tabPanel("Hutton Criteria",
                 plotOutput("hc_plot"),
                 h4("Daily summary of Hutton Criteria for selected month "),
                 dataTableOutput("hctable"),
                )
        )
    )
  )
)
