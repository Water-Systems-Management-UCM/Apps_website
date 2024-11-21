library(shiny)
library(readr)
library(DT)
library(leaflet)
library(dplyr)
library(sf)
library(tigris)

# Specify the name of your CSV file here
csv_filename <- "County_Ag_Commissioner_Report_2022_data_by_commodity.csv"
# Read the CSV file once at startup
initial_data <- read_csv(csv_filename)
# Load California counties
options(tigris_use_cache = TRUE)
ca_counties <- counties(state = "CA", cb = TRUE)

ui <- fluidPage(
  titlePanel("Agricultural Data Map"),
  sidebarLayout(
    sidebarPanel(
      selectInput("crop_category", "Crop Category",
                  choices = unique(initial_data[[2]]),
                  selected = unique(initial_data[[2]])[1]),
      selectInput("year", "Year",
                  choices = unique(initial_data[[1]]),
                  selected = unique(initial_data[[1]])[1]),
      selectInput("variable", "Variable (Placeholder)",
                  choices = c("Placeholder 1", "Placeholder 2", "Placeholder 3"),
                  selected = "Placeholder 1"),
      hr(),
      downloadButton("downloadData", "Download Data"),
      downloadButton("downloadPlot", "Download Plot")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Map", leafletOutput("map")),
        tabPanel("Data", DTOutput("contents")),
        tabPanel("Summary", verbatimTextOutput("summary"))
      )
    )
  )
)

server <- function(input, output, session) {
  filtered_data <- reactive({
    initial_data %>%
      filter(!!sym(names(initial_data)[2]) == input$crop_category,
             !!sym(names(initial_data)[1]) == input$year)
  })
  
  summarized_data <- reactive({
    filtered_data() %>%
      group_by(!!sym(names(initial_data)[6])) %>%
      summarise(TotalHarvestedAcres = sum(as.numeric(!!sym(names(initial_data)[8])), na.rm = TRUE))
  })
  
  output$map <- renderLeaflet({
    req(summarized_data())
    
    # Join the data with the county polygons
    ca_counties_data <- left_join(ca_counties, summarized_data(), by = c("NAME" = names(initial_data)[6]))
    
    # Remove NA values from the domain for the color palette
    valid_acres <- ca_counties_data$TotalHarvestedAcres[!is.na(ca_counties_data$TotalHarvestedAcres)]
    
    # Create color palette with blue gradient
    pal <- colorNumeric(
      palette = "Blues",
      domain = valid_acres
    )
    
    leaflet(ca_counties_data) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(TotalHarvestedAcres),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label = ~paste0(NAME, ": ", formatC(TotalHarvestedAcres, big.mark = ","), " acres")
      ) %>%
      addLegend(pal = pal, values = valid_acres, opacity = 0.7, title = "Harvested Acres",
                position = "bottomright") %>%
      setView(lng = -119.4179, lat = 36.7783, zoom = 6)  # Centered on California
  })
  
  output$contents <- renderDT({
    datatable(filtered_data(), options = list(pageLength = 10))
  })
  
  output$summary <- renderPrint({
    summary(filtered_data())
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("agricultural-data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
  
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("agricultural-map-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      mapview::mapshot(leafletProxy("map"), file = file)
    }
  )
}

shinyApp(ui = ui, server = server)