library('shiny')
library('leaflet')
library('rjson')
library('geojsonio')
library('shiny')
library('ggplot2')
library('plyr')
library('dplyr')


json_cali_file <- geojsonio::geojson_read("California_County_Boundaries.geojson", what = "sp") 
#cali <- fromJSON(file=json_file)

json_data_frame <- as.data.frame(json_cali_file)

Counties <- unique(json_data_frame[,2])

CSV_data <- read.csv("AppBridge.csv")

CSV_Counties <- CSV_data$County

Unique_County_Bridge <- unique(CSV_Counties)



m <- leaflet(json_cali_file) %>%
  setView(-119, 35, 5) %>%
  addTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))

m %>% addPolygons()


ui <- fluidPage(
  
  # App title ----
  titlePanel("Applied Water Map"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      #selectInput(inputId="CountyChosen", label=" Choose County", choices=c(Unique_County), selected = NULL, multiple = FALSE),
      selectInput(inputId="Crop_Chosen", label=" Choose Crop", choices=c(Unique_Crops), selected = NULL, multiple = FALSE),
      selectInput(inputId="Year_Chosen", label=" Choose Year", choices=c(Unique_Years), selected = NULL, multiple = FALSE),
      
      #sliderInput(inputId = "YearsChosen",
                  # = "Year Range",
                  #min = 1998,
                  #max = 2018,
                  #value = c(2006,2010)),
      
      #radioButtons("radio", label = h3("Desired Stat"),
                   #choices = list("Yield Proxy" = "Yp", 
                                  #"Acres Proxy" = "Ap", 
                                  #"Price Proxy" = "Pp"), 
                   #selected = "Yp")
      
    ),
    
    
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Map", leafletOutput("Cali_Map", height = 600),
                                                p(),))
      
      
      
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output, session){
  
  # Reactive expression to generate the requested distribution ----
  # This is called whenever the inputs change. The output functions
  # defined below then use the value computed from this expression
  
  #d <- reactive({
    #radio <- switch(input$radio,
                    #Yp = Coloumns[3],
                    #Ap = Coloumns[4],
                    #Pp = Coloumns[5],
                    #Coloumns[3])
    
    
  #})
  
  
  
  
  # Generate a plot of the data ----
  # Also uses the inputs to build the plot label. Note that the
  # dependencies on the inputs and the data reactive expression are
  # both tracked, and all expressions are called in the sequence
  # implied by the dependency graph.
  output$Cali_Map<- renderLeaflet({
    leaflet(json_cali_file) %>%
      setView(-119, 35, 5) %>%
      addTiles("MapBox", options = providerTileOptions(
        id = "mapbox.light",
        accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))
    
    m %>% addPolygons()
  
  
  })  
}




shinyApp(ui = ui, server = server)


#counties<- geojsonio::geojson_read("https://data.edd.ca.gov/api/views/bpwh-bcb3/rows.json?accessType=DOWNLOAD", what = "sp")
