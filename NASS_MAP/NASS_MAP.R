library('shiny')
library('leaflet')
library('shiny')
library('ggplot2')
library('plyr')
library(dplyr)
library(tigris) #download shp file from U.S. census website
library(rgdal)
library(tmap)
library(tidyverse)
library(tmaptools)
library(reshape)
library(rvest)
library(quantmod)
library(DT)
library(blscrapeR)
library(RColorBrewer)



names <- read_csv("names.csv")


#Bring NASS

#All years data
link <- "https://www.nass.usda.gov"
list_links <- read_html("https://www.nass.usda.gov/Statistics_by_State/California/Publications/AgComm/index.php") %>% html_nodes("a") %>% html_attr("href")
list_links <- grep(".csv",list_links,value=TRUE)
colnames <- c('Year','Commodity Code','Crop','County Code','County','Acres','Yield','Production','Price','Unit','Value')
list_links <- list_links[-3]
all_data <- lapply(list_links, function(i) read_csv(paste(link,i,sep=""),col_names = FALSE))%>%bind_rows()%>%set_names(colnames)%>%filter(!grepl("Year|year",Year))
all_data$County[all_data$County=="San Luis Obisp"]="San Luis Obispo"
all_data <- all_data%>%filter(!County%in%c("Sum of Others","State Totals","State Total"))


#Put group names

group_data <- all_data%>%left_join(names,by="Commodity Code")%>%drop_na(`Commodity Code`)%>%dplyr::select(Year,"Commodity Code",County,Acres,Yield,Price,`Crop Name`,Group_name,Value,`County Code`)
group_data$Acres <-  as.numeric(group_data$Acres)
group_data$Price <-  as.numeric(group_data$Price)
group_data$Yield <-  as.numeric(group_data$Yield)
group_data$Value <-  as.numeric(group_data$Value)


#Find proxy

dgroup_data <- group_data%>%group_by(Group_name,Year,`County Code`)%>%mutate(`Total Area` = sum(Acres,na.rm=TRUE)/1000,`Value category`=sum(Value,na.rm=TRUE))%>%ungroup()
dgroup_data2 <-  dgroup_data%>%group_by(`Commodity Code`,`County Code`,Year)%>%mutate(Rate = Acres/`Total Area`)%>%mutate(`Gross revenue`=Price*Yield)%>%ungroup()
dgroup_data2 <-  dgroup_data2%>%group_by(`County Code`,Year)%>%mutate("Total Area1" = sum(Acres,na.rm=TRUE)/1000,"Tot Rev"=sum(Value,na.rm=TRUE)/1000)%>%ungroup()
dgroup_data2 <- dgroup_data2%>%group_by(`County Code`,Group_name,Year)%>%arrange(desc(Rate))%>%slice(1)%>%ungroup() 



#Adjust for inflation

# Price inflation data using 2020 as base year
Inflation <-  inflation_adjust(base_year=2020)
Inflation_1 <- Inflation%>%dplyr::select(year,adj_value)%>%dplyr::rename(Year=year)
#look at the table
#Column adj value is the % of 2019 dollars in each year if we want to bring x value from any year to 2019 then x(2019) = x(any year)*(1+(1-adj_value))
proxy <- dgroup_data2 %>%left_join(Inflation_1,by="Year")
proxy <- proxy%>%mutate(`Adjusted Price`=Price*(1+(1-adj_value)),`Adjusted Total Production Value`=`Value category`*(1+(1-adj_value))/1000,`Adjusted Gross Revenue`=`Gross revenue`*(1+(1-adj_value)),`Tot Rev`=`Tot Rev`*(1+(1-adj_value)))
proxy <- proxy%>%dplyr::select(c("Year","Yield","Group_name","Total Area1","Tot Rev", "Total Area","Adjusted Price","Adjusted Gross Revenue","Adjusted Total Production Value","Crop Name","County"))
proxy <- melt(as.data.frame(proxy),id=c("Year","Group_name","County"))
proxy[proxy$variable%in%c("Total Area1","Tot Rev"),]$Group_name = "Sum All"
proxy[proxy$variable=="Total Area1",]$variable = "Total Area"
proxy[proxy$variable=="Tot Rev",]$variable = "Adjusted Total Production Value"
proxy <- proxy%>%unique(by=c("Year","County","value","variable"))
proxy$Year <- as.numeric(proxy$Year)
proxy$value <- suppressWarnings(as.numeric(proxy$value))



# App


data <- proxy%>%dplyr::rename(NAME=County)
data$variable <- as.character(data$variable)
Unique_Crops <- sort(unique(data$Group_name))
Unique_County <- sort(unique(data$NAME))
data$Year<- as.integer(data$Year)
Unique_Years <- unique(na.omit(as.numeric(data$Year)),fromLast = TRUE)
Unique_Vars <- sort(unique(data$variable))
Unique_Vars <- Unique_Vars[Unique_Vars != "Crop Name"]



n <- 23
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))




ca_geo<-tigris::counties(state = "ca", class = "sf") 


labels <-c("USD per ton"="Adjusted Price","Tons per acre"="Yield","Thousand Acres"="Total Area","Thousand USD"="Adjusted Total Production Value","USD per ton"="Price","USD per acre"="Adjusted Gross Revenue")



ui <- fluidPage(
    
    # App title ----
    titlePanel("NASS Data Map"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        # Sidebar panel for inputs ----
        sidebarPanel(
            
            selectInput(inputId="Crop_Chosen", label="Crop Category", choices=c(Unique_Crops), selected = NULL, multiple = FALSE),
            selectInput(inputId="Year_Chosen", label="Year", choices=Unique_Years, selected = NULL, multiple = FALSE),
            selectInput(inputId="Var_Chosen", label="Variable", choices=c(Unique_Vars), selected = NULL, multiple = FALSE),
            
            # Button
            downloadButton("downloadtable","Download"),
            
            downloadButton("downloadData2","Download All Data")
        ),
        
        
        
        
        
        # Main panel for displaying outputs ----
        mainPanel(
            
            # Output: Tabset w/ plot, summary, and table ----
            tabsetPanel(type = "tabs",
                        tabPanel("Map", leafletOutput("Cali_Map", height = 542),
                                 p(),)))
        
        
        
        
    )
)


server <- shinyServer(function(input, output){
    
    output$Cali_Map<- renderLeaflet({
        
        
        Data2 <- data%>%filter(Group_name==input$Crop_Chosen,Year==input$Year_Chosen,variable==input$Var_Chosen)
        
        
        ca_geo2 <- geo_join(ca_geo,Data2,"NAME","NAME",how="inner")%>%sf::st_transform(4326)%>%filter(value!=0)%>%drop_na(value) 
        
        
        popup <- paste0("County: ", ca_geo2$NAME, "<br>","Year: ",input$Year_Chosen,"<br>","Crop Category: ",as.character(input$Crop_Chosen),"<br>",as.character(input$Var_Chosen),": ", round(ca_geo2$value,2))
        
        pal <- colorNumeric(
            palette = "YlGnBu",
            domain = ca_geo2$value
        )
        
        
        leaflet() %>%
            addProviderTiles("CartoDB.Positron") %>%
            addPolygons(data = ca_geo2, 
                        fillColor = ~pal(value), 
                        color = "#b2aeae", # you need to use hex colors
                        fillOpacity = 0.7, 
                        weight = 0.3, 
                        smoothFactor = 0.2,
                        popup = popup) %>%
            leaflet::addLegend(pal = pal, 
                               values = ca_geo2$value, 
                               position = "bottomright", 
                               title = names(labels[which(labels == input$Var_Chosen)]),
                               labFormat = labelFormat(suffix = ""),
                               bins=6)
    }) 
    
    #download output table
    
    output$downloadtable <- downloadHandler(
        filename = function() {
            paste('Output','.csv', sep='')
        },
        content = function(file) {
            
            data1 <-  data%>%filter(Crop==input$Crop_Chosen,Year==input$Year_Chosen,variable==input$Var_Chosen)
            
            write.csv(data1, file)
        }
    )
    
    
    output$downloadData2 <- downloadHandler(
        filename = function() {
            "All_data_nass.csv"
        },
        content = function(file) {
            write.csv(all_data,file,row.names = FALSE)
        }
    )
    
    
})

shinyApp(ui = ui, server = server)



