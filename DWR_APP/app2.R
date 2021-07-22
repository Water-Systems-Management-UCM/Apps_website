
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
library(dplyr)




names <- read_csv("names.csv")
CSV_data <- read.csv("AW.DWRdata.csv")
CSV_data <- CSV_data%>%mutate(AW=dplyr::na_if(AW,0),Total_Land=dplyr::na_if(Total_Land,0),TW=dplyr::na_if(TW,0))
CSV_data <- CSV_data%>%group_by(NAME,Year,Crop)%>%mutate(max=max(Total_Land))%>%mutate(Total_Land1=sum(Total_Land)/1000,TW=sum(TW)/1000)%>%mutate(AW2=TW/Total_Land1)%>%mutate(proxy=ifelse(Total_Land==max,1,0))%>%filter(proxy==1)%>%ungroup()
CSV_data <- CSV_data%>%group_by(NAME,Year)%>%mutate(`Total land`=sum(Total_Land1),`Total water`=sum(TW))%>%ungroup()%>%dplyr::select(-c("proxy","max","Total_Land"))

###################################################

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
proxy <- proxy%>%mutate(`Adjusted Price`=Price*(1+(1-adj_value)),`Adjusted Gross Revenue`=`Gross revenue`*(1+(1-adj_value)))%>%dplyr::rename(NAME=County,Crop=Group_name)
proxy$Year <- as.numeric(proxy$Year)
proxy <- proxy%>%dplyr::select(Year,Crop,NAME,`Adjusted Gross Revenue`)


###################################################

CSV_data <- CSV_data%>%left_join(proxy,by=c("NAME","Crop","Year"))
CSV_data <- CSV_data%>%mutate("Gross revenue per acre-foot" = `Adjusted Gross Revenue`/AW2)
CSV_data <- CSV_data%>%mutate("Adjusted Total Production Value" = (`Adjusted Gross Revenue`*Total_Land1)/1000)
CSV_data <- CSV_data%>%group_by(NAME,Year)%>%mutate("Adjusted Total Production Value1" = sum(`Adjusted Total Production Value`,na.rm=TRUE))
Data <- melt(as.data.frame(CSV_data),id=c("NAME","Crop","Year"))
Data <- Data%>%filter(variable!="AW")
Data[Data$variable%in%c("Total water","Total land","Adjusted Total Production Value1"),]$Crop = "Sum All"
Data <- Data %>% unique(by=c("NAME","Year","Crop","variable","value"))
Data$variable <- as.character(Data$variable)
Data[Data$variable=="AW2",]$variable = "Applied water per acre"
Data[Data$variable=="Total_Land1",]$variable = "Land use"
Data[Data$variable=="TW",]$variable = "Water use"
Data[Data$variable=="Total water",]$variable = "Water use"
Data[Data$variable=="Total land",]$variable = "Land use"
Data[Data$variable=="Adjusted Total Production Value1",]$variable = "Adjusted Total Production Value"
Data$Year <- as.numeric(Data$Year)
###################################################


Unique_Crops <- unique(sort(Data$Crop))
Unique_Years <- unique(sort(Data$Year,decreasing=TRUE))
Unique_Vars <- unique(Data$variable)
Unique_County <- unique(Data$County)


ca_geo<-tigris::counties(state = "ca", class = "sf") 



labels <-c("Acre-feet/acre" = "Applied water per acre","Thousand acres"="Land use","Thousand acre-feet"="Water use","USD/Acre-feet","USD/acre-foot"="Gross revenue per acre-foot","USD/Acre"="Adjusted Gross Revenue","Thousand USD"="Adjusted Total Production Value")



ui <- fluidPage(
    
    # App title ----
    titlePanel("Agricultural Data Map"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        # Sidebar panel for inputs ----
        sidebarPanel(
            
            selectInput(inputId="Crop_Chosen", label="Crop Category", choices=c(Unique_Crops), selected = NULL, multiple = FALSE),
            selectInput(inputId="Year_Chosen", label="Year", choices=c(Unique_Years), selected = NULL, multiple = FALSE),
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
        
        
        Data2 <- Data%>%filter(Crop==input$Crop_Chosen,Year==input$Year_Chosen,variable==input$Var_Chosen)
   
        
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
            
            Data2 <- Data%>%filter(Crop==input$Crop_Chosen,Year==input$Year_Chosen,variable==input$Var_Chosen)
            
            write.csv(Data2, file)
        }
    )
    
    
    output$downloadData2 <- downloadHandler(
        filename = function() {
            "All_data_dwr.csv"
        },
        content = function(file) {
            write.csv(CSV_data,file,row.names = FALSE)
        }
    )
    
    
})




shinyApp(ui = ui, server = server)





