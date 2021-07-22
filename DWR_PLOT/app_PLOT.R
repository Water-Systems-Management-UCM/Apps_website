library(tidyverse)
library(XML)
library(rvest)
library(quantmod)
library(blscrapeR)
library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(RColorBrewer)
library(ggthemes)
library(ggiraph)
library(glue)
library(reshape)
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
Data <- Data%>%dplyr::rename(County=NAME)
###################################################


Unique_Crops <- unique(sort(Data$Crop))
Unique_Years <- unique(sort(Data$Year,decreasing=TRUE))
Unique_Vars <- unique(Data$variable)
Unique_County <- unique(Data$County)



n <- 23
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))


labels <-c("Acre-feet/acre" = "Applied water per acre","Thousand acres"="Land use","Thousand acre-feet"="Water use","USD/Acre-feet","USD/acre-foot"="Gross revenue per acre-foot","USD/Acre"="Adjusted Gross Revenue","Thousand USD"="Adjusted Total Production Value")





ui <- fluidPage(
    
    # App title ----
    titlePanel("Water and Land Use"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        # Sidebar panel for inputs ----
        sidebarPanel(
            
            selectInput(inputId="CropChosen", label="Crop Category", choices=Unique_Crops, selected = "Almonds & Pistachios", multiple = FALSE),
            selectInput(inputId="CountyChosen", label="County", choices=Unique_County, selected = c("Fresno","Kern"), multiple = TRUE),
            selectInput(inputId="variable", label="Variable", choices=Unique_Vars, selected = "Land use", multiple = FALSE),
            
            
            
            sliderInput(inputId = "YearsChosen",
                        label = "Year Range",
                        min = 1998,
                        max = 2015,
                        value = c(1998,2015),
                        step = 1,round=TRUE,sep = ""),
            
            
            
            # Button
            downloadButton("downloadtable","Download"),
            
            downloadButton("downloadData2","Download All Data")
        ),
        
        
        
        
        
        # Main panel for displaying outputs ----
        mainPanel(
            
            girafeOutput("plot1")   )
    ))
        
        
        
        


server <- shinyServer(function(input, output){
    
    output$plot1 <- renderGirafe({
        
      
       
        
        data1 <-  Data %>% filter(Crop %in% input$CropChosen & County %in% input$CountyChosen & Year >= min(input$YearsChosen) & Year <=max(input$YearsChosen)& variable == input$variable)
        
       plot <- ggplot(data1, aes(x=Year, y= value, colour=County)) + geom_line(alpha=1,size=1.5,aes(colour=County))+ geom_point_interactive(aes(x=Year, y=value,colour=County,tooltip=paste(glue("County: {County}\nYear: {Year}\nVariable: {variable}\nValue:{value}")),data_id=value),size=2) + scale_colour_manual(values= col_vector)+theme_minimal()+labs(title= paste(as.character(input$variable),"\n ","Crop Category:",as.character(input$CropChosen)),y= names(labels[which(labels == input$variable)]))+theme(
            plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5),
            plot.caption = element_text(hjust= 0),panel.border = element_rect(colour = "black", fill=NA, size=0.5),plot.caption.position =  "plot")+
         scale_y_continuous(labels = function(x) format(x, big.mark = ",",
                                                        scientific = FALSE))
        
        girafe(ggobj = plot, 
               options = list(opts_selection(type = "single"),code=print(plot), opts_tooltip(
                   opacity = 0.8, use_fill = TRUE,
                   use_stroke = FALSE, 
                   css = "padding:5pt;font-family:Times;color:black"),
                   opts_hover_inv(css = "opacity:0.8"), 
                   opts_hover(css = "fill:#4c6061;")))
        
        
        
    })
    
    #download output table
    
    output$downloadtable <- downloadHandler(
        filename = function() {
            paste('Output','.csv', sep='')
        },
        content = function(file) {
            
            data1 <-  Data %>% filter(Group_name %in% input$CropChosen & County %in% input$CountyChosen & Year >= min(input$YearsChosen) & Year <=max(input$YearsChosen))
            
            write.csv(data1, file)
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





