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

group_data <- all_data%>%left_join(names,by="Commodity Code")%>%drop_na(`Group_name`)%>%dplyr::select(Year,"Commodity Code",County,Acres,Yield,Price,`Crop Name`,Group_name,Value,`County Code`)
group_data$Acres <-  as.numeric(group_data$Acres)
group_data$Price <-  as.numeric(group_data$Price)
group_data$Yield <-  as.numeric(group_data$Yield)
group_data$Value <-  as.numeric(group_data$Value)


#Find proxy


dgroup_data <- group_data%>%group_by(Group_name,Year,`County Code`)%>%mutate(`Total Area` = sum(Acres,na.rm=TRUE)/1000,`Value category`=sum(Value,na.rm=TRUE))%>%ungroup()
dgroup_data2 <-  dgroup_data%>%group_by(`Commodity Code`,`County Code`,Year)%>%mutate(Rate = Acres/`Total Area`)%>%ungroup()
dgroup_data2 <-  dgroup_data2%>%group_by(`County Code`,Year)%>%mutate("Total Area1" = sum(Acres,na.rm=TRUE)/1000,"Tot Rev"=sum(Value,na.rm=TRUE)/1000)%>%ungroup()
dgroup_data2 <- dgroup_data2%>%group_by(`County Code`,Group_name,Year)%>%mutate(max=max(Rate),`Gross revenue`=Price*Yield)%>%mutate(proxy=ifelse(Rate==max,1,0))%>%filter(proxy==1)%>%ungroup()




#Adjust for inflation

# Price inflation data using 2019 as base year
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

#Theme


theme_set(
    theme_pander() +
        theme(text = element_text(family = "Times"),
              panel.background = element_rect(color = "black")) 
)



# App


data <- proxy
data$variable <- as.character(data$variable)
Unique_Crops <- sort(unique(data$Group_name))
Unique_County <- sort(unique(data$County))
Years <- as.numeric(data$Year)
data$Year<- as.integer(data$Year)
Unique_Years <- unique(as.numeric(Years))
Unique_Vars <- sort(unique(data$variable))
Unique_Vars <- Unique_Vars[Unique_Vars != "Crop Name"]



n <- 23
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))




labels <-c("USD per ton"="Adjusted Price","Tons per acre"="Yield","Thousand Acres"="Total Area","Thousand USD"="Adjusted Total Production Value","USD per ton"="Price","USD per acre"="Adjusted Gross Revenue")



ui <- shinyUI(fluidPage(
    
    # App title ----
    titlePanel("Historical Data NASS"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        # Sidebar panel for inputs ----
        sidebarPanel(
            
            selectInput(inputId="CropChosen", label="Crop Category", choices=Unique_Crops, selected = "Almonds & Pistachios", multiple = FALSE),
            selectInput(inputId="CountyChosen", label="County", choices=Unique_County, selected = c("Fresno","Kern"), multiple = TRUE),
            selectInput(inputId="variable", label="Variable", choices=Unique_Vars, selected = "Yield", multiple = FALSE),
            
            
            
            sliderInput(inputId = "YearsChosen",
                        label = "Year Range",
                        min = 1980,
                        max = 2019,
                        value = c(1980,2019),
                        step = 1,round=TRUE,sep = ""),
            
            # Button
            downloadButton("downloadtable","Download"),
            
            downloadButton("downloadData2","Download All Data")
        ),
        
        
        
        
        
        
        
        
        # Main panel for displaying outputs ----
        mainPanel(
            
            girafeOutput("plot1")   )
    )))


# Define server logic required to draw a histogram ----
server <- shinyServer(function(input, output){
    
    
    
    output$plot1 <- renderGirafe({
        
        data1 <-  data %>% filter(Group_name %in% input$CropChosen & County %in% input$CountyChosen & Year >= min(input$YearsChosen) & Year <=max(input$YearsChosen) & variable == input$variable)
        
        
        plot <- ggplot(data1, aes(x=Year, y= value, colour=County)) + geom_line(alpha=1,size=1.5,aes(colour=County))+ geom_point_interactive(aes(x=Year, y=value,colour=County,tooltip=paste(glue("County: {County}\nYear: {Year}\nVariable: {variable}\nValue:"),value)),size=2) + scale_colour_manual(values= col_vector)+theme_minimal()+labs(title= paste(as.character(input$variable),"\n ","Crop Category:",as.character(input$CropChosen)),y= names(labels[which(labels == input$variable)]))+theme(
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
            
            data1 <-  data %>% filter(Group_name %in% input$CropChosen & County %in% input$CountyChosen & Year >= min(input$YearsChosen) & Year <=max(input$YearsChosen) & variable == input$variable)
            
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

shinyApp(ui=ui, server=server) 



