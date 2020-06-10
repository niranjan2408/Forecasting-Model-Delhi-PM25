library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinycssloaders)
library(forecast)
library(padr)
library(imputeTS)
library(readxl)
library(dplyr)
library(DT)
library(ggplot2)

ui <- dashboardPage(
    skin = "black", 
    dashboardHeader(title = "Forecasting Delhi PM2.5 Recording", titleWidth = 350,
                    
                    tags$li(a(href = 'https://www.excelr.com/', img(src = 'https://excelrcom.b-cdn.net/assets/media/general/excelr-logo.png', title = "Company Website", height = "30px"),
                              style = "padding-top:10px; padding-bottom:10px;"),class = "dropdown")),

    dashboardSidebar(width = 350,
             sidebarMenu(
                            h3(menuItem("Project Background", tabName = "Background", icon = icon("home"))),
                            tags$hr(),
                            h3(menuItem("Input Data", tabName = "InputData", icon = icon("angle-double-right"))),
                            tags$hr(),
                            tags$hr(),
                            h3(menuItem("Historical Data", tabName = "HistoricalData", icon = icon("th"))),
                            tags$hr(),
                            tags$hr(),
                            h3(menuItem("Forecast", tabName = "Forecast", icon = icon("dashboard"))),
                            tags$hr(),
                            tags$hr()
                        )
                    ),
    dashboardBody(
        
        tabItems(
            
          tabItem(
                        tabName = "Background",
                            fluidRow(
                                      h2(
                                            tags$ul("Business Objective:",style = "color: blue"),
                                          ),
                                     
                                       h3(
                                         tags$ul("Particulate matter (PM ) has been recorded in Delhi to understand the air quality on an hourly basis. Our objective is to forecast the data based on available input data.",style = "color: blue"),
                                          ),
                                     
                                      h4(
                                        
                                        tags$ul(tags$li(tags$span("AQI is used by government agencies to communicate to the public how polluted  the air currently is or how polluted it is forecast to become in upcoming days"))),
                                        tags$br(),
                                        tags$ul(tags$li(tags$span("Public health risks increase as the AQI rises.Air quality measurement are commonly reported in terms of micrograms per cubic meter parts per million or parts per billion"))),
                                        tags$br(),
                                        tags$ul(tags$li(tags$span("PM2.5 refers to atmospheric particulate matter (PM) that have a diameter of less than 2.5 micrometers, which is about 3% the diameter of a human hair"))),
                                        tags$br(),
                                        tags$ul(tags$li(tags$span("AQI converts the measured pollutant concentrations in a community's air to a number on a scale of 0 to 500 and above"))),
                                        tags$br(),
                                        tags$ul(tags$li(tags$span("PM2.5 can come from various sources.They include power plants, motor vehicles, airplanes, residential wood burning, forest fires, agricultural burning, volcanic eruptions and dust storms.Some are emitted directly into the air, while others are formed when gases and particles interact with one another in the atmosphere"))),
                                        tags$br(),
                                        tags$ul(tags$li(tags$span("Studies have found a close link between exposure to fine particles and premature death from heart and lung disease.There are various studies published by associations such as  American Heart Association, Journal of the American Medical Association, etc.on adverse effect on PM2.5 on human health."))),
                                        tags$br(),
                                         ),
                                      
                                      htmlOutput("PM25picture")
                                      
                                      )
                      ),
          
          
          tabItem(
                        tabName = "InputData",
                            fluidRow(
                                        h3(fileInput("file1", "Choose Excel File with date and pm25 columns in Ascending Order",multiple = F,accept = c(".xlsx"),width = 700)),
                                        tags$hr(),
                                        tags$hr(),
                                        tags$hr(),
                        
                                        h3(numericInput("daysahead", "Days to Forecast Ahead", value = 7)), 
                                        tags$hr(),
                                        tags$hr(),
                                        tags$hr(),
                                        
                                        h3("Preview Sample Input Data"),
                                        h4("Column 1: date- YYYY:MM:DD time- HH:MM:SS"),
                                        h4("Column 2: pm25 - numeric"),
                                        dataTableOutput("Preview"),
                                    )
                    ),
            
            tabItem(
                        tabName = "Forecast",
                            fluidRow(
                                        h3("Expected Trend"),
                                        plotOutput("forecastplot",click = "plot_click")%>% withSpinner(color="#0dc5c1"),
                                        verbatimTextOutput("info"),
                                        h3("Expected Values"),
                                        dataTableOutput("forecasttable"),
                                    )
                    ),
            
            tabItem(    
                        tabName = "HistoricalData",
                            fluidRow(
                                        h3("Input Data with Missing Values Interpolation with na_seasplit_ma (Moving Average) Technique"),
                                        plotOutput("interpolation"),
                                        h3("Hourly Fluctuations in PM2.5 Values"),
                                        plotOutput("hourplot"),
                                        h3("Daily Fluctuations in PM2.5 Values"),
                                        plotOutput("dailyplot"),
                                        h3("Monthly Fluctuations in PM2.5 Values"),
                                        plotOutput("monthlyplot")
                                    )
                    )
                )
            )
        )
                                   

server <- function(input, output,session)   {
  
  data <- eventReactive(input$file1,{    rnorm(1:100000)  })
  
    src = "https://static.wixstatic.com/media/55ff98_f5efc78fbce64f78ac1de176abd81eba~mv2.png/v1/fill/w_985,h_430,al_c,q_90,usm_0.66_1.00_0.01/55ff98_f5efc78fbce64f78ac1de176abd81eba~mv2.webp"
    
    output$PM25picture<-renderText({c('<img src="',src,'">')})

    output$Preview <- renderDataTable({
                                        Preview <- data.frame(date = c(seq(from = as.POSIXct("2018-01-01 00:00",tz="UTC"), 
                                           to = as.POSIXct("2018-01-01 9:00",tz="UTC"), by = "hour")),
                                            pm25=c("423","414","417","466","470","449","404","380","371","365"))
                                        Preview$date <- as.character(Preview$date)
                                        Preview
        
                                      })
    
    re <- reactive({
                                        inFile <- input$file1
                                        if (is.null(inFile))
                                            return(NULL)
                                        file.rename(inFile$datapath,paste(inFile$datapath, ".xlsx", sep=""))
                                        delhi <- read_excel(paste(inFile$datapath, ".xlsx", sep=""))
                                        delhi$pm25 <- as.numeric(delhi$pm25)
                                        date_time <-pad(as.data.frame(delhi$date))
                                        colnames(date_time) <- 'date'
                                        updated_data<- full_join(date_time,delhi)
                                        updated_data$na_seasplit_ma<- na_seasplit(updated_data$pm25,algorithm = "ma",find_frequency=TRUE )
                                        model_data <- updated_data[,c(1,3)]
                                        colnames(model_data) <- c("Date","PM25")
                                        model_data$PM25 <- ts( model_data$PM25,frequency = 24)
                                        model_data
                    })
    
    output$forecastplot <- renderPlot({
                                        model_data <- re()
                                        if (is.null(model_data))
                                            return(NULL)
                                        model <- stlf(model_data$PM2,method = "arima", h =input$daysahead*24)
                                        pred <- as.data.frame (model$mean)
                                        pred["Date"] <- data.frame(seq(from = as.POSIXct("2018-04-20 01:00",tz="UTC"),length.out = input$daysahead*24, by = "hour"))
        
                                        ggplot()+
                                            geom_line(data = tail(model_data,336L),aes(x = tail(Date,336L), y = tail(PM25,336L),color="Last Two Weeks Data")) +
                                            geom_line(data = pred,aes(x=Date, y=x,color="Predictions")) +
                                            labs(title=paste("PM2.5 Predictions for Next",input$daysahead,"Days",sep = " ") ,x ="Day", y = "PM2.5") +
                                            labs(color = "LINE")+
                                            scale_x_datetime(date_breaks = "1 days",date_labels = "%b %d")+
                                              theme_light()+
                                              theme(axis.text.x=element_text(angle=60, hjust=1)) +
                                              theme(axis.title.x = element_text(size = 20,color = "grey20")) +
                                              theme(axis.title.y = element_text(size = 20, color = "grey20")) +
                                              theme(axis.text = element_text(size = 13, color = "blue"))+
                                              theme(legend.title = element_text(color="black", size=20))+
                                              theme(plot.title = element_text(color="grey20", size=20))+
                                              theme(legend.text = element_text(size=15))
                                        })
    
    
    output$forecasttable <- renderDataTable({
                                            model_data <- re()
                                            if (is.null(model_data))
                                                return(NULL)
                                            model <- arima(model_data$PM25, c(1,0,1),seasonal = list(order = c(0,1,2), period = 24))
                                            pred <- as.data.frame (forecast(model,h=input$daysahead*24)) 
                                            pred["Date"] <- seq(from = as.POSIXct("2018-04-20 01:00",tz="UTC"), length.out = input$daysahead, by = "hour")
                                            pred$'Point Forecast' <- round(pred$'Point Forecast',2)
                                            pred <- pred[c(6,1)]
                                            rownames(pred) <- 1:nrow(pred)
                                            pred
                                    })
    
    output$info <- renderText({
                                            if (is.null(input$plot_click))
                                                return(NULL)
                                            paste0("PM2.5 = ", round(input$plot_click$y,2))
                                })
    
    
    output$hourplot <- renderPlot({
                                            model_data <- re()
                                            if (is.null(model_data))
                                              return(NULL)
                                            model_data$Time<- format(as.POSIXct(model_data$Date,format="%Y:%m:%d %H:%M:%S"),"%H:%M:%S")
                                            model_data$Date <- as.Date(model_data$Date,format ="%Y:%m:%d")
                                            ggplot(model_data,aes(x=Time,y=PM25,fill=Time))+theme(axis.text.x = element_text(angle = 90, hjust = 1))+geom_boxplot()+theme_light()+
                                            theme(axis.text.x=element_text(angle=60, hjust=1)) +
                                            theme(axis.title.x = element_text(size = 20,color = "grey20")) +
                                            theme(axis.title.y = element_text(size = 20, color = "grey20")) +
                                            theme(axis.text = element_text(size = 13, color = "blue"))+
                                            theme(legend.position = "none")
                                    })
    
    output$dailyplot <- renderPlot({
                                            model_data <- re()
                                            if (is.null(model_data))
                                              return(NULL)
                                            model_data$Day <- weekdays(as.Date(model_data$Date,format ="%Y:%m:%d"))
                                            model_data$Day <- factor(model_data$Day,levels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"),ordered=TRUE)
                                            ggplot(model_data,aes(x=Day,y=PM25,fill=Day))+theme(axis.text.x = element_text(angle = 90, hjust = 1))+geom_boxplot()+theme_light()+
                                            theme(axis.text.x=element_text(angle=60, hjust=1)) +
                                            theme(axis.title.x = element_text(size = 20,color = "grey20")) +
                                            theme(axis.title.y = element_text(size = 20, color = "grey20")) +
                                            theme(axis.text = element_text(size = 13, color = "blue"))+
                                            theme(legend.position = "none")
                                            
                                    })
    
    output$monthlyplot <- renderPlot({
                                            model_data <- re()
                                            if (is.null(model_data))
                                              return(NULL)
                                            model_data$Month <- months(as.Date(model_data$Date,format ="%Y:%m:%d"))
                                            model_data$Month <- factor(model_data$Month,levels=c("January","February","March","April"),ordered=TRUE)
                                            ggplot(model_data,aes(x=Month,y=PM25,fill=Month))+theme(axis.text.x = element_text(angle = 90, hjust = 1))+geom_boxplot()+theme_light()+
                                            theme(axis.text.x=element_text(angle=60, hjust=1)) +
                                            theme(axis.title.x = element_text(size = 20,color = "grey20")) +
                                            theme(axis.title.y = element_text(size = 20, color = "grey20")) +
                                            theme(axis.text = element_text(size = 13, color = "blue"))+
                                            theme(legend.position = "none")
            
                                    })
    
    
    output$interpolation <- renderPlot({
                                            inFile <- input$file1
                                            if (is.null(inFile))
                                                return(NULL)
                                            file.rename(inFile$datapath,paste(inFile$datapath, ".xlsx", sep=""))
                                            delhi <- read_excel(paste(inFile$datapath, ".xlsx", sep=""))
                                            delhi$pm25 <- as.numeric(delhi$pm25)
                                            date_time <-pad(as.data.frame(delhi$date))
                                            colnames(date_time) <- 'date'
                                            updated_data<- full_join(date_time,delhi)
                                            updated_data$na_seasplit_ma<- na_seasplit(updated_data$pm25,algorithm = "ma",find_frequency=TRUE )
                                            ggplot()+
                                                geom_line(data = updated_data,aes(x=date,y=na_seasplit_ma,color="Missing Data")) +
                                                geom_line(data = updated_data,aes(x=date,y=pm25,color="Input Data"))+
                                                labs(x ="Day", y = "PM2.5") +
                                                labs(color = "LINE")+ scale_x_datetime(date_breaks = "10 days",date_labels = "%b %d")+
                                                theme_light()+
                                                theme(axis.text.x=element_text(angle=60, hjust=1)) +
                                                theme(axis.title.x = element_text(size = 20,color = "grey20")) +
                                                theme(axis.title.y = element_text(size = 20, color = "grey20")) +
                                                theme(axis.text = element_text(size = 13, color = "blue"))+
                                                theme(legend.title = element_text(color="black", size=20))+
                                                theme(legend.text = element_text(size=15))
            
                                        })
  
}

shinyApp(ui, server)                

