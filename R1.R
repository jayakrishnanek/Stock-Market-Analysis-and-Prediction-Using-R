
library(shiny)
library(shinydashboard)
#install.packages("shinythemes")
library(shinythemes)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("rtsdata")
library(rtsdata)
#install.packages("reshape")
library(reshape)
#install.packages("dygraphs")
library(dygraphs)
#install.packages("forecast")
library(forecast)
#install.packages("tidyquant")
library(tidyquant)
#install.packages("glue")
library(glue)
#install.packages("rvest")
library(rvest)
#install.packages("stringr")
library(stringr)
#install.packages("quantmod")
library(quantmod)
#install.packages("tseries")
library(tseries)
#install.packages("TTR")
library(TTR)


#________________________________________________________________#

# Load file with company names and Tickers to extract values from yahoo finances
elements <- read.csv("C:/Stock-Market-Analysis-and-Prediction-Using-R-main/stock_data.csv", header = TRUE, sep = ';', fileEncoding = "UTF-8")

# Create variable with company names and their Symbols as values
CompanySymbol <- unname(unlist(elements['Ticker']))

names(CompanySymbol) <- unname(unlist(elements['Name']))

# Variable with the type of values to display
vNames <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")
names(vNames) <- 1:6

ui <- dashboardPage(skin = "blue",
                    dashboardHeader(title = "Jayakrishnan E K Project"),
                    dashboardSidebar(
                      sidebarMenu(
                        sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                                          label = "Search...", icon = shiny::icon("search")),
                        menuItem("Home", tabName = "HomeTab", icon = icon("home")),
                        menuItem("Data Visualisation", tabName = "DataVisualisationTab", icon = icon("chart-bar")),
                        menuSubItem("Data Analysis", tabName = "DataAnalysisTab", icon = icon("chart-line")),
                        menuSubItem("Daily Data", tabName = "DailyDataTab", icon = icon("list-alt")),
                        sliderInput("slider1", "Select number of days to be forecasted:", min = 0, max = 30, value = 5),
                        menuItem("ARIMA Forecast", tabName = "Forecast", icon = icon("calendar"))
                      )
                    ),
                    dashboardBody(
                      # Custom CSS
                      tags$head(
                        tags$style(HTML("
        body {
          font-family: 'Arial', sans-serif;
        }
        .sidebar-menu > li {
          font-size: 16px;
          font-weight: bold;
        }
        .box {
          border-radius: 10px;
          box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);
        }
        .box-header {
          border-radius: 10px 10px 0 0;
          background-color: #0073b7;
          color: white;
          text-align: center;
        }
        .box-body {
          padding: 20px;
        }
        .box .box-title {
          font-size: 18px;
          font-weight: bold;
        }
        .nav-tabs-custom .nav-tabs li.active {
          border-top-color: #0073b7;
        }
        .form-control {
          border-radius: 5px;
        }
        .btn {
          border-radius: 5px;
          background-color: #0073b7;
          color: white;
        }
        .btn:hover {
          background-color: #005bb5;
        }
        .sliderInput {
          background-color: #f2f2f2;
          padding: 10px;
          border-radius: 10px;
        }
        .dataTables_wrapper .dataTables_paginate .paginate_button {
          background-color: #0073b7;
          color: white;
          border-radius: 5px;
        }
        .dataTables_wrapper .dataTables_paginate .paginate_button:hover {
          background-color: #005bb5;
        }
      "))
                      ),
                      tabItems(
                        # First tab content
                        tabItem(tabName = "HomeTab",
                                fluidRow(
                                  box(title = "Stock Market Analysis", status = "primary", width = 12, solidHeader = TRUE,
                                      box(title = "Company", status = "info", width = 12, solidHeader = TRUE,
                                          selectizeInput("Company", h4("Select the company name:", align = 'left'), 
                                                         choices = c('Enter company name' = '', unique(names(CompanySymbol))), 
                                                         selected = NULL, multiple = FALSE, options = NULL)
                                      ),
                                      box(title = "About", status = "warning", width = 12, solidHeader = TRUE, 
                                          collapsible = TRUE, textOutput("comdesc"))
                                  )
                                )
                        ),
                        
                        # Second tab content
                        tabItem(tabName = "DataVisualisationTab",
                                fluidRow(
                                  box(title = "Inputs", status = "primary", width = 3, solidHeader = TRUE,
                                      radioButtons("ranges", h3("Select Date Range"), 
                                                   choices = list("1 week" = 1, "1 year" = 2, "5 years" = 3, "Customized" = 4), selected = 4),
                                      dateRangeInput(inputId = 'Date', label = NULL, start = Sys.Date() - 90, end = Sys.Date() - 1, max = Sys.Date(), 
                                                     format = "dd/mm/yyyy", separator = " - "),
                                      checkboxGroupInput("variableSelect", h3("Select Values"), 
                                                         choices = list("Open Values" = 1, "Close Values" = 4, "High" = 2, "Low" = 3, "Volume" = 5, "Adjusted" = 6), 
                                                         selected = 1)
                                  ),
                                  tabBox(title = "Graphs", id = "tabset1", width = 9, 
                                         tabPanel("Time Series Plot", plotOutput("distPlot")),
                                         tabPanel("Interactive Plot", dygraphOutput("dygraph")),
                                         tabPanel("Values", tableOutput("InfoValues"))
                                  )
                                )
                        ),
                        
                        # Sub Item 1 tab content
                        tabItem(tabName = "DataAnalysisTab",
                                fluidRow(
                                  tabBox(width = 12,
                                         tabPanel("Bollinger Bands with RSI curve", plotOutput("plot3", height = "700px"), icon = icon("chart-bar")),
                                         tabPanel("MACD with Golden Cross", plotOutput("plot4", height = "700px"), icon = icon("chart-bar"))
                                  )
                                )
                        ),
                        
                        # Sub Item 2 tab content
                        tabItem(tabName = "DailyDataTab",
                                fluidRow(
                                  box(title = "Inputs", status = "primary", width = 5, solidHeader = TRUE,
                                      sliderInput("slider", "Slider input:", 1, 30, 5)
                                  ),
                                  box(title = "Data", status = "warning", width = 7, solidHeader = TRUE,
                                      tableOutput("view")
                                  )
                                )
                        ),
                        
                        # Forecast tab content
                        tabItem(tabName = "Forecast",
                                fluidRow(
                                  box(title = "FORECAST VALUES", status = "warning", width = 6, collapsible = TRUE, solidHeader = TRUE,
                                      tabBox(width = 12,
                                             tabPanel(title = "Seasonal Data", tableOutput("Data2")),
                                             tabPanel(title = "Non Seasonal Data", tableOutput("Data1"))
                                      )
                                  ),
                                  box(title = "ARIMA FORECASTS", status = "info", width = 6, collapsible = TRUE, solidHeader = TRUE,
                                      tabBox(width = 12,
                                             tabPanel(title = "Auto Arima - Seasonal", plotOutput(height = 350, "plot2", click = "plot_click"), br(), verbatimTextOutput("info")),
                                             tabPanel(title = "Auto Arima - Non Seasonal", plotOutput(height = 350, "plot1", click = "plot_click1"), br(), verbatimTextOutput("info1"))
                                      )
                                  )
                                )
                        )
                      )
                    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  
  # Implement observations to automate the updates of the inputs in the date range
  observeEvent(input$ranges, {
    aux <<- input$ranges
    if (input$ranges == "1"){
      updateDateRangeInput(session,
                           'Date',
                           start = Sys.Date() - 7)
    }
    else if (input$ranges == "2"){
      updateDateRangeInput(session,
                           'Date',
                           start = Sys.Date() - 365)
    }
    else if (input$ranges == "3"){
      updateDateRangeInput(session,
                           'Date',
                           start = Sys.Date() - 1825)
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$Date, {
    myDates <<- input$Date
    if (input$Date[1]== Sys.Date() - 7){
      updateRadioButtons(session,
                         'ranges',
                         selected = 1)
    }
    else if (input$Date[1]== Sys.Date() - 365){
      updateRadioButtons(session,
                         'ranges',
                         selected = 2)
    }
    else if (input$Date[1]== Sys.Date() - 1825){
      updateRadioButtons(session,
                         'ranges',
                         selected = 3)
    }
    else {
      updateRadioButtons(session,
                         'ranges',
                         selected = 4)
    }
  }, ignoreInit = TRUE)
  
  
  # Gets executed only when company changes
  companyName <- reactive({
    if (input$Company == "") {
      return("Company description will be shown here...")
    } else {
      selectedCompanySymbol <- str_replace_all(CompanySymbol[input$Company], "\"", "")
      
      tryCatch({
        webpage <- read_html(glue("https://finance.yahoo.com/quote/{selectedCompanySymbol}/profile?p={selectedCompanySymbol}")) %>%
          html_nodes(".quote-sub-section") %>%
          html_text()
        
        return(str_replace(webpage, "Description", ""))
      }, error = function(e) {
        return("Failed to fetch company description. Please try again later.")
      })
    }
  })
  
  # Responsible for printing company description
  output$comdesc <- renderText({
    companyName()
  })
  
  
  
  dataInput <<- reactive({
    # Added 'require' to wait until the user selects a company
    req(input$Company)
    
    # Give values from the date range selected
    start <- format(input$Date[1])
    end <- format(input$Date[2])
    
    # Implemented try to handle errors
    tryCatch({
      # Use 'rtsdata' to obtain data from yahoo finances using the Symbol from the company selected in the interface
      data <- ds.getSymbol.yahoo(CompanySymbol[input$Company], from = start, to = end)
      
      # Transform to integer the variables selected from the check box to filter dataframe for those values
      variablesSelected <<- as.integer(input$variableSelect)
      dataAux <- data[,variablesSelected]
      names(dataAux) <- vNames[variablesSelected]
      return(dataAux)
    }, message = function(e) {
      return(e$message)
    }, warning = function(e) {
      return(e$message)
    }, error = function(e) {
      dataAux <- xts(1, as.Date(1))
      return(dataAux)
    })
    
  })
  # Regular plot
  output$distPlot <- renderPlot({
    
    # Create dataframe from the xts object
    dataAux <- data.frame(date=index(dataInput()), coredata(dataInput()))
    
    # Use function melt to define proper structure of Dataframe to represent with ggplot
    dataAux <- melt(data = dataAux, id.vars = c("date"), measure.vars = c(2:ncol(dataAux)))
    
    if (nrow(dataAux)<2){
      dataAux <- data.frame()
      ggplot(dataAux) + geom_point() + xlim(0, 10) + ylim(0, 100) +
        annotate("text", x = 4, y = 25, label = paste("Unable to import:", CompanySymbol[input$Company], "for the period of time selected.")) +
        annotate("text", x = 4, y = 20, label = "Please select a different company or change the date range selected")
    }
    
    else{
      # Display dataframe
      ggplot(dataAux, aes(x = date, y = value, colour = variable)) + 
        geom_line() + 
        geom_point() +
        ggtitle(input$Company) +
        theme(plot.title = element_text(size=14, face="bold")) +
        labs (colour = "Values")
    }
    
  })
  
  
  # Interactive plot
  output$dygraph <- renderDygraph({
    
    # Display interactive plot
    dygraph(dataInput()) %>% dyRangeSelector() #%>% 
    #dyShading(from = start, to = end, color = "white")
    
  })
  
  # Table  ----
  output$InfoValues <- renderTable({
    
    # Transform to integer the variables selected from the check box to filter dataframe for those values
    variablesSelected <- as.integer(input$variableSelect)
    
    # create empty dataframe for our table
    tableInfo <- data.frame(Minimum=numeric(),
                            Maximum=numeric(),
                            Average=numeric(),
                            Median=numeric(),
                            Standard_deviation=numeric(),
                            SD=character(),
                            stringsAsFactors=FALSE)
    
    names(tableInfo)[names(tableInfo) == "SD"] <- "SD(%)"
    
    # Fill the table
    for (i in 1:length(variablesSelected)){
      tableInfo[nrow(tableInfo)+1,] <- c("Minimum" = round(min(as.numeric(coredata(dataInput()[,i])), na.rm = TRUE), 2), 
                                         "Maximum" = round(max(as.numeric(coredata(dataInput()[,i])), na.rm = TRUE), 2),
                                         "Average" = round(mean(as.numeric(coredata(dataInput()[,i])), na.rm = TRUE), 2),
                                         "Median" = round(median(as.numeric(coredata(dataInput()[,i])), na.rm = TRUE),  2),
                                         "Standard deviation" = round(sd(as.numeric(coredata(dataInput()[,i])), na.rm = TRUE),  2),
                                         "SD(%)"= paste(round(sd(as.numeric(coredata(dataInput()[,i])), na.rm = TRUE) / mean(as.numeric(coredata(dataInput()[,i])), na.rm = TRUE), 3)*100, "%"))
    }
    rownames(tableInfo) <- vNames[variablesSelected]
    tableInfo
  }, rownames = TRUE)
  
  #Daily Data table
  
  output$view <- renderTable({
    
    data1 <- ds.getSymbol.yahoo(CompanySymbol[input$Company], to = Sys.Date())
    colnames(data1)=c("Open", "High", "Low", "Close", "Volume", "Adjusted")
    tail(data1, n = input$slider)
    
  }, include.rownames = TRUE)
  
  
  #AUTO-ARIMA (Non Seasonal)
  
  output$plot1<-renderPlot({
    df<-tq_get(CompanySymbol[input$Company],get = "stock.prices", from = "2020-01-01")
    auto_arima<-auto.arima(df$close, lambda = "auto",) 
    autoplot(forecast(auto_arima, input$slider1), main="Non-Seasonal Forecast", ylab="Closing Prices\n", xlab="\nDays")+theme_minimal()
  })
  
  #Table
  
  output$Data1<-renderTable({
    df<-tq_get(CompanySymbol[input$Company] ,get = "stock.prices", from = "2020-01-01")
    pi<-auto.arima(df$close, lambda = "auto")
    q<- forecast(pi, h=input$slider1)
    
    x=seq(as.Date(Sys.Date()+1),as.Date(Sys.Date()+(input$slider1)),by=1)
    Date=as.character(x)
    a=data.frame(Date,q)
    colnames(a)=c('Date','Point Forecast','Low(80)','High(80)','Low(95)','High(95)')
    print(a)
    
  })
  
  #Interactive
  
  output$info <- renderText({
    paste0("x=", round(input$plot_click$x, 2), "\ny=", round(input$plot_click$y, 2))
  })
  
  #AUTO-ARIMA (Seasonal)
  
  output$plot2<-renderPlot({
    df<-as.data.frame(tq_get(CompanySymbol[input$Company],
                             get = "stock.prices", from = "2020-01-01"))
    df$Open = df[,3]
    df$High = df[,4]
    df$Low = df[,5]
    df$Close = df[,6]
    df$Volume = df[,7]
    df$Adj = df[,8]
    df <- df[,c(9,10,11,12,13,14)]
    
    df$v7_MA = ma(df$Close, order=7)
    df$v30_MA <- ma(df$Close, order=30)
    
    #STL
    rental_ma <- ts(na.omit(df$v7_MA), frequency=30)
    decomp_rental <- stl(rental_ma, s.window="periodic")
    adj_rental <- seasadj(decomp_rental)
    
    #arima
    fit_s<-auto.arima(adj_rental, seasonal=TRUE)
    autoplot(forecast(fit_s, input$slider1), main="Seasonal Forecast", ylab="Closing Prices\n", xlab="\nMonths")+theme_minimal()
  })
  
  #Table
  
  output$Data2<-renderTable({
    df<-as.data.frame(na.omit(tq_get(CompanySymbol[input$Company],get = "stock.prices", from = "2020-01-01")))
    df$Open = df[,3]
    df$High = df[,4]
    df$Low = df[,5]
    df$Close = df[,6]
    df$Volume = df[,7]
    df$Adj = df[,8]
    df <- df[,c(9,10,11,12,13,14)]
    
    df$v7_MA = ma(df$Close, order=7)
    df$v30_MA <- ma(df$Close, order=30)
    
    #STL
    rental_ma <- ts(na.omit(df$v7_MA), frequency=30)
    decomp_rental <- stl(rental_ma, s.window="periodic")
    adj_rental <- seasadj(decomp_rental)
    
    #arima
    fit_s<-auto.arima(adj_rental, seasonal=TRUE)
    fcast_s <- forecast(fit_s, input$slider1)
    
    x=seq(as.Date(Sys.Date()+1),as.Date(Sys.Date()+(input$slider1)),by=1)
    Date=as.character(x)
    a=data.frame(Date,fcast_s)
    colnames(a)=c('Date','Point Forecast','Low(80)','High(80)','Low(95)','High(95)')
    print(a)
  })
  
  #Interactive
  
  output$info1 <- renderText({
    paste0("x=", round(input$plot_click1$x, 2), "\ny=", round(input$plot_click1$y, 2))
  })
  
  #Bollinger Bands & RSI curve
  
  output$plot3<-renderPlot({
    df<-as.data.frame(na.omit(tq_get(CompanySymbol[input$Company],
                                     get = "stock.prices", from = "2020-01-01")))
    df$Date = df[,2]
    df$Open = df[,3]
    df$High = df[,4]
    df$Low = df[,5]
    df$Close = df[,6]
    df$Volume = df[,7]
    df$Adj = df[,8]
    df <- df[,c(9,10,11,12,13,14)]
    
    BollingerBands <- xts(OHLCV(df), df[["Date"]])
    chartSeries(
      BollingerBands, subset='2021-01::2022-11',
      theme=chartTheme('white'),  TA= c(addBBands(n=10,sd=2),addRSI(),addVo()), 
      main="Bollinger Bands")
  })
  
  #MACD with Golden Cross
  
  output$plot4<-renderPlot({
    df<-as.data.frame(na.omit(tq_get(CompanySymbol[input$Company],
                                     get = "stock.prices", from = "2020-01-01")))
    df$Date = df[,2]
    df$Open = df[,3]
    df$High = df[,4]
    df$Low = df[,5]
    df$Close = df[,6]
    df$Volume = df[,7]
    df$Adj = df[,8]
    df <- df[,c(9,10,11,12,13,14)]
    
    MACD <- xts(OHLCV(df), df[["Date"]])
    chartSeries(
      MACD, subset='2021-01::2022-11',
      theme=chartTheme('white'),
      TA= c(addSMA(n=200,on=1,col = "red"),addSMA(n=50,on=1,col = "blue"),
            addMACD(fast=12,slow=26,signal=9,type="EMA"), addVo()), 
      main="MACD (Golden Cross)")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

