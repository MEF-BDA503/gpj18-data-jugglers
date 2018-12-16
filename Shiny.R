#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

bist30<-readxl::read_excel("XU030_v2.xlsx",skip=0,col_names=TRUE)
#bist30 <- cbind(bist30, apply(bist30[seq(2,60,2)], 2, function (a) a / a[[1]] * 100))
#colnames(bist30)[62:91] <- paste(colnames(bist30[seq(2,60,2)]), "norm", sep = "_")

stockNames = gsub("_Price", "", colnames(bist30[seq(2,60,2)]))

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Data Jugglers"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "stocks", 
                    label = "Stocks:",
                    choices = stockNames,
                    multiple = TRUE),
        
        dateRangeInput('dateRange',
                       label = 'Date',
                       start  = "2011-01-03",
                       end    = "2018-11-19",
                       min    = "2011-01-03",
                       max    = "2018-11-19",
                       format = "dd/mm/yyyy",
                       separator = " - "
        )
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("stockPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$stockPlot <- renderPlot({
     
     date_range_data <- bist30
     date_range_data <- date_range_data %>% filter(
       Dates >= input$dateRange[1]
       & Dates <= input$dateRange[2]
     )
     
     date_range_data <- cbind(date_range_data, apply(date_range_data[seq(2,60,2)], 2, function (a) a / a[[1]] * 100))
     colnames(date_range_data)[62:91] <- paste(colnames(date_range_data[seq(2,60,2)]), "norm", sep = "_")

     selectedStocks = input$stocks
     
     p <- ggplot(date_range_data, aes(x=Dates))
     if(!is.null(selectedStocks)){
       
      
       for (i in 1:length(selectedStocks)) {
         #print(selectedStocks[i])
         #p <- p + geom_line(aes(y = date_range_data[[paste(selectedStocks[i], "_Price_norm", sep = "")]], colour = selectedStocks[i]))
         
         if(i == 1){
           p <- p + geom_line(aes(y = date_range_data[[paste(selectedStocks[1], "_Price_norm", sep = "")]], colour = selectedStocks[1]))
         }
         if(i == 2){
           p <- p + geom_line(aes(y = date_range_data[[paste(selectedStocks[2], "_Price_norm", sep = "")]], colour = selectedStocks[2]))
         }
         if(i == 3){
           p <- p + geom_line(aes(y = date_range_data[[paste(selectedStocks[3], "_Price_norm", sep = "")]], colour = selectedStocks[3]))
         }         
         if(i == 4){
           p <- p + geom_line(aes(y = date_range_data[[paste(selectedStocks[4], "_Price_norm", sep = "")]], colour = selectedStocks[4]))
         }
         if(i == 5){
           p <- p + geom_line(aes(y = date_range_data[[paste(selectedStocks[5], "_Price_norm", sep = "")]], colour = selectedStocks[5]))
         }
         if(i == 6){
           p <- p + geom_line(aes(y = date_range_data[[paste(selectedStocks[6], "_Price_norm", sep = "")]], colour = selectedStocks[6]))
         }        
         
       }
     }

     #p <- p + geom_line(aes(y = date_range_data[["SAHOL_Price_norm"]], colour = "SAHOL"))
     #p <- p + geom_line(aes(y = BIMAS_Price_norm, colour = "BIMAS"))
     p <- p + labs(y = "Normalized Stock Price",
                   x = "Date",
                   color = "Stocks")
     p <- p + theme(legend.position = c(0.95, 0.9))
     p
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

