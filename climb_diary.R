#Climbing diary application

library(shiny)
library(rhandsontable)
library(dplyr)
library(ggplot2)
library(data.table)
library(googlesheets)
library(magrittr)
library(zoo)

gs_auth(new_user = F)

climb_diary <- gs_title('climb_diary')
data <- gs_read(climb_diary, col_names = F)
colnames(data) <- c('Date', 'Grade', 'Style')
data$Date %<>% zoo::as.Date(origin = "1970-01-01")
#data$Grade %<>% as.numeric()
data %<>% as.data.table()

#as.Date(DP$variable, origin = "1899-12-30")

#labels function
labels <- function(x){
  ifelse(x==4.25, '4+', 
         ifelse(x==4.5, '4+/5-',
                ifelse(x== 4.75, '5-', 
                       ifelse(x== 5.25, '5+', 
                              ifelse(x==5.5, '5+/6-',
                                     ifelse(x==5.75, '6-',
                                            ifelse(x==6.25, '6+',
                                                   ifelse(x==6.5, '6+/7-',
                                                          ifelse(x==6.75, '7-',
                                                                 ifelse(x==7.25, '7+',
                                                                        x))))))))))
}

#Interface

ui <- fluidPage(
  
  titlePanel("Liza's climbing diary"),
  
  sidebarLayout(
    sidebarPanel(
      
      helpText("Insert new record and click 'Send'. Update the page to see new data"),
      
      h3('New record'),
      dataTableOutput('new_record'),
      #textInput('name', 'Name'),
      dateInput('date', 'Date'),
      sliderInput('grade', 'Grade', 4,8, 5, step= 0.25),
      selectInput('style', 'Style', c('OS', 'TR', 'AF'), multiple = F),
      actionButton("send", "Send"),
      
      br(),
      br(),
      
      h3('Filter'),
      dateRangeInput('date', 'Date', start = '2019-03-19', weekstart = 1, autoclose = T)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotOutput("plot")), 
        tabPanel("Frequency",  plotOutput('freq')), 
        tabPanel("Records", 
                 fluidPage(
                   fluidRow(
                     column(6,
                            h3("Top-10 routes"),
                            tableOutput('top')),
                     
                     column(6,
                            h3("Last records"),
                            tableOutput('table')))))
        
      )
    )
  )
)



# Server

#options(shiny.maxRequestSize = 9*1024^2)

server = function(input, output) {
  
  observeEvent(input$send, {
    gs_add_row(climb_diary, input = c(as.numeric(input$date), 
                                      as.double(input$grade), 
                                      as.character(input$style)))
  })
  
  
  
  output$plot = renderPlot(
    ggplot(data[Date %in% as.Date(min(input$date)):as.Date(max(input$date))], aes(x = Date, y = Grade)) + 
      geom_jitter(aes(col=as.factor(Style)), size = 6, width = 0.1, height=0.1, alpha=0.75) +
      scale_y_continuous(breaks = seq(4, 8, 0.5), limits = c(4, 6)) +
      geom_text(label=labels(data[Date %in% as.Date(min(input$date)):as.Date(max(input$date))]$Grade), 
                nudge_x = -0.4, size=5, col='grey6') +
      labs(color = 'Style') +
      scale_color_manual(values=c("tomato", "steelblue3", "slategrey")) +
      theme_minimal()
  )
  
  output$top = renderTable(
    head(data[,Route := as.character(labels(Grade))][order(-Grade)][,-2][, Date := as.character(data$Date)], 10)
  )
  
  #mb write here last 20 records? mb convert data into date format here
  output$table = renderTable(
    data.table('Date' = data$Date, #as.character(data$Date)
               'Grade' = labels(data$Grade),
               'Style' = as.character(data$Style))[
                 Date %in% as.Date(min(input$date)):as.Date(max(input$date)),
                 ][order(-Date)][,Date := as.character(Date)]
  )
  
  output$freq = renderPlot(
    ggplot(data[Date %in% as.Date(min(input$date)):as.Date(max(input$date))], aes(Date)) +
      geom_bar(fill = 'steelblue3', alpha=0.75, width = 0.7) +
      labs(y = '# of routes') +
      scale_y_continuous(breaks = seq(1, 10, 1)) +
      #geom_text(aes(y=count(Date))) +
      theme_minimal()
  )
  
}


# Run the application 
shinyApp(ui = ui, server = server)

