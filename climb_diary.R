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
        selectInput('style', 'Style', c('OS', 'RC'), multiple = F),
        actionButton("send", "Send"),
      
        br()
      
        # h3('Filter'),
        # dateRangeInput('date', 'Date'),
        # br()
    ),
    
    mainPanel(
      plotOutput("plot"),
      tableOutput('table')
    )
  )
)


# Server
  
options(shiny.maxRequestSize = 9*1024^2)

server = function(input, output) {
  
  observeEvent(input$send, {
    gs_add_row(climb_diary, input = c(as.numeric(input$date), 
                                      as.double(input$grade), 
                                      as.character(input$style)))
  })
  
lab <- ifelse(data$Grade==4.25, '4+', 
              ifelse(data$Grade==4.5, '4+/5-',
                ifelse(data$Grade== 4.75, '5-', 
                       ifelse(data$Grade== 5.25, '5+', 
                              ifelse(data$Grade==5.5, '5+/6-',
                                     ifelse(data$Grade==5.75, '6-',
                                            ifelse(data$Grade==6.25, '6+',
                                                   ifelse(data$Grade==6.5, '6+/7-',
                                                          ifelse(data$Grade==6.75, '7-',
                                                                 ifelse(data$Grade==7.25, '7+',
                                                                 data$Grade))))))))))
                             

 output$plot = renderPlot(
    ggplot(data, aes(x = Date, y = Grade, col=as.factor(Style))) +
      geom_jitter(size = 5, width = 0.1, height=0.1, alpha=0.75) +
      scale_y_continuous(breaks = seq(4, 8, 0.5), limits = c(4, 6)) +
      geom_text(label=lab, col='cadetblue4') +
      labs(color = 'Style') +
      theme_minimal()
 )
  output$table = renderTable(
    data.table('Date' = as.character(data$Date),
               'Grade' = lab,
               'Style' = as.character(data$Style))[order(-Date)]
  )

}


# Run the application 
shinyApp(ui = ui, server = server)

