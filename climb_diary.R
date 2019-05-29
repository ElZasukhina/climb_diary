#Climbing diary application

library(shiny)
library(dplyr)
library(ggplot2)
library(data.table)
library(googlesheets)
library(magrittr)
library(zoo)
library(plotly)
library(lubridate)

# libraries <- c('shiny', 'dplyr', 'ggplot2', 'data.table', 'googlesheets', 'magrittr', 'zoo', 'plotly')
# lapply(libraries, require, character.only = TRUE)

# connecting to the google spreadsheet - source table
gs_auth(new_user = F)

climb_diary <- gs_title('climb_diary')
data <- gs_read(climb_diary, col_names = F)
#data <- read.csv('climb_diary.csv')
colnames(data) <- c('Date', 'Grade', 'Style', 'Type', 'Comment')
data$Date %<>% as.numeric()
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
      
      helpText("Insert new record and click 'Send'.
               The main plot is scalable"),
      
      h3('New record'),
      dataTableOutput('new_record'),
      #textInput('name', 'Name'),
      dateInput('date', 'Date'),
      sliderInput('grade', 'Grade', 4,8, 5, step = 0.25),
      selectInput('style', 'Style', c('OS', 'AF', 'RP', 'TR'), multiple = F),
      radioButtons('type', 'Type', c('Indoor', 'Outdoor'), selected = 'Indoor'),
      textInput('comment', 'Comment', value = "keywords"),
      # add filtering by the keyword
      actionButton("send", "Send")
      
      # br(),
      # 
      # h3('Filter'),
      # dateRangeInput('daterange', 'Date', start = '2019-03-19', weekstart = 1, autoclose = T)
      ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotlyOutput("plot")), 
        tabPanel("Frequency",  plotlyOutput('freq')), 
        tabPanel("Records", 
                 fluidPage(
                   fluidRow(
                     column(4,
                            h3("Top-10 routes (OS)"),
                            tableOutput('top')),
                     
                     column(8,
                            h3("Last records"),
                            DT::dataTableOutput('table')))))
        
      )
    )
  )
)



# Server

#options(shiny.maxRequestSize = 9*1024^2)

server = function(input, output) {
  
  # writing into the source table  
  observeEvent(input$send, {
    gs_add_row(climb_diary, input = c(as.numeric(input$date), 
                                      as.numeric(input$grade), 
                                      as.character(input$style),
                                      as.character(input$type),
                                      as.character(input$comment)))
  })
  
  #[Date %in% as.Date(input$daterange[1]):as.Date(input$daterange[2])]
  
  # introduce last month or two to limit the graph
  last_month <- ymd(Sys.Date()) - 60
  
  pal <- c("black", "steelblue3", "red3", "grey")
  pal <- setNames(pal, c("AF", "OS", "RP", "TR"))
  
  # main graph with perfomance 
  output$plot = renderPlotly(
    plot_ly(data, x = ~Date, y = ~jitter(as.numeric(Grade)), 
            type = 'scatter', symbol = ~factor(Type),
            color = ~factor(Style),colors = pal,
            text = ~paste("Grade: ", labels(data$Grade),
                          '<br>Comment:', Comment)) %>%
      layout(yaxis = list(title = 'Grade'))
  )
  
  data[, ':=' (Date = as.character(Date),
               Grade = as.character(labels(Grade)),
               Comment = as.character(Comment),
               Type = as.character(Type))] -> data_fine
  
  # Top-10 routes climbed OS
  output$top = renderTable(
    head(data_fine[order(-Grade)][
      Style == 'OS'][,c('Date', 'Type', 'Grade')], 10)
  )
  
  
  output$table = DT::renderDataTable(
    DT::datatable(data_fine[order(-Date)][,c('Date', 'Grade', 'Style', 'Type', 'Comment')], 
                  options = list(pageLength = 10))  
  )
  
  
  output$freq = renderPlotly(
    plot_ly(data[, .(val = sum(length(Grade))), by=list(Type, month(Date))], 
            x = ~month, y = ~val, 
            type='bar', color = ~factor(Type),
            text = ~ifelse(month==month(Sys.Date()),
                           paste(val, "routes in", month.name[month], ". Keep going!"),
                           paste(val, "routes in", month.name[month])))  %>%
      layout(yaxis = list(title = '# of Routes'), xaxis = list(range=range(1,12)), barmode = 'stack')
  )
  
}


# Run the application 
shinyApp(ui = ui, server = server)

