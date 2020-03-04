#Climbing diary application

library(shiny)
library(dplyr)
# library(ggplot2)
library(data.table)
library(DT)
# library(googlesheets)
library(googledrive)
library(googlesheets4)
library(magrittr)
library(zoo)
library(plotly)
library(lubridate)
library(readr)
# source('gs_tidyverse_functions.R')
# library(shinyWidgets)

# carabiner image
car <- 'https://png.pngtree.com/png-vector/20191028/ourlarge/pngtree-carabiner-icon-cartoon-style-png-image_1893658.jpg'

# connecting to the google spreadsheet - source table
drive_deauth()
# drive_auth(email = 'e.a.zasukhina@gmail.com')
# drive_user()
gs_climb_diary <-
  googledrive::drive_get(as_id("1AjnrXLs_JF02OpIwWbZc52mstBG7go50PkupYSHANdo"))
drive_download(
  gs_climb_diary,
  path = "gs_climb_diary.csv",
  overwrite = T
)

data <- read_csv("gs_climb_diary.csv", 
                 col_names = c('Date', 'Grade', 'Style', 'Type', 'Comment'),
                 col_types = "nnccc")

# sheets_auth(token = drive_token())

data$Date %<>% zoo::as.Date(origin = "1970-01-01")
data %<>% as.data.table()


### FUNCTIONS

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

# Scatter plot function 

functionForPlots <- function(TYPE) {
  pal <- c("black", "steelblue3", "red3", "grey")
  last_quarter <- ymd(Sys.Date()) - 90
  #mod <- lm(Grade ~ Date, data = data[Type == TYPE & Date >= last_quarter,], 
  #          na.action = na.exclude)
  
  plot_ly(data[Type == TYPE & Date >= last_quarter,], 
          x = ~Date, y = ~jitter(as.numeric(Grade)), 
          #type = 'scatter', symbol = ~factor(Type),
          color = ~factor(Style), colors = pal,
          text = ~paste("Grade: ", labels(data[Type == TYPE & Date >= last_quarter,]$Grade),
                        '<br>Comment:', Comment)) %>%
    layout(yaxis = list(title = 'Grade')) # %>% 
    #add_lines(x = ~Date, y = predict(mod), 
    #          showlegend = FALSE)
}



### APP

#Interface

ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
      @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
    "))
  ),
  
  headerPanel(h1("Liza's climbing diary",
        style = "font-family: 'Lobster', cursive;
        font-weight: 500; line-height: 1.1; 
        color: #ff4e08;")),
  
  sidebarLayout(
    sidebarPanel(
      
      helpText("Insert new record and click 'Send'.
               The main plot is scalable. WRITING IS NOT AVALIABLE ATM"),
      
      h3('New record'),
      dataTableOutput('new_record'),
      #textInput('name', 'Name'),
      dateInput('date', 'Date'),
      sliderInput('grade', 'Grade', 4,8, 5, step = 0.25),
      selectInput('style', 'Style', c('OS', 'AF', 'RP', 'TR'), multiple = F),
      radioButtons('type', 'Type', c('Indoor', 'Outdoor'), selected = 'Indoor'),
      textInput('comment', 'Comment', value = "keywords"),
      # add filtering by the keyword
      # actionButton("send", "Send")
      
      # br(),
      # 
      # h3('Filter'),
      # dateRangeInput('daterange', 'Date', start = '2019-03-19', weekstart = 1, autoclose = T)
      ), 
    
    mainPanel(
      # place icon
      img(src=car, width = 50, align = "right"),
      tabsetPanel(
        tabPanel("Indoor", 
                 fluidPage(
                   column(8,
                          plotlyOutput("plot1")),
                   column(4,
                          h3('Top-10 (OS & RP)'),
                          tableOutput('top1')))),
        tabPanel('Outdoor', 
                 fluidPage(
                    column(8,
                        plotlyOutput("plot2")),
                 column(4,
                       h3('Top-10 (OS & RP)'),
                        tableOutput('top2')))),
        tabPanel("Frequency", plotlyOutput('freq')), 
        tabPanel("Records", h3("Last records"), DT::dataTableOutput('table'))
        
      )
    )
  )
)



# Server

#options(shiny.maxRequestSize = 9*1024^2)

server = function(input, output) {

  # writing into the source table - DOESNT WORK ATM
  observeEvent(input$send, {
    sheets_append(c(as.numeric(input$date),
                  as.numeric(input$grade),
                  as.character(input$style),
                  as.character(input$type),
                  as.character(input$comment)),
                  ss = gs_climb_diary)
  })
  
  
  # observeEvent(input$send, {
  #   sendSweetAlert(
  #     session = session,
  #     title = "Success!",
  #     text = "The record sent",
  #     type = "success"
  #   )
  # })
  
  
  
  # main graph with perfomance - Indoor
  p1 <- functionForPlots('Indoor')
  
  output$plot1 = renderPlotly(
    add_markers(p1)
  )
  
  # main graph with perfomance - Outdoor
  p2 <- functionForPlots('Outdoor')
  
  output$plot2 = renderPlotly(
    add_markers(p2)
  )
  
  data[, ':=' (Date = as.character(Date),
               Route = as.numeric(Grade), #for the sorting
               Grade = as.character(labels(Grade)),
               Comment = as.character(Comment),
               Type = as.character(Type))] -> data_fine
  
  # Top-10 routes climbed OS n RP
  output$top1 = renderTable(
    head(data_fine[order(-Route)][
      Type == 'Indoor' & (Style == 'OS' | Style == 'RP') ][,  
                                                        c('Date', 'Grade')], 10)
  )
  
  output$top2 = renderTable(
    head(data_fine[order(-Route)][
      Type == 'Outdoor' & (Style == 'OS' | Style == 'RP')][,
                                                         c('Date', 'Grade')], 10)
  )
  
  # Records 
  output$table = DT::renderDataTable(
    DT::datatable(data_fine[order(-Date)][,c('Date', 'Grade', 'Style', 'Type', 'Comment')], 
                  options = list(pageLength = 10))  
  )
  
  # Frequency plot
  output$freq = renderPlotly(
    plot_ly(data[year(Date) == year(Sys.Date()), 
            .(val = sum(length(Grade))), by = list(Type, month(Date))], 
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

