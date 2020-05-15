library(AnomalyDetection)
library(DT)                               # R interface to the JavaScript library DataTables.   
#https://rstudio.github.io/DT/

library(readr)
library(shiny)

#tidyverts family package
library(tibbletime)    #retired. switched to #tsibble
#tsibble ： fill_gaps()
#as_tsibble(df, key = catg, index = time_hour)    #package?tsibble    #vignette("intro-tsibble")
#index_by() + summarise()

library(tidyverse)           #read_csv

#https://shiny.rstudio.com/reference/shiny/1.4.0/session.html
shinyServer(function(input, output, session) {
  dataInput <- reactive({
    req(input$inputFile)
    ts_data <-
      read_csv(input$inputFile$datapath, 
               #datapath: datapath
               #...The path to a temp file containing data uploaded. 
               #...file may be deleted if performs another upload operation.
               
               #fileInput(inputId, label, multiple = FALSE, accept = NULL,
               #          width = NULL, buttonLabel = "Browse...",
               #          placeholder = "No file selected")
               
               col_names = input$headers)
    return(ts_data)
  })
  
  getDateFromInput <- reactive({
    data <- dataInput()
    dateCol <- input$dateCol
    date <- pull(data, dateCol)
    return(date)
  })
  
  getDataFromInput <- reactive({
    data <- dataInput()
    dataCol <- input$dataCol
    data <- pull(data, dataCol)
    return(data)
  })
  
  output$dataTable <- DT::renderDataTable({
    date <- getDateFromInput()
    data <- getDataFromInput()
    return(data.frame(date, data))
  })
#  renderDT(df, options = list(
#    pageLength = 5,
#    initComplete = JS('function(setting, json) { alert("done"); }')
#  )
#)
  
  #renderDT: datatable(DF) : formatStyle : X = styleInterval(#, c('col1', 'col2')
  
  observeEvent(dataInput(), {
    updateSelectInput(session, "dataCol", choices = names(dataInput()))
  })
  
  observeEvent(dataInput(), {
    updateSelectInput(session, "dateCol", choices = names(dataInput()))
  })
  
  make_ts <- reactive({
    data <- getDataFromInput()
    startDate <- input$startDate
    freq <- input$freq
    ts_data <- ts(data,
                  start = startDate,
                  frequency = as.integer(freq))
    return(ts_data)
  })
  
  output$decomp <- renderPlot({
    ts_data <- make_ts()
    return(plot(stl(ts_data, "periodic")))
  })
  
  findAnomalies <- reactive({
    ts_data <- make_ts()
    anom_data <- AnomalyDetectionVec(
      x = as.vector(ts_data),
      max_anoms = as.double(input$max_anoms),
      direction = input$direction,
      alpha = as.double(input$alpha),
      period = as.integer(input$freq),
      e_value = T,
      plot = T
    )
    return(anom_data)
  })
  
  output$anomalies <- renderPlot({
    anomalies <- findAnomalies()
    return(anomalies$plot)
  })
  
  output$anomalyTable <- DT::renderDataTable({
    anomalies <- findAnomalies()
    return(anomalies$anoms)
  })
})

