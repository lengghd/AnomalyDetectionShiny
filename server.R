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
  
  #date
  getDateFromInput <- reactive({                       #reactive: dataInput(): input$dateCol: pull(data, dateCol)
    data <- dataInput()
    dateCol <- input$dateCol
    date <- pull(data, dateCol)
    return(date)
  })
  
  
  #data
  getDataFromInput <- reactive({                       #reactive
    data <- dataInput()                                #dataInput()
    dataCol <- input$dataCol                           #input$dataCol
    data <- pull(data, dataCol)                        #pull(data, dataCol)
    return(data)
  })
  #
  output$dataTable <- DT::renderDataTable({           #DT::renderDataTable({
    date <- getDateFromInput()                         # getDateFromInput()
    data <- getDataFromInput()                         # getDataFromInput()
    return(data.frame(date, data))                     #data.frame(date, data)
  })
#  renderDT(df, options = list(
#    pageLength = 5,
#    initComplete = JS('function(setting, json) { alert("done"); }')
#  )
#)
  
  #renderDT: datatable(DF) : formatStyle : X = styleInterval(#, c('col1', 'col2')
  
  #observe和reactive之间的区别。
  #一个打算在某个反应变量“触发”时运行，并具有副作用（observeEvent），
  eventReactive创建的对象yo的定义与reactive相同，
  
  。但是，它像其他reactives一样被延迟评估和缓存。



因此，如果您需要数据框，向量，图或其他东西，但又想与通常的反应链反应脱钩，请使用eventReactive。

如果您只是想立即造成副作用，请使用observeEvent。
  
  #另一个则返回反应值，并用作变量（eventReactive）。
  # #eventReactive创建一个基于eventExpr而变化的反应值，
  #ventReactive没有通常的连锁反应行为, can get from rective
  
  # observeEvent则仅基于eventExpr被触发
  #即使在这些功能的文档中，前者也没有分配给变量（因为它只是产生副作用）而已显示，而后者也被分配给了变量并在以后使用。
  #observeEvent无法创建您定义的对象（它将创建其他对象）。立即评估并且不对其进行缓存。它用于引起副作用。
  
  #observeEvent returns an observer reference class object
  observeEvent(
    dataInput(), 
    {   # change the value of a select input on the client
      updateSelectInput(session,
                        "dataCol",
                        choices = names(dataInput()))
  })
  
  observeEvent(
    dataInput(), 
    {
    updateSelectInput(session, 
                      "dateCol", 
                      choices = names(dataInput()))
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
    
    anom_data <- AnomalyDetectionVec(                #AnomalyDetectionVec
      
      x = as.vector(ts_data),                        # data as.vector
      max_anoms = as.double(input$max_anoms),        #as.double : max_anoms
      #reate a double-precision array automatically when you assign a numeric scalar or array to a variable, such as A = [1 2 3; 4 5 6]
      #https://www.mathworks.com/help/matlab/ref/double.html
      #http://pyweb.swan.ac.uk/~allton/VisualBasic/node9.html
      
      direction = input$direction,                    #input$direction
      alpha = as.double(input$alpha),                  # double : alpha       
      period = as.integer(input$freq),                # freq
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

#resets variables further down the chain, 
#nd doesn't respond to user inputs unless the value has changed. 
#his avoids duplicate or missing events.
