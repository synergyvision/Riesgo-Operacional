shinyServer(function(input, output, session) {
  
  
  
  
  # login status and info will be managed by shinyauthr module and stores here
  
  # logout status managed by shinyauthr module and stored here
 
  
  
  
 
 
 
  
  
  # only when credentials()$user_auth is TRUE, render your desired sidebar menu
  output$sidebar <- renderMenu({
    
    
      
    
    
    
    sidebarMenu(id = "tabs",
                
                menuItem("Datos", tabName = "datos", icon = icon("fal fa-database"),
                         menuSubItem("Capital requerido", tabName = "subitem1", icon = icon("circle-o")),
                         menuSubItem("Incidencias", tabName = "subitem2", icon = icon("circle-o"))
                         
                         
                        
                ),
                
                menuItem("Requerimiento de capital", tabName = "subitem1-1", icon = icon("fal fa-database"),
                         menuSubItem("Enfoque estandarizado", tabName = "ES", icon = icon("circle-o")),
                         menuSubItem("Enfoque básico", tabName = "EB", icon = icon("circle-o")),
                         menuSubItem("Enfoque de Medida Estan. ", tabName = "ES2", icon = icon("circle-o"))
                         
                )
                
                ,
                
                menuItem("Incidencias", tabName = "subitem2-1", icon = icon("fal fa-database"),
                         menuSubItem("Distribución geográfica", tabName = "Prueba", icon = icon("circle-o")),
                         menuSubItem("Distribución por incidencia", tabName = "Prueba1", icon = icon("circle-o"))
                        )
                         
                
                
                
                
                
                ,
                
                
               
                   
              
                
                
                menuItem("Acerca", icon = icon("exclamation-circle"), tabName = "acerca"),
                
                
                  actionButton("help2", "Instrucciones")
                
                )
  
    
  })
  
  
  
### Capital requerido
  
  
  datasetInput <- reactive({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    inFile <- input$file_data
    
    if (is.null(inFile))
      return(NULL)
    read.table(inFile$datapath, header = input$header,
               sep = input$sep, quote = input$quote)
    
  })
  
  
  
  ####### Datos de ejemplo de una institucion financiera alemana###
  
  datasetSelect <- reactive({
    datasetSelect <- stand1
  })
  
  
  ###### Cargando datos con que se trabajara: entre los de ejemplo y los propios
  
  data1 <- reactive({
    if(input$dataset && !input$userFile){
      data <- datasetSelect()}
    
    else if(!input$dataset && input$userFile){
      data <- datasetInput()
    }
  })
  
  
  ####Se muestran los datos
  
  
  output$datatable<-renderDataTable({
    data1()
  },options = list(scrollX=T,scrollY=300))
  
  
  
  #
  a <- reactive(ildc(mean(as.numeric(data1()[1,2:4])),mean(as.numeric(data1()[2,2:4])),mean(as.numeric(data1()[3,2:4])),mean(as.numeric(data1()[4,2:4]))))
  b <- reactive( sc(mean(as.numeric(data1()[5,2:4])),mean(as.numeric(data1()[6,2:4])),mean(as.numeric(data1()[7,2:4])),mean(as.numeric(data1()[8,2:4]))))
  c1 <- reactive(fc(mean(as.numeric(data1()[9,2:4])),mean(as.numeric(data1()[10,2:4]))))
  d <- reactive(bi(a(),b(),c1()))
  e <- reactive( bic(d()))
  f <- reactive(ilm(as.numeric(data1()[11,2]),e()))
  h <- reactive(orc(e(),f()))
  
  
  
  output$ILDC<- renderText({paste(format(a(),big.mark=".",scientific = FALSE),"Bs")    })
  output$SC <- renderText({paste(format(b(),big.mark=".",scientific = FALSE),"Bs")})
  output$FC <- renderText({paste(format(c1(),big.mark=".",scientific = FALSE),"Bs")})
  
  output$BI <- renderText({ 
    paste(format(d(),big.mark=".",scientific = FALSE),"Bs")
    
    
    })
  
  output$BIC <- renderText({ 
    paste(format(e(),big.mark=".",scientific = FALSE),"Bs")
   
    
  })
  
  output$ILM <- renderText({ 
    
    f()
    
  })
  
  output$CR <- renderText({ 
    paste(format(h(),big.mark=".",scientific = FALSE),"Bs")
   
    
  })
  
  
  
  
  datasetInput2 <- reactive({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    inFile <- input$file_data2
    
    if (is.null(inFile))
      return(NULL)
    read.table(inFile$datapath, header = input$header2,
               sep = input$sep2, quote = input$quote2)
    
  })
  
  
  
  ####### Datos de ejemplo de una institucion financiera alemana###
  
  datasetSelect2 <- reactive({
    datasetSelect <- basi1
  })
  
  
  ###### Cargando datos con que se trabajara: entre los de ejemplo y los propios
  
  data2 <- reactive({
    if(input$dataset2 && !input$userFile2){
      data <- datasetSelect2()}
    
    else if(!input$dataset2 && input$userFile2){
      data <- datasetInput2()
    }
  })
  
  
  ####Se muestran los datos
  
  
  output$datatable2<-renderDataTable({
    data2()
  },options = list(scrollX=T,scrollY=300))
  
  
  
  
  a1 <- reactive(bas(data2()[1,2:4],data2()[2,2]))
  
  
  output$CR1 <- renderText({ 
    paste(format(a1(),big.mark=".",scientific = FALSE),"Bs")
      
    
  })
  
  
  datasetInput3 <- reactive({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    inFile <- input$file_data3
    
    if (is.null(inFile))
      return(NULL)
    read.table(inFile$datapath, header = input$header3,
               sep = input$sep3, quote = input$quote3)
    
  })
  
  
  
  ####### Datos de ejemplo de una institucion financiera alemana###
  
  datasetSelect3 <- reactive({
    datasetSelect3 <- stand2
  })
  
  
  ###### Cargando datos con que se trabajara: entre los de ejemplo y los propios
  
  data3 <- reactive({
    if(input$dataset3 && !input$userFile3){
      data <- datasetSelect3()}
    
    else if(!input$dataset3 && input$userFile3){
      data <- datasetInput3()
    }
  })
  
  
  ####Se muestran los datos
  
  
  output$datatable3<-renderDataTable({
    data3()
  },options = list(scrollX=T,scrollY=300))
  
  
  

  
  a2 <- reactive(rc2(cbind(data3()[2],data3()[3],data3()[4])))
  
  output$CR2 <- renderText({ 
    paste(format(a2(),big.mark=".",scientific = FALSE),"Bs") 
 
    
   
  })
  
  output$for1 <- renderUI({
    withMathJax('$$\\textrm{ILDC}=Min(\\overline{|II-GI|};0.025\\times\\overline{AI})+\\overline{ID}$$')
  })
  
  output$for2 <- renderUI({
    withMathJax('$$\\textrm{SC}=Max(\\overline{IO};\\overline{GP})+Max(\\overline{IH};\\overline{GH})$$')
  })
  
  output$for3 <- renderUI({
    withMathJax('$$\\textrm{FC}=\\overline{|GPN|}+\\overline{|GPM|}$$')
  })
  
  output$for4 <- renderUI({
    withMathJax('$$\\textrm{BI}=ILDC+SC+FC$$')
  })
  
  output$for5 <- renderUI({
    withMathJax('$$\\textrm{BIC}=BI\\times\\alpha_i$$')
  })
  
  output$for6 <- renderUI({
    withMathJax('$$\\textrm{ILM}=Ln\\bigg{(}e^{1}-1+\\big{(}\\frac{LC}{BIC}\\big{)}^{0.8}\\bigg{)}$$')
  })
  
  output$for7 <- renderUI({
    withMathJax('$$\\textrm{CR}=BIC\\times ILM$$')
  })
  
  output$for21 <- renderUI({
    withMathJax('$$\\textrm{CR}=\\sum_{i=1}^3\\frac{IB_i\\times \\alpha}{3}$$')
  })
  
  output$for31 <- renderUI({
    withMathJax('$$\\textrm{CR}=\\sum_{years=1-3}Max\\bigg{[}\\sum_{i=1}^8LF_i\\times \\alpha_i;0\\bigg{]}\\bigg{/} 3$$')
  })
  
  
  
  datasetInput4 <- reactive({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    inFile <- input$file_data4
    
    if (is.null(inFile))
      return(NULL)
    read.table(inFile$datapath, header = input$header4,
               sep = input$sep4, quote = input$quote4)
    
  })
  
  
  
  ####### Datos de ejemplo de una institucion financiera alemana###
  
  datasetSelect4 <- reactive({
    datasetSelect4 <- inci
  })
  
  
  ###### Cargando datos con que se trabajara: entre los de ejemplo y los propios
  
  data4 <- reactive({
    if(input$dataset4 && !input$userFile4){
      data <- datasetSelect4()}
    
    else if(!input$dataset4 && input$userFile4){
      data <- datasetInput4()
    }
  })
  
  
  ####Se muestran los datos
  
  
  output$datatable4<-renderDataTable({
    data4()
  },options = list(scrollX=T,scrollY=300))
  
  
 
  grafi <- reactive({
    state_select = JS("function(event) {
       Shiny.onInputChange('sel_state', { abb: event.target['hc-a2'], name: event.target.name, nonce: Math.random() });
  }")
    
    state_unselect = JS("function(event) {
                        // Queue is defined in www/sender-queue.js
                        queue.send('unsel_state', { abb: event.target['hc-a2'], nonce: Math.random()})
  }")
    
    geojson <- download_map_data("countries/ve/ve-all")
    
    data <- get_data_from_map(geojson) 
    value = resul(contador(sup(data4(),input$type_filter)))
    
    
    data <- mutate(data, value )
    

    mapdata <- geojson
    
    return(list(highchart(type = "map") %>%
                  hc_exporting(
                    enabled = TRUE,
                    buttons = tychobratools::hc_btn_options()
                  ) %>%
                  hc_add_series(
                    mapData = mapdata, 
                    data = list_parse(data), 
                    joinBy = c("hc-a2"),
                    allAreas = FALSE,
                    dataLabels = list(enabled = TRUE, format = '{point.value:,.0f}'),
                    name = "Distribución por estado",
                    tooltip = list(
                      valueDecimals = 0, 
                      valuePrefix = "Num "
                    )
                  ) %>% 
                  hc_plotOptions(
                    series = list(
                      allowPointSelect = TRUE,
                      states = list(
                        select = list(
                          color = "#32cd32"
                        )
                      ),
                      point = list(
                        events = list(
                          unselect = state_unselect,
                          select = state_select
                        )
                      )
                    )        
                  ) %>%
                  hc_colorAxis(auxpar = NULL) %>%
                  hc_title(text = "Distribución de incidencias"),state_unselect))
    
    
    
  })
  
  
  
  output$hcmap <- renderHighchart({
    
  grafi()[[1]]  
  
  })
  
  output$datatable41<-renderDataTable({
    
    shown1(data4(),input$type_filter)
  },options = list(scrollX=T,scrollY=300))
  
  
  
  
  
  dataprev <- reactive({
    
    dato <- data4()
    
    
    
    dato1 = filter(dato, Estado %in% input$type_filter1)
    
    
    
    
    
    result <- as.data.frame(apply(dato1[c(3,4,5,6,7,8,9)], 2, sum))
    
    colnames(result) <- c("Cantidad")
    result
    
    
    
    
    
    
    
  })
  

  data5 <- reactive({
    
    datos <- data4()
    
    result <- as.data.frame(apply(datos[c(3,4,5,6,7,8,9)], 2, sum))
    
    colnames(result) <- c("Cantidad")
    result
    
    
    
  })
  
  
  output$datatable42<-renderDataTable({
    
    
    dataprev()
   
    
  },options = list(scrollX=T,scrollY=300))
  
  
  
  
  
  output$hcmap1 <- renderPlot({
  
  
    
    
    data =  cbind(rownames(dataprev()),dataprev())
    
    colnames(data) <- c("Incidencia","Cantidad")
    
    ggplot(data, aes(area = Cantidad, fill = Cantidad, label = Incidencia)) +
      geom_treemap() +
      geom_treemap_text(fontface = "italic", colour = "white", place = "centre",
                        grow = F)
    
  
  
    
    
    
    
    
    
  })
  
 
  
  
  
  
  
  
  
  
  
})
