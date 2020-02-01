shinyServer(function(input, output, session) {
  
  source("srv-demo.R", local = TRUE)
  
  
  # login status and info will be managed by shinyauthr module and stores here
  
  # logout status managed by shinyauthr module and stored here
 
  
  
  
 
 
 
  
  
  # only when credentials()$user_auth is TRUE, render your desired sidebar menu
  output$sidebar <- renderMenu({
    
    
      
    
    
    
    sidebarMenu(id = "tabs",
                
                menuItem("Datos", tabName = "datos", icon = icon("fal fa-database"),
                         menuSubItem("Capital requerido", tabName = "subitem1", icon = icon("circle-o"))
                         
                        
                ),
                
                menuItem("Requerimiento de capital", tabName = "subitem1-1", icon = icon("fal fa-database"),
                         menuSubItem("Enfoque estandarizado", tabName = "ES", icon = icon("circle-o")),
                         menuSubItem("Enfoque básico", tabName = "EB", icon = icon("circle-o")),
                         menuSubItem("Enfoque estandarizado (II)", tabName = "ES2", icon = icon("circle-o"))
                         
                ),
                
                
               
                   
              
                
                
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
  c <- reactive(fc(mean(as.numeric(data1()[9,2:4])),mean(as.numeric(data1()[10,2:4]))))
  d <- reactive(bi(a(),b(),c()))
  e <- reactive( bic(d()))
  f <- reactive(ilm(as.numeric(data1()[11,2]),e()))
  h <- reactive(orc(e(),f()))
  
  
  
  output$ILDC<- renderText({a()})
  output$SC <- renderText({b()})
  output$FC <- renderText({c()})
  
  output$BI <- renderText({ 
    
    d()
    
    })
  
  output$BIC <- renderText({ 
    
   e()
    
  })
  
  output$ILM <- renderText({ 
    
    f()
    
  })
  
  output$CR <- renderText({ 
    
    h()
    
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
    
    a1()
    
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
    
   a2()
    
   
  })
  
  output$for1 <- renderUI({
    withMathJax('$$\\textrm{ILDC}=Min(\\overline{|II-GI|};0.025*\\overline{AI})+\\overline{ID}$$')
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
    withMathJax('$$\\textrm{BIC}=BI*\\alpha_i$$')
  })
  
  output$for6 <- renderUI({
    withMathJax('$$\\textrm{ILM}=Ln\\bigg{(}e^{1}-1+\\big{(}\\frac{LC}{BIC}\\big{)}^{0.8}\\bigg{)}$$')
  })
  
})