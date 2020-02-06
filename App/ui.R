shinyUI(
  dashboardPage(
    
    # put the shinyauthr logout ui module in here
    dashboardHeader(
      title = NULL, titleWidth = 188, 
      tags$li(textOutput("bienvenida"), class = "dropdown", style = "padding: 8px;",
             
              
              dropdownMenu(type = "messages",
                           
                           messageItem(
                             from = "Alerta",
                             message = "Niveles de Riesgo Atípicos",
                             icon = icon("exclamation-triangle"),
                             time = "2018-05-12"
                           ),
                           
                           messageItem(
                             from = "Señal",
                             message = "Volatilidad Anormal",
                             icon = icon("life-ring"),
                             time = "2018-05-12"
                           )
              )
              
      )
    ),
    
    # setup a sidebar menu to be rendered server-side
    dashboardSidebar(
       sidebarMenuOutput("sidebar")
      
    ),
    
    
    dashboardBody(
      VisionHeader(),
      
    
    
   
    
      
      
      
      
      
 
                  
                  tabItems(
                    
                    
                    tabItem(tabName = "subitem1",
                            
                            
                            wellPanel(id="panel1",
                                      
                                      
                                      
                           fluidRow(
                             
                             
                             tabBox( height = "1250px", width = 12,side = "left",
                            
                            tabPanel(title = tagList(shiny::icon("gear"), strong('Enfoque estandarizado')),
                                      
                           fluidRow( 
                             
                             

                             fluidRow(column(6,box(id="paso1",width = 11,background="yellow", checkboxInput( "dataset", strong("Datos de Ejemplo"), FALSE)))
                               ,
                               column(6,box(id="paso2",width = 12,background="yellow", checkboxInput('userFile', strong('Datos Propios'), FALSE))))
                            ),
                            conditionalPanel(condition = "input.userFile == true",
                                             fluidRow(
                                               box(width = 15, title = h3(UPLOADDATA_TEXT),
                                                   box( width=15,background = "yellow",
                                                        fileInput('file_data', SELECTFILE_TEXT, accept = UPLOADFILETYPE_CONF,
                                                                  placeholder = FILESELEC_TEXT, buttonLabel = BUTTSELEC_TEXT )
                                                   ),
                                                   fluidRow(
                                                     box(width=4,background="yellow",strong(ENCABEZADO_TEXT),
                                                         checkboxInput( width="80%", 'header', WITHHEADER_TEXT, TRUE)),
                                                     box(width=4,background="yellow",
                                                         radioButtons( width="40%", 'sep', SEPARATOR_TEXT, UPLOADFILESEP_CONF, ';')),
                                                     box(width=4,background="yellow",
                                                         radioButtons( width="40%", 'quote', COMILLAS_TEXT, UPLOADCOMILLAS_CONF, ''))
                                                   )
                                               )
                                             )),
                            
                           conditionalPanel(condition = "input.userFile == true|| input.dataset == true",
                           fluidRow(
                              box( style = "overflow-x:scroll",width=12,status = "warning",dataTableOutput('datatable'))
                            ))
                           ),
                           
                           tabPanel( title = tagList(shiny::icon("gear"), strong('Enfoque básico')),
                                     
                                     fluidRow(column(6,box(id="paso4",background="yellow",width = 200, checkboxInput("dataset2", strong("Datos de Ejemplo"), FALSE))),
                                              column(6,box(id="paso5",background="yellow", width = 200,checkboxInput('userFile2', strong('Datos Propios'), FALSE)))),
                                     
                                     conditionalPanel(condition = "input.userFile2 == true",
                                                      fluidRow(
                                                        box(width = 15, title = h3("Cargar el Archivo con los Datos"),
                                                            box( width=15,background = "yellow",
                                                                 fileInput('file_data2', 'Seleccione el archivo', accept = c('text/csv',
                                                                                                                                'text/comma-separated-values',
                                                                                                                                'text/tab-separated-values',
                                                                                                                                'text/plain',
                                                                                                                                '.csv',
                                                                                                                                '.tsv',
                                                                                                                                '.rda'),
                                                                           placeholder = 'Aún no Seleccionas el Archivo...', buttonLabel = 'Buscar' )
                                                            ),
                                                            fluidRow(
                                                              box(width=4,background="yellow",strong("Encabezado de los Datos"),
                                                                  checkboxInput( width="80%", 'header2', "Con Encabezado", TRUE)),
                                                              box(width=4,background="yellow",
                                                                  radioButtons( width="40%", 'sep2', "Separador", c('Coma'=',',
                                                                                                                       'Punto y coma'=';',
                                                                                                                       'Tab'='\t'), ';')),
                                                              box(width=4,background="yellow",
                                                                  radioButtons( width="40%", 'quote2', "Comillas", c('Ninguna'='',
                                                                                                                        'Comilla doble'='"',
                                                                                                                        'Comilla simple'="'"), ''))
                                                              
                                                            ))
                                                      )),
                                     
                                     conditionalPanel(condition = "input.userFile2 == true|| input.dataset2 == true",
                                                      fluidRow(
                                       box(width=12,style = "overflow-x:scroll",status = "warning",dataTableOutput('datatable2'))
                                     ))
                                     
                                     
                                     ),
                           tabPanel(  tabName = "ES2",title = tagList(shiny::icon("gear"), strong('Enfoque estandarizado (II)')),
                                     
                                     fluidRow(
                                       fluidRow(column(6,box(id="paso6",width = 11,background="yellow", checkboxInput("dataset3", strong("Datos de Ejemplo"), FALSE))),
                                                column(6,box(id="paso7",width = 12,background="yellow", checkboxInput('userFile3', strong('Datos Propios'), FALSE))))
                                     ),
                                     conditionalPanel(condition = "input.userFile3 == true",
                                                      fluidRow(
                                                        box(width = 15, title = h3(UPLOADDATA_TEXT),
                                                            box( width=15,background = "yellow",
                                                                 fileInput('file_data3', SELECTFILE_TEXT, accept = UPLOADFILETYPE_CONF,
                                                                           placeholder = FILESELEC_TEXT, buttonLabel = BUTTSELEC_TEXT )
                                                            ),
                                                            fluidRow(
                                                              box(width=4,background="yellow",strong(ENCABEZADO_TEXT),
                                                                  checkboxInput( width="80%", 'header3', WITHHEADER_TEXT, TRUE)),
                                                              box(width=4,background="yellow",
                                                                  radioButtons( width="40%", 'sep3', SEPARATOR_TEXT, UPLOADFILESEP_CONF, ';')),
                                                              box(width=4,background="yellow",
                                                                  radioButtons( width="40%", 'quote3', COMILLAS_TEXT, UPLOADCOMILLAS_CONF, ''))
                                                            )
                                                        )
                                                      )),
                                     conditionalPanel(condition = "input.userFile3 == true|| input.dataset3 == true",
                                                      fluidRow(
                                                        box( style = "overflow-x:scroll",width=12,status = "warning",dataTableOutput('datatable3'))
                                                      ))
                                     
                                     )
                             
                    ))))
                    ,
                    
                    
                    
                    
                    tabItem( tabName = "ES",wellPanel(id="panel8",
                             fluidRow(
                               tabBox( height = "1250px", width = 12,side = "left",
                                       
                                       
                                       
                                       tabPanel( title = tagList(shiny::icon("gear"), strong('Ecuaciones y formulas')),
                                                 
                                                   withMathJax(),
                                                   box(width=12,  status = "warning",solidHeader = T,title = "El componente de intereses, arrendamientos y dividendos (ILDC)", uiOutput("for1")),
                                                   box(width=12,  status = "warning",solidHeader = T,title = "El componente de servicios (SC)", uiOutput("for2")),
                                                   
                                                   box(width=12,status = "warning",solidHeader = T,title = "El componente de finanzas (FC)", uiOutput("for3")),
                                                   box(width=12,status = "warning",solidHeader = T,title = "El indicador de negocios (BI)", uiOutput("for4")),
                                                   box(width=12,status = "warning",solidHeader = T,title = "El indicador de negocios (BIC)", uiOutput("for5")),
                                                   box(width=12,status = "warning",solidHeader = T,title = "El multiplo de pérdida interna (ILM)", uiOutput("for6")),
                                                   box(width=12,status = "warning",solidHeader = T,title = "Capital requerido (CR)", uiOutput("for7"))
                                                 
                                                   
                                                 ),
                                       
                                       
                                       tabPanel( title = tagList(shiny::icon("gear"), strong('Métricas y capital requerido')),
                                                
                                                 
                                                 
                                                 fluidRow(
                                                   
                                                   box(width = 12 ,
                                                       fluidRow(column(4,box(width=12,status = "warning",solidHeader = T,title = h3("ILDC"),h3(textOutput("ILDC")))),column(4,box(width=12,solidHeader = T,status = "warning",title = h3("SC"),h3(textOutput("SC")))),column(4, box(width=12,solidHeader = T,status = "warning",title = h3("FC"),h3(textOutput("FC"))))),
                                                       fluidRow(column(4,box(width=12,solidHeader = T,status = "warning",title = h3("BI"),h3(textOutput("BI")))),column(4,box(width=12,solidHeader = T,status = "warning",title = h3("BIC"),h3(textOutput("BIC")))),column(4, box(width=12,solidHeader = T,status = "warning",title = h3("ILM"),h3(textOutput("ILM"))))),
                                                       fluidRow(column(4,box(width=12,solidHeader = T,status = "warning",title = h3("Capital Requerido"),h3(textOutput("CR")))))
                                                       
                                                       
                                                   
                                                 )
                                                 
                                                 
                                                
                                                 
                                                 
                                                 
                                                 
                                                 
                                                 
                                       ))
                                       
                                       
                                       
                                       
                                       
                                       
                                      
                               )
                             )
                    )),
                    
                    
                    tabItem(tabName = "subitem2",
                            
                            
                            wellPanel(
                                      
                                      
                                      
                                      fluidRow(
                                        
                                        
                                        tabBox( height = "1250px", width = 12,side = "left",
                                                
                                                tabPanel(title = tagList(shiny::icon("gear"), strong('Incidencias')),
                                                         
                                                         fluidRow(column(6,box(background="yellow",width = 200, checkboxInput("dataset4", strong("Datos de Ejemplo"), FALSE))),
                                                                  column(6,box(background="yellow", width = 200,checkboxInput('userFile4', strong('Datos Propios'), FALSE)))),
                                                         
                                                         conditionalPanel(condition = "input.userFile4 == true",
                                                                          fluidRow(
                                                                            box(width = 15, title = h3("Cargar el Archivo con los Datos"),
                                                                                box( width=15,background = "yellow",
                                                                                     fileInput('file_data4', 'Seleccione el archivo', accept = c('text/csv',
                                                                                                                                                 'text/comma-separated-values',
                                                                                                                                                 'text/tab-separated-values',
                                                                                                                                                 'text/plain',
                                                                                                                                                 '.csv',
                                                                                                                                                 '.tsv',
                                                                                                                                                 '.rda'),
                                                                                               placeholder = 'Aún no Seleccionas el Archivo...', buttonLabel = 'Buscar' )
                                                                                ),
                                                                                fluidRow(
                                                                                  box(width=4,background="yellow",strong("Encabezado de los Datos"),
                                                                                      checkboxInput( width="80%", 'header4', "Con Encabezado", TRUE)),
                                                                                  box(width=4,background="yellow",
                                                                                      radioButtons( width="40%", 'sep4', "Separador", c('Coma'=',',
                                                                                                                                        'Punto y coma'=';',
                                                                                                                                        'Tab'='\t'), ';')),
                                                                                  box(width=4,background="yellow",
                                                                                      radioButtons( width="40%", 'quote4', "Comillas", c('Ninguna'='',
                                                                                                                                         'Comilla doble'='"',
                                                                                                                                         'Comilla simple'="'"), ''))
                                                                                  
                                                                                ))
                                                                          )),
                                                         
                                                         conditionalPanel(condition = "input.userFile4 == true|| input.dataset4 == true",
                                                                          fluidRow(
                                                                            box(width=12,style = "overflow-x:scroll",status = "warning",dataTableOutput('datatable4'))
                                                                          ))
                                                         
                                                         
                                                         
                                                        
                                                         
                                                         
                                                ))))),
                    
                    
                    tabItem(tabName = "EB",wellPanel(id="panel9",
                            
                            fluidRow(
                              tabBox( height = "1250px", width = 12,side = "left",
                                      
                                      
                                      tabPanel( title = tagList(shiny::icon("gear"), strong('Ecuaciones y formulas')),
                                                
                                                withMathJax(),
                                                box(width=12,  status = "warning",solidHeader = T,title = "Capial requerido", uiOutput("for21"))
                                                 ),
                                      
                                      
                                      tabPanel( title = tagList(shiny::icon("gear"), strong('Métricas y capital requerido')),
                                                
                                                
                                                fluidRow(
                                                 
                                                  box(width = 12 ,
                                                      fluidRow(column(8,box(width=12,solidHeader = T,status = "warning",title = h3("Capital Requerido"),h3(textOutput("CR1")))))
                                                      
                                                  )
                                                  
                                                  ))
                                      
                                      
                                      
                              )))),
                    
                    tabItem(tabName = "ES2",wellPanel(id="panel10",
                            
                            fluidRow(
                              tabBox( height = "1250px", width = 12,side = "left",
                                      
                                      
                                      
                                      tabPanel( title = tagList(shiny::icon("gear"), strong('Ecuaciones y formulas')),
                                                
                                                withMathJax(),
                                                box(width=12,  status = "warning",solidHeader = T,title = "Capital requerido (CR)", uiOutput("for31"))
                                               
                                      ),
                                      
                                      
                                      
                                      
                                      tabPanel( title = tagList(shiny::icon("gear"), strong('Métricas y capital requerido')),
                                                
                                                
                                                fluidRow(
                                                  
                                                  box(width = 12 ,
                                                      fluidRow(column(8,box(width=12,solidHeader = T,status = "warning",title = h3("Capital Requerido"),h3(textOutput("CR2")))))
                                                      
                                                  )
                                                  
                                                )         
                                               
                                                
                                                
                                                
                                      )
                                      
                                      
                              )))),
                    
                    tabItem(
                      tabName = "Prueba",
                      fluidRow(
                        
                        
                        box(width = 4,
                            
                            h2("Tipo de Incidencias"),shinyWidgets::pickerInput(
                              inputId = "type_filter", 
                              choices = c("Fraude interno","Fraude externo","Prácticas de empleo y seguridad en el lugar de trabajo",
                                          "Clientes, productos y prácticas comerciales","Daño a los bienes físicos",
                                          "Interrupción del negocio y fallas del sistema","Ejecución, entrega y gestión de procesos"), 
                              options = list(
                                `actions-box` = TRUE,
                                `selected-text-format`="count"
                              ), 
                              multiple = TRUE,
                              selected =  c("Fraude interno","Fraude externo","Prácticas de empleo y seguridad en el lugar de trabajo",
                                            "Clientes, productos y prácticas comerciales","Daño a los bienes físicos",
                                            "Interrupción del negocio y fallas del sistema","Ejecución, entrega y gestión de procesos")
                            )),
                        
                        
                        #Nombre de la salida
                        #tags$b(align="left",h2(textOutput("Contenido", container = span))),
                        box(width = 8, highchartOutput("hcmap"),
                            textOutput("Texto"))
                        
                        
                      ))
                   
                   
                   , tabItem(tabName = "acerca",
                            
                            wellPanel(id="panel20",
                                                         
                                       fluidRow(                    
                                                           
                                                           
                                                          
                            box(id="paso80", width = 12, status="warning",
                                 h3(ACERTITLE_TEXT),
                                 tags$hr(),
                                 h4(ACERVER_TEXT),
                                 h4(ACERRIF_TEXT),
                                 h4(ACERRS_TEXT),
                                 h4(ACERRS_TEXT2),
                                 tags$hr(),
                                 tags$img(src="img/visionrisk.png", width=300, align = "left"),
                                 br(),
                                 h5(ACERSUBSV_TEXT),
                                 br(),
                                 tagList(shiny::icon("map-marker"), ACERDIR_TEXT),br(),
                                 tagList(shiny::icon("phone"), ACERTLF_TEXT),br(),
                                 tagList(shiny::icon("envelope-o"), ACERCORR_TEXT)
                            )
                    )))
                  )
    
  )
)
)
