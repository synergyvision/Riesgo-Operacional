
rm(list = ls())

library(shinythemes)
library(shinyjs)
library(rmarkdown)
library(dygraphs)
library(readr)
library(glue)
library(webshot)
library(rintrojs)
library(highcharter)
library(GARPFRM)
library(rugarch)
library(jsonlite)
library(neo4r)
library(dplyr)
library(shinyauthr)
library(reactlog)
library(tidyr)
library(sodium)
library(V8)
library(fastDummies)
library(readr)
library(shinyWidgets)
library(shiny)
library(shinydashboard)
library(latex2exp)
library(grid)
library(gridExtra)
library(plyr)
library(MASS)
library(caret)
library(e1071)
library(pROC)
library(plotly)
library(plyr)
library(DT)

library(highcharter)
#library(shinythemes)

library(rmarkdown)
#library(dygraphs)
#library(readr)


#library(rintrojs)
#library(highcharter)

#library(rugarch)
#library(jsonlite)

#library(shinyauthr)
#library(reactlog)
#library(tidyr)
#library(sodium)
#library(readr)
#library(shinyWidgets)

library(shinydashboard)
#library(latex2exp)
#library(grid)
#library(gridExtra)
#library(plyr)
#library(MASS)
#library(caret)
#library(e1071)



#library(DT)
#library(shiny)
#library(highcharter)


source(paste(getwd(),"script/estan.R",sep = "/"))
source(paste(getwd(),"script/basic.R",sep = "/"))
source(paste(getwd(),"script/stan2.R",sep = "/"))



# sample logins dataframe with passwords hashed by sodium package
user_base <- tibble(
  user = c("User1", "user2", "gaby","danshiny"),
  password = sapply(c("pass1", "pass2","proteino40.","c1037729"), sodium::password_store),
  permissions = c("admin", "standard","standard", "admin"),
  name = c("User One", "User Two", "Gabriela Alvarado", "Danny Morales")
)


options("scipen"=100, "digits"=4) 

# Encabezado Vision
VisionHeader <- function(){tags$head(
  tags$link(rel = "stylesheet", type = "text/css", href = "app.css"),
  tags$img(src="img/vision1.png" , id = "VisionLogo", width = 130 ),
  singleton(includeScript("www/js/d3.js")),
  singleton(includeScript("www/js/underscore.js")),
  singleton(includeScript("www/js/jquery-ui.js")),
  singleton(includeCSS("www/css/app.css"))
)}


ACERTITLE_TEXT<-"Acerca de VisionRisk™"
ACERSUBSV_TEXT<-"Tecnología para Especulación, Inversión, Economía, Finanzas y Riesgo"
ACERVER_TEXT<-"Versión: 1.0.0"
ACERRIF_TEXT<-"Rif: "
ACERRS_TEXT<-"Copyright © 2014-2019 Synergy Vision"
ACERRS_TEXT2 <- "All Rights Reserved"
ACERDIR_TEXT<-"Centro San Ignacio, La Castellana"
ACERTLF_TEXT<-"0212-2630808 / 0414-2769752"
ACERCORR_TEXT<-"contacto@synergy.vision"

####Data de Ejemplo


stand1 <- read.csv("data/standar.csv",sep = ";")
basi1 <-  read.csv("data/basi.csv",sep = ";")
stand2 <- read.csv("data/standar2.csv",sep = ";")
inci <- read_delim("data/inci.csv", ";", escape_double = FALSE, trim_ws = TRUE)



#source(paste(getwd(),"funciones.R",sep = "/"))


############################################# DATA TEXTO###############################################

UPLOADDATA_TEXT<-"Cargar el Archivo con los Datos"
SELECTFILE_TEXT<-'Seleccione el Archivo'
FILESELEC_TEXT<-'Aún no Seleccionas el Archivo...'
BUTTSELEC_TEXT<-'Buscar'
WITHHEADER_TEXT<-"Con Encabezado"
SEPARATOR_TEXT<-"Separador"
COMILLAS_TEXT<-"Comillas"
ENCABEZADO_TEXT<-"Encabezado de los Datos"


UPLOADFILETYPE_CONF<-c('text/csv',
                       'text/comma-separated-values',
                       'text/tab-separated-values',
                       'text/plain',
                       '.csv',
                       '.tsv',
                       '.rda')

UPLOADFILESEP_CONF<-c('Coma'=',',
                      'Punto y Coma'=';',
                      'Tab'='\t')

UPLOADCOMILLAS_CONF<-c('Ninguna'='',
                       'Comilla Doble'='"',
                       'Comilla Simple'="'")
