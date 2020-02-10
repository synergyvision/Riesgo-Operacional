library(dplyr)












contador =  function(estados) {
  
  
  nombres = estados$Estado[!duplicated(estados$Estado)]
  
  cuent = NULL
  
  for (i in 1:length(nombres)) {
    
    total = estados %>% # del data frame completo
      filter( Estado == nombres[i] ) %>%  # selecciona los registros del año 2000
      summarise( total = sum( I1, I2, I3, I4, I5, I6, I7 ) )
    
    cuent[i] = as.numeric(total[1])
    
    
    
  }
  
  
  names(cuent) = str_to_lower(chartr('áéíóúñ','aeioun',nombres))
  return(cuent)
  
}



resul <- function(datos) {
  
  geojson <- download_map_data("countries/ve/ve-all")
  
  
  orden = NULL
  
  for (i in 2:26) {
    
    
    orden[i-1]= str_to_lower(chartr('áéíóúñ','aeioun', geojson[["features"]][[i]][["properties"]][["name"]]))
    
  }
  
  
  datosF <- NULL
  
  for (i in 1:length(orden)) {
    
    datosF[i] = datos[orden[i]]
    
  }
  
  return(c(0,datosF))
  
  
}









