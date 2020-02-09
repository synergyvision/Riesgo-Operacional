library(dplyr)

total = estados %>% # del data frame completo
  filter( Estado == 'Apure' ) %>%  # selecciona los registros del año 2000
  summarise( total = sum( I1, I2, I3, I4, I5, I6, I7 ) )

numer = as.numeric(total[1])












contador =  function(estados) {
  
  
  nombres = estados$Estado[!duplicated(estados$Estado)]
  
  cuent = NULL
  
  for (i in 1:length(nombres)) {
    
    total = estados %>% # del data frame completo
      filter( Estado == nombres[i] ) %>%  # selecciona los registros del año 2000
      summarise( total = sum( I1, I2, I3, I4, I5, I6, I7 ) )
    
    cuent[i] = as.numeric(total[1])
    
    
    
  }
  
  
  names(cuent) = nombres
  return(cuent)
  
}

contador(estados)









