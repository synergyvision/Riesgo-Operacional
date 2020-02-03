beta <- c(0.18,0.18,0.12,0.15,0.18,0.15,0.12,0.12)

rc2 <- function(dat){
  
  dat <-dat*beta
  return(sum(dat)/3)
  
  
  
}
