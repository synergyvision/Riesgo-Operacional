ildc <- function(ii,ie,iea,di){
  
  return(min(abs(ii-ie),(0.025*iea))+di)
}
  


sc <-function(ooi,ooe,fi,fe){
  
  return(max(ooi,ooe)+max(fi,fe))
}


fc <- function(plt,plb){
  
  return(abs(plt)+abs(plb))
  
}
  
  
  
bi <-function(ildc,sc,fc){
  return(ildc+sc+fc)
  
}
  



bic <- function(bi=10000000){
  
  if (bi<1000000 || bi==1000000) {
    return(bi*0.12)
  }else if((bi<30000000||bi==30000000)&&bi>1000000){
    return(((bi-1000000)*0.15)+(1000000*0.12))
  }else{
    return(((bi-30000000)*0.18)+(29000000*0.15)+(1000000*0.12))
  }
 
}


ilm <- function(lc=12,bic=12){
  
  return(log(exp(1)-1+((lc/bic)^(0.8))))
}

orc <- function(bic=12,ilm=12){
  
  return(bic*ilm)
  
}
  











