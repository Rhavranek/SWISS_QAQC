outlet_slope <- function(data,window){
  outlet_Slope_Window <- numeric()
  for (i in 1:nrow(data)){    
    if(i<=window/2){                                                                   
      outlet_Slope_Window[i] = 
        as.numeric(coef(lm(data$OutletValve[1:(i+window/2)]~
                             data$seconds[1:(i+window/2)]))[2])
    }else{
      if(i >= (nrow(data)-window/2)){
        outlet_Slope_Window[i] = 
          as.numeric(coef(lm(data$OutletValve[(i-window/2):nrow(data)]~
                               data$seconds[(i-window/2):nrow(data)]))[2])
      }else{
        outlet_Slope_Window[i] = 
          as.numeric(coef(lm(data$OutletValve[(i-window/2):(i+window/2)]~
                               data$seconds[(i-window/2):(i+window/2)]))[2])       
      }
    }
  }
  return(outlet_slope)
} 