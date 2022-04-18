peaks.read <- function(filepath){
  files <-  list.files(path=filepath, recursive = T,full.names = T) 
  #lists the file names in the filepath directory
  tables <- lapply(files, read_table2,header = T, stringsAsFactors = F) 
  #reads in the files
  df <- do.call("rbind",tables) #combines all the tables into one dataframe
  df2 <- df[,c(1:2,17:19)] #subsets the dataframe to include only necessary columns
  df3 <- df2[complete.cases(df2),] #subsets the dataframe to include only 
  #complete cases, eliminating rows at the end that aren't complete
  return(df3)
} #peaks.read is a function that will read files from all sub-folders of the
#directory named in filepath and combine them into one dataframe


h2o_slope <- function(data,window){
  H2O_Slope_Window <- numeric()
  for (i in 1:nrow(data)){    
    if(i<=window/2){                                                                   
      H2O_Slope_Window[i] = 
        as.numeric(coef(lm(data$H2O[1:(i+window/2)]~
                             data$seconds[1:(i+window/2)]))[2])
    }else{
      if(i >= (nrow(data)-window/2)){
        H2O_Slope_Window[i] = 
          as.numeric(coef(lm(data$H2O[(i-window/2):nrow(data)]~
                               data$seconds[(i-window/2):nrow(data)]))[2])
      }else{
        H2O_Slope_Window[i] = 
          as.numeric(coef(lm(data$H2O[(i-window/2):(i+window/2)]~
                               data$seconds[(i-window/2):(i+window/2)]))[2])       
      }
    }
  }
  return(H2O_Slope_Window)
} 


#This funciton never worked
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