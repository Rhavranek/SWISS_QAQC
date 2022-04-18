# A function to append time in MDT. Uses the lubridate package
BoulderTime <- function(df){
  df %>% 
    mutate(UTC = hms(TIME), #by using the lubridate hms, it makes the UTC time filterable with dplyr
           UTC_full = with_tz(ymd_hms(paste(DATE, TIME)), tzone = "UTC"), 
           MDT = with_tz(UTC_full, tzone = "America/Denver"),
           seconds = row_number()) 
  
}

#given a time period, this function averages the relevant parameters
flask_out_ave <- function (df) {
  flask.out.ave <- df %>% summarise(
    start_time = MDT[1],
    SDH2O_out = round(sd(H2O), 2),
    H2O_out = round(mean(H2O),2),
    SD_18O_out = round(sd (Delta_18_16),2),
    d18O_out = round(mean (Delta_18_16),2),
    SD_2H_out = round(sd(Delta_D_H),2),
    d2H_out = round(mean(Delta_D_H),2)
  )
  return(flask.out.ave)
}


#This function breaks up data when there is a clear break between flasks on a plot of OutletValve
outlet_batches <- function (df) {
  outlet.batches <- df %>% 
    mutate(
      seconds = row_number(),
      tentimes = OutletValve-100 > as.numeric(c("", head(OutletValve, -1))),
      tentimes = ifelse (is.na(tentimes), F, tentimes),
      batch = cumsum(tentimes),
      newbatch = as.character(batch)
    )
  return(outlet.batches)
}


# This function removes the first 100 seconds of a flask measurement, and then averages data across the next 3 minutes 
flask_ave_outlet <- function (df) {
  flask_aves <- df %>% 
    group_by(batch) %>% 
      mutate(
        row = row_number(),
        totalrows= n()) %>% 
          filter(totalrows>100 & row > 90 & row < 271) %>% 
            flask_out_ave()
  
  return(flask_aves)
}

#Very similar to outlet batches, but groups data by water concentration
h2o_batches <- function (df) {
  H2O.batches <- df %>% 
  mutate(
    seconds = row_number(),
    waterdiff = H2O-100 > as.numeric(c("", head(H2O, -1))),
    waterdiff = ifelse (is.na(waterdiff), F, waterdiff),
    H2O_Batch = cumsum(waterdiff),
    H2O_Newbatch = as.character(H2O_Batch)
  )
  
  return(H2O.batches)
}

#Rather than taking the first clean 3 minutes of a measurement, this function looks at the last three minutes
#this function is really helpful for the flask filling process 
flask_last3mintues <- function(df) {
  flask.aves <- df %>% 
    group_by(H2O_Batch) %>% 
      mutate( 
        row = row_number(),
        totalrows= n()
      ) %>% 
        filter (totalrows > 100) %>% 
          filter(row < (totalrows - 60)) %>% 
            filter(row > (totalrows - 300)) %>% 
              flask_out_ave()
    
  return(flask.aves)  
}

#for a flask where its broken up by water concentration, but you want the first three minutes 
flask_first3minutes <- function (df) {
  flask.aves <- df %>% 
    group_by(H2O_Batch) %>% 
      mutate( 
        row = row_number(),
        totalrows= n()
      ) %>% 
    filter(totalrows>100 & row > 270 & row < 485) %>% 
          flask_out_ave()
  
  return(flask.aves)
}




