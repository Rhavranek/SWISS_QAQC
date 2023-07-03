#Works with the Outlet_batch function
outlet_batch_plt <- function(df) {
  plt <- df %>% 
    ggplot()+
    aes(x =MDT, y = OutletValve, color = newbatch) +
    geom_point() +
    theme_figure()
  
  ggplotly(plt, dynamicTicks = TRUE)
}


#plots the water concentration in a flask - most useful for a dry air test
Flask_h2o_plt <- function (df){
  plt <- df %>% 
    ggplot()+
    aes(x = Flask, y = H2O_out, size = 2)+
    geom_point() +
    scale_x_continuous(breaks = c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)) +
    theme_figure()+
    theme(
      legend.position = "none"
    )
  
  print(plt)
}

# works with teh flask_aves function, plots both d18O and d2H for each flask
Flask_isotopes_plt <- function (df){
  plt <-df %>% 
    pivot_longer(cols = c(d18O_out, d2H_out)) %>% 
      ggplot()+
        aes(x = Flask, y = value, size = 2) +
        facet_wrap (~name, scales = "free", ncol = 1) +
        geom_point()+
        scale_x_continuous(breaks = c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)) +
        theme_figure()+
        theme(
          legend.position = "none"
        )
  
  print(plt)
}



Flask_d18O_plt <- function (df) {
  plt <- df %>% 
    ggplot()+
    aes(x = Flask, y = d18O_out, size = 2)+
    geom_point() +
    geom_errorbar(aes(ymin = d18O_out + SD_18O_out, ymax = d18O_out - SD_18O_out))+
    scale_x_continuous(breaks = c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)) +
    theme_figure()
  
  print(plt)
}


dual_isotope_plt <- function (df){
  plt <- df %>% 
    ggplot()+
    aes(x = d18O_out, y = d2H_out)+
    geom_point(aes(size =2, text = Flask )) +
    geom_errorbar(aes(ymin = d2H_out + SD_2H_out, ymax = d2H_out - SD_2H_out))+
    geom_errorbarh(aes(xmin = d18O_out + SD_18O_out, xmax = d18O_out - SD_18O_out)) +
    theme_figure()+
    theme(
      legend.position = "none"
    )
  
  print(plt)
  ggplotly(plt)
}

#plots outlet valve vs. time. useful for seeing if using outlet batches vs. h2o batches will work better
outletValve_plt <- function(df){
  plt <- df %>% 
    ggplot() + 
    aes(x = MDT, y = OutletValve)+
    geom_point()+
    theme_figure()
  
  ggplotly(plt, dynamicTicks = TRUE)
}
  
#plots water concentration through time 
waterConcentration_plt <- function(df){
  plt <- df %>% 
    ggplot() + 
    aes(x = MDT, y= H2O)+
    geom_point()+
    theme_figure ()
  
  ggplotly(plt, dynamicTicks = TRUE)
}

d180_time_plt <- function(df){
  plt <- df %>% 
    ggplot() + 
    aes(x = MDT, y= Delta_18_16)+
    geom_point()+
    theme_figure ()
  
  ggplotly(plt, dynamicTicks = TRUE)
}

d2H_time_plt <- function(df){
  plt <- df %>% 
    ggplot() + 
    aes(x = MDT, y= Delta_D_H)+
    geom_point()+
    theme_figure ()
  
  ggplotly(plt, dynamicTicks = TRUE)
}

  
h2o_batch_plt <- function(df){
  plt <- df %>% 
    ggplot()+
    aes(x =MDT, y = H2O, color = H2O_Newbatch) +
    geom_point() +
    theme_figure()+
    theme(
      legend.position = "none"
    )
  
  ggplotly(plt, dynamicTicks = TRUE)

}
  
h2o_LastThreeMinutes <- function (df){
  groups <- df %>% 
    group_by(H2O_Batch) %>% 
    mutate( 
      row = row_number(),
      totalrows= n()
    ) %>% 
    filter (totalrows > 100) %>% 
    filter(row < (totalrows - 60)) %>% 
    filter(row > (totalrows - 300))
  
  plt_groups <- groups %>% 
    ggplot()+
    aes(x =MDT, y = H2O, color = H2O_Newbatch) +
    geom_point() +
    theme_figure()+
    theme(legend.position = "none")
    
  ggplotly(plt_groups, dynamicTicks = TRUE)
}
  
h2o_Last3Minutes_MOREcuttoff <- function (df){
  groups <- df %>% 
    group_by(H2O_Batch) %>% 
    mutate( 
      row = row_number(),
      totalrows= n()
    ) %>% 
    filter (totalrows > 100) %>% 
    filter(row < (totalrows - 100)) %>% 
    filter(row > (totalrows - 340))
  
  plt_groups <- groups %>% 
    ggplot()+
    aes(x =MDT, y = H2O, color = H2O_Newbatch) +
    geom_point() +
    theme_figure()+
    theme(legend.position = "none")
  
  ggplotly(plt_groups, dynamicTicks = TRUE)
}


d18o_last3mintues_MOREcuttoff <- function (df){
  groups <- df %>% 
    group_by(H2O_Batch) %>% 
    mutate( 
      row = row_number(),
      totalrows= n()
    ) %>% 
    filter (totalrows > 100) %>% 
    filter(row < (totalrows - 100)) %>% 
    filter(row > (totalrows - 340))
  
  plt_groups <- groups %>% 
    ggplot()+
    aes(x =MDT, y = Delta_18_16, color = H2O_Newbatch) +
    geom_point() +
    theme_figure()+
    theme(legend.position = "none")
  
  ggplotly(plt_groups, dynamicTicks = TRUE)
}


h2O_FirstThreeMinutes <- function(df){
  groups <- df %>% 
    group_by(H2O_Batch) %>% 
    mutate( 
      row = row_number(),
      totalrows= n()
    ) %>% 
    filter(totalrows>100 & row > 270 & row < 485) 
  
  plt_groups <- groups %>% 
    ggplot()+
    aes(x =MDT, y = H2O, color = H2O_Newbatch) +
    geom_point() +
    theme_figure()+
    theme(legend.position = "none")
  
  ggplotly(plt_groups, dynamicTicks = TRUE)
  
}  


h2o_First3_LessCutoff <-  function(df){
  groups <- df %>% 
    group_by(H2O_Batch) %>% 
    mutate( 
      row = row_number(),
      totalrows= n()
    ) %>% 
    filter(totalrows>100 & row > 120 & row < 335) 
  
  plt_groups <- groups %>% 
    ggplot()+
    aes(x =MDT, y = H2O, color = H2O_Newbatch) +
    geom_point() +
    theme_figure()+
    theme(legend.position = "none")
  
  ggplotly(plt_groups, dynamicTicks = TRUE)
  
}  

d18O_three_minute_start <-  function(df){
  groups <- df %>% 
    group_by(H2O_Batch) %>% 
    mutate( 
      row = row_number(),
      totalrows= n()
    ) %>% 
    filter(totalrows>100 & row > 270 & row < 485) 
  
  plt_groups <- groups %>% 
    ggplot()+
    aes(x =MDT, y = Delta_18_16, color = H2O_Newbatch) +
    geom_point() +
    theme_figure()+
    theme(legend.position = "none")
  
  ggplotly(plt_groups, dynamicTicks = TRUE)
  
}  



