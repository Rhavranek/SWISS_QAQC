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


Flask_isotopes_plt <- function (df){
  plt <-df %>% 
    pivot_longer(cols = c(d180_out, d2H_out)) %>% 
      ggplot()+
        aes(x = Flask, y = value, size = 2) +
        facet_wrap (~name,scales = "free") +
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
    geom_errorbar(aes(ymin = d18O_out - SD_18O_out, ymax = d18O_out - SD_18O_out))+
    scale_x_continuous(breaks = c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)) +
    theme_figure()
  
  print(plt)
}



OutletValve_plt <- function(df){
  plt <- df %>% 
    ggplot() + 
    aes(x = MDT, y = OutletValve)+
    geom_point()+
    theme_figure()
  
  ggplotly(plt, dynamicTicks = TRUE)
}
  
WaterConcentration_plt <- function(df){
  plt <- df %>% 
    ggplot() + 
    aes(x = MDT, y= H2O)+
    geom_point()+
    theme_figure ()
  
  ggplotly(plt, dynamicTicks = TRUE)
}
  