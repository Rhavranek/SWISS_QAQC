Flask_h2o_plt <- function (df){
  plt <- df %>% 
    ggplot()+
    aes(x = Flask, y = H2O_out, size = 2)+
    geom_point() +
    #scale_y_continuous(limits =c(0, 1500), expand = c(0,0)) +
    scale_x_continuous(breaks = c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)) +
    theme_figure()
  
  print(plt)
}

