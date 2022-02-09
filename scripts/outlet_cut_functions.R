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


plt_outlet_batch <- function(df) {
  plt <- df %>% 
    ggplot()+
      aes(x =MDT, y = OutletValve, color = newbatch) +
      geom_point() +
      theme_figure()
  
  return(plt)
}

flask_ave_outlet <- function (df) {
  flask_aves <- df %>% 
    group_by(batch) %>% 
      mutate(row = row_number(),
           totalrows= n()) %>% 
        filter(totalrows>100 & row > 90 & row < 271) %>% 
          flask_out_ave()
  
  return(flask_aves)
}
