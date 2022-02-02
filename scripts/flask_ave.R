flask_out_ave <- function (df) {
  poop <- df %>% summarise(
    #FlaskNumber = FlaskNumber,
    SDH2O_out = round(sd(H2O), 2),
    H2O_out = round(mean(H2O),2),
    SD_18O_out = round(sd (Delta_18_16),2),
    d180_out = round(mean (Delta_18_16),2),
    SD_2H_out = round(sd(Delta_D_H),2),
    d2H_out = round(mean(Delta_D_H),2)
  )
  return(poop)
}
  