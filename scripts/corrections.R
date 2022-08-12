#Here I use the correction from Rothfuss et al., 2013 which accounts for quasi-thermodynamic equilibrium temperature dependency 

liq_rothfuss2013 <- function(df){
  liq_rothfuss2013 <- df %>% mutate(
    T = 27,
    d18O_liq = 11.45 - 0.0795*T +1.0012*d18O_out,
    d2H_liq = 104.96 - 1.0342*T + 1.0724*d2H_out
  )
  return(liq_rothfuss2013)
}
  
march182022_corr <- function(df){
  march18 <- df %>% mutate(
    d18O_corr = round(((d18O_out - 1.68206)/1.026412),1),
    d2H_corr = round(((d2H_out - 5.669731)/1.045704),1)
  )
  
  return(march18)
}


April42022_corr <- function(df){
  April4 <- df %>% mutate(
    d18O_corr =round(((d18O_out - 1.4572)/1.0281), 1),
    d2H_corr = round(((d2H_out - 5.595052)/1.046536), 1 )
  )
  
  return(April4)
}

April4_phlox_corr <- function(df){
  April4_phlox_corr <- df %>% mutate(
    d18O_corr_phlox = (d18O_out - 4.229)/1.045868,
    d2H_corr_phlox = (d2H_out + 0.00710443)/1.050112
)
  
  return (April4_phlox_corr)
}