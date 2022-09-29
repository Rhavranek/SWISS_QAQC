#Here I use the correction from Rothfuss et al., 2013 which accounts for quasi-thermodynamic equilibrium temperature dependency 

liq_rothfuss2013 <- function(df){
  liq_rothfuss2013 <- df %>% mutate(
    T = 27,
    d18O_liq = round((11.45 - 0.0795*T +1.0012*d18O_out),1),
    d2H_liq = round((104.96 - 1.0342*T + 1.0724*d2H_out),1)
  )
  return(liq_rothfuss2013)
}
  

SWISS_offset <- function(df){
  offset <- df %>% mutate(
   d18O_offset = d18O_corr - 1.0,
   dD_offset = d2H_corr - 2.6
  )
  return(offset)
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

August10_12_corr <- function(df) {
  August10_12 <- df %>% mutate(
    d18O_corr = round(((d18O_liq - -8.994) / 1.000),1),
    d2H_corr = round(((d2H_liq - -80.7073) / 0.92),)
  )
  return (August10_12)
}



August17_phlox_corr <- function(df){
  August17 <- df %>% mutate(
    d18O_corr = (d18O_out - 0.7917232) / 1.02183,
    d2H_corr = (d2H_out - 2.226) / 1.036
  )
  return (August17)
}


August18_phlox_corr <- function(df){
  August17 <- df %>% mutate(
    d18O_corr = (d18O_out - 0.7713) / 1.0321,
    d2H_corr = (d2H_out - 0.3376) / 1.0344
  )
  return (August17)
}
