BUY_POS_FILTER = function(DF){
  DF = DF %>%
    filter(Volume_PD_Norm > -0.007752632,
           Volatility_Close < 19.86710)
  return(DF)
}