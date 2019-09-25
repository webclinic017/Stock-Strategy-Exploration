BUY_POS_FILTER = function(DF){
  DF = DF %>%
    filter(Close > 10,
           Close_SD_50_Norm < 2.5498,
           CMO < 73.4763,
           DVI < 0.9518,
           WAD > -4.669000e+01)
  return(DF)
}