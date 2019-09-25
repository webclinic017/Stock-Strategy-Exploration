BUY_POS_FILTER = function(DF){
  DF = DF %>%
    filter(Close > 10,
           WAD > -4.669000e+01)
  return(DF)
}