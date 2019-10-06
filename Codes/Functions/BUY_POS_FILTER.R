BUY_POS_FILTER = function(DF){
  DF = DF %>%
    filter(Beta_Stock < 3.58250e+00,
           chaikin_AD > -1.43387e+08)
  return(DF)
}