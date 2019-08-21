BUY_POS_FILTER = function(DF){
  DF = DF %>%
    filter(B_MAVG < 9.454580e+01,
           B_Up < 9.786350e+01,
           Chaikin_Volatility > 0.0493000000,
           DI > -25.29034,
           DIP > 1.052460e+01,
           Industry_Alpha > -0.0001666667,
           RSI > 2.532460e+01,
           RSI_Delta < 4.933600e+00,
           Ultimate_Oscillator < 48.8197700000)
  return(DF)
}