BUY_POS_FILTER = function(DF){
  DF %>%
    filter(Volatility_Zhang < 0.883,
           Chaikin_Volatility < 0.2009,
           Volatility_Close < 1.03870,
           Ultimate_Oscillator > 39.616,
           Close_PD_200_Norm > -0.2154,
           Volume_SD_Norm > -0.5719,
           Price_Range_15_SD_Norm < 3.1024,
           Pseudo_Alpha_PD < 4.8991,
           CCI > -177.6927,
           CLV > -0.58330,
           RSI_Delta < 4.83465,
           SMI > -49.1631,
           SMI_Signal > -53.9173,
           SMI_Delta < 2.88,
           SMI_Delta > -4.0882,
           Fast_K > 0.2497,
           WPR < 0.7503,
           AROON_Down > 25,
           AROON_Up > 0,
           VHF > 0.3477
           )
  return(DF)
}