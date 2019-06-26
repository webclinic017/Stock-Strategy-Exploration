BUY_POS_FILTER = function(DF){
  DF %>%
    filter(CCI < 49.69,
           CCI > 12.55195,
           CCI_Delta > 10.2829,
           Chaikin_Volatility < 0.1042,
           Close_High_PD_Norm < -0.0888,
           Close_SD_50_Norm > 0.9744,
           CMO > 8.1379,
           DVI_MAG > 0.5374,
           SAR < 20.67573333,
           SMI_Delta > 0.5692,
           TRIX < 0.04196667,
           )
  return(DF)
}