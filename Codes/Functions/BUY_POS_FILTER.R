BUY_POS_FILTER = function(DF){
  DF = DF %>%
    filter( 
      AROON_Osc > -91.666700000,
      CCI > -1.547982e+02,
      CLV > -0.666700000,
      DIL < 3.461490e+01,
      Slow_K > 0.196100000,
      VHF_Delta < 6.890000e-02)
  return(DF)
}