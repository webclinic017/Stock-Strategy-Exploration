BUY_POS_FILTER = function(DF){
  DF = DF %>%
    filter(Beta_Stock < 3.58250e+00,
           chaikin_AD < 1.152566e+08,
           chaikin_AD > -1.43387e+08,
           Close_Slope_50_Norm < 0.0045,
           SMI_Sig_Delta > -7.6514,
           VHF_Delta > -0.1037,
           Volume_PD_Norm > -0.6635,
           WAD > -69.4433)
  return(DF)
}