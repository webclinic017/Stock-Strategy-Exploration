## Optimization Function TTR Window Indicators
OPT_Window_TI = function(DF_Eval,
                         DF_Store,
                         Range = c(2,100),
                         TTR_Name = NULL,
                         Target = NULL,
                         Col_Names = NULL){
  # ### Sample Data ###
  # DF_Eval = HLC(DF_Orig)
  # DF_Store = DF
  # Range = c(2,100)
  # TTR_Name = "ADX"
  # Target = "ADX"
  # Col_Names = c("DIP","DIL","DX","ADX")
  # #####################
  TMP_OPT = function(n){
    Result = eval(parse(text = paste0("as.data.frame(",
                                      TTR_Name,"(DF_Eval,n = n))")))
    
    TMP = bind_cols(DF_Store,Result) %>%
      BS_Indicator_Function(Column = "Adjusted") %>%
      na.omit()
    
    Mod = lm(paste0("Buy~",Target),
             data = TMP)
    Score = summary(Mod)$adj.r.squared
  }
  Best = data.frame(Score = 0,n = 0)
  for(i in Range[1]:Range[2]){
    Results = TMP_OPT(i)
    if(Results > Best$Score){
      Best$Score = Results
      Best$n = i
    }
  }
  NEW_DF = eval(parse(text = paste0("as.data.frame(",
                                    TTR_Name,"(DF_Eval,n = Best$n))")))
  colnames(NEW_DF) = Col_Names
  return(NEW_DF)
}