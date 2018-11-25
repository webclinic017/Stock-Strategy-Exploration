## Optimization Function TTR Window Indicators
OPT_Window_TI = function(DF_Eval,
                         DF_Store,
                         Range = c(2,100),
                         TTR_Name = NULL,
                         Target = NULL,
                         Col_Names = NULL,
                         Volume = NULL){
  # ### Sample Data ###
  # DF_Eval = HLC(DF_Orig)
  # DF_Store = DF
  # Range = c(2,100)
  # TTR_Name = "ADX"
  # Target = "ADX"
  # Col_Names = c("DIP","DIL","DX","ADX")
  # #####################
  TMP_OPT = function(n){
    if(!is.null(Volume)){
      Result = eval(parse(text = paste0("as.data.frame(",
                                        TTR_Name,"(DF_Eval,Volume,n = n))")))
    }else{
      Result = eval(parse(text = paste0("as.data.frame(",
                                        TTR_Name,"(DF_Eval,n = n))")))
    }    
    TMP = bind_cols(DF_Store,Result) %>%
      BS_Indicator_Function(Column = "Adjusted") %>%
      na.omit()
    
    Mod = lm(paste0("Buy~",Target),
             data = TMP)
    Score = summary(Mod)$adj.r.squared
  }
  Best = data.frame(Score = 0,n = 0)
  Start = 14
  Initial = TMP_OPT(Start)
  Initial_U = TMP_OPT(Start + 1)
  Initial_L = TMP_OPT(Start - 1)
  
  if(Initial_L > Initial){
    Start = Start - 1
    while(Initial_L > Initial & Start >= 4){
      Initial = Initial_L
      Start = Start - 1
      Initial_L = TMP_OPT(Start)
      if(Initial_L < Initial){
        Start = Start + 1
      }
    }
  }else if(Initial_U > Initial){
    Start = Start + 1
    while(Initial_U > Initial & Start <= nrow(DF_Store)*0.33){
      Initial = Initial_U
      Start = Start + 1
      Initial_U = TMP_OPT(Start)
      if(Initial_U < Initial){
        Start = Start - 1
      }
    }
  }
  

  if(!is.null(Volume)){
    NEW_DF = eval(parse(text = paste0("as.data.frame(",
                                      TTR_Name,"(DF_Eval,Volume,n = Start))")))
  }else{
    NEW_DF = eval(parse(text = paste0("as.data.frame(",
                                      TTR_Name,"(DF_Eval,n = Start))")))
  }
  colnames(NEW_DF) = Col_Names
  NEW_DF[,paste0(TTR_Name,"_Window")] = Start
  return(NEW_DF)
}