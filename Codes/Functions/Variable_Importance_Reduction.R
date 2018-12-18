Variable_Importance_Reduction = function(DF,Target,Remove){
  # Defining Formula
  (fmla = as.formula(paste0(Target,"~.")))

  # Reducing to Numeric Predictors
  DF = na.omit(DF)
  DF_mod = DF %>%
    dplyr::select(-Remove)
print("Test")
  TMP = DF[,Target]
  # NZV, Highly Correlated, Linear Combo Reduction
  nzv = nearZeroVar(DF_mod)
  if(!is_empty(nzv)){
    DF_mod = DF_mod[,-nzv]
  }
  DF_cor = cor(DF_mod)
  highlyCor = findCorrelation(DF_cor, cutoff = .999)
  if(!is_empty(highlyCor)){
    DF_mod = DF_mod[,-highlyCor]
  }
  combos = findLinearCombos(DF_mod)
  if(!is_empty(combos$remove)){
    DF_mod = DF_mod[,-combos$remove]
  }
  DF_mod[,Target] = TMP
  
  # # Boruta Variable Importance
  # Stage_1 = Boruta(fmla,
  #                  DF_mod,
  #                  doTrace = 1,
  #                  maxRuns = 25)
  # Imp = as.data.frame(Stage_1$finalDecision)
  # Keep_Stage_1 = rownames(Imp)[Imp$`Stage_1$finalDecision` == "Confirmed"]
  # DF_mod = DF_mod[c(Target,Keep_Stage_1)]
  
  ## MARS Variable Importance
  Stage_2 = earth(fmla,
                  data = DF_mod,
                  Scale.y = F)
  Keep_Stage_2 = rownames(evimp(Stage_2))
  

  # Final_Keep = Keep_Stage_2[Keep_Stage_2 %in% Keep_Stage_1]
  Final_Keep = Keep_Stage_2

  Output = Final_Keep
  return(Output)
}
