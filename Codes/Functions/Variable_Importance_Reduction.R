Variable_Importance_Reduction = function(DF,
                                         Target = NULL,
                                         Remove = NULL){
  # Defining Formula
  (fmla = as.formula(paste0(Target,"~.")))

  # Reducing to Numeric Predictors
  DF = na.omit(DF)
  DF_mod = DF %>%
    dplyr::select(-Remove)
  
  TMP = DF[,Target]
  # NZV, Highly Correlated, Linear Combo Reduction
  nzv = nearZeroVar(DF_mod)
  if(!is_empty(nzv)){
    DF_mod = DF_mod[,-nzv]
  }
  DF_cor = cor(DF_mod)
  highlyCor = findCorrelation(DF_cor, cutoff = .95)
  if(!is_empty(highlyCor)){
    DF_mod = DF_mod[,-highlyCor]
  }
  combos = findLinearCombos(DF_mod)
  if(!is_empty(combos$remove)){
    DF_mod = DF_mod[,-combos$remove]
  }
  DF_mod[,Target] = TMP

  ## GLM Variable Importance
  while_stop = F
  counter = 0
  while(!while_stop){
    counter = counter + 1
    mod_1 = glm(fmla,
                DF_mod,
                family = "binomial")
    Imp = as.data.frame(coefficients(summary(mod_1))) %>%
      mutate(Var = rownames(.)) %>%
      filter(Var != "(Intercept)")
    Max = max(Imp$`Pr(>|z|)`)
    Rm = Imp$Var[Imp$`Pr(>|z|)` == Max]
    if(Max >= 0.05){
      DF_mod = DF_mod %>%
        select(-Rm)
    }else{
      while_stop = T
    }
  }
  Final_Keep = Imp$Var
  
  Output = Final_Keep
  return(Output)
}
