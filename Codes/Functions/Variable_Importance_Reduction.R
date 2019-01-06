Variable_Importance_Reduction = function(DF,
                                         Target = NULL,
                                         Remove = NULL,
                                         Weights = NULL){
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
  highlyCor = findCorrelation(DF_cor, cutoff = .999)
  if(!is_empty(highlyCor)){
    DF_mod = DF_mod[,-highlyCor]
  }
  combos = findLinearCombos(DF_mod)
  if(!is_empty(combos$remove)){
    DF_mod = DF_mod[,-combos$remove]
  }
  DF_mod[,Target] = TMP

  ## GLM Variable Importance
  mod_1 = glm(fmla,
              DF_mod,
              weights = Weights,
              family = "binomial")
  Imp = as.data.frame(coefficients(summary(mod_1))) %>%
    mutate(Var = rownames(.)) %>%
    filter(`Pr(>|z|)` <= 0.15)
  Keep_Stage_1 = Imp$Var
  Keep_Stage_1 = Keep_Stage_1[!Keep_Stage_1 %in% "(Intercept)"]
  DF_mod = DF_mod[c(Target,Keep_Stage_1)]
  
  ## MARS Variable Importance
  mod_2 = earth(fmla,
                data = DF_mod)
  Keep_Stage_2 = rownames(evimp(mod_2))
  DF_mod = DF_mod[c(Target,Keep_Stage_2)]

  Final_Keep = Keep_Stage_2

  Final_Keep = Keep_Stage_1
  
  Output = Final_Keep
  return(Output)
}
