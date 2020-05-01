Modeling_Function = function(ID_DF,Timeframes,Risk_Free_Rate = 0.02){
  
  Sub_Model_Function = function(ID_DF,Timeframe){
    ## Defining Target Variable
    DF = ID_DF %>%
      group_by(Stock) %>%
      mutate(Adjusted_Lead = (lead(Close,Timeframe) - Close)/Close,
             Adjusted_Lead = Adjusted_Lead - (exp(log(1 + Risk_Free_Rate)/(1/(Timeframe/365))) - 1),
             Adjusted_Lead = ((1+Adjusted_Lead)^(365/Timeframe) - 1) / Volatility_Klass) %>%
      filter(Adjusted_Lead <= 5,
             Adjusted_Lead >= -5) %>%
      ungroup() %>%
      na.omit() %>%
      filter(!str_detect(Stock,"^\\^"))
    Target = "Adjusted_Lead"
    
    # Defining Formula
    TMP = as.numeric(as_vector(DF[,Target]))
    Factors = DF %>%
      select_if(is.factor)
    
    # Reducing to Numeric Predictors
    DF_mod = na.omit(DF) %>%
      select(-Stock,-Date) 
    DF_mod = DF_mod[,setdiff(colnames(DF_mod),colnames(Factors))]
    
    # Linear Combos
    combos = findLinearCombos(DF_mod)
    if(!is_empty(combos$remove)){
      DF_mod = DF_mod[,-combos$remove]
    }
    
    
    # Highly Correlated
    DF_cor = cor(DF_mod)
    highlyCor = findCorrelation(DF_cor)
    if(!is_empty(highlyCor)){
      DF_mod = DF_mod[,-highlyCor]
    }
    
    # Near Zero Variance
    nzv = nearZeroVar(DF_mod[sample(x = nrow(DF_mod),
                                    size = 10000,
                                    replace = T),])
    if(!is_empty(nzv)){
      DF_mod = DF_mod[,-nzv]
    }
    DF_mod$Adjusted_Lead = NULL
    
    # Adding Target Variable ##
    Y = TMP
    X = as.matrix(DF_mod)
    
    ## Training Elastic Net ##
    cv_output = cv.glmnet(
      x = X,
      y = Y,
      nfolds = 100,
      alpha = 1
    )
    best_lam = cv_output$lambda.1se
    mod = glmnet(x = X,
                 y = Y,
                 alpha = 1,
                 lambda = best_lam)
    # print(coef(mod))
    preds = predict(mod,
                    s = best_lam,
                    newx = X)
    Preds2 = predict(mod,
                     s = best_lam,
                     newx = as.matrix(DF[,setdiff(rownames(coef(mod)),"(Intercept)")]))
    all(preds == Preds2)
    MAE = MLmetrics::MAE(preds,Y)
    
    return(list(
      Model = mod,
      s = best_lam,
      MAE = MAE,
      Timeframe = Timeframe
    ))
  }
  
  ## Creating Models
  Models = list()
  p = progress_estimated(length(Timeframes))
  for(i in Timeframes){
    message(str_c("Fitting Timeframe of ",i))
    Models[[i]] = Sub_Model_Function(ID_DF,i)
    print(Models[[i]]$MAE)
  }

  ## Returning Values ##
  return(Models)
}