Stepwise_Log = function(DF){
  require(MASS)
  require(tidyverse)
  require(lubridate) 
  require(Boruta)
  require(caret)
  
  # ################ Sample Data #################
  # load("C://users//aayorde//desktop//window_results.rdata")
  # DF = Window_Results %>%
  # na.omit() %>%
  #   filter(Stock == "AMZN")
  # DF_Train = DF[1:round(nrow(DF)*0.8),]
  # DF_Test = DF[(round(nrow(DF)*0.8)+1):nrow(DF),]
  # Weight_Vector = abs(DF_Train$PR)
  # DF_mod = na.omit(DF_Train) %>%
  #   dplyr::select(-c(Date,Stock,Adj_Smooth,Max,Days,PR))
  # ################## End Sample Data ###########
  
  Weight_Vector = abs(DF$PR)
  DF_mod = na.omit(DF) %>%
    dplyr::select(-c(Date,Stock,Adj_Smooth,Max,Days,PR))
  
  # NZV, Highly Correlated, Linear Combo Reduction
  nzv = nearZeroVar(DF_mod)
  DF_mod = DF_mod[,-nzv]
  DF_cor = cor(DF_mod)
  highlyCor = findCorrelation(DF_cor, cutoff = .999)
  DF_mod = DF_mod[,-highlyCor]
  combos = findLinearCombos(DF_mod)
  DF_mod = DF_mod[,-combos$remove]
  
  # Running Boruta 1st to Limit Columns
  Stage_1 = Boruta(Buy~.,
                   DF_mod,
                   doTrace = 2)
  Imp = as.data.frame(Stage_1$finalDecision)
  Keep = rownames(Imp)[Imp$`Stage_1$finalDecision` == "Confirmed"]
  DF_mod = DF_mod %>%
    select("Buy",Keep)
  
  # Importance Plot
  Inf_Func = function(x){sum(is.infinite(x)) == 0}
  varImp = as.data.frame(Stage_1$ImpHistory) %>%
    select_if(Inf_Func) %>%
    summarise_all(median) %>%
    t()
  colnames(varImp) = "Importance"
  varImp = sort(varImp,
       decreasing = T)
  
  # Fit the model
  model_base = glm(Buy ~.,
                   data = DF_mod, 
                   family = binomial,
                   weights = Weight_Vector)
  
  ## Summarize the final selected model
  summary(model_base)
  ## Make predictions
  probabilities <- model_base %>% predict(DF_Test, type = "response")
  probabilities.df = model_base %>%
    predict(DF,
            type = "response")
  predicted.classes <- ifelse(probabilities > 0.5, 1, 0)
  predicted.classes.df <- ifelse(probabilities.df > 0.5, 1, 0)
  DF$Prob = probabilities.df
  DF = na.omit(DF)
  DF_Save = DF %>%
    dplyr::select(PR_1D,Adjusted,Buy,Prob,PR,Date)
  save(DF_Save,
       file = "C://users//aayorde//desktop//Step_Reg_Results.RDATA")
  DF_Test$Pred = predicted.classes
  DF_Test$Prob = probabilities
  DF_Test$Correct = DF_Test$Pred == DF_Test$Buy
  Check = DF_Test[DF_Test$Pred == 1,]
  Cumulatie_Return = cumsum(Check$PR_1D)
  plot(Cumulatie_Return)
  abline(h = 0)
  ## Model accuracy
  Acc = mean(predicted.classes==DF_Test$Buy,na.rm=T)

  DF_Test$x = seq(1,nrow(DF_Test))
  ggplot(DF_Test,aes(Date,Buy)) +
    geom_point(aes(color = Correct)) +
    geom_point(aes(y = Prob,color = Correct)) +
    labs(x = "",
         y = "Probability & Actual\n",
         title = "Amazon Test Set",
         subtitle = paste("Stepwise Logistic Regression Accuracy\n% Reuturn",
                          round(sum(DF_Test$PR_1D[DF_Test$Pred == 1]),2)),
         caption = paste("Buy Sell Accuracy :",
                         scales::percent(Acc)))
  
  return(model_step)
}