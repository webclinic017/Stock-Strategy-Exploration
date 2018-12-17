Stepwise_Log = function(DF){
  require(MASS)
  require(tidyverse)
  require(lubridate) 
  require(Boruta)
  require(caret)
  require(doParallel)
  
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
  DF = na.omit(DF)
  Weight_Vector = abs(DF$PR_1D)
  DF_mod = DF %>%
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
                   doTrace = 1,
                   maxRuns = 25)
  Imp = as.data.frame(Stage_1$finalDecision)
  Keep = rownames(Imp)[Imp$`Stage_1$finalDecision` == "Confirmed"]
  DF_mod = DF_mod %>%
    dplyr::select("Buy",Keep)
  
  # Importance Plot
  Inf_Func = function(x){sum(is.infinite(x)) == 0}
  varImp = as.data.frame(Stage_1$ImpHistory) %>%
    select_if(Inf_Func) %>%
    summarise_all(median) %>%
    t()
  colnames(varImp) = "Importance"
  varImp = sort(varImp,
       decreasing = T)
  
  ## Building Default GBM to further reduce columns
  set.seed(11)
  # n.trees more needed as shrinkage decreases
  ## default = 100
  # interaction.depth variable interactions (1st order, 2nd order, etc.)
  ## default = 1
  # shrinkage is the effective learning rate.
  ## default = 0.1
  # n.minobsinnode should be lower for imbalanced problems
  ## default = 10
  
  # Initializing Parallel Clusters
  c1 = makeCluster(detectCores() - 1)
  registerDoParallel(c1)
  
  # ## Tuning grid, adjust if needed based on above descriptions
  tgrid = expand.grid(n.trees = nrow(DF_mod),
                      interaction.depth = c(3,5,7,9),
                      shrinkage = 0.1,
                      n.minobsinnode = c(7,14,30))
  
  ## 10 Fold CV, adjust if needed to fit the problem
  trControl = trainControl(method = "cv",
                           number = 10,
                           verboseIter = T)
  ## Training GBM
  logit_gbm = train(factor(Buy)~.,
                    data = DF_mod,
                    verbose = FALSE,
                    method = "gbm",
                    metric = "Kappa",
                    weights = Weight_Vector,
                    tuneGrid = tgrid,
                    trControl = trControl)
  plot(logit_gbm)
  
  # Spinning Down the Clusters
  stopCluster(c1)
  registerDoSEQ()
  
  ## Examining Feature Importance
  Vars = as.data.frame(gbm::relative.influence(object = logit_gbm$finalModel,
                                               n.trees = logit_gbm$bestTune$n.trees,
                                               scale. = T,
                                               sort. = T))
  ## Creating data frame
  colnames(Vars) = "Vars"
  Vars = Vars %>%
    mutate(Variables = rownames(.)) %>%
    filter(Vars > 0.15,
           Variables != "Count") %>%
    arrange(desc(Vars))
  Vars$Variables = factor(Vars$Variables, levels = c(Vars$Variables))
  
  ## Plotting Importance Values
  ggplot(Vars,aes(x = Variables,y = Vars)) + 
    geom_col() +
    coord_flip() +
    scale_y_continuous(labels = scales::percent) +
    labs(y = "Variable Importance",
         x = "",
         title = "Stock Buy Variable Importance") 
  
  
  # Fit the final model
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
  Names = colnames(DF_mod)
  Output = list(Names = Names,
                Mod = model_base)
  return(Output)
}