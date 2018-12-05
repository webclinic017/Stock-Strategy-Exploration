Stepwise_Log = function(DF){
  require(MASS)
  require(tidyverse)
  require(lubridate)  
  # ################ Sample Data #################
  # load("C://users//aayorde//desktop//window_results.rdata")
  # DF = Window_Results %>%
  #   filter(Stock == "AMZN")
  # ################## End Sample Data ###########
  DF_mod = na.omit(DF) %>%
    dplyr::select(-c(Date,Stock,Adj_Smooth,Max,Days,PR))
  
  # DF_Train = DF_mod[1:round(nrow(DF)*0.8),]
  # DF_Test = DF[(round(nrow(DF)*0.8)+1):nrow(DF),]
  Weight_Vector = abs(DF_Train$PR)
  # Fit the model
  model <- glm(Buy ~.,
               data = DF_mod, 
               family = binomial,
               weights = Weight_Vector) %>%
    stepAIC(trace = FALSE,
            direction = "backward")
  # Summarize the final selected model
  # summary(model)
  # Make predictions
  # probabilities <- model %>% predict(DF_Test, type = "response")
  # probabilities.df = model %>%
  #   predict(DF,
  #           type = "response")
  # predicted.classes <- ifelse(probabilities > 0.5, 1, 0)
  # predicted.classes.df <- ifelse(probabilities.df > 0.5, 1, 0)
  # DF$Prob = probabilities.df
  # DF = na.omit(DF)
  # DF_Save = DF %>%
  #   dplyr::select(PR_1D,Adjusted,Buy,Prob,PR)
  # save(DF_Save,
  #      file = "C://users//aayorde//desktop//Step_Reg_Results.RDATA")
  # DF_Test$Pred = predicted.classes
  # DF_Test$Prob = probabilities
  # DF_Test$Correct = DF_Test$Pred == DF_Test$Buy
  # Model accuracy
  # mean(predicted.classes==DF_Test$Buy,na.rm=T)
  # 
  # DF_Test$x = seq(1,nrow(DF_Test))
  # ggplot(DF_Test,aes(Date,Buy)) +
  #   geom_point(aes(color = Correct)) +
  #   geom_point(aes(y = Prob,color = Correct)) +
  #   labs(x = "",
  #        y = "Probability & Actual\n",
  #        title = "Amazon Test Set",
  #        subtitle = paste("Stepwise Logistic Regression Accuracy\n% Reuturn",
  #                         round(sum(DF_Test$PR_1D[DF_Test$Pred == 1]),2)),
  #        caption = paste("Remaining Columns :",
  #                        str_flatten(names(model$coefficients)[-1],collapse = ", ")))
  
  return(model)
}