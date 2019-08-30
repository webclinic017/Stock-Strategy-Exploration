  Modeling_Function = function(PR_Stage_R4,Max_Date = max(PR_Stage_R4$Date)){
    
    TMP = PR_Stage_R4 %>%
      filter(Date <= Max_Date,
             Date >= Max_Date-365) %>%
      BUY_POS_FILTER()
    

    ## Reducing Variable Pool
    Names_Profit = Variable_Importance_Reduction(DF = dplyr::select(TMP,
                                                             -c(Adjusted_Lead)),
                                                 Type = 'C',
                                                 Target = "Target",
                                                 Plot = F)
    Names_Futures = Variable_Importance_Reduction(DF = dplyr::select(TMP,
                                                                     -c(Target)),
                                                  Type = 'R',
                                                  Target = "Adjusted_Lead",
                                                  Plot = F)
    
    Train_Profit = TMP[,c(Names_Profit$Var,"Target")]
    Train_Futures = TMP[,c(Names_Futures$Var,"Adjusted_Lead")]

    
    CDM = 5
    counter = 0
    while(CDM >= 5){
      counter = counter + 1
      print(counter)
      Model_Profit = glm(formula = Target~.,
                         data = Train_Profit,
                         family = quasibinomial())
      CD = (cooks.distance(Model_Profit)-mean(cooks.distance(Model_Profit),na.rm = T))/
        sd(cooks.distance(Model_Profit),na.rm = T)
      CDM = max(CD,na.rm = T)
      print(CDM)
      if(CDM >=5){
        Train_Profit = Train_Profit[-which(CD >= 5),]
      }
    }
    P_Max = 0.05
    counter = 0
    while(P_Max >= 0.05){
      counter = counter + 1
      print(counter)
      Model_Profit = glm(formula = Target~.,
                          data = Train_Profit,
                          family = gaussian())
      P_Vals = coef(summary(Model_Profit))[-1,]
      P_Max = max(P_Vals[,4])
      print(P_Max)
      P_Var = rownames(P_Vals)[which.max(P_Vals[,4])]
      print(P_Var)
      if(P_Max >= 0.05){
        Train_Profit = Train_Profit %>%
          select(-P_Var)
      }
    }
    
    CDM = 5
    counter = 0
    while(CDM >= 5){
      counter = counter + 1
      print(counter)
      Model_Futures = glm(formula = Adjusted_Lead~.,
                       data = Train_Futures,
                       family = gaussian())
      CD = (cooks.distance(Model_Futures)-mean(cooks.distance(Model_Futures),na.rm = T))/
        sd(cooks.distance(Model_Futures),na.rm = T)
      CDM = max(CD,na.rm = T)
      print(CDM)
      if(CDM >=5){
        Train_Futures = Train_Futures[-which(CD >= 5),]
      }
    }
    P_Max = 0.05
    counter = 0
    while(P_Max >= 0.05){
      counter = counter + 1
      print(counter)
      Model_Futures = glm(formula = Adjusted_Lead~.,
                          data = Train_Futures,
                          family = gaussian())
      P_Vals = coef(summary(Model_Futures))[-1,]
      P_Max = max(P_Vals[,4])
      print(P_Max)
      P_Var = rownames(P_Vals)[which.max(P_Vals[,4])]
      print(P_Var)
      if(P_Max >= 0.05){
        Train_Futures = Train_Futures %>%
          select(-P_Var)
      }
    }
    
    
    return(list(Model_Futures = Model_Futures,
                Model_Profit = Model_Profit,
                Names_Profit = Names_Profit,
                Names_Futures = Names_Futures))
  }