Modeling_Function = function(PR_Stage_R4,Max_Date = max(PR_Stage_R4$Date)){
  PR_Stage_R4 = PR_Stage_R4 %>%
    filter(Date <= Max_Date)
  
  ## Reducing Variable Pool
  Names_Profit = Variable_Importance_Reduction(DF = select(PR_Stage_R4,
                                                           -c(Open,High,Low,Close,Adjusted,
                                                              Volume,Adjusted_Lead)) %>%
                                                 sample_frac(0.05),
                                               Type = 'C',
                                               Target = "Target")
  Names_Futures = Variable_Importance_Reduction(DF = select(PR_Stage_R4,-c(Target,Volume)) %>%
                                                  sample_frac(0.05),
                                                Type = 'R',
                                                Target = "Adjusted_Lead")
  
  ## Reducing Data
  LL = function(x){median(x,na.rm = T) - 5*mad(x,na.rm = T)}
  UL = function(x){median(x,na.rm = T) + 5*mad(x,na.rm = T)}
  PR_Stage_R5 = PR_Stage_R4 %>%
    select(Stock,Date,Adjusted,Names_Profit$Var,Names_Futures$Var,Target,Adjusted_Lead)
  
  ## Defining Filter Columns
  Filter = PR_Stage_R5 %>%
    select(Names_Profit$Var,Names_Futures$Var,Adjusted_Lead) %>% 
    colnames()
  
  ## Removing Outliers
  for(i in Filter){
    Column = as_vector(PR_Stage_R5[,i])
    Keep = Column <= UL(Column) & Column >= LL(Column)
    PR_Stage_R5 = PR_Stage_R5[Keep,]
  }
  
  Split_Profit = createDataPartition(y = PR_Stage_R5$Target,p = 0.70,list = F)
  Split_Futures = createDataPartition(y = PR_Stage_R5$Adjusted_Lead,p = 0.70,list = F)
  
  Train_Profit = PR_Stage_R5[Split_Profit,c(Names_Profit$Var,"Target","Date","Adjusted","Stock")]
  Test_Profit = PR_Stage_R5[-Split_Profit,c(Names_Profit$Var,"Target","Date","Adjusted","Stock")]
  Train_Futures = PR_Stage_R5[Split_Futures,c(Names_Futures$Var,"Adjusted_Lead","Date","Adjusted","Stock")]
  Test_Futures = PR_Stage_R5[-Split_Futures,c(Names_Futures$Var,"Adjusted_Lead","Date","Adjusted","Stock")]
  
  Weights_Profit = scales::rescale(as.numeric(Train_Profit$Date),to = c(0,1))
  Weights_Futures = scales::rescale(as.numeric(Train_Futures$Date),to = c(0,1))
  
  Model_Profit = glm(Target~.,
                     data = select(Train_Profit,
                                   -c(Stock,Date,Adjusted)),
                     family = "quasibinomial",
                     weights = Weights_Profit)
  Model_Futures = lm(Adjusted_Lead~.,
                     data = select(Train_Futures,
                                   -c(Stock,Date,Adjusted)),
                     weights = Weights_Futures)
  
  Pred_Train = predict(Model_Profit,type = "response")
  Pred_Test = predict(Model_Profit,Test_Profit,type = "response")
  Cutoff = median(Pred_Test) + mad(Pred_Test)
  
  Pred_Train[Pred_Train >= Cutoff] = 1
  Pred_Train[Pred_Train < Cutoff] = 0
  Pred_Test[Pred_Test >= Cutoff] = 1
  Pred_Test[Pred_Test < Cutoff] = 0
  
  Specif_Train = MLmetrics::Specificity(Pred_Train,Train_Profit$Target)
  Specif_Test = MLmetrics::Specificity(Pred_Test,Test_Profit$Target)
  
  Pred_Futures_Train = predict(Model_Futures)
  Pred_Futures_Test = predict(Model_Futures,Test_Futures)
  
  ACC_Train = MLmetrics::MAPE(Pred_Futures_Train,Train_Futures$Adjusted_Lead)
  ACC_Test = MLmetrics::MAPE(Pred_Futures_Test,Test_Futures$Adjusted_Lead)
  
  return(list(Model_Futures = Model_Futures,
              Model_Profit = Model_Profit))
}