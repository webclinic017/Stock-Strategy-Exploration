  Modeling_Function = function(PR_Stage_R4,Max_Date = max(PR_Stage_R4$Date)){
    
    stripGlmLR = function(cm) {
      cm$y = c()
      cm$model = c()
      
      cm$residuals = c()
      cm$fitted.values = c()
      cm$effects = c()
      cm$qr$qr = c()
      cm$linear.predictors = c()
      cm$weights = c()
      cm$prior.weights = c()
      cm$data = c()
      
      
      cm$family$variance = c()
      cm$family$dev.resids = c()
      cm$family$aic = c()
      cm$family$validmu = c()
      cm$family$simulate = c()
      attr(cm$terms,".Environment") = c()
      attr(cm$formula,".Environment") = c()
      
      cm
    }
    
    
    PR_Stage_R4 = PR_Stage_R4 %>%
      filter(Date <= Max_Date)
    
    ## Reducing Variable Pool
    Names_Profit = Variable_Importance_Reduction(DF = select(PR_Stage_R4,
                                                             -c(Open,High,Low,Close,Adjusted,
                                                                Volume,Adjusted_Lead)) %>%
                                                   sample_frac(0.25),
                                                 Type = 'C',
                                                 Target = "Target")
    Names_Futures = Variable_Importance_Reduction(DF = select(PR_Stage_R4,-c(Target,Volume)) %>%
                                                    sample_frac(0.25),
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
    
    
    Model_Profit = stripGlmLR(glm(Target~.,
                       data = select(Train_Profit,
                                     -c(Stock,Date,Adjusted)),
                       family = "quasibinomial"))
    Model_Futures = stripGlmLR(glm(Adjusted_Lead~.,
                       data = select(Train_Futures,
                                     -c(Stock,Date,Adjusted))))
    
    return(list(Model_Futures = Model_Futures,
                Model_Profit = Model_Profit,
                Names_Profit = Names_Profit,
                Names_Futures = Names_Futures))
  }