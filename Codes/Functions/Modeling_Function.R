  Modeling_Function = function(PR_Stage_R4,Max_Date = max(PR_Stage_R4$Date)){
    require('speedglm')
    
    PR_Stage_R4 = PR_Stage_R4 %>%
      filter(Date <= Max_Date,
             Date >= Max_Date-365) %>%
      BUY_POS_FILTER()
    

    ## Reducing Variable Pool
    Names_Profit = Variable_Importance_Reduction(DF = dplyr::select(PR_Stage_R4,
                                                             -c(Adjusted_Lead)),
                                                 Type = 'C',
                                                 Target = "Target")
    Names_Futures = Variable_Importance_Reduction(DF = dplyr::select(PR_Stage_R4,-c(Target)),
                                                  Type = 'R',
                                                  Target = "Adjusted_Lead")
    
    ## Reducing Data
    LL = function(x){median(x,na.rm = T) - 5*mad(x,na.rm = T)}
    UL = function(x){median(x,na.rm = T) + 5*mad(x,na.rm = T)}
    PR_Stage_R5 = PR_Stage_R4 %>%
      dplyr::select(Stock,
             Date,
             Adjusted,
             Names_Profit$Var,
             Names_Futures$Var,
             Target,
             Adjusted_Lead)
    
    ## Defining Filter Columns
    Filter = PR_Stage_R5 %>%
      dplyr::select(Names_Profit$Var,Names_Futures$Var,Adjusted_Lead) %>% 
      colnames()
    
    ## Removing Outliers
    for(i in Filter){
      Column = as_vector(PR_Stage_R5[,i])
      Keep = Column <= UL(Column) & Column >= LL(Column)
      PR_Stage_R5 = PR_Stage_R5[Keep,]
    }
    
    Train_Profit = PR_Stage_R5[,c(Names_Profit$Var,"Target")]
    Train_Futures = PR_Stage_R5[,c(Names_Futures$Var,"Adjusted_Lead")]

    
    Model_Profit = glm(formula = Target~.^2,
                       data = Train_Profit,
                       family = quasibinomial())
    Model_Futures = glm(formula = Adjusted_Lead~.^2,
                       data = Train_Futures,
                       family = gaussian())
    
    return(list(Model_Futures = Model_Futures,
                Model_Profit = Model_Profit,
                Names_Profit = Names_Profit,
                Names_Futures = Names_Futures))
  }