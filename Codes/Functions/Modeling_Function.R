  Modeling_Function = function(ID_DF,Projection,Quant = 0.9,Max_Date){
    Model_CD_PV_Loop = function(Train_Profit){
      Model_Profit = glm(formula = Adjusted_Lead~.,
                         data = Train_Profit,
                         family = quasibinomial())
      Model_Profit = EmersonDataScience:::stripGlmLR(Model_Profit)
      return(Model_Profit)
    }
    
    ## Defining Target Variable
    Short = ID_DF %>%
      group_by(Stock) %>%
      mutate(Adjusted_Lead = (lead(Close,Projection) - Close)/ATR,
             Adjusted_Lead = case_when(
               Adjusted_Lead >= quantile(Adjusted_Lead,Quant,na.rm = T) ~ 1,
               Adjusted_Lead < quantile(Adjusted_Lead,Quant,na.rm = T) ~ 0
             )) %>%
      ungroup() %>%
      na.omit() %>%
      filter(!str_detect(Stock,"^\\^"))
    
    Mid = Short %>%
      group_by(Stock) %>%
      mutate(Adjusted_Lead = (lead(Close,Projection*4) - Close)/ATR,
             Adjusted_Lead = case_when(
               Adjusted_Lead >= quantile(Adjusted_Lead,Quant,na.rm = T) ~ 1,
               Adjusted_Lead < quantile(Adjusted_Lead,Quant,na.rm = T) ~ 0
             )) %>%
      ungroup() %>%
      na.omit()
    
    Long = Mid %>%
      group_by(Stock) %>%
      mutate(Adjusted_Lead = (lead(Close,Projection*4*4) - Close)/ATR,
             Adjusted_Lead = case_when(
               Adjusted_Lead >= quantile(Adjusted_Lead,Quant,na.rm = T) ~ 1,
               Adjusted_Lead < quantile(Adjusted_Lead,Quant,na.rm = T) ~ 0
             )) %>%
      ungroup() %>%
      na.omit()
    
    ## Reducing Model DFs
    TMP_Short = Short %>%
      filter(Date <= Max_Date,
             Date >= Max_Date-365) %>%
      BUY_POS_FILTER()
    TMP_Mid = Mid %>%
      filter(Date <= Max_Date,
             Date >= Max_Date-365) %>%
      BUY_POS_FILTER()
    TMP_Long = Long %>%
      filter(Date <= Max_Date,
             Date >= Max_Date-365) %>%
      BUY_POS_FILTER()
    
    ## Reducing Variable Pool
    Names_Short = Variable_Importance_Reduction(DF = TMP_Short,
                                                Type = 'C',
                                                Target = "Adjusted_Lead",
                                                Plot = F)
    Names_Mid = Variable_Importance_Reduction(DF = TMP_Mid,
                                              Type = 'C',
                                              Target = "Adjusted_Lead",
                                              Plot = F)
    Names_Long = Variable_Importance_Reduction(DF = TMP_Long,
                                               Type = 'C',
                                               Target = "Adjusted_Lead",
                                               Plot = F)
    
    ## Defining Training Sets
    Train_Short = TMP_Short[,c(Names_Short$Var,"Adjusted_Lead")]
    Train_Mid = TMP_Mid[,c(Names_Mid$Var,"Adjusted_Lead")]
    Train_Long = TMP_Long[,c(Names_Long$Var,"Adjusted_Lead")]

    ## Building Models
    Model_Short = Model_CD_PV_Loop(Train_Profit = Train_Short)
    Model_Mid = Model_CD_PV_Loop(Train_Mid)
    Model_Long = Model_CD_PV_Loop(Train_Long)
    
    ## Returning Values
    return(list(Names_Short = Names_Short,
                Model_Short = Model_Short,
                Names_Mid = Names_Mid,
                Model_Mid = Model_Mid,
                Names_Long = Names_Long,
                Model_Long = Model_Long))
  }