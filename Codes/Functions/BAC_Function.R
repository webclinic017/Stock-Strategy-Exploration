BAC_Function = function(PR_Stage,Total_Alpha_Slope,Group_Columns,width = 50){
  if(all(Group_Columns == "Stock")){
    TMP = PR_Stage %>%
      mutate(Return = Close_Slope_50_Norm) %>%
      inner_join(Total_Alpha_Slope,by = "Date") %>%
      na.omit()
  }else{
    TMP = PR_Stage %>%
      left_join(Auto_Stocks,by = c("Stock" = "Symbol")) %>%
      group_by_at(vars(Date,Group_Columns)) %>%
      summarise(Return = mean(Close_Slope_50_Norm,trim = 0.05)) %>%
      inner_join(Total_Alpha_Slope,by = "Date") %>%
      na.omit() %>%
      ungroup()
  }
  
  Group_DF = TMP %>%
    select(Group_Columns) %>%
    distinct()
  
  counter = 0
  p = progress_estimated(nrow(Group_DF))
  for(i in 1:nrow(Group_DF)){
    counter = counter + 1
    TMP2 = TMP
    for(j in 1:ncol(Group_DF)){
      TMP2 = TMP2 %>%
        filter_at(colnames(Group_DF)[j],any_vars(. == as_vector(Group_DF[i,j])))
    }
    Y = as.matrix(TMP2$Return)
    X = as.matrix(TMP2$Total_Alpha)
    
    Results = roll_lm(x = X,
                      y = Y,
                      width = width,
                      intercept = T,
                      na_restore = T)
    Alpha = Results$coefficients[,1]
    Beta = Results$coefficients[,2]
    Cor = Results$r.squared
    Output = data.frame(Alpha = Alpha,
                        Beta = Beta,
                        Cor = Cor)
    colnames(Output) = str_c(colnames(Output),"_",str_c(Group_Columns,collapse = "_"))
    Output = Output %>%
      bind_cols(TMP2) %>%
      select(-Return,-Total_Alpha) %>%
      na.omit()
    if(counter == 1){
      Total_Out = Output
    }else{
      Total_Out = bind_rows(Total_Out,Output)
    }
    p$pause(0.1)$tick()$print()
  }
  return(Total_Out)
}