BAC_Function = function(PR_Stage,
                        Total_Alpha_Slope,
                        Group_Columns,
                        Required_Packages,
                        width = 50){
  
  ## Spinning down any left open clusters
  on.exit(installr::kill_all_Rscript_s())
  
  if(all(Group_Columns == "Stock")){
    TMP = PR_Stage %>%
      group_by(Stock) %>%
      mutate(Return = (Close - lag(Close,1))/lag(Close,1)) %>%
      inner_join(Total_Alpha_Slope,by = "Date") %>%
      na.omit() %>%
      select(Date,Stock,Return,Total_Alpha) %>%
      ungroup()
  }else{
    TMP = PR_Stage %>%
      left_join(Auto_Stocks,by = c("Stock" = "Symbol")) %>%
      group_by_at(vars(Date,Group_Columns)) %>%
      summarise(Return = mean(Close,trim = 0.05)) %>%
      mutate(Return = (Return - lag(Return,1))/lag(Return,1)) %>%
      inner_join(Total_Alpha_Slope,by = "Date") %>%
      na.omit() %>%
      ungroup()
  }
  
  Groups = list()
  for(i in Group_Columns){
    Groups[[i]] = as_vector(TMP[,i])
  }
  
  ## Looping All Stocks Through Spline Optimization
  c1 = makeCluster(detectCores())
  registerDoParallel(c1)
  
  Symbols = isplit(x = TMP,f = Groups)
  
  Total_Out = foreach(i = Symbols,
                      .errorhandling = "stop",
                      .inorder = F,
                      .packages = Required_Packages) %dopar% {  
                        
                        TMP2 = i$value
                        
                        Y = as.matrix(TMP2$Return)
                        X = as.matrix(TMP2$Total_Alpha)
                        
                        Results = roll_lm(x = X,
                                          y = Y,
                                          width = width,
                                          intercept = T,
                                          na_restore = T)
                        Alpha = Results$coefficients[,1]
                        Z_Alpha = Alpha/Results$std.error[,1]
                        P_Alpha = exp(-0.717 * Z_Alpha - 0.416 * Z_Alpha * Z_Alpha)
                        Beta = Results$coefficients[,2]
                        Z_Beta = Beta/Results$std.error[,2]
                        P_Beta = exp(-0.717 * Z_Beta - 0.416 * Z_Beta * Z_Beta)
                        
                        Cor = Results$r.squared
                        Output = data.frame(Alpha = Alpha,
                                            Z_Alpha = Z_Alpha,
                                            P_Alpha = P_Alpha,
                                            Beta = Beta,
                                            Z_Beta = Z_Beta,
                                            P_Beta = P_Beta,
                                            Cor = Cor)
                        colnames(Output) = str_c(colnames(Output),"_",str_c(Group_Columns,collapse = "_"))
                        Output = Output %>%
                          bind_cols(TMP2) %>%
                          select(-Return,-Total_Alpha) %>%
                          na.omit() %>%
                          as.data.frame()
                        Output
                      }
  # Total_Out List and Removing Try-Errors
  Total_Out = Total_Out[str_detect(sapply(Total_Out,class),"data.frame")] %>%
    plyr::ldply(data.frame)
  
  return(Total_Out)
}