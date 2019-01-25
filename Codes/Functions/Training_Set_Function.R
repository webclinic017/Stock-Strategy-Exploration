Training_Set_Function = function(Combined_Results){

    ## Looping All Stocks Through Spline Optimization
    Combined_Results = as.data.frame(Combined_Results)
    Tickers = as.character(unique(Combined_Results$Stock))
    Window_Results = list()
    
    p = progress_estimated(n = length(Tickers),min_time = 3)
    for(i in 1:length(Tickers)){
      p$pause(0.1)$tick()$print()
      # Ticker to Optimize
      Stock_Loop = Tickers[i]
      
      # Subsetting to Specific Stock
      DF = Combined_Results %>%
        filter(Stock == Stock_Loop) %>%
        mutate(Adjust_TMP = Adjusted) %>%
        mutate(Open = as.numeric(rollapply(data = Open,width = 90,align = "right",fill = NA,FUN = scale)),
               High = as.numeric(rollapply(data = High,width = 90,fill = NA,align = "right",FUN = scale)),
               Low = as.numeric(rollapply(data = Low,width = 90,fill = NA,align = "right",FUN = scale)),
               Close = as.numeric(rollapply(data = Close,width = 90,fill = NA,align = "right",FUN = scale)),
               Adjusted = as.numeric(rollapply(data = Adjusted,width = 90,fill = NA,align = "right",FUN = scale)),
               Volume = as.numeric(rollapply(data = Volume,width = 90,fill = NA,align = "right",FUN = scale))) %>% 
        na.omit() %>%
        as.data.frame()
      
      if(nrow(DF) >= 365){
        
        TMP = DF %>%
          mutate(Buy = 0)
        Min = TMP$Adjust_TMP[1]
        for(k in 2:(nrow(TMP)-1)){
          Stance = TMP$Buy[k-1]
          Delta = (TMP$Adjust_TMP[k] - TMP$Adjust_TMP[k-1])/TMP$Adjust_TMP[k-1]
          
          # Not Currently Holding and Price Reduction
          if(Stance == 0 & Delta < 0){
            if(TMP$Adjust_TMP[k] < Min){
              Min = TMP$Adjust_TMP[k]
            }
            TMP$Buy[k] = 0
          }
          # Not Currently Holding and Price Increase
          if(Stance == 0 & Delta > 0){
            if((TMP$Adjust_TMP[k]-Min)/Min >= 0.05){
              TMP$Buy[k] = 1
              Purchase = TMP$Adjust_TMP[k]
              Max = Purchase
              counter = 0
            }else{
              TMP$Buy[k] = 0
            }
          }
          # Currently Holding and Price Reduction
          if(Stance == 1 & Delta <= 0){
            Diff_Purc = (TMP$Adjust_TMP[k] - Purchase)/Purchase
            Diff_Max = (TMP$Adjust_TMP[k] - Max)/Max
            if(Diff_Max <= -0.10 | Diff_Purc <= -0.05){
              TMP$Buy[k] = 0
              Min = TMP$Adjust_TMP[k]
            }else{
              TMP$Buy[k] = 1
            }
          }
          # Currently Holding and Price Increase
          if(Stance == 1 & Delta > 0){
            if(TMP$Adjust_TMP[k] > Max){
              Max = TMP$Adjust_TMP[k]
            }
            TMP$Buy[k] = 1
          }
        }
        
        TMP = TMP %>%
          mutate(Indicator = sequence(rle(Buy)$lengths),
                 Max = ifelse(lead(Indicator) == 1,Indicator + 1,NA)) %>%
          na.locf(fromLast = T,na.rm = F) %>%
          mutate(W = 1/Indicator)
        TMP$Buy[TMP$Max < 10] = 0
        
    
    # ggplot(TMP,aes(x = Date,y = Adjust_TMP)) +
    #   geom_point(aes(color = factor(Buy)))

      }else{
        TMP = NA
      }
      ## Finding Optimal Stat Windows
      ## Removing Date Information
      Smooth_Data =  try(TMP %>%
                           Stat_Appendage_Function())

      # Output to ForEach Loop
      Window_Results[[i]] = Smooth_Data
    }
    
    # Simplifying List and Removing Try-Errors
    Window_Results = Window_Results[str_detect(sapply(Window_Results,class),"data.frame")] %>%
      plyr::ldply(data.frame)
    
    return(Window_Results)
}