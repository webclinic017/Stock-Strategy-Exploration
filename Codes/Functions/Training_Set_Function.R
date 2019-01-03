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
        mutate(PR_1D = (Adjusted - lag(Adjusted))/Adjusted,
               PR_1D = lead(PR_1D,1),
               Cum_Ret = cumsum(PR_1D)) %>%
        mutate(Open = rollapply(data = Open,width = 90,FUN = scale,fill = NA),
               High = rollapply(data = High,width = 90,FUN = scale,fill = NA),
               Low = rollapply(data = Low,width = 90,FUN = scale,fill = NA),
               Close = rollapply(data = Close,width = 90,FUN = scale,fill = NA),
               Adjusted = rollapply(data = Adjusted,width = 90,FUN = scale,fill = NA),
               Volume = rollapply(data = Volume,width = 90,FUN = scale,fill = NA)) %>% 
        na.omit() %>%
        as.data.frame()
      
      ## Finding Optimal Stat Windows
      ## Removing Date Information
      Smooth_Data =  try(DF %>%
                           BS_Indicator_Function(Column = "Cum_Ret")
        Stat_Appendage_Function(Column = "Cum_Ret"),
        silent = T)

      # Output to ForEach Loop
      Window_Results[[i]] = Smooth_Data
    }
    
    # Simplifying List and Removing Try-Errors
    Window_Results = Window_Results[str_detect(sapply(Window_Results,class),"data.frame")] %>%
      plyr::ldply(data.frame)
    
    return(Window_Results)
}