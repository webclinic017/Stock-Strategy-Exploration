Training_Set_Function = function(Combined_Results){

    ## Looping All Stocks Through Spline Optimization
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
               PR_1D = lead(PR_1D,1)) %>%
        na.omit()
      
      # Attmepting Spline Optimization
      OptSplineParameter = try(optimize(PR_Cost_Function, 
                                        c(0,1), 
                                        DF = DF,
                                        Column = "PR_1D",
                                        maximum = TRUE,
                                        tol = 0.0001),
                               silent = T)
      
      ## Finding Optimal Stat Windows
      Smooth_Data =  try(DF %>%
                           mutate(Adj_Smooth = Spline_Par_Optim(.,Column = "PR_1D")) %>%
                           BS_Indicator_Function("Adj_Smooth") %>%
                           Stat_Appendage_Function(Column = "PR_1D"))

      # Output to ForEach Loop
      Smooth_Data
    }
    
    # Simplifying List and Removing Try-Errors
    Window_Results = Window_Results[str_detect(sapply(Window_Results,class),"data.frame")] %>%
      plyr::ldply(data.frame)
    
    return(Window_Results)
}