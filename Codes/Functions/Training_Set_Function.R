Training_Set_Function = function(Combined_Results){
  require(doParallel)
    ## Initializing Cluster
    c1 = makeCluster(detectCores())
    registerDoParallel(c1)

    ## Looping All Stocks Through Spline Optimization
    Tickers = as.character(unique(Combined_Results$Stock))
    
    Window_Results = foreach(i = 1:length(Tickers),
                             .inorder = F,
                             .export = c("Spline_Par_Optim",
                                         "BS_Indicator_Function",
                                         "OPT_Window_TI",
                                         "PR_Cost_Function",
                                         "Stat_Appendage_Function",
                                         "Tickers",
                                         "Combined_Results"),
                             .packages = c("tidyverse",
                                           "zoo",
                                           "TTR",
                                           "quantmod",
                                           "lubridate")
    ) %dopar% {
      # Ticker to Optimize
      Stock_Loop = Tickers[i]
      
      # Subsetting to Specific Stock
      DF = Combined_Results %>%
        filter(Stock == Stock_Loop) %>%
        mutate(PR_1D = (Adjusted - lag(Adjusted))/Adjusted) %>%
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
    
    # Removing Clusters
    registerDoSEQ()
    
    # Simplifying List and Removing Try-Errors
    Window_Results = Window_Results[str_detect(sapply(Window_Results,class),"data.frame")] %>%
      plyr::ldply(data.frame)
    
    return(Window_Results)
}