PR_Appendage = function(Combined_Results = NULL,
                        parallel = T,
                        NCores = detectCores()){
  
  Loop_Function = function(Combined_Results,
                           Stock_Loop){
    # Subsetting to Specific Stock
    DF = Combined_Results %>%
      filter(Stock == Stock_Loop) %>%
      na.omit() %>%
      mutate(Volume_50_SMA = rollapply(data = Volume,
                                       width = 50,
                                       FUN = mean,
                                       na.rm = T,
                                       fill = NA,
                                       align = "right"),
             Volume_50_SD = rollapply(data = Volume,
                                      width = 50,
                                      FUN = sd,
                                      na.rm = T,
                                      fill = NA,
                                      align = "right"),
             Volume_PD_Norm = (Volume - Volume_50_SMA)/Volume_50_SMA,
             Volume_SD_Norm = (Volume - Volume_50_SMA)/Volume_50_SD) %>%
      mutate(Close_Max_50 =  rollapply(data = Close,
                                       width = 50,
                                       FUN = max,
                                       na.rm = T,
                                       fill = NA,
                                       align = "right"),
             Close_High_PD_Norm = (Close - Close_Max_50)/Close_Max_50,
             Close_50_SMA = rollapply(data = Close,
                                      width = 50,
                                      FUN = mean,
                                      na.rm = T,
                                      fill = NA,
                                      align = "right"),
             Close_200_SMA = rollapply(data = Close,
                                      width = 200,
                                      FUN = mean,
                                      na.rm = T,
                                      fill = NA,
                                      align = "right"),
             Close_50_SD = rollapply(data = Close,
                                     width = 50,
                                     FUN = sd,
                                     na.rm = T,
                                     fill = NA,
                                     align = "right"),
             Close_PD_50_Norm = (Close - Close_50_SMA)/Close_50_SMA,
             Close_PD_200_Norm = (Close - Close_200_SMA)/Close_200_SMA,
             Close_SD_50_Norm = (Close - Close_50_SMA)/Close_50_SD,
             Close_PD_50_200_Norm = (Close_50_SMA - Close_200_SMA)/Close_200_SMA,
             Close_Slope_Inst = (Close - lag(Close,1)),
             Close_Slope_50 = rollapply(data = Close_Slope_Inst,
                                        width = 50,
                                        FUN = mean,
                                        na.rm = T,
                                        fill = NA,
                                        align = "right"),
             Close_Slope_50_Norm = Close_Slope_50/Close_50_SMA,
             Close_Slope_200 = rollapply(data = Close_Slope_Inst,
                                         width = 200,
                                         FUN = mean,
                                         na.rm = T,
                                         fill = NA,
                                         align = "right"),
             Close_Slope_200_Norm = Close_Slope_200/Close_200_SMA,
             Close_50_Positive = Close_PD_50_Norm >= 0,
             Close_50_Time_Norm = sequence(rle(Close_50_Positive)$lengths)) %>%
      mutate(Price_Range = High - Low,
             Price_Range_15_SMA = rollapply(data = Price_Range,
                                            width = 15,
                                            FUN = mean,
                                            na.rm = T,
                                            fill = NA,
                                            align = "right"),
             Price_Range_15_SD = rollapply(data = Price_Range,
                                           width = 15,
                                           FUN = sd,
                                           na.rm = T,
                                           fill = NA,
                                           align = "right"),
             Price_Range_15_SD_Norm = Price_Range_15_SMA/Price_Range_15_SD) %>%
      na.omit() %>%
      select(Stock,Date,Open,High,Low,Close,Adjusted,Volume,contains("Norm")) %>%
      as.data.frame()
    
    return(DF)
  }
  
    ## Quick Reduction
    Combined_Results = Combined_Results %>%
      group_by(Stock) %>%
      filter(n() >= 365) %>%
      ungroup()
  
    ## Looping All Stocks Through Spline Optimization
    Tickers = as.character(unique(Combined_Results$Stock))
    
    if(!parallel){
      Window_Results = list()
      
      p = progress_estimated(n = length(Tickers),min_time = 3)
      for(i in 1:length(Tickers)){
        p$pause(0.1)$tick()$print()
        # Ticker to Optimize
        Stock_Loop = Tickers[i]
        # Output to ForEach Loop
        Window_Results[[i]] = Loop_Function(Combined_Results,
                                            Stock_Loop)
      }
    }else{
      NCores = min(c(detectCores(),NCores))
      c1 = makeCluster(NCores)
      registerDoParallel(c1)
      Window_Results = foreach(i = 1:length(Tickers),
                               .inorder = F,
                               .packages = c("tidyverse",
                                             "quantmod",
                                             "lubridate")) %dopar% {
                                             # Ticker to Optimize
                                             Stock_Loop = Tickers[i]
                                             # Output to ForEach Loop
                                             Loop_Function(Combined_Results,
                                                           Stock_Loop)
                                           }
      stopCluster(c1)
      registerDoSEQ()
    }
    # Simplifying List and Removing Try-Errors
    Window_Results = Window_Results[str_detect(sapply(Window_Results,class),"data.frame")] %>%
      plyr::ldply(data.frame)
    
    return(Window_Results)
}
