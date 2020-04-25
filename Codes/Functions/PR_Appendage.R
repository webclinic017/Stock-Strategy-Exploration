PR_Appendage = function(Combined_Results,Required_Packages){
  
  ## Spinning down any left open clusters
  on.exit(installr::kill_all_Rscript_s())
  
  Loop_Function = function(DF){
    # Subsetting to Specific Stock
    DF = DF %>%
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
      select(Stock,Date,Open,High,Low,Close,Volume,contains("Norm")) %>%
      as.data.frame()
    
    return(DF)
  }
  
    ## Looping All Stocks Through Spline Optimization
    c1 = makeCluster(detectCores())
    registerDoSNOW(c1)
    p <- progress_estimated(length(unique(Combined_Results$Stock)))
    progress <- function(n) p$tick()$print()
    opts <- list(progress = progress)
    
    Symbols = isplit(Combined_Results,Combined_Results$Stock)
    
    Window_Results = foreach(i = Symbols,
                             .errorhandling = "stop",
                             .inorder = F,
                             .options.snow = opts,
                             .packages = Required_Packages) %dopar% {
                                             # Output to ForEach Loop
                                             Loop_Function(i$value)
                                           }
    
    # Simplifying List and Removing Try-Errors
    Window_Results = Window_Results[str_detect(sapply(Window_Results,class),"data.frame")] %>%
      plyr::ldply(data.frame)
    
    return(Window_Results)
}
