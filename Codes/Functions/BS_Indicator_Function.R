## Labels all Valleys and Peaks
## Back fills all Buy / Sells and calculates % return from current point to the next peak / valley
## Calculates holding time and days to the next peak / valley
## Returns Original DF appended with...
# PR           (% Return to next peak or valley)
# Max          (Total Holding Time in Days for Buy or Sell Period)
# Days         (Days to the next valley or peak)
BS_Indicator_Function = function(DF,Column = NULL){
  ########################## Sample Data #######################
    # DF = Combined_Results %>%
    #   group_by(Stock) %>%
    #   filter(Stock == "AMZN")
    # # load(file = "//climsidfs07/RefEng/1 Ref. Engineering (SH, Scroll & IPD)/13) Analytics/Small Projects/Stocks/Data/NASDAQ Historical.RDATA")
    # Column = "Adjusted"
  ##############################################################
  require(tidyverse)
  require(quantmod)

    ## Defining Buy Sell Indicators (Local Mins/Maxs)
    DF = DF %>%
      mutate(Buy = NA,
             Sell = NA)
    DF = as.data.frame(DF)
    Peaks = findPeaks(DF[,Column])-1
    Valleys = findValleys(DF[,Column])-1
    DF[Peaks,"Sell"] = 1
    DF[Peaks,"Buy"] = 0
    DF[Valleys,"Buy"] = 1
    DF[Valleys,"Sell"] = 0
    DF = na.locf(DF)
    
    ## Solving Issue When No Valleys or Peaks
    if(is_empty(Peaks) | is_empty(Valleys)){
      DF2 = DF %>%
        mutate(Buy = 1,
               Max = 6,
               PR = NA) %>%
        select(-Sell)
      return(DF2)
    }
    
    ## Calculating Price Ratio (Percentage Return / Number of Days Invested)
    DF2 = DF %>%
      mutate(Indicator = sequence(rle(Buy)$lengths),
             Max = ifelse(lead(Indicator) == 1,Indicator + 1,NA)) %>%
      na.locf(fromLast = T,na.rm = F) %>%
      mutate(Days = Max - Indicator,
             End_Price_Buy = ifelse(Sell == 1 & Indicator == 1,
                                    Adjusted,NA),
             End_Price_Sell = ifelse(Buy == 1 & Indicator == 1,
                                     Adjusted,NA)) %>%
      na.locf(fromLast = T,na.rm = F) %>%
      mutate(PR = ifelse(Buy == 1,
                         (End_Price_Buy-Adjusted)/(Days*Adjusted),
                         (End_Price_Sell-Adjusted)/(Days*Adjusted))) %>%
      select(-c(Indicator,End_Price_Buy,End_Price_Sell,Sell))
    DF2[is.na(DF2)] = 0
      
      # ggplot(DF2[1:100,],aes(Date,Adjusted,color = factor(Buy))) +
      #   geom_point()
  return(DF2)
}



  
