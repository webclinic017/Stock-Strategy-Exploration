BS_Indicator_Function = function(DF,Column = NULL){
  # ########################## Sample Data #######################
  #   TEST = Combined_Results %>%
  #     group_by(Stock) %>%
  #     PV("Adjusted") 
  #   load(file = "//climsidfs07/RefEng/1 Ref. Engineering (SH, Scroll & IPD)/13) Analytics/Small Projects/Stocks/Data/NASDAQ Historical.RDATA")
  #   Column = "Adjusted"
  # ##############################################################
  require(tidyverse)
  require(quantmod)
  
  DF = DF %>%
    mutate(Buy = 0,
           Sell = 0) 
  Peaks = findPeaks(DF$Adjusted)
  Valleys = findValleys(DF$Adjusted)
  DF[Peaks,"Sell"] = 1
  DF[Valleys,"Buy"] = 1
  return(DF)
}

