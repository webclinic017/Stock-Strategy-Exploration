BS_Indicator_Function = function(DF,Column = NULL){
  ########################## Sample Data #######################
    # DF = Combined_Results %>%
    #   group_by(Stock) %>%
    #   filter(Stock == "AMZN")
    # load(file = "//climsidfs07/RefEng/1 Ref. Engineering (SH, Scroll & IPD)/13) Analytics/Small Projects/Stocks/Data/NASDAQ Historical.RDATA")
    # Column = "Adjusted"
  ##############################################################
  require(tidyverse)
  require(quantmod)

  
  Target_Functions = function(DF){
    DF = DF %>%
      mutate(Buy = NA,
             Sell = NA)
    Peaks = findPeaks(DF$Adjusted)
    Valleys = findValleys(DF$Adjusted)
    DF[Peaks,"Sell"] = 1
    DF[Peaks,"Buy"] = 0
    DF[Valleys,"Buy"] = 1
    DF[Valleys,"Sell"] = 0
    DF = na.locf(DF)
    
    DF$PR = NA
    for(i in 1:(nrow(DF) - 1)){
      TMP = DF[i+1:nrow(DF),]
      Signal = DF$Buy[i]
      Start_Price = as.numeric(DF[i,Column])
      Stop = ifelse(Signal == 0,which.max(TMP$Buy == 1),which.max(TMP$Buy == 0))
      End_Price = as.numeric(TMP[Stop,Column])
      DF[i,"PR"] = (End_Price - Start_Price)/(Stop*Start_Price) 
    }
    DF = na.omit(DF)
    return(DF)
  }
  
  
  DF = DF %>%
    group_by(Stock) %>%
    nest() %>%
    mutate(Target = map(data,Target_Functions))
  
  return(DF)
}

