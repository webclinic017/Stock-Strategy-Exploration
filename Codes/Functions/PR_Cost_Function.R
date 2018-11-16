PR_Cost_Function = function(Parameter = NULL,DF){
  require(tidyverse)
  require(quantmod)
  
  #### Notes ####
  # Optimization should be limitied to smoothing parameters in the range (0,1]
  # 
  # PR should be maximized, the way it is summarised currently (sum) may not be the best method
  #
  # My understanding is that R optimizers work on the first input to a function, DF will need assigned
  
  ###############
  
  ########################## Sample Data #######################
  # load(file = "//climsidfs07/RefEng/1 Ref. Engineering (SH, Scroll & IPD)/13) Analytics/Small Projects/Stocks/Data/NASDAQ Historical.RDATA")
  # DF = Combined_Results %>%
  #   group_by(Stock) %>%
  #   filter(Stock == "AMZN")
  # Column = "Adjusted"
  # Parameter = 0.5
  ##############################################################
  
  # Appending the smoothed spline fit
  Smooth = smooth.spline(DF$Date, DF$Adjusted,spar=Parameter)
  DF$Adj_Smooth = as.numeric(Smooth[["y"]])
  DF = as.data.frame(DF)
  
  DF2 = BS_Indicator_Function(DF,Column = "Adj_Smooth")
  
  DF2 = DF2 %>%
    filter(Buy == 1,
           Max >= 5)

  PR = sum(DF2$PR)

  return(PR)
}