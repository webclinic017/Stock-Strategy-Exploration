PR_Cost_Function = function(DF,Column = "Fit"){
  require(tidyverse)
  require(quantmod)
  require(Hmisc)
  
  #### Notes ####
  # Optimization should be limitied to smoothing parameters in the range (0,1]
  # 
  # PR should be maximized, the way it is summarised currently (sum) may not be the best method
  #
  # My understanding is that R optimizers work on the first input to a function, DF will need assigned
  
  ###############
  
  ########################## Sample Data #######################
  # load(file = "C:/Users/plfullen/Desktop/NASDAQ Historical.RDATA")
  # DF = Combined_Results %>%
  #   group_by(Stock) %>%
  #   filter(Stock == "AMZN")
  # Column = "Adjusted"
  # Parameter = 0.2
  ##############################################################
  
  # Appending the smoothed spline fit
  DF = na.locf(DF)
  
  DF2 = BS_Indicator_Function(DF,Column = Column)
  
  DF_Risk = DF2 %>%
    filter(Buy == 0)
  
  ## Weighted Mean Time Decreasing
  timeElapsed = as.numeric(max(ymd(DF_Risk$Date)) - ymd(DF_Risk$Date))
  K_constant = 1
  T_constant = 365
  W=K_constant*exp(-timeElapsed/T_constant)
  PR_Risk_Mean <- wtd.mean(DF_Risk$PR_1D,W)
  var <- wtd.var(DF_Risk$PR_1D,W)
  PR_Risk_SD <- sqrt(var)
  
  
  DF_Reward = DF2 %>%
    filter(Buy == 1,
           Max >= 5)
  
  ## Weighted Mean Time Decreasing
  timeElapsed = as.numeric(max(ymd(DF_Reward$Date)) - ymd(DF_Reward$Date))
  K_constant = 1
  T_constant = 365
  W=K_constant*exp(-timeElapsed/T_constant)
  PR_Reward_Mean <- wtd.mean(DF_Reward$PR_1D,W)
  var <- wtd.var(DF_Reward$PR_1D,W)
  PR_Reward_SD <- sqrt(var)
    
  return(data.frame(PR_Reward_Mean,
                      PR_Reward_SD,
                      PR_Risk_Mean,
                      PR_Risk_SD))
}
  
  