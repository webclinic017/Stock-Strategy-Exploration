PR_Cost_Function = function(Parameter,DF = NULL,Column = "Adjusted",Optimize = T){
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
  # load(file = "C:/Users/plfullen/Desktop/NASDAQ Historical.RDATA")
  # DF = Combined_Results %>%
  #   group_by(Stock) %>%
  #   filter(Stock == "AMZN")
  # Column = "Adjusted"
  # Parameter = 0.2
  ##############################################################
  
  # Appending the smoothed spline fit
  DF = na.locf(DF)
  Smooth = smooth.spline(x = seq(1,nrow(DF)),
                         y = DF[[Column]],
                         spar=Parameter)
  DF$Adj_Smooth = as.numeric(Smooth[["y"]])
  
  DF2 = BS_Indicator_Function(DF,Column = "Adj_Smooth")
  
  DF_Risk = DF2 %>%
    filter(Buy == 0,
           Days == Max-1)
  
  PR_Risk = median(DF_Risk$PR) - 2*mad(DF_Risk$PR)
  
  DF_Reward = DF2 %>%
    filter(Buy == 1,
           Max >= 5,
           Days == Max-1)
  
  PR_Opt = sum(DF_Reward$PR)
  PR_Reward= median(DF_Reward$PR) + 2*mad(DF_Reward$PR)
  if(Optimize){
    return(PR_Opt)
  }else{
    return(data.frame(PR_Reward,PR_Risk))
  }
}
  
  