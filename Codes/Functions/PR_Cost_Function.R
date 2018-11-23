PR_Cost_Function = function(Parameter,DF = NULL,Optimize = T){
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
  Smooth = smooth.spline(DF$Date, DF$Adjusted,spar=Parameter)
  Smooth_Auto = smooth.spline(DF$Date,DF$Adjusted)
  DF$Adj_Smooth = as.numeric(Smooth[["y"]])
  DF$Adj_Auto_Smooth = as.numeric(Smooth_Auto[["y"]])
  DF = as.data.frame(DF)
  ggplot(DF[1:365,],aes(Date)) + 
    geom_point(aes(y = Adj_Smooth,color = "Opt"),alpha = 0.25) + 
    geom_point(aes(y = Adj_Auto_Smooth,color = "Auto"),alpha = 0.25) + 
    geom_point(aes(y = Adjusted,color = "Base"),alpha = 0.25)
  
  DF2 = BS_Indicator_Function(DF,Column = "Adj_Smooth")
  
  ggplot(DF2,aes(Date,Adj_Smooth,color = factor(Buy))) + 
    geom_point()
  
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
  
  