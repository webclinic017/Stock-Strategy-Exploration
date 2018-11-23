# Optimizing spline fit for maximizing return on stock investments
Spline_Par_Optim = function(DF){
  require(optimization)
  require(tidyverse)
  
#################### Sample Data #############################
# load(file = "C:/Users/aayorde/Desktop/NASDAQ Historical.RDATA")
# 
# DF <<- Combined_Results %>%
#   group_by(Stock) %>%
#   filter(Stock == "ALT")
##############################################################
  
## Running the optimization
OptSplineParameter = optimize(PR_Cost_Function, c(0,1), DF,maximum = TRUE,tol = 0.0001)

## Appending Smoothed Spline
Smooth = smooth.spline(DF$Date, DF$Adjusted,spar=OptSplineParameter$maximum)
DF$Adj_Smooth = as.numeric(Smooth[["y"]])
DF = as.data.frame(DF)

Adj_Smooth = DF$Adj_Smooth

return(Adj_Smooth)
}
