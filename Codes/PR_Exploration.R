## Loading Project Functions
sourceDir <- function(path, trace = TRUE, ...) {
  for (nm in list.files(path, pattern = "\\.[RrSsQq]$")) {
    if(trace) cat(nm,":")           
    source(file.path(path, nm), ...)
    if(trace) cat("\n")
  }
}
sourceDir(paste0(getwd(),'/Codes/Functions/'))

## Loading Historical Stock Data
load(file = "C:/Users/aayorde/Desktop/NASDAQ Historical.RDATA")
require(tidyverse)
require(optimization)

## Looping All Stocks Through Spline Optimization
Tickers = as.character(unique(Combined_Results$Stock))
Total_Results = list()
Window_Results = list()
p = progress_estimated(n = length(Tickers),min_time = 3)
for(i in 1:length(Tickers)){
  p$pause(0.1)$tick()$print()
  Stock_Loop = Tickers[i]
  DF = Combined_Results %>%
    filter(Stock == Stock_Loop)
  
  OptSplineParameter = try(optimize(PR_Cost_Function, 
                                    c(0,1), 
                                    DF,
                                    maximum = TRUE,
                                    tol = 0.0001),
                           silent = T)
  if("try-error" %in% class(OptSplineParameter)){
    TMP = data.frame(Stock = paste(Stock_Loop,"Opt_Error"),
                     Opt = 0,
                     Reward = 0,
                     Risk = 0,
                     Ratio = 0,
                     stringsAsFactors = F)
  }else{
    Risk_Reward = PR_Cost_Function(OptSplineParameter$maximum,
                                   DF,
                                   Optimize = F)
    TMP = data.frame(Stock = Stock_Loop,
                     Opt = OptSplineParameter$objective,
                     Reward = Risk_Reward$PR_Reward,
                     Risk = Risk_Reward$PR_Risk,
                     Ratio = Risk_Reward$PR_Reward/-Risk_Reward$PR_Risk,
                     stringsAsFactors = F)
  }
  Total_Results[[i]] = TMP
  
  ## Finding Optimal Stat Windows
  Smooth_Data =  try(DF %>%
    mutate(Adj_Smooth = Spline_Par_Optim(.)) %>%
    BS_Indicator_Function("Adj_Smooth") %>%
    Stat_Appendage_Function() %>%
    group_by(Stock) %>%
    select(contains("Window")) %>%
    summarise_all(mean))
  Window_Results[[i]] = Smooth_Data
}
Output = plyr::ldply(Total_Results,data.frame)
write.csv(Output,
          file = paste0(getwd(),
                       "//Data//Opt_PR_Results.csv"))
Output2 = Window_Results
save(Output2,
     file = "C://Users//aayorde//desktop//Window_Results.RDATA")

          