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

Combined_Results = Combined_Results %>%
  group_by(Stock) %>%
  filter(n() > 500) %>%
  ungroup()

## Looping All Stocks Through Spline Optimization
Tickers = as.character(unique(Combined_Results$Stock))
Total_Results = list()
Window_Results = list()
p = progress_estimated(n = length(Tickers),min_time = 3)
for(i in 1:length(Tickers)){
  p$pause(0.1)$tick()$print()
  Stock_Loop = Tickers[i]
  DF = Combined_Results %>%
    filter(Stock == Stock_Loop) %>%
    mutate(PR_1D = (Adjusted - lag(Adjusted))/Adjusted) %>%
    na.omit()
  
  OptSplineParameter = try(optimize(PR_Cost_Function, 
                                    c(0,1), 
                                    DF = DF,
                                    Column = "PR_1D",
                                    maximum = TRUE,
                                    tol = 0.0001),
                           silent = T)
  if("try-error" %in% class(OptSplineParameter)){
    TMP = data.frame(Stock = paste(Stock_Loop,"Opt_Error"),
                     Opt = NA,
                     Reward = NA,
                     Risk = NA,
                     Ratio = NA,
                     Median_Hold = NA,
                     stringsAsFactors = F)
  }else{
    Risk_Reward = PR_Cost_Function(OptSplineParameter$maximum,
                                   DF = DF,
                                   Column = "PR_1D",
                                   Optimize = F)
    TMP = data.frame(Stock = Stock_Loop,
                     Opt = OptSplineParameter$objective,
                     Reward = Risk_Reward$PR_Reward,
                     Risk = Risk_Reward$PR_Risk,
                     Ratio = Risk_Reward$PR_Reward/-Risk_Reward$PR_Risk,
                     Median_Hold = NA,
                     stringsAsFactors = F)
  }
  
  
  ## Finding Optimal Stat Windows
  Smooth_Data =  try(DF %>%
    mutate(Adj_Smooth = Spline_Par_Optim(.,
                                         Column = "PR_1D")) %>%
    BS_Indicator_Function("Adj_Smooth") %>%
    Stat_Appendage_Function(Column = "PR_1D"))
  
  if("try-error" %in% class(Smooth_Data)){
  }else{
  TMP$Median_Hold = median(Smooth_Data$Max,na.rm = T)
  }
  
  Total_Results[[i]] = TMP
  Window_Results[[i]] = Smooth_Data
}
Output = plyr::ldply(Total_Results,data.frame)
save(Output,
     file = "C://Users//aayorde//desktop//Opt_PR_Results.RDATA")
write.csv(Output,
          file = paste0(getwd(),
                       "//Data//Opt_PR_Results.csv"))

Window_Results = Window_Results[str_detect(sapply(Window_Results,class),"data.frame")] %>%
  plyr::ldply(data.frame)
save(Window_Results,
     file = "C://Users//aayorde//desktop//Window_Results.RDATA")
write.csv(Window_Results,
          row.names = F,
          file = "C://users//aayorde//desktop//Window_Results.csv")


          