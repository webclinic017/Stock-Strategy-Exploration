Window_Calc = F
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
require(lubridate)
require(Hmisc)


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
                     Reward_Mean = NA,
                     Reward_SD = NA,
                     Risk_Mean = NA,
                     Risk_SD = NA,
                     Ratio = NA,
                     Median_Hold = NA,
                     Days_History = NA,
                     Price_Growth = NA,
                     Price_Mean = NA,
                     Volume_Mean = NA,
                     Volume_SD = NA,
                     Volume_Norm = NA,
                     Volume_Trajectory = NA,
                     Last_Date = NA,
                     stringsAsFactors = F)
  }else{
    Risk_Reward = PR_Cost_Function(Parameter = OptSplineParameter$maximum,
                                   DF = DF,
                                   Column = "PR_1D",
                                   Optimize = F)
    Days_History = nrow(DF)
    Last_Date = ymd(max(DF$Date))
    ## Weighting Most Recent Dates
    timeElapsed = as.numeric(max(ymd(DF$Date)) - ymd(DF$Date))
    K_constant = 1
    T_constant = 365
    W=K_constant*exp(-timeElapsed/T_constant)
    
    mod.data = DF %>%
      select(Adjusted,Date) %>%
      mutate(Date = as.numeric(ymd(Date)))
    mod = lm(Adjusted~.,
             mod.data,
             weights = W)
    Price_Trajectory = as.numeric(coef(mod)["Date"])
    Price_Mean = weighted.mean(mod.data$Adjusted,W)
    Price_Growth = (((Price_Trajectory/Price_Mean)+1)^365 - 1)*100
    
    Volume_Mean <- round(wtd.mean(DF$Volume,W))
    var <- wtd.var(DF$Volume,W)
    Volume_SD <- sqrt(var)
    Volume_Norm = Volume_Mean/Volume_SD
    
    mod.data = DF %>%
      select(Volume,Date) %>%
      mutate(Date = as.numeric(Date))
    mod = lm(Volume~.,
             mod.data,
             weights = W)
    Volume_Trajectory = as.numeric(coef(mod)["Date"])
    
    TMP = data.frame(Stock = Stock_Loop,
                     Opt = OptSplineParameter$objective,
                     Reward_Mean = Risk_Reward$PR_Reward_Mean,
                     Reward_SD = Risk_Reward$PR_Reward_SD,
                     Risk_Mean= Risk_Reward$PR_Risk_Mean,
                     Risk_SD = Risk_Reward$PR_Risk_SD,
                     Ratio = (Risk_Reward$PR_Reward_Mean + 2*Risk_Reward$PR_Reward_SD)/
                       -(Risk_Reward$PR_Risk_Mean - 2*Risk_Reward$PR_Risk_SD),
                     Median_Hold = NA,
                     Days_History = Days_History,
                     Price_Growth = Price_Growth,
                     Price_Mean = Price_Mean,
                     Volume_Mean = Volume_Mean,
                     Volume_SD = Volume_SD,
                     Volume_Norm = Volume_Norm,
                     Volume_Trajectory = Volume_Trajectory,
                     Last_Date = Last_Date,
                     stringsAsFactors = F)
  }
  
  if(Window_Calc){
    ## Finding Optimal Stat Windows
    Smooth_Data =  try(DF %>%
                         mutate(Adj_Smooth = Spline_Par_Optim(.,
                                                              Column = "PR_1D")) %>%
                         BS_Indicator_Function("Adj_Smooth") %>%
                         Stat_Appendage_Function(Column = "PR_1D"))
  }else{
    ## Finding Optimal Stat Windows
    Smooth_Data =  try(DF %>%
                         mutate(Adj_Smooth = Spline_Par_Optim(.,
                                                              Column = "PR_1D")) %>%
                         BS_Indicator_Function("Adj_Smooth"))
  }
  
  if("try-error" %in% class(Smooth_Data)){
  }else{
    TMP$Median_Hold = median(Smooth_Data$Max[Smooth_Data$Buy == 1],na.rm = T)
  }
  
  Total_Results[[i]] = TMP
  if(Window_Calc){
    Window_Results[[i]] = Smooth_Data
  }
}
Pool_Results = plyr::ldply(Total_Results,data.frame)
save(Pool_Results,
     file = "C://Users//aayorde//desktop//Opt_PR_Results.RDATA")


if(Window_Calc){
  Window_Results = Window_Results[str_detect(sapply(Window_Results,class),"data.frame")] %>%
    plyr::ldply(data.frame)
  save(Window_Results,
       file = "C://Users//aayorde//desktop//Window_Results.RDATA")
}



          