load("//climsidfs07/refeng/1 Ref. Engineering (SH, Scroll & IPD)/13) Analytics/Small Projects/Stocks/Data/Window_Results.RDATA")


DF = Window_Results %>%
  filter(Stock == "AMZN") %>%
  select(Date, everything(), -Stock) %>%
  na.omit() %>%
  arrange(Date)
 # rownames(DF) = NULL
 
# DF = DF2[1:300,]

start_width = 125
step = 1

Benchmark = function(DF, start_width = 125, step = 1, expand = T){

  library(tidyverse)
  library(zoo)
  library(xts)
  library(forecast)
  
  
  # DF=DF %>%
  #   select(Date, Total_Quantity) %>%
  #   arrange(Date)
  # 
  # Year = DF[(nrow(DF)-11):nrow(DF),]
  # year(Year$Date) = year(Year$Date) + 1
  # Year$Total_Quantity = 0
  # DF = rbind(DF, Year)
  # 
  # DF = as.xts(x = DF$Total_Quantity,order.by = DF$Date)
  

  
  
  PRED_Func = function(TRAIN,TEST,H){
    suppressWarnings(expr)
    Model = glm(Buy ~ .-Date,
               TRAIN,
               family = 'binomial')
    Pred = predict(Model,
                   newdata = TEST, 
                   type = "response")
    
    RESULTS = TEST %>%
      mutate(Buy_Pred = round(Pred,3)) %>%
      select(Date, Buy, Buy_Pred)
    
    return(RESULTS)
  }
  
  Walk_FWD_CV = function(DF,start_width,step,expand = T){
print("Test")
    Train = DF[1:start_width,]
    CV_Results = list() 
    for(i in 1:floor((nrow(DF)-start_width)/step -12)){
      Test = DF[(nrow(Train)+1):(nrow(Train)+step),]
    
      # TRAIN = Train
      # TEST=Test

            
      CV_Results[[i]] = PRED_Func(Train,Test,step)
      Train = rbind(Train,Test)
    
      # if(!expand){
      #   DF = DF[(step+1):length(DF),]
      #   Train = Train[(step+1):length(Train),]
      # }
    }
    CV_Results = plyr::ldply(CV_Results,data.frame)
    return(CV_Results)
  }

  CV = Walk_FWD_CV(DF = DF,
                   start_width = start_width,
                   step = step,
                   expand = expand)
  
  return(CV)
}




RN = CV$Date

library(dygraphs)
library(xts)
Plot = CV
rownames(Plot) = RN
Plot$Date = NULL

dygraph(Plot) %>%
  dySeries("Buy", color = "red", label = "Buy", strokeWidth = 2, strokePattern = "dashed") %>%
  dySeries("Buy_Pred", color = "green", label = "Prediction", strokeWidth = 2, strokePattern = "dashed")

