
Benchmark = function(DF, start_width = 125, step = 1, expand = T){
  
  library(tidyverse)
  library(zoo)
  library(xts)
  
  
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
    if(expand){
      for(i in 1:floor((nrow(DF)-start_width)/step -12)){
        Test = DF[(nrow(Train)+1):(nrow(Train)+step),]
        
        # TRAIN = Train
        # TEST=Test
        
        
        CV_Results[[i]] = PRED_Func(Train,Test,step)
        Train = rbind(Train,Test)
      }
    }
    if(!expand){
      Iteration = 1
      for(i in 1:floor((nrow(DF)-start_width)/step -12)){
        
        Test = DF[(nrow(Train)+Iteration):(nrow(Train)+Iteration),]
        
        # TRAIN = Train
        # TEST=Test
        
        CV_Results[[i]] = PRED_Func(Train,Test,step)
        
        
        Train = DF[(Iteration+step):(nrow(Train)+Iteration),]
        Iteration = Iteration + 1
      }
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

load("//climsidfs07/refeng/1 Ref. Engineering (SH, Scroll & IPD)/13) Analytics/Small Projects/Stocks/Data/Window_Results.RDATA")

library(tidyverse)
DF = Window_Results %>%
  filter(Stock == "AMZN") %>%
  select(Date, everything(), -Stock) %>%
  na.omit() %>%
  arrange(Date)

### expand = T - expanding train set
#          = F - sliding window            
### start width - for expanding window this is the inital starting window size and it builts upon it.
#               - for sliding window this is the size of train and slide by 1 (the step size)

CV_Results = Benchmark(DF, start_width = 200, expand = F)

RN = CV_Results$Date

library(dygraphs)
library(xts)
Plot = CV_Results
rownames(Plot) = RN
Plot$Date = NULL

dygraph(Plot) %>%
  dySeries("Buy", color = "red", label = "Buy", strokeWidth = 2, strokePattern = "dashed") %>%
  dySeries("Buy_Pred", color = "green", label = "Prediction", strokeWidth = 2, strokePattern = "dashed")

