## Smoothing Function
library(tidyverse)
library(stats)
library(xts)
library(dygraphs)
library(quantmod)

# load("//climsidfs07/refeng/1 Ref. Engineering (SH, Scroll & IPD)/13) Analytics/Small Projects/Stocks/Data/NASDAQ Historical.RDATA")
# 
# DF = Combined_Results %>%
#   group_by(Stock) %>%
#   nest()

SplineFunction = function(DF){
  spars = seq(.1,1,by=.1) 
  for(i in 1:length(spars)){
    if(nrow(DF)>12){
      DF = DF %>%
        na.omit()
      Smooth = smooth.spline(DF$Date, DF$Close,spar=spars[i])
      Smooth = Smooth[["y"]]
      name = paste("Smooth", i, sep = "_")
      DF$Temp = Smooth
      str_replace(colnames(DF), "Temp", name)
      colnames(DF) = str_replace(colnames(DF), "Temp", name)
    }
  }
  return(DF)
}

# 
# Results = DF %>%
#   mutate(model = map(data, SplineFunction))
# 
# Results2 = Results %>%
#   unnest(model)
# 
# 
# DF_Residuals = Combined %>%
#   group_by(Stock) %>%
#   summarise(Mean_Smooth_1_Error = mean(abs(Close - Smooth_1)/Close),
#             Mean_Smooth_2_Error = mean(abs(Close - Smooth_2)/Close),
#             Mean_Smooth_3_Error = mean(abs(Close - Smooth_3)/Close),
#             Mean_Smooth_4_Error = mean(abs(Close - Smooth_4)/Close),
#             Mean_Smooth_5_Error = mean(abs(Close - Smooth_5)/Close),
#             Mean_Smooth_6_Error = mean(abs(Close - Smooth_6)/Close),
#             Mean_Smooth_7_Error = mean(abs(Close - Smooth_7)/Close),
#             Mean_Smooth_8_Error = mean(abs(Close - Smooth_8)/Close),
#             Mean_Smooth_9_Error = mean(abs(Close - Smooth_9)/Close),
#             Mean_Smooth_10_Error = mean(abs(Close - Smooth_10)/Close))
# 
# 
# 
# DF_Plot = DF %>%
#   select(Date,Close, Smooth_1, Smooth_2, Smooth_3, Smooth_4, Smooth_5, Smooth_6, Smooth_7, Smooth_8)
# RN = DF_Plot[1]
# DF_Plot$Date = NULL
# rownames(DF_Plot) = RN$Date
# DF_Plot = as.xts(DF_Plot)
# 
# 
# dygraph(DF_Plot)
# 
# ########## Test
# 
# 
# load("//climsidfs07/refeng/1 Ref. Engineering (SH, Scroll & IPD)/13) Analytics/Small Projects/Stocks/Data/NASDAQ Historical.RDATA")
# 
# DF = Combined_Results %>%
#   group_by(Stock) %>%
#   nest()
# 
# Results = DF %>%
#   mutate(Spline = map(data, SplineFunction))
# 
# Results2 = Results %>%
#   unnest(Spline) %>%
#   select(Stock, Spline)
# 
# DF = Combined_Results %>%
#   filter(Stock == "AMZN")

SplineFunction = function(DF){
  
  spars = seq(.1,.8,by=.01) 
  for(i in 1:length(spars)){
    if(nrow(DF)>12){
      DF1 = DF %>%
        na.omit()
      Smooth = smooth.spline(DF1$Date, DF1$Adjusted,spar=spars[i])
      Smooth = Smooth[["y"]]
      name = paste("Smooth", i, sep = "_")
      DF1$Temp = Smooth
      # str_replace(colnames(DF), "Temp", name)
      # colnames(DF) = str_replace(colnames(DF), "Temp", name)
      DF1 = data.frame(DF1)
      DF1 = DF1 %>%
        mutate(Buy = NA,
               Sell = NA)
      Peaks = findPeaks(DF1[,"Temp"])-1
      Valleys = findValleys(DF1[,"Temp"])-1
      DF1[Peaks,"Sell"] = 1
      DF1[Peaks,"Buy"] = 0
      DF1[Valleys,"Buy"] = 1
      DF1[Valleys,"Sell"] = 0
      DF1 = na.locf(DF1)
      
      ## Calculating Price Ratio (Percentage Return / Number of Days Invested)
      DF2 = DF1 %>%
        mutate(Indicator = sequence(rle(Buy)$lengths),
               Max = ifelse(lead(Indicator) == 1,Indicator + 1,NA)) %>%
        na.locf(fromLast = T) %>%
        na.omit() %>%
        mutate(Days = Max - Indicator,
               End_Price_Buy = ifelse(Sell == 1 & Indicator == 1,
                                      Adjusted,NA),
               End_Price_Sell = ifelse(Buy == 1 & Indicator == 1,
                                       Adjusted,NA)) %>%
        na.locf(fromLast = T) %>%
        mutate(PR = ifelse(Buy == 1,
                           (End_Price_Buy-Adjusted)/(Days*Adjusted),
                           (End_Price_Sell-Adjusted)/(Days*Adjusted))) %>%
        select(-c(Indicator,Max,End_Price_Buy,End_Price_Sell,Sell))
      Med = DF2 %>%
        filter(Buy ==1 & Max >5) %>%
        summarise(SumPR = sum(PR))
      
      name = paste("Med_Smooth", i, sep = "_")
      rownames(Med) = (name)
      
      if(i == 1){
        Combined = Med
      }
      if(i !=1){
        Combined = rbind(Combined, Med)
      }
      
    }

  }
  Max = which.max(Combined$SumPR)
  
  Max_Spline = spars[Max]
  print(Max_Spline)
  return(Max_Spline)

}


