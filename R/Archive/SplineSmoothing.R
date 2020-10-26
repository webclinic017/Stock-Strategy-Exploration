## Smoothing Function
require(tidyverse)
require(quantmod)
require(stats)
library(tidyverse)
library(stats)
library(xts)
library(dygraphs)
library(quantmod)
load("//climsidfs07/refeng/1 Ref. Engineering (SH, Scroll & IPD)/13) Analytics/Small Projects/Stocks/Data/NASDAQ Historical.RDATA")

DF = Combined_Results %>%
  group_by(Stock) %>%
  nest()


DF= Combined_Results %>%
  filter(Stock == "AMZN")

SplineFunction = function(DF){
  spars = seq(.1,1,by=.1) 
  for(i in 1:length(spars)){
    if(nrow(DF)>12){
      DF = DF %>%
        na.omit()
      Smooth = smooth.spline(DF$Date, DF$Adjusted,spar=spars[i])
      Smooth = Smooth[["y"]]
      name = paste("Smooth", i, sep = "_")
      DF$Temp = Smooth
      # str_replace(colnames(DF), "Temp", name)
      # colnames(DF) = str_replace(colnames(DF), "Temp", name)
    
      DF = DF %>%
        mutate(Buy = NA,
               Sell = NA)
      Peaks = findPeaks(DF[,"Temp"])-1
      Valleys = findValleys(DF[,"Temp"])-1
      DF[Peaks,"Sell"] = 1
      DF[Peaks,"Buy"] = 0
      DF[Valleys,"Buy"] = 1
      DF[Valleys,"Sell"] = 0
      Test = DF %>%
        do(na.locf(.))
      
      Test = read.zoo(DF, split = 1, index =2)
      
      DF[] <- lapply(DF, na.locf0)
      
      
      
      ## Calculating Price Ratio (Percentage Return / Number of Days Invested)
      DF2 = DF %>%
        mutate(Indicator = sequence(rle(Buy)$lengths),
               Max = ifelse(lead(Indicator) == 1,Indicator + 1,NA)) 
      DF2[] <- lapply(DF2, na.locf0(fromLast = T))
      DF2 = DF2 %>%
        na.omit() %>%
        mutate(Days = Max - Indicator,
               End_Price_Buy = ifelse(Sell == 1 & Indicator == 1,
                                      Adjusted,NA),
               End_Price_Sell = ifelse(Buy == 1 & Indicator == 1,
                                       Adjusted,NA))
      DF2[] <- lapply(DF2, na.locf0(fromLast = T))
      DF2 = DF2 %>%
        mutate(PR = ifelse(Buy == 1,
                           (End_Price_Buy-Adjusted)/(Days*Adjusted),
                           (End_Price_Sell-Adjusted)/(Days*Adjusted))) %>%
        select(-c(Indicator,Max,End_Price_Buy,End_Price_Sell,Sell))
    }
  }
  

Results = DF %>%
  mutate(model = map(data, SplineFunction))

Results2 = Results %>%
  unnest(model)


DF_Residuals = Combined %>%
  group_by(Stock) %>%
  summarise(Mean_Smooth_1_Error = mean(abs(Close - Smooth_1)/Close),
            Mean_Smooth_2_Error = mean(abs(Close - Smooth_2)/Close),
            Mean_Smooth_3_Error = mean(abs(Close - Smooth_3)/Close),
            Mean_Smooth_4_Error = mean(abs(Close - Smooth_4)/Close),
            Mean_Smooth_5_Error = mean(abs(Close - Smooth_5)/Close),
            Mean_Smooth_6_Error = mean(abs(Close - Smooth_6)/Close),
            Mean_Smooth_7_Error = mean(abs(Close - Smooth_7)/Close),
            Mean_Smooth_8_Error = mean(abs(Close - Smooth_8)/Close),
            Mean_Smooth_9_Error = mean(abs(Close - Smooth_9)/Close),
            Mean_Smooth_10_Error = mean(abs(Close - Smooth_10)/Close))



DF_Plot = DF %>%
  select(Date,Close, Smooth_1, Smooth_2, Smooth_3, Smooth_4, Smooth_5, Smooth_6, Smooth_7, Smooth_8, Smooth_9, Smooth_10)
RN = DF_Plot[1]
DF_Plot$Date = NULL
rownames(DF_Plot) = RN$Date
DF_Plot = as.xts(DF_Plot)


dygraph(DF_Plot) 