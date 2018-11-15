## Smoothing Function
library(tidyverse)
library(stats)
library(xts)
library(dygraphs)
load("//climsidfs07/refeng/1 Ref. Engineering (SH, Scroll & IPD)/13) Analytics/Small Projects/Stocks/Data/NASDAQ Historical.RDATA")

DF = Combined_Results %>%
  group_by(Stock) %>%
  nest()

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