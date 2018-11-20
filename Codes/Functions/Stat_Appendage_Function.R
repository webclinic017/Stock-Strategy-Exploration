Stat_Appendage_Function = function(DF){
########################## Sample Data #######################
  # load(file = "//climsidfs07/RefEng/1 Ref. Engineering (SH, Scroll & IPD)/13) Analytics/Small Projects/Stocks/Data/NASDAQ Historical.RDATA")
  # DF = Combined_Results %>%
  #   group_by(Stock) %>%
  #   filter(Stock == "AMZN")
##############################################################
require(tidyverse)
require(lubridate)
require(quantmod)
require(QuantTools)
require(TTR)

  ## Applying TTR Functions
  DF = as.data.frame(DF)
  DF2 = DF
  
  rownames(DF2) = ymd(DF2$Date)
  DF2$Date = NULL
  DF2$Stock = NULL
  DF_Orig = as.xts(DF2)
  
  
  ## ADX Optimization
  # Welles Wilder's Directional Movement Index
  ADX_Opt = function(n){
    Result = as.data.frame(ADX(HLC(DF_Orig),n = n)) 
    TMP = bind_cols(DF,Result) %>%
      BS_Indicator_Function(Column = "Adjusted") %>%
      na.omit()
    Mod = lm(Buy~ADX,
              data = TMP)
    Score = summary(Mod)$adj.r.squared 
  }
  Best = data.frame(Score = 0,n = 0)
  for(i in 2:100){
    Results = ADX_Opt(i)
    if(Results > Best$Score){
      Best$Score = Results
      Best$n = i
    }
  }
  ADX_DF = as.data.frame(ADX(HLC(DF_Orig),n = Best$n))
  colnames(ADX_DF) = c("DIP","DIL","DX","ADX")
  ## Aroon Optimization
  # Indicator attempts to idnetify starting trends
  AROON_Opt = function(n){
    Result = as.data.frame(aroon(DF_Orig[,c("High","Low")],n = n)) 
    TMP = bind_cols(DF,Result) %>%
      BS_Indicator_Function(Column = "Adjusted") %>%
      na.omit()
    Mod = lm(Buy~oscillator,
             data = TMP)
    Score = summary(Mod)$adj.r.squared 
  }
  Best = data.frame(Score = 0,n = 0)
  for(i in 2:100){
    Results = AROON_Opt(i)
    if(Results > Best$Score){
      Best$Score = Results
      Best$n = i
    }
  }
  AROON_DF = as.data.frame(aroon(DF_Orig[,c("High","Low")],n = Best$n))
  colnames(AROON_DF) = c("AROON_Up","AROON_Down","AROON_Osc")
  ## ATR Optimization
  # Measure of the volatility for a HLC series
  ATR_Opt = function(n){
    Result = as.data.frame(ATR(DF_Orig,n))
    TMP = bind_cols(DF,Result) %>%
      BS_Indicator_Function(Column = "Adjusted") %>%
      na.omit()
    Mod = lm(Buy~atr,
             data = TMP)
    Score = summary(Mod)$adj.r.squared 
  }
  Best = data.frame(Score = 0,n = 0)
  for(i in 2:250){
    Results = ATR_Opt(i)
    if(Results > Best$Score){
      Best$Score = Results
      Best$n = i
    }
  }
  ATR_DF = as.data.frame(ATR(DF_Orig,n = Best$n))
  colnames(ATR_DF) = c("TR","ATR","trueHigh","trueLow")
  ## BBands Optimization
  # Compares volatility and Price Levels Over Time
  BBANDS_Opt = function(n){
    Result = as.data.frame(BBands(HLC(DF_Orig),n = n))
    TMP = bind_cols(DF,Result) %>%
      BS_Indicator_Function(Column = "Adjusted") %>%
      na.omit()
    Mod = lm(Buy~pctB,
             data = TMP)
    Score = summary(Mod)$adj.r.squared 
  }
  Best = data.frame(Score = 0,n = 0)
  for(i in 2:100){
    Results = BBANDS_Opt(i)
    if(Results > Best$Score){
      Best$Score = Results
      Best$n = i
    }
  }
  BBANDS_DF = as.data.frame(BBands(HLC(DF_Orig),n = Best$n)) 
  colnames(BBANDS_DF) = c("B_Down","B_MAVG","B_Up","B_Pct")
  ## CCI Optimization
  # Commodity Channel Index attempts to find start/end trends
  CCI_Opt = function(n){
    Result = as.data.frame(CCI(HLC(DF_Orig),n = n))
    TMP = bind_cols(DF,Result) %>%
      BS_Indicator_Function(Column = "Adjusted") %>%
      na.omit()
    Mod = lm(Buy~cci,
             data = TMP)
    Score = summary(Mod)$adj.r.squared 
  }
  Best = data.frame(Score = 0,n = 0)
  for(i in 2:100){
    Results = CCI_Opt(i)
    if(Results > Best$Score){
      Best$Score = Results
      Best$n = i
    }
  }
  CCI_DF = as.data.frame(CCI(HLC(DF_Orig),n = Best$n))
  colnames(CCI_DF) = "CCI"
  ## chaikinVolatility Optimization
  # Measures the rate of change for the trading range
  chaikinVolatility_Opt = function(n){
    Result = as.data.frame(chaikinVolatility(DF_Orig,n = n))
    TMP = bind_cols(DF,Result) %>%
      BS_Indicator_Function(Column = "Adjusted") %>%
      na.omit()
    Mod = lm(Buy~EMA,
             data = TMP)
    Score = summary(Mod)$adj.r.squared 
  }
  Best = data.frame(Score = 0,n = 0)
  for(i in 2:100){
    Results = chaikinVolatility_Opt(i)
    if(Results > Best$Score){
      Best$Score = Results
      Best$n = i
    }
  }
  chaikinVolatility_DF = as.data.frame(chaikinVolatility(DF_Orig,n = Best$n))
  colnames(chaikinVolatility_DF) = "Chaikin_Volatility"
  ## CMF Optimization
  # Chaikin Money Flow Looks at volume fluctionations
  CMF_Opt = function(n){
    Result = as.data.frame(CMF(HLC(DF_Orig),DF_Orig$Volume,n = n))
    TMP = bind_cols(DF,Result) %>%
      BS_Indicator_Function(Column = "Adjusted") %>%
      na.omit()
    Mod = lm(Buy~V1,
             data = TMP)
    Score = summary(Mod)$adj.r.squared 
  }
  Best = data.frame(Score = 0,n = 0)
  for(i in 2:100){
    Results = CMF_Opt(i)
    if(Results > Best$Score){
      Best$Score = Results
      Best$n = i
    }
  }
  CMF_DF = as.data.frame(CMF(HLC(DF_Orig),DF_Orig$Volume,n = Best$n))
  colnames(CMF_DF) = "CMF"
  ## CMO Optimization
  # Chande Momentum Oscillator (Modifed Relative Strength Index)
  CMO_Opt = function(n){
    Result = as.data.frame(CMO(DF_Orig$Adjusted,n = n))
    TMP = bind_cols(DF,Result) %>%
      BS_Indicator_Function(Column = "Adjusted") %>%
      na.omit()
    Mod = lm(Buy~cmo,
             data = TMP)
    Score = summary(Mod)$adj.r.squared 
  }
  Best = data.frame(Score = 0,n = 0)
  for(i in 2:100){
    Results = CMO_Opt(i)
    if(Results > Best$Score){
      Best$Score = Results
      Best$n = i
    }
  }
  CMO_DF = as.data.frame(CMO(DF_Orig$Adjusted,n = Best$n))
  colnames(CMO_DF) = "CMO"
  ## DonchianChannel Optimization
  # Created to generate Buy/Sell signals for the turtle system
  DonchianChannel_Opt = function(n){
    Result = as.data.frame(DonchianChannel(DF_Orig[,c("High","Low")],n = n))
    TMP = bind_cols(DF,Result) %>%
      BS_Indicator_Function(Column = "Adjusted") %>%
      na.omit()
    Mod = lm(Buy~mid,
             data = TMP)
    Score = summary(Mod)$adj.r.squared 
  }
  Best = data.frame(Score = 0,n = 0)
  for(i in 2:500){
    Results = DonchianChannel_Opt(i)
    if(Results > Best$Score){
      Best$Score = Results
      Best$n = i
    }
  }
  DonchianChannel_DF = as.data.frame(DonchianChannel(DF_Orig[,c("High","Low")],n = Best$n))
  colnames(DonchianChannel_DF) = c("Donchian_High",
                                   "Donchian_Mid",
                                   "Donchian_Low")
  ## DPO Optimization
  # De-trended Price Oscillator
  DPO_Opt = function(n){
    Result = as.data.frame(DPO(DF_Orig$Adjusted,n = n))
    TMP = bind_cols(DF,Result) %>%
      BS_Indicator_Function(Column = "Adjusted") %>%
      na.omit()
    Mod = lm(Buy~Adjusted,
             data = TMP)
    Score = summary(Mod)$adj.r.squared 
  }
  Best = data.frame(Score = 0,n = 0)
  for(i in 2:100){
    Results = DPO_Opt(i)
    if(Results > Best$Score){
      Best$Score = Results
      Best$n = i
    }
  }
  DPO_DF = as.data.frame(DPO(DF_Orig$Adjusted,n = Best$n))
  colnames(DPO_DF) = "DPO"
  ## DVI Optimization
  # Very Smooth Price Oscillator
  DVI_Opt = function(n){
    Result = as.data.frame(DVI(DF_Orig$Adjusted,n = n))
    TMP = bind_cols(DF,Result) %>%
      BS_Indicator_Function(Column = "Adjusted") %>%
      na.omit()
    Mod = lm(Buy~dvi,
             data = TMP)
    Score = summary(Mod)$adj.r.squared 
  }
  Best = data.frame(Score = 0,n = 0)
  for(i in 200:600){
    Results = DVI_Opt(i)
    if(Results > Best$Score){
      Best$Score = Results
      Best$n = i
    }
  }
  DVI_DF = as.data.frame(DVI(DF_Orig$Adjusted,n = Best$n))
  colnames(DVI_DF) = c("DVI_MAG","DVI_STR","DVI")
  ## EMV Optimization
  # Arms' Ease of Movement minimizes days where the security moves easily
  EMV_Opt = function(n){
    Result = as.data.frame(EMV(DF_Orig[,c("High","Low")],DF_Orig$Volume,n = n))
    TMP = bind_cols(DF,Result) %>%
      BS_Indicator_Function(Column = "Adjusted") %>%
      na.omit()
    Mod = lm(Buy~maEMV,
             data = TMP)
    Score = summary(Mod)$adj.r.squared 
  }
  Best = data.frame(Score = 0,n = 0)
  for(i in 2:100){
    Results = EMV_Opt(i)
    if(Results > Best$Score){
      Best$Score = Results
      Best$n = i
    }
  }
  EMV_DF = as.data.frame(EMV(DF_Orig[,c("High","Low")],DF_Orig$Volume,n = Best$n))
  colnames(EMV_DF) = c("EMV","MA_EMV")
  ## MFI Optimization
  # Money Flow Index is a ratio of positive and negative money flow over time
  MFI_Opt = function(n){
    Result = as.data.frame(MFI(HLC(DF_Orig),DF_Orig$Volume,n = n))
    TMP = bind_cols(DF,Result) %>%
      BS_Indicator_Function(Column = "Adjusted") %>%
      na.omit()
    Mod = lm(Buy~mfi,
             data = TMP)
    Score = summary(Mod)$adj.r.squared 
  }
  Best = data.frame(Score = 0,n = 0)
  for(i in 2:100){
    Results = MFI_Opt(i)
    if(Results > Best$Score){
      Best$Score = Results
      Best$n = i
    }
  }
  MFI_DF = as.data.frame(MFI(HLC(DF_Orig),DF_Orig$Volume,n = Best$n))
  colnames(MFI_DF) = "MFI"
  ## SMI Optimization
  # Stochastic Momentum Index
  SMI_Opt = function(n){
    Result = as.data.frame(SMI(HLC(DF_Orig),n = n))
    TMP = bind_cols(DF,Result) %>%
      BS_Indicator_Function(Column = "Adjusted") %>%
      na.omit()
    Mod = lm(Buy~SMI,
             data = TMP)
    Score = summary(Mod)$adj.r.squared 
  }
  Best = data.frame(Score = 0,n = 0)
  for(i in 2:100){
    Results = SMI_Opt(i)
    if(Results > Best$Score){
      Best$Score = Results
      Best$n = i
    }
  }
  SMI_DF = as.data.frame(SMI(HLC(DF_Orig),n = Best$n))
  colnames(SMI_DF) = c("SMI","SMI_Signal")
  ## PBands Optimization
  # Compares volatility and Price Levels Over Time
  PBANDS_Opt = function(n){
    Result = as.data.frame(PBands(DF_Orig$Adjusted,n = n))
    TMP = bind_cols(DF,Result) %>%
      BS_Indicator_Function(Column = "Adjusted") %>%
      na.omit()
    Mod = lm(Buy~center,
             data = TMP)
    Score = summary(Mod)$adj.r.squared 
  }
  Best = data.frame(Score = 0,n = 0)
  for(i in 2:200){
    Results = PBANDS_Opt(i)
    if(Results > Best$Score){
      Best$Score = Results
      Best$n = i
    }
  }
  PBANDS_DF = as.data.frame(PBands(DF_Orig$Adjusted,n = Best$n))
  colnames(PBANDS_DF) = c("P_Down","P_Center","P_Up")
  ## RSI Optimization
  # Relative Strength Index
  RSI_Opt = function(n){
    Result = as.data.frame(RSI(DF_Orig$Adjusted,n = n))
    TMP = bind_cols(DF,Result) %>%
      BS_Indicator_Function(Column = "Adjusted") %>%
      na.omit()
    Mod = lm(Buy~rsi,
             data = TMP)
    Score = summary(Mod)$adj.r.squared 
  }
  Best = data.frame(Score = 0,n = 0)
  for(i in 2:100){
    Results = RSI_Opt(i)
    if(Results > Best$Score){
      Best$Score = Results
      Best$n = i
    }
  }
  RSI_DF = as.data.frame(RSI(DF_Orig$Adjusted,n = Best$n))
  colnames(RSI_DF) = "RSI"
  ## TDI Optimization
  # Trend Detection Index
  TDI_Opt = function(n){
    Result = as.data.frame(TDI(DF_Orig$Adjusted,n = n))
    TMP = bind_cols(DF,Result) %>%
      BS_Indicator_Function(Column = "Adjusted") %>%
      na.omit()
    Mod = lm(Buy~tdi,
             data = TMP)
    Score = summary(Mod)$adj.r.squared 
  }
  Best = data.frame(Score = 0,n = 0)
  for(i in 2:100){
    Results = TDI_Opt(i)
    if(Results > Best$Score){
      Best$Score = Results
      Best$n = i
    }
  }
  TDI_DF = as.data.frame(TDI(DF_Orig$Adjusted,n = Best$n))
  colnames(TDI_DF) = c("TDI","DI")
  ## TRIX Optimization
  # Triple Smoothed Exponential Oscillator
  TRIX_Opt = function(n){
    Result = as.data.frame(TRIX(DF_Orig$Adjusted,n = n))
    TMP = bind_cols(DF,Result) %>%
      BS_Indicator_Function(Column = "Adjusted") %>%
      na.omit()
    Mod = lm(Buy~TRIX,
             data = TMP)
    Score = summary(Mod)$adj.r.squared 
  }
  Best = data.frame(Score = 0,n = 0)
  for(i in 2:100){
    Results = TRIX_Opt(i)
    if(Results > Best$Score){
      Best$Score = Results
      Best$n = i
    }
  }
  TRIX_DF = as.data.frame(TRIX(DF_Orig$Adjusted,n = Best$n))
  colnames(TRIX_DF) = c("TRIX","TRIX_Signal")
  ## VHF Optimization
  # Vertical Horizontal Filter
  VHF_Opt = function(n){
    Result = as.data.frame(VHF(DF_Orig$Adjusted,n = n))
    TMP = bind_cols(DF,Result) %>%
      BS_Indicator_Function(Column = "Adjusted") %>%
      na.omit()
    Mod = lm(Buy~V1,
             data = TMP)
    Score = summary(Mod)$adj.r.squared 
  }
  Best = data.frame(Score = 0,n = 0)
  for(i in 2:100){
    Results = VHF_Opt(i)
    if(Results > Best$Score){
      Best$Score = Results
      Best$n = i
    }
  }
  VHF_DF = as.data.frame(VHF(DF_Orig$Adjusted,n = Best$n))
  colnames(VHF_DF) = "VHF"
  ## Volatility Optimization
  # Money Flow Index is a ratio of positive and negative money flow over time
  Volatility_Opt = function(n){
    Result = as.data.frame(volatility(DF_Orig,n = n))
    TMP = bind_cols(DF,Result) %>%
      BS_Indicator_Function(Column = "Adjusted") %>%
      na.omit()
    Mod = lm(Buy~V1,
             data = TMP)
    Score = summary(Mod)$adj.r.squared 
  }
  Best = data.frame(Score = 0,n = 0)
  for(i in 3:100){
    Results = Volatility_Opt(i)
    if(Results > Best$Score){
      Best$Score = Results
      Best$n = i
    }
  }
  Volatility_DF = as.data.frame(volatility(DF_Orig,n = Best$n))
  colnames(Volatility_DF) = "Volatility"
  ## WPR Optimization
  # William's %R
  WPR_Opt = function(n){
    Result = as.data.frame(WPR(HLC(DF_Orig),n = n))
    TMP = bind_cols(DF,Result) %>%
      BS_Indicator_Function(Column = "Adjusted") %>%
      na.omit()
    Mod = lm(Buy~Close,
             data = TMP)
    Score = summary(Mod)$adj.r.squared 
  }
  Best = data.frame(Score = 0,n = 0)
  for(i in 2:500){
    Results = WPR_Opt(i)
    if(Results > Best$Score){
      Best$Score = Results
      Best$n = i
    }
  }
  WPR_DF = as.data.frame(WPR(HLC(DF_Orig),n = Best$n))
  colnames(WPR_DF) = "WPR"
  #################### End TTR Window Optimizations ##################
  
  #################### TTR Non-Window Functions ######################
  ## Chaikin AD
  # Measure of Money Flow
  chaikinAD_DF = as.data.frame(chaikinAD(HLC(DF_Orig),DF_Orig$Volume))
  colnames(chaikinAD_DF) = "chaikin_AD"
  ## CLV
  # Close Location Value relates close value to trading range
  CLV_DF = as.data.frame(CLV(DF_Orig))
  colnames(CLV_DF) = "CLV"
  ## GMMA
  # Guppy Multiple Moving Averages
  GMMA_DF = as.data.frame(GMMA(DF_Orig$Adjusted))
  ## KST
  # Know Sure Thing smooth summed rate of change indicator
  KST_DF = as.data.frame(KST(DF_Orig$Adjusted))
  colnames(KST_DF) = c("KST","KST_Signal")
  ## MACD
  # MACD Oscillator
  MACD_DF = as.data.frame(MACD(DF_Orig$Adjusted))
  colnames(MACD_DF) = c("MACD","MACD_Signal")
  ## OBV
  # On Balance Volume
  OBV_DF = as.data.frame(OBV(DF_Orig$Adjusted,DF_Orig$Volume))
  colnames(OBV_DF) = "OBV"
  ## SAR
  # Parabolic Stop-and-Reverse
  SAR_DF = as.data.frame(SAR(DF_Orig[,c("High","Low")]))
  colnames(SAR_DF) = "SAR"
  ## stoch
  # Stochastic Oscillator
  stoch_DF = as.data.frame(stoch(HLC(DF_Orig)))
  colnames(stoch_DF) = c("Fast_K","Slow_K","Slow_D")
  ## ultimateOscillator
  # Capture momentum across different time frames
  UO_DF = as.data.frame(ultimateOscillator(DF_Orig))
  colnames(UO_DF) = "Ultimate_Oscillator"
  ## williamsAD
  WAD_DF = as.data.frame(williamsAD(DF_Orig))
  colnames(WAD_DF) = "WAD"
  ######################## End TTR Functions #################
  
  ################ Additional Technical Indicators ###########
  DF_Add = DF_Stats %>%
  mutate(EMA50 = ema(Adjusted,50),
         EMA100 = ema(Adjusted,100),
         EMA200 = ema(Adjusted,200),
         MA50 = sma(Adjusted,50),
         MA100 = sma(Adjusted,100),
         MA200 = sma(Adjusted,200),
         DEMA50 = DEMA(Adjusted,50),
         DEMA100 = DEMA(Adjusted,100),
         DEMA200 = DEMA(Adjusted,200),
         WMA50 = WMA(Adjusted,50),
         WMA100 = WMA(Adjusted,100),
         WMA200 = WMA(Adjusted,200),
         EVWMA50 = EVWMA(Adjusted,Volume,50),
         EVWMA100 = EVWMA(Adjusted,Volume,100),
         EVWMA200 = EVWMA(Adjusted,Volume,200),
         ZLEMA50 = ZLEMA(Adjusted,50),
         ZLEMA100 = ZLEMA(Adjusted,100),
         ZLEMA200 = ZLEMA(Adjusted,200),
         VWAP50 = VWAP(Adjusted,Volume,50),
         VWAP100 = VWAP(Adjusted,Volume,100),
         VWAP200 = VWAP(Adjusted,Volume,200),
         HMA50 = HMA(Adjusted,50),
         HMA100 = HMA(Adjusted,100),
         HMA200 = HMA(Adjusted,200),
         Diff = (Adjusted - lag(Adjusted,1))/lag(Adjusted,1),
         Gain = ifelse(Diff >= 0, Diff, NA),
         Loss = ifelse(Diff < 0, -Diff, NA),
         AVG_Gain_14 = rollapply(Gain,14,mean,na.rm = T, fill = NA, align = "right"),
         AVG_Loss_14 = rollapply(Loss,14,mean,na.rm = T,fill = NA,align = "right")) %>%
    select(-c(Diff, Gain, Loss)) %>%
    na.omit()
  
  return(DF_Add)
}  