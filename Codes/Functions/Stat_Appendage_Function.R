Stat_Appendage_Function = function(DF,Timeframe = 10){
  require(tidyverse)
  require(lubridate)
  require(quantmod)
  require(TTR)
  
  ## Applying TTR Functions
  DF = as.data.frame(DF)[!duplicated(DF$Date),]
  DF2 = DF %>%
    select(-Stock,-Date)
  rownames(DF2) = ymd(DF$Date)
  DF_Orig = as.xts(DF2)
  
  
  # ## ADX Optimization
  # Welles Wilder's Directional Movement Index
  # ADX < 20 Indicates Weak Trend (Ranging)
  # ADX > 50 Indicates Strong Trend (Trending)
  ADX_DF = ADX(DF_Orig[,c("High","Low","Close")],
               n = Timeframe) %>%
    as.data.frame() %>%
    setNames(.,c("DIP","DIL","DX","ADX"))

  ## Aroon Optimization
  # Indicator attempts to idnetify starting trends
  # AROON_Osc > 0 Indicates Bullish Move
  # AROON_Osc < 0 Indicates Bearish Move
  AROON_DF = aroon(DF_Orig[,c("High","Low")],
                   n = round(Timeframe*20/14)) %>%
    as.data.frame() %>%
    setNames(.,c("AROON_Up","AROON_Down","AROON_Osc"))
  
  # ## ATR Optimization
  # # Measure of the volatility for a HLC series
  ATR_DF = ATR(DF_Orig[,c("High","Low","Close")],
               n = Timeframe) %>%
    as.data.frame() %>%
    setNames(.,c("TR","ATR","trueHigh","trueLow"))
  
  ## BBands Optimization
  # Compares volatility and Price Levels Over Time
  # Price Close to B_Down Indicates Upward Movement
  # Price Close to B_Up Idicates Downward Movement
  BBANDS_DF = BBands(DF_Orig[,c("High","Low","Close")],
                     n = round(Timeframe*20/14)) %>%
    as.data.frame() %>%
    setNames(.,c("B_Down","B_MAVG","B_Up","B_Pct"))
  
  ## CCI Optimization
  # Commodity Channel Index attempts to find start/end trends
  CCI_DF = CCI(DF_Orig[,c("High","Low","Close")],
               n = round(Timeframe*20/14)) %>%
    as.data.frame() %>%
    setNames(.,"CCI")

  ## chaikinVolatility Optimization
  # Measures the rate of change for the trading range
  CHV_DF = chaikinVolatility(DF_Orig[,c("High","Low","Close")],
                             n = round(Timeframe*10/14)) %>%
    as.data.frame() %>%
    setNames(.,"Chaikin_Volatility")

  
  ## CMO Optimization
  # Chande Momentum Oscillator (Modifed Relative Strength Index)
  CMO_DF = CMO(DF_Orig$Close,
               n = Timeframe) %>%
    as.data.frame() %>%
    setNames(.,"CMO")

  # ## DonchianChannel Optimization
  # # Created to generate Buy/Sell signals for the turtle system
  DC_DF = DonchianChannel(DF_Orig[,c("High","Low")],
                          n = round(Timeframe*10/14)) %>%
    as.data.frame() %>%
    setNames(.,c("Donchian_High","Donchian_Mid","Donchian_Low"))
  
  ## DVI Optimization
  # Very Smooth Price Oscillator
  # DVI_DF = DVI(DF_Orig$Close) %>%
  #   as.data.frame() %>%
  #   setNames(.,c("DVI_MAG","DVI_STR","DVI"))
  
  ## SMI Optimization
  # Stochastic Momentum Index
  # > 40 Bullish, < -40 Bearish
  SMI_DF = SMI(DF_Orig[,c("High","Low","Close")],
               n = round(Timeframe*13/14),
               nFast = ceiling(3/14*Timeframe),
               nSlow = round(25/14*Timeframe),
               nsig = round(9/14*Timeframe)) %>%
    as.data.frame() %>%
    setNames(.,c("SMI","SMI_Signal"))
  
  ## PBands Optimization
  # Compares volatility and Price Levels Over Time
  PBANDS_DF = PBands(DF_Orig$Close,
                     n = round(20/14*Timeframe),
                     nFast = ceiling(2/14*Timeframe)) %>%
    as.data.frame() %>%
    setNames(.,c("P_Down","P_Center","P_Up"))
  
  ## RSI Optimization
  # Relative Strength Index
  RSI_DF = RSI(DF_Orig$Close,
               n = Timeframe) %>%
    as.data.frame() %>%
    setNames(.,"RSI")
  
  ## TDI Optimization
  # Trend Detection Index
  # +- TDI Indicates +- Trend
  # Buy if TDI and DI are Positive
  # Sell if TDI is positive while DI is negative
  TDI_DF = TDI(DF_Orig$Close,
               n = round(Timeframe*20/14)) %>%
    as.data.frame() %>%
    setNames(.,c("TDI","DI"))
  
  ## TRIX Optimization
  # Triple Smoothed Exponential Oscillator
  TRIX_DF = TRIX(DF_Orig$Close,
                 n = round(Timeframe*20/14),
                 nSig = round(Timeframe*9/14)) %>%
    as.data.frame() %>%
    setNames(.,c("TRIX","TRIX_Signal"))
  
  ## VHF Optimization
  # Vertical Horizontal Filter
  VHF_DF = VHF(DF_Orig$Close,
               n = round(Timeframe*28/14)) %>%
    as.data.frame() %>%
    setNames(.,"VHF")
  
  ## Volatility Optimization
  VOLT1_DF = volatility(DF_Orig[,c("Open","High","Low","Close")],
                        n = round(Timeframe*10/14)) %>%
    as.data.frame() %>%
    setNames(.,"Volatility_Close")
  VOLT2_DF = volatility(DF_Orig[,c("Open","High","Low","Close")],
                        calc = "garman.klass",
                        n = round(Timeframe*10/14)) %>%
    as.data.frame() %>%
    setNames(.,"Volatility_Klass")
  VOLT3_DF = volatility(DF_Orig[,c("Open","High","Low","Close")],
                        calc = "parkinson",
                        n = round(Timeframe*10/14)) %>%
    as.data.frame() %>%
    setNames(.,"Volatility_Parkinson")
  VOLT4_DF = volatility(DF_Orig[,c("Open","High","Low","Close")],
                        calc = "rogers.satchell",
                        n = round(Timeframe*10/14)) %>%
    as.data.frame() %>%
    setNames(.,"Volatility_Satchell")
  VOLT5_DF = volatility(DF_Orig[,c("Open","High","Low","Close")], 
                        calc = "gk.yz",
                        n = round(Timeframe*10/14)) %>%
    as.data.frame() %>%
    setNames(.,"Volatility_Garman")
  VOLT6_DF = volatility(DF_Orig[,c("Open","High","Low","Close")],
                        calc = "yang.zhang",
                        n = round(Timeframe*10/14)) %>%
    as.data.frame() %>%
    setNames(.,"Volatility_Zhang")
  
  ## WPR Optimization
  # William's %R
  WPR_DF = WPR(DF_Orig[,c("High","Low","Close")],
               n = Timeframe) %>%
    as.data.frame() %>%
    setNames("WPR")
  
  #################### End TTR Window Optimizations ##################
  v1 <- ls(pattern='_DF$')
  Window_TTR<-(do.call(bind_cols,mget(v1)))
  rm(list = v1)
  #################### TTR Non-Window Functions ######################
  # ## Chaikin AD
  # Measure of Money Flow
  chaikinAD_DF = as.data.frame(chaikinAD(DF_Orig[,c("High","Low","Close")],
                                         DF_Orig$Volume))
  colnames(chaikinAD_DF) = "chaikin_AD"
  ## CLV
  # Close Location Value relates close value to trading range
  CLV_DF = as.data.frame(CLV(DF_Orig[,c("High","Low","Close")]))
  colnames(CLV_DF) = "CLV"
  ## GMMA
  # Guppy Multiple Moving Averages
  GMMA_DF = as.data.frame(GMMA(DF_Orig$Close))
  ## MACD
  # MACD Oscillator
  MACD_DF = as.data.frame(MACD(DF_Orig$Close,
                               nFast = round(Timeframe*12/14),
                               nSlow = round(round(Timeframe*26/14)),
                               nSig = round(Timeframe*9/14)))
  colnames(MACD_DF) = c("MACD","MACD_Signal")
  ## SAR
  # # Parabolic Stop-and-Reverse
  SAR_DF = as.data.frame(SAR(DF_Orig[,c("High","Low")]))
  colnames(SAR_DF) = "SAR"
  ## stoch
  # Stochastic Oscillator
  stoch_DF = as.data.frame(stoch(DF_Orig[,c("High","Low","Close")]))
  colnames(stoch_DF) = c("Fast_K","Slow_K","Slow_D")
  ## ultimateOscillator
  # Capture momentum across different time frames
  UO_DF = as.data.frame(ultimateOscillator(DF_Orig[,c("High","Low","Close")],
                                           n = round(Timeframe*c(7,14,28)/14)))
  colnames(UO_DF) = "Ultimate_Oscillator"
  # ## williamsAD
  WAD_DF = as.data.frame(williamsAD(DF_Orig[,c("High","Low","Close")]))
  colnames(WAD_DF) = "WAD"
  ######################## End TTR Functions #################
  v1 <- ls(pattern='_DF$')
  Fixed_TTR<-(do.call(bind_cols,mget(v1)))
  rm(list = v1)
  ################ Additional Technical Indicators ###########
  Standard_TI = DF %>%
  dplyr::mutate(EMA1 = EMA(Close,round(Timeframe*50/14)),
         EMA2 = EMA(Close,n = round(Timeframe*100/14)),
         MA1 = SMA(Close,n = round(Timeframe*50/14)),
         MA2 = SMA(Close,n = round(Timeframe*100/14)),
         DEMA1 = DEMA(Close,n = round(Timeframe*50/14)),
         WMA1 = WMA(Close,n = round(Timeframe*50/14)),
         WMA2 = WMA(Close,n = round(Timeframe*100/14)),
         EVWMA1 = EVWMA(price = Close,volume = Volume,n = round(Timeframe*50/14)),
         EVWMA2 = EVWMA(price = Close,volume = Volume,n = round(Timeframe*100/14)),
         ZLEMA1 = ZLEMA(x = Close,n = round(Timeframe*50/14)),
         ZLEMA2 = ZLEMA(x = Close,n = round(Timeframe*100/14)),
         VWAP1 = VWAP(price = Close,volume = Volume,n = round(Timeframe*50/14)),
         VWAP2 = VWAP(price = Close,volume = Volume,n = round(Timeframe*100/14)))
  
  Combined_Indicators = bind_cols(Standard_TI,
                           Window_TTR,
                           Fixed_TTR) %>%
    na.omit()
  
  return(Combined_Indicators)
}  

