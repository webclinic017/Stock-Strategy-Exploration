Stat_Appendage_Function = function(DF){
########################## Sample Data #######################
  # load(file = "//climsidfs07/RefEng/1 Ref. Engineering (SH, Scroll & IPD)/13) Analytics/Small Projects/Stocks/Data/NASDAQ Historical.RDATA")
  # DF = Combined_Results %>%
  #   group_by(Stock) %>%
  #   filter(Stock == "AMZN")
##############################################################
  # ## Loading Project Functions
  # sourceDir <- function(path, trace = TRUE, ...) {
  #   for (nm in list.files(path, pattern = "\\.[RrSsQq]$")) {
  #     if(trace) cat(nm,":")           
  #     source(file.path(path, nm), ...)
  #     if(trace) cat("\n")
  #   }
  # }
  # sourceDir(paste0(getwd(),'/Codes/Functions/'))
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
  ADX_DF = ADX(DF_Orig[,c("High","Low","Close")]) %>%
    as.data.frame() %>%
    setNames(.,c("DIP","DIL","DX","ADX"))

  ## Aroon Optimization
  # Indicator attempts to idnetify starting trends
  # AROON_Osc > 0 Indicates Bullish Move
  # AROON_Osc < 0 Indicates Bearish Move
  AROON_DF = aroon( DF_Orig[,c("High","Low")]) %>%
    as.data.frame() %>%
    setNames(.,c("AROON_Up","AROON_Down","AROON_Osc"))
  
  # ## ATR Optimization
  # # Measure of the volatility for a HLC series
  ATR_DF = ATR(DF_Orig[,c("High","Low","Close")]) %>%
    as.data.frame() %>%
    setNames(.,c("TR","ATR","trueHigh","trueLow"))
  
  ## BBands Optimization
  # Compares volatility and Price Levels Over Time
  # Price Close to B_Down Indicates Upward Movement
  # Price Close to B_Up Idicates Downward Movement
  BBANDS_DF = BBands(DF_Orig[,c("High","Low","Close")]) %>%
    as.data.frame() %>%
    setNames(.,c("B_Down","B_MAVG","B_Up","B_Pct"))
  
  ## CCI Optimization
  # Commodity Channel Index attempts to find start/end trends
  CCI_DF = CCI(DF_Orig[,c("High","Low","Close")]) %>%
    as.data.frame() %>%
    setNames(.,"CCI")

  ## chaikinVolatility Optimization
  # Measures the rate of change for the trading range
  CHV_DF = chaikinVolatility(DF_Orig[,c("High","Low","Close")]) %>%
    as.data.frame() %>%
    setNames(.,"Chaikin_Volatility")

  
  ## CMO Optimization
  # Chande Momentum Oscillator (Modifed Relative Strength Index)
  CMO_DF = CMO(DF_Orig$Close) %>%
    as.data.frame() %>%
    setNames(.,"CMO")

  # ## DonchianChannel Optimization
  # # Created to generate Buy/Sell signals for the turtle system
  DC_DF = DonchianChannel(DF_Orig[,c("High","Low")]) %>%
    as.data.frame() %>%
    setNames(.,c("Donchian_High","Donchian_Mid","Donchian_Low"))
  
  ## DVI Optimization
  # Very Smooth Price Oscillator
  DVI_DF = DVI(DF_Orig$Close) %>%
    as.data.frame() %>%
    setNames(.,c("DVI_MAG","DVI_STR","DVI"))
  
  ## SMI Optimization
  # Stochastic Momentum Index
  # > 40 Bullish, < -40 Bearish
  SMI_DF = SMI(DF_Orig[,c("High","Low","Close")]) %>%
    as.data.frame() %>%
    setNames(.,c("SMI","SMI_Signal"))
  
  ## PBands Optimization
  # Compares volatility and Price Levels Over Time
  PBANDS_DF = PBands(DF_Orig$Close) %>%
    as.data.frame() %>%
    setNames(.,c("P_Down","P_Center","P_Up"))
  
  ## RSI Optimization
  # Relative Strength Index
  RSI_DF = RSI(DF_Orig$Close) %>%
    as.data.frame() %>%
    setNames(.,"RSI")
  
  ## TDI Optimization
  # Trend Detection Index
  # +- TDI Indicates +- Trend
  # Buy if TDI and DI are Positive
  # Sell if TDI is positive while DI is negative
  TDI_DF = TDI(DF_Orig$Close) %>%
    as.data.frame() %>%
    setNames(.,c("TDI","DI"))
  
  ## TRIX Optimization
  # Triple Smoothed Exponential Oscillator
  TRIX_DF = TRIX(DF_Orig$Close) %>%
    as.data.frame() %>%
    setNames(.,c("TRIX","TRIX_Signal"))
  
  ## VHF Optimization
  # Vertical Horizontal Filter
  VHF_DF = VHF(DF_Orig$Close) %>%
    as.data.frame() %>%
    setNames(.,"VHF")
  
  ## Volatility Optimization
  VOLT1_DF = volatility(DF_Orig[,c("Open","High","Low","Close")]) %>%
    as.data.frame() %>%
    setNames(.,"Volatility_Close")
  VOLT2_DF = volatility(DF_Orig[,c("Open","High","Low","Close")],calc = "garman.klass") %>%
    as.data.frame() %>%
    setNames(.,"Volatility_Klass")
  VOLT3_DF = volatility(DF_Orig[,c("Open","High","Low","Close")],calc = "parkinson") %>%
    as.data.frame() %>%
    setNames(.,"Volatility_Parkinson")
  VOLT4_DF = volatility(DF_Orig[,c("Open","High","Low","Close")],calc = "rogers.satchell") %>%
    as.data.frame() %>%
    setNames(.,"Volatility_Satchell")
  VOLT5_DF = volatility(DF_Orig[,c("Open","High","Low","Close")], calc = "gk.yz") %>%
    as.data.frame() %>%
    setNames(.,"Volatility_Garman")
  VOLT6_DF = volatility(DF_Orig[,c("Open","High","Low","Close")],calc = "yang.zhang") %>%
    as.data.frame() %>%
    setNames(.,"Volatility_Zhang")
  
  ## WPR Optimization
  # William's %R
  WPR_DF = WPR(DF_Orig[,c("High","Low","Close")]) %>%
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
  MACD_DF = as.data.frame(MACD(DF_Orig$Close))
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
  UO_DF = as.data.frame(ultimateOscillator(DF_Orig[,c("High","Low","Close")]))
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
  dplyr::mutate(EMA50 = EMA(Close,50),
         EMA100 = EMA(Close,100),
         EMA200 = EMA(Close,200),
         MA50 = SMA(Close,50),
         MA100 = SMA(Close,100),
         MA200 = SMA(Close,200),
         DEMA50 = DEMA(Close,50),
         DEMA100 = DEMA(Close,100),
         DEMA200 = DEMA(Close,200),
         WMA50 = WMA(Close,50),
         WMA100 = WMA(Close,100),
         WMA200 = WMA(Close,200),
         EVWMA50 = EVWMA(Close,Volume,50),
         EVWMA100 = EVWMA(Close,Volume,100),
         EVWMA200 = EVWMA(Close,Volume,200),
         ZLEMA50 = ZLEMA(Close,50),
         ZLEMA100 = ZLEMA(Close,100),
         ZLEMA200 = ZLEMA(Close,200),
         VWAP50 = VWAP(Close,Volume,50),
         VWAP100 = VWAP(Close,Volume,100),
         VWAP200 = VWAP(Close,Volume,200),
         HMA50 = HMA(Close,50),
         HMA100 = HMA(Close,100),
         HMA200 = HMA(Close,200))
  
  Combined_Indicators = bind_cols(Standard_TI,
                           Window_TTR,
                           Fixed_TTR) %>%
    na.omit()
  
  return(Combined_Indicators)
}  

