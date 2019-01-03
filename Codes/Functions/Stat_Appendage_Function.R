Stat_Appendage_Function = function(DF,Column = "Adjusted"){
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
  DF = as.data.frame(DF)
  DF$Column = DF[[Column]]
  DF2 = DF
  
  rownames(DF2) = ymd(DF2$Date)
  DF2$Date = NULL
  DF2$Stock = NULL
  DF2$Market_Status = NULL
  DF_Orig = as.xts(DF2)
  
  
  # ## ADX Optimization
  # # Welles Wilder's Directional Movement Index
  # ADX_DF = OPT_Window_TI(DF_Eval = HLC(DF_Orig),
  #                        DF_Store = DF,
  #                        Range = c(2,100),
  #                        TTR_Name = "ADX",
  #                        Target = "ADX",
  #                        Col_Names = c("DIP","DIL","DX","ADX"),
  #                        Column = Column)
  ## Aroon Optimization
  # Indicator attempts to idnetify starting trends
  AROON_DF = OPT_Window_TI(DF_Eval = DF_Orig[,c("High","Low")],
                           DF_Store = DF,
                           Range = c(2,100),
                           TTR_Name = "aroon",
                           Target = "oscillator",
                           Col_Names = c("AROON_Up","AROON_Down","AROON_Osc"),
                           Column = Column)
  # ## ATR Optimization
  # # Measure of the volatility for a HLC series
  # ATR_DF = OPT_Window_TI(DF_Eval = DF_Orig,
  #                        DF_Store = DF,
  #                        Range = c(2,250),
  #                        TTR_Name = "ATR",
  #                        Target = "atr",
  #                        Col_Names = c("TR","ATR","trueHigh","trueLow"),                          
  #                        Column = Column)
  ## BBands Optimization
  # Compares volatility and Price Levels Over Time
  BBANDS_DF = OPT_Window_TI(DF_Eval = HLC(DF_Orig),
                            DF_Store = DF,
                            Range = c(2,100),
                            TTR_Name = "BBands",
                            Target = "pctB",
                            Col_Names = c("B_Down","B_MAVG","B_Up","B_Pct"),                          
                            Column = Column)
  BBANDS_DF = BBANDS_DF %>%
    select(BBands_Window,B_Pct)
  ## CCI Optimization
  # Commodity Channel Index attempts to find start/end trends
  CCI_DF = OPT_Window_TI(DF_Eval = HLC(DF_Orig),
                         DF_Store = DF,
                         Range = c(2,100),
                         TTR_Name = "CCI",
                         Target = "cci",
                         Col_Names = "CCI",
                         Column = Column)
  ## chaikinVolatility Optimization
  # Measures the rate of change for the trading range
  CHV_DF = OPT_Window_TI(DF_Eval = HLC(DF_Orig),
                         DF_Store = DF,
                         Range = c(2,100),
                         TTR_Name = "chaikinVolatility",
                         Target = "EMA",
                         Col_Names = "Chaikin_Volatility",
                         Column = Column)
  ## CMF Optimization
  # Chaikin Money Flow Looks at volume fluctionations
  CMF_DF = OPT_Window_TI(DF_Eval = HLC(DF_Orig),
                         DF_Store = DF,
                         TTR_Name = "CMF",
                         Target = "V1",
                         Col_Names = "CMF",
                         Volume = DF_Orig$Volume,
                         Column = Column)
  ## CMO Optimization
  # Chande Momentum Oscillator (Modifed Relative Strength Index)
  CMO_DF = OPT_Window_TI(DF_Eval = DF_Orig$Column,
                         DF_Store = DF,
                         Range = c(2,100),
                         TTR_Name = "CMO",
                         Target = "cmo",
                         Col_Names = "CMO",
                         Column = Column)
  # ## DonchianChannel Optimization
  # # Created to generate Buy/Sell signals for the turtle system
  # DC_DF = OPT_Window_TI(DF_Eval = DF_Orig[,c("High","Low")],
  #                       DF_Store = DF,
  #                       Range = c(200,600),
  #                       TTR_Name = "DonchianChannel",
  #                       Target = "mid",
  #                       Col_Names = c("Donchian_High",
  #                                     "Donchian_Mid",
  #                                     "Donchian_Low"),
  #                       Column = Column)
  ## DPO Optimization
  # De-trended Price Oscillator
  DPO_DF = OPT_Window_TI(DF_Eval = DF_Orig$Column,
                         DF_Store = DF,
                         Range = c(2,100),
                         TTR_Name = "DPO",
                         Target = "Adjusted",
                         Col_Names = "DPO",
                         Column = Column)
  ## DVI Optimization
  # Very Smooth Price Oscillator
  DVI_DF = OPT_Window_TI(DF_Eval = DF_Orig$Column,
                         DF_Store = DF,
                         Range = c(200,600),
                         TTR_Name = "DVI",
                         Target = "dvi",
                         Col_Names = c("DVI_MAG","DVI_STR","DVI"),
                         Column = Column)
  ## EMV Optimization
  # Arms' Ease of Movement minimizes days where the security moves easily
  # EMV_DF = try(OPT_Window_TI(DF_Eval = DF_Orig[,c("High","Low")],
  #                        DF_Store = DF,
  #                        Range = c(2,100),
  #                        TTR_Name = "EMV",
  #                        Target = "maEMV",
  #                        Col_Names =  c("EMV","MA_EMV"),
  #                        Volume = DF_Orig$Volume,
  #                        Column = Column))
  # if(class(EMV_DF) %in% "try-error"){rm(EMV_DF)}
    
  ## MFI Optimization
  # Money Flow Index is a ratio of positive and negative money flow over time
  MFI_DF = OPT_Window_TI(DF_Eval = HLC(DF_Orig),
                         DF_Store = DF,
                         Range = c(2,100),
                         TTR_Name = "MFI",
                         Target = "mfi",
                         Col_Names =  "MFI",
                         Volume = DF_Orig$Volume,
                         Column = Column)
  ## SMI Optimization
  # Stochastic Momentum Index
  SMI_DF = OPT_Window_TI(DF_Eval = HLC(DF_Orig),
                         DF_Store = DF,
                         Range = c(2,100),
                         TTR_Name = "SMI",
                         Target = "SMI",
                         Col_Names =c("SMI","SMI_Signal"),
                         Column = Column)
  ## PBands Optimization
  # Compares volatility and Price Levels Over Time
  PBANDS_DF = OPT_Window_TI(DF_Eval = DF_Orig$Column,
                            DF_Store = DF,
                            Range = c(2,200),
                            TTR_Name = "PBands",
                            Target = "center",
                            Col_Names = c("P_Down","P_Center","P_Up"),
                            Column = Column)
  ## RSI Optimization
  # Relative Strength Index
  RSI_DF = OPT_Window_TI(DF_Eval = DF_Orig$Column,
                         DF_Store = DF,
                         Range = c(2,100),
                         TTR_Name = "RSI",
                         Target = "rsi",
                         Col_Names = "RSI",
                         Column = Column)
  ## TDI Optimization
  # Trend Detection Index
  TDI_DF = OPT_Window_TI(DF_Eval = DF_Orig$Column,
                         DF_Store = DF,
                         Range = c(2,100),
                         TTR_Name = "TDI",
                         Target = "tdi",
                         Col_Names = c("TDI","DI"),
                         Column = Column)
  ## TRIX Optimization
  # Triple Smoothed Exponential Oscillator
  TRIX_DF = OPT_Window_TI(DF_Eval = DF_Orig$Column,
                          DF_Store = DF,
                          Range = c(2,100),
                          TTR_Name = "TRIX",
                          Target = "TRIX",
                          Col_Names = c("TRIX","TRIX_Signal"),
                          Column = Column)
  ## VHF Optimization
  # Vertical Horizontal Filter
  VHF_DF = OPT_Window_TI(DF_Eval = DF_Orig$Column,
                         DF_Store = DF,
                         Range = c(2,100),
                         TTR_Name = "VHF",
                         Target = "V1",
                         Col_Names = "VHF",
                         Column = Column)
  ## Volatility Optimization
  # Money Flow Index is a ratio of positive and negative money flow over time
  VOLT_DF = OPT_Window_TI(DF_Eval = DF_Orig,
                          DF_Store = DF,
                          Range = c(3,100),
                          TTR_Name = "volatility",
                          Target = "V1",
                          Col_Names = "Volatility",
                          Column = Column)
  ## WPR Optimization
  # William's %R
  WPR_DF = OPT_Window_TI(DF_Eval = HLC(DF_Orig),
                         DF_Store = DF,
                         Range = c(200,600),
                         TTR_Name = "WPR",
                         Target = "Close",
                         Col_Names = "WPR",
                         Column = Column)
  #################### End TTR Window Optimizations ##################
  v1 <- ls(pattern='_DF$')
  Window_TTR<-(do.call(bind_cols,mget(v1)))
  rm(list = v1)
  #################### TTR Non-Window Functions ######################
  # ## Chaikin AD
  # # Measure of Money Flow
  # chaikinAD_DF = as.data.frame(chaikinAD(HLC(DF_Orig),DF_Orig$Volume))
  # colnames(chaikinAD_DF) = "chaikin_AD"
  ## CLV
  # Close Location Value relates close value to trading range
  CLV_DF = as.data.frame(CLV(HLC(DF_Orig)))
  colnames(CLV_DF) = "CLV"
  ## GMMA
  # Guppy Multiple Moving Averages
  GMMA_DF = as.data.frame(GMMA(DF_Orig$Column))
  ## KST
  # Know Sure Thing smooth summed rate of change indicator
  KST_DF = as.data.frame(KST(DF_Orig$Adjusted))
  colnames(KST_DF) = c("KST","KST_Signal")
  ## MACD
  # MACD Oscillator
  MACD_DF = as.data.frame(MACD(DF_Orig$Column))
  colnames(MACD_DF) = c("MACD","MACD_Signal")
  # ## OBV
  # # On Balance Volume
  # OBV_DF = as.data.frame(OBV(DF_Orig$Adjusted,DF_Orig$Volume))
  # colnames(OBV_DF) = "OBV"
  ## SAR
  # # Parabolic Stop-and-Reverse
  # SAR_DF = as.data.frame(SAR(DF_Orig[,c("High","Low")]))
  # colnames(SAR_DF) = "SAR"
  ## stoch
  # Stochastic Oscillator
  stoch_DF = as.data.frame(stoch(HLC(DF_Orig)))
  colnames(stoch_DF) = c("Fast_K","Slow_K","Slow_D")
  ## ultimateOscillator
  # Capture momentum across different time frames
  UO_DF = as.data.frame(ultimateOscillator(HLC(DF_Orig)))
  colnames(UO_DF) = "Ultimate_Oscillator"
  # ## williamsAD
  # WAD_DF = as.data.frame(williamsAD(HLC(DF_Orig)))
  # colnames(WAD_DF) = "WAD"
  ######################## End TTR Functions #################
  v1 <- ls(pattern='_DF$')
  Fixed_TTR<-(do.call(bind_cols,mget(v1)))
  rm(list = v1)
  ################ Additional Technical Indicators ###########
  Standard_TI = DF %>%
  mutate(EMA50 = EMA(Column,50),
         EMA100 = EMA(Column,100),
         EMA200 = EMA(Column,200),
         MA50 = SMA(Column,50),
         MA100 = SMA(Column,100),
         MA200 = SMA(Column,200),
         DEMA50 = DEMA(Column,50),
         DEMA100 = DEMA(Column,100),
         DEMA200 = DEMA(Column,200),
         WMA50 = WMA(Column,50),
         WMA100 = WMA(Column,100),
         WMA200 = WMA(Column,200),
         # EVWMA50 = EVWMA(Column,Volume,50),
         # EVWMA100 = EVWMA(Column,Volume,100),
         # EVWMA200 = EVWMA(Column,Volume,200),
         ZLEMA50 = ZLEMA(Column,50),
         ZLEMA100 = ZLEMA(Column,100),
         ZLEMA200 = ZLEMA(Column,200),
         # VWAP50 = VWAP(Column,Volume,50),
         # VWAP100 = VWAP(Column,Volume,100),
         # VWAP200 = VWAP(Column,Volume,200),
         HMA50 = HMA(Column,50),
         HMA100 = HMA(Column,100),
         HMA200 = HMA(Column,200)) %>%
    select(-Column)
  
  Combined_Indicators = bind_cols(Standard_TI,
                           Window_TTR,
                           Fixed_TTR)
  
  Final_Result = Combined_Indicators
  
  return(Final_Result)
}  

