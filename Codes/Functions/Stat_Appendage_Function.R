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
  DF = as.data.frame(DF)
  DF2 = DF
  
  rownames(DF2) = ymd(DF2$Date)
  DF2$Date = NULL
  DF2$Stock = NULL
  DF_Orig = as.xts(DF2)
  
  
  ## ADX Optimization
  # Welles Wilder's Directional Movement Index
  ADX_DF = OPT_Window_TI(DF_Eval = HLC(DF_Orig),
                         DF_Store = DF,
                         Range = c(2,100),
                         TTR_Name = "ADX",
                         Target = "ADX",
                         Col_Names = c("DIP","DIL","DX","ADX"))
  ## Aroon Optimization
  # Indicator attempts to idnetify starting trends
  AROON_DF = OPT_Window_TI(DF_Eval = DF_Orig[,c("High","Low")],
                           DF_Store = DF,
                           Range = c(2,100),
                           TTR_Name = "aroon",
                           Target = "oscillator",
                           Col_Names = c("AROON_Up","AROON_Down","AROON_Osc"))
  ## ATR Optimization
  # Measure of the volatility for a HLC series
  ATR_DF = OPT_Window_TI(DF_Eval = DF_Orig,
                         DF_Store = DF,
                         Range = c(2,250),
                         TTR_Name = "ATR",
                         Target = "atr",
                         Col_Names = c("TR","ATR","trueHigh","trueLow"))
  ## BBands Optimization
  # Compares volatility and Price Levels Over Time
  BBANDS_DF = OPT_Window_TI(DF_Eval = HLC(DF_Orig),
                            DF_Store = DF,
                            Range = c(2,100),
                            TTR_Name = "BBands",
                            Target = "pctB",
                            Col_Names = c("B_Down","B_MAVG","B_Up","B_Pct"))
  ## CCI Optimization
  # Commodity Channel Index attempts to find start/end trends
  CCI_DF = OPT_Window_TI(DF_Eval = HLC(DF_Orig),
                         DF_Store = DF,
                         Range = c(2,100),
                         TTR_Name = "CCI",
                         Target = "cci",
                         Col_Names = "CCI")
  ## chaikinVolatility Optimization
  # Measures the rate of change for the trading range
  CHV_DF = OPT_Window_TI(DF_Eval = HLC(DF_Orig),
                         DF_Store = DF,
                         Range = c(2,100),
                         TTR_Name = "chaikinVolatility",
                         Target = "EMA",
                         Col_Names = "Chaikin_Volatility")
  ## CMF Optimization
  # Chaikin Money Flow Looks at volume fluctionations
  CMF_DF = OPT_Window_TI(DF_Eval = HLC(DF_Orig),
                         DF_Store = DF,
                         TTR_Name = "CMF",
                         Target = "V1",
                         Col_Names = "CMF",
                         Volume = DF_Orig$Volume)
  ## CMO Optimization
  # Chande Momentum Oscillator (Modifed Relative Strength Index)
  CMO_DF = OPT_Window_TI(DF_Eval = DF_Orig$Adjusted,
                         DF_Store = DF,
                         Range = c(2,100),
                         TTR_Name = "CMO",
                         Target = "cmo",
                         Col_Names = "CMO")
  ## DonchianChannel Optimization
  # Created to generate Buy/Sell signals for the turtle system
  DC_DF = OPT_Window_TI(DF_Eval = DF_Orig[,c("High","Low")],
                        DF_Store = DF,
                        Range = c(200,600),
                        TTR_Name = "DonchianChannel",
                        Target = "mid",
                        Col_Names = c("Donchian_High",
                                      "Donchian_Mid",
                                      "Donchian_Low"))
  ## DPO Optimization
  # De-trended Price Oscillator
  DPO_DF = OPT_Window_TI(DF_Eval = DF_Orig$Adjusted,
                         DF_Store = DF,
                         Range = c(2,100),
                         TTR_Name = "DPO",
                         Target = "Adjusted",
                         Col_Names = "DPO")
  ## DVI Optimization
  # Very Smooth Price Oscillator
  DVI_DF = OPT_Window_TI(DF_Eval = DF_Orig$Adjusted,
                         DF_Store = DF,
                         Range = c(200,600),
                         TTR_Name = "DVI",
                         Target = "dvi",
                         Col_Names = c("DVI_MAG","DVI_STR","DVI"))
  ## EMV Optimization
  # Arms' Ease of Movement minimizes days where the security moves easily
  # EMV_DF = OPT_Window_TI(DF_Eval = DF_Orig[,c("High","Low")],
  #                        DF_Store = DF,
  #                        Range = c(2,100),
  #                        TTR_Name = "EMV",
  #                        Target = "maEMV",
  #                        Col_Names =  c("EMV","MA_EMV"),
  #                        Volume = DF_Orig$Volume)
  ## MFI Optimization
  # Money Flow Index is a ratio of positive and negative money flow over time
  MFI_DF = OPT_Window_TI(DF_Eval = HLC(DF_Orig),
                         DF_Store = DF,
                         Range = c(2,100),
                         TTR_Name = "MFI",
                         Target = "mfi",
                         Col_Names =  "MFI",
                         Volume = DF_Orig$Volume)
  ## SMI Optimization
  # Stochastic Momentum Index
  SMI_DF = OPT_Window_TI(DF_Eval = HLC(DF_Orig),
                         DF_Store = DF,
                         Range = c(2,100),
                         TTR_Name = "SMI",
                         Target = "SMI",
                         Col_Names =c("SMI","SMI_Signal"))
  ## PBands Optimization
  # Compares volatility and Price Levels Over Time
  PBANDS_DF = OPT_Window_TI(DF_Eval = DF_Orig$Adjusted,
                            DF_Store = DF,
                            Range = c(2,200),
                            TTR_Name = "PBands",
                            Target = "center",
                            Col_Names = c("P_Down","P_Center","P_Up"))
  ## RSI Optimization
  # Relative Strength Index
  RSI_DF = OPT_Window_TI(DF_Eval = DF_Orig$Adjusted,
                         DF_Store = DF,
                         Range = c(2,100),
                         TTR_Name = "RSI",
                         Target = "rsi",
                         Col_Names = "RSI")
  ## TDI Optimization
  # Trend Detection Index
  TDI_DF = OPT_Window_TI(DF_Eval = DF_Orig$Adjusted,
                         DF_Store = DF,
                         Range = c(2,100),
                         TTR_Name = "TDI",
                         Target = "tdi",
                         Col_Names = c("TDI","DI"))
  ## TRIX Optimization
  # Triple Smoothed Exponential Oscillator
  TRIX_DF = OPT_Window_TI(DF_Eval = DF_Orig$Adjusted,
                          DF_Store = DF,
                          Range = c(2,100),
                          TTR_Name = "TRIX",
                          Target = "TRIX",
                          Col_Names = c("TRIX","TRIX_Signal"))
  ## VHF Optimization
  # Vertical Horizontal Filter
  VHF_DF = OPT_Window_TI(DF_Eval = DF_Orig$Adjusted,
                         DF_Store = DF,
                         Range = c(2,100),
                         TTR_Name = "VHF",
                         Target = "V1",
                         Col_Names = "VHF")
  ## Volatility Optimization
  # Money Flow Index is a ratio of positive and negative money flow over time
  VOLT_DF = OPT_Window_TI(DF_Eval = DF_Orig,
                          DF_Store = DF,
                          Range = c(3,100),
                          TTR_Name = "volatility",
                          Target = "V1",
                          Col_Names = "Volatility")
  ## WPR Optimization
  # William's %R
  WPR_DF = OPT_Window_TI(DF_Eval = HLC(DF_Orig),
                         DF_Store = DF,
                         Range = c(200,600),
                         TTR_Name = "WPR",
                         Target = "Close",
                         Col_Names = "WPR")
  #################### End TTR Window Optimizations ##################
  v1 <- ls(pattern='_DF$')
  Window_TTR<-(do.call(bind_cols,mget(v1)))
  rm(list = v1)
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
  v1 <- ls(pattern='_DF$')
  Fixed_TTR<-(do.call(bind_cols,mget(v1)))
  rm(list = v1)
  ################ Additional Technical Indicators ###########
  Standard_TI = DF %>%
  mutate(EMA50 = EMA(Adjusted,50),
         EMA100 = EMA(Adjusted,100),
         EMA200 = EMA(Adjusted,200),
         MA50 = SMA(Adjusted,50),
         MA100 = SMA(Adjusted,100),
         MA200 = SMA(Adjusted,200),
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
    select(-c(Diff, Gain, Loss))
  
  Combined_Indicators = bind_cols(Standard_TI,
                           Window_TTR,
                           Fixed_TTR)
  
  Final_Result = Combined_Indicators
  
  return(Final_Result)
}  

