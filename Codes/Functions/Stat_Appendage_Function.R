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
  
  DF_Stats = bind_cols(DF,as.data.frame(ADX(HLC(DF_Orig)))) %>%
    bind_cols(as.data.frame(aroon(DF_Orig[,c("High","Low")]))) %>%
    bind_cols(as.data.frame(ATR(DF_Orig))) %>%
    bind_cols(as.data.frame(BBands(HLC(DF_Orig)))) %>%
    bind_cols(as.data.frame(CCI(HLC(DF_Orig)))) %>%
    bind_cols(as.data.frame(chaikinAD(HLC(DF_Orig),DF_Orig$Volume))) %>%
    bind_cols(as.data.frame(chaikinVolatility(DF_Orig))) %>%
    bind_cols(as.data.frame(CLV(DF_Orig))) %>%
    bind_cols(as.data.frame(CMF(DF_Orig,volume = DF_Orig$Volume))) %>%
    bind_cols(as.data.frame(CMO(DF_Orig$Adjusted))) %>%
    bind_cols(as.data.frame(DonchianChannel(DF_Orig[,c("High","Low")]))) %>%
    bind_cols(as.data.frame(DPO(DF_Orig$Adjusted))) %>%
    bind_cols(as.data.frame(DVI(DF_Orig$Adjusted))) %>%
    bind_cols(as.data.frame(EMV(DF_Orig[,c("High","Low")],DF_Orig$Volume))) %>%
    bind_cols(as.data.frame(GMMA(DF_Orig$Adjusted))) %>%
    bind_cols(as.data.frame(as.data.frame(KST(DF_Orig$Adjusted)))) %>%
    bind_cols(as.data.frame(MACD(DF_Orig$Adjusted))) %>%
    bind_cols(as.data.frame(MFI(HLC(DF_Orig),DF_Orig$Volume))) %>%
    bind_cols(as.data.frame(OBV(DF_Orig$Adjusted,DF_Orig$Volume))) %>%
    bind_cols(as.data.frame(PBands(DF_Orig$Adjusted))) %>%
    bind_cols(as.data.frame(RSI(DF_Orig$Adjusted))) %>%
    bind_cols(as.data.frame(SAR(DF_Orig[,c("High","Low")]))) %>%
    bind_cols(as.data.frame(stoch(HLC(DF_Orig)))) %>%
    bind_cols(as.data.frame(TDI(DF_Orig$Adjusted))) %>%
    bind_cols(as.data.frame(TRIX(DF_Orig$Adjusted))) %>%
    bind_cols(as.data.frame(ultimateOscillator(DF_Orig))) %>%
    bind_cols(as.data.frame(VHF(HLC(DF_Orig)))) %>%
    bind_cols(as.data.frame(volatility(DF_Orig))) %>%
    bind_cols(as.data.frame(williamsAD(DF_Orig))) %>%
    bind_cols(as.data.frame(WPR(HLC(DF_Orig))))
  ## End TTR Functions
  ## Additional Technical Indicators
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