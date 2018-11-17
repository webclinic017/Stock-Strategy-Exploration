Stat_Appendage_Function = function(DF){
########################## Sample Data #######################
  # load(file = "//climsidfs07/RefEng/1 Ref. Engineering (SH, Scroll & IPD)/13) Analytics/Small Projects/Stocks/Data/NASDAQ Historical.RDATA")
  DF = Combined_Results %>%
    group_by(Stock) %>%
    filter(Stock == "AMZN")
##############################################################
require(tidyverse)
require(lubridate)
require(quantmod)
require(QuantTools)
require(TTR)

  
  ## Applying TTR Functions
  DF = as.data.frame(DF)
  Stock = unique(DF$Stock)
  
  rownames(DF) = ymd(DF$Date)
  DF$Date = NULL
  DF$Stock = NULL
  DF_Orig = as.xts(DF)
  
  DF_ADX = ADX(HLC(DF_Orig))
  DF_aroon = aroon(DF_Orig[,c("High","Low")])
  DF_ATR = ATR(DF_Orig)
  DF_BBands = BBands(HLC(DF_Orig))
  DF_CCI = CCI(HLC(DF_Orig))
  DF_chaikinAD = chaikinAD(HLC(DF_Orig),DF_Orig$Volume)
  DF_chaikinVolatility = chaikinVolatility(DF_Orig)
  DF_CLV = CLV(DF_Orig)
  DF_CMF = CMF(DF_Orig,volume = DF_Orig$Volume)
  DF_CMO = CMO(DF_Orig$Adjusted)
  DF_DonchianChannel = DonchianChannel(DF_Orig[,c("High","Low")])
  DF_DPO = DPO(DF_Orig$Adjusted)
  DF_DVI = DVI(DF_Orig$Adjusted)
  DF_EMV = EMV(DF_Orig[,c("High","Low")],DF_Orig$Volume)
  DF_GMMA = GMMA(DF_Orig$Adjusted)
  DF_KST = as.data.frame(KST(DF_Orig$Adjusted))
  DF_MACD = as.data.frame(MACD(DF_Orig$Adjusted))
  DF_MFI = as.data.frame(MFI(HLC(DF_Orig),DF_Orig$Volume))
  DF_OBV = as.data.frame(OBV(DF_Orig$Adjusted,DF_Orig$Volume))
  DF_PBands = as.data.frame(PBands(DF_Orig$Adjusted))
  DF_RSI = as.data.frame(RSI(DF_Orig$Adjusted))
  DF_SAR = as.data.frame(SAR(DF_Orig[,c("High","Low")]))
  DF_stoc = stoch(HLC(DF_Orig))
  DF_TDI = TDI(DF_Orig$Adjusted)
  DF_TRIX = TRIX(DF_Orig$Adjusted)
  DF_UltimateOscillator = ultimateOscillator(DF_Orig)
  DF_VHF = VHF(HLC(DF_Orig))
  DF_volatility = volatility(DF_Orig)
  DF_williamsAD = williamsAD(DF_Orig)
  DF_WPR = WPR(HLC(DF_Orig))
  ## End TTR Functions
  
  DF = DF %>%
  mutate(Return = (Adjusted - lag(Adjusted,1))/lag(Adjusted,1),
         EMA50 = ema(Adjusted,50),
         EMA100 = ema(Adjusted,100),
         EMA200 = ema(Adjusted,200),
         MA50 = sma(Adjusted,50),
         MA100 = sma(Adjusted,100),
         MA200 = sma(Adjusted,200),
         Gain = ifelse(Diff >= 0, Diff, NA),
         Loss = ifelse(Diff < 0, -Diff, NA),
         AVG_Gain_14 = rollapply(Gain,14,mean,na.rm = T, fill = NA, align = "right"),
         AVG_Loss_14 = rollapply(Loss,14,mean,na.rm = T,fill = NA,align = "right")) %>%
    select(-c(Diff, Gain, Loss)) %>%
    na.omit()
  
  
}  
############ Technical Terminology ##################
# The Accumulation/Distribution (AD) study
# attempts to quantify the amount of volume flowing into or out of
# an instrument by identifying the position of the close of the period
# in relation to that period’s high/low range. The volume for the
# period is then allocated accordingly to a running continuous total.