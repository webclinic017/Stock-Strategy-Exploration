Project_Folder = "C://Users//aayorde//documents//github//stock-strategy-exploration/"

library(EmersonDataScience)

## Loading and Installing Packages if necessacary
Required_Packages = c('tidyverse','installr','psych','quantmod','lubridate','dygraphs','doParallel','XML',
                      'earth', 'googledrive','cumstats','dummy','knitr','xts','reshape2','mboost','glmnet','broom','recipes'
                      ,'caret','cluster','factoextra',"HiClimR","rpart","rpart.plot","caret","AlpacaforR","lubridate")
load_or_install(Required_Packages)

## Loading Required Functions
sourceDir(paste0(Project_Folder,"/Codes/Functions"))

## Disabling API Warning
options("getSymbols.yahoo.warning" = FALSE)
options("getSymbols.warning4.0" = FALSE)

## General RMD Options
Re_Pull = T
PAPER = T
NClusters = 8

## Perfromance Function Parameters
Max_Loss = 0.05
Max_Holding = 0.05
Max_Holding_Live = 0.25

## Timeline For Profit Model (Trading Days)
Projection = 15

## Cap Preferences (one of All/Mega/Large/Mid/Small)
Cap = "All" 

Hour = hour(Sys.time())

if(Hour < 12){
  ## Pulling Historical Data
  if(Re_Pull){
    ## Fresh Historical Data Pull
    Initial_Pull(Cap = Cap,PAPER = F)
  }else{
    ## Historical Table Update
    Ticker_Pull_Function(Location = paste0(Project_Folder,"/Data/"),
                         Google_Drive = F)
  }
  
  ## Loading Historical Stock Data
  load(paste0(Project_Folder,"/Data/NASDAQ Historical.RDATA"))
  Combined_Results = Combined_Results %>%
    filter(Date != Sys.Date())
  
  ## Bear/Bull Calculations
  Market_Ind = Market_Direction(Combined_Results,Plot = F)
  ## General Fear Calculations
  Fear_Ind = Fear_Direction(Combined_Results,Market_Ind,Plot = F)
  
  ## Saving Market Indicators
  save(Market_Ind,Fear_Ind,
       file = paste0(Project_Folder,"/Data/Market Direction.RDATA"))
  
  ## Normalizing OHLCV Values  
  Start = Sys.time()
  print("Initial Stat Calculation for Pool Selection")
  PR_Stage = PR_Appendage(Combined_Results,
                          parallel = T,
                          NCores = NClusters)
  Sys.time() - Start
  ## Saving Results
  save(PR_Stage,
       file = paste0(Project_Folder,"/Data/Initial Stats.RDATA"))
  
  
  ## Compairing Performance to Major Indexs
  Major_Indexs = c("^GSPC","^IXIC","^DJI")
  Index_Alpha_Slope = PR_Stage %>%
    filter(Stock %in% Major_Indexs) %>%
    select(Stock,Date,Close_Slope_50_Norm) %>%
    spread(Stock,Close_Slope_50_Norm) %>%
    mutate(Alpha_Slope = rowMeans(cbind(`^GSPC`,`^IXIC`,`^DJI`))) %>%
    select(Date,Alpha_Slope)
  
  ## Appending Results
  PR_Stage_R2 = PR_Stage %>%
    left_join(Index_Alpha_Slope,by = "Date") %>%
    na.omit() %>%
    mutate(Pseudo_Alpha_PD = (Close_Slope_50_Norm - Alpha_Slope)/Alpha_Slope) %>%
    filter_all(all_vars(!is.infinite(.)))
  
  
  ## Removing Dead Stocks Or Baby Stocks
  Time_Stop = max(PR_Stage_R2$Date)
  Time_Start = Time_Stop - 365*5
  Last_Time = PR_Stage_R2 %>% 
    group_by(Stock) %>%
    summarise(Max_Time = max(Date),
              Min_Time = min(Date)) %>%
    filter(Max_Time == Time_Stop,
           Min_Time <= Time_Start)
  
  ## Calculating Technical Indicators
  Stocks = unique(Last_Time$Stock)
  
  ## Spinning Up Clusters
  c1 = makeCluster(NClusters)
  registerDoParallel(c1)
  
  ## Parallel Execution
  Results = foreach(i = 1:length(Stocks),
                    .inorder = F,
                    .errorhandling = "remove",
                    .packages = c("tidyverse",
                                  "quantmod",
                                  "lubridate",
                                  "TTR"),
                    .verbose = F) %dopar% {
                      ## Subsetting Data
                      TMP = PR_Stage_R2 %>%
                        filter(Stock == Stocks[i])
                      
                      ## Prevents Error During Stat Appendage (Technical Indicators)
                      if(nrow(TMP) < 1000){
                        stop("Not Enough Data")
                      }else{
                        ## Calculating Technical Indicators
                        Stat_Appendage_Function(DF = TMP)
                      }
                    }
  
  ## Spinning Down Clusters
  stopCluster(c1)
  registerDoSEQ()
  
  ## Consolidating Results
  PR_Stage_R3 = plyr::ldply(Results,data.frame)
  
  ## Saving Results
  save(PR_Stage_R3,
       file = paste0(Project_Folder,"/Data/Normalized Historical and Technical Indicators.RDATA"))
  
  ## Initial Data
  ID_DF = PR_Stage_R3 %>%
    left_join(Market_Ind) %>%
    left_join(Fear_Ind) %>%
    mutate(WAD_Delta = WAD - lag(WAD,1),
           Close_PD = (Close - lag(Close,1))/lag(Close,1),
           SMI_Delta = (SMI - lag(SMI,1)),
           SMI_Sig_Delta = (SMI_Signal - lag(SMI_Signal,1)),
           CCI_Delta = (CCI - lag(CCI,1)),
           VHF_Delta = (VHF - lag(VHF,1)),
           RSI_Delta = (RSI - lag(RSI,1)))
  
  ## Defining Target Variable
  PR_Stage_R4 = PR_Stage_R3 %>%
    group_by(Stock) %>%
    mutate(Adjusted_Lead = lead(Close,Projection),
           PD_Lead = (Adjusted_Lead - Close)/Close,
           Target = ifelse(PD_Lead > 0,1,0),
           Adjusted_Lead = PD_Lead) %>%
    mutate(WAD_Delta = WAD - lag(WAD,1),
           Close_PD = (Close - lag(Close,1))/lag(Close,1),
           SMI_Delta = (SMI - lag(SMI,1)),
           SMI_Sig_Delta = (SMI_Signal - lag(SMI_Signal,1)),
           CCI_Delta = (CCI - lag(CCI,1)),
           VHF_Delta = (VHF - lag(VHF,1)),
           RSI_Delta = (RSI - lag(RSI,1))) %>%
    ungroup() %>%
    select(-c(PD_Lead)) %>%
    na.omit() %>%
    filter(!str_detect(Stock,"^\\^"))
  
  Models = Modeling_Function(PR_Stage_R4 = PR_Stage_R4,
                             Max_Date = max(PR_Stage_R4$Date))
  
  TODAY = ID_DF %>%
    filter(Date == max(Date))
  
  PREDS = Prediction_Function(Models = Models,
                              TODAY = TODAY,
                              FinViz = T)
  
  RESULT = PREDS$RESULT %>%
    BUY_POS_FILTER() 
  FUTURES = PREDS$FUTURES
  SHORTS = PREDS$SHORTS
  
  save(RESULT,FUTURES,SHORTS,TODAY,PR_Stage_R4,ID_DF,Models,
       file = paste0(Project_Folder,"/data/Report Outputs.RDATA"))
}else{

  load(file = paste0(Project_Folder,"/data/Report Outputs.RDATA"))
  load(file = paste0(Project_Folder,"/Data/Normalized Historical and Technical Indicators.RDATA"))
  load(paste0(Project_Folder,"/Data/NASDAQ Historical.RDATA"))
  load(file = paste0(Project_Folder,"/Data/Market Direction.RDATA"))
}  
load(file = paste0(Project_Folder,"/Data/Stock_META.RDATA"))
  
  ## Running Position Setting Function (Paper and Live)
ALPACA_Performance_Function(PR_Stage_R3 = PR_Stage_R3,
                            RESULT = RESULT,
                            FUTURES = FUTURES,
                            SHORTS = SHORTS,
                            Auto_Stocks = Auto_Stocks,
                            Project_Folder = Project_Folder,
                            Max_Holding = Max_Holding,
                            Projection = Projection,
                            Max_Loss = Max_Loss,
                            PAPER = T)

ALPACA_Performance_Function(PR_Stage_R3 = PR_Stage_R3,
                            RESULT = RESULT,
                            FUTURES = FUTURES,
                            SHORTS = SHORTS,
                            Auto_Stocks = Auto_Stocks,
                            Project_Folder = Project_Folder,
                            Max_Holding = Max_Holding_Live,
                            Projection = Projection,
                            Max_Loss = Max_Loss,
                            PAPER = F)
