Project_Folder = "C://Users//aayorde//documents//github//stock-strategy-exploration/"

library(EmersonDataScience)

## Loading and Installing Packages if necessacary
Required_Packages = c('tidyverse','installr','psych','quantmod','lubridate','dygraphs','doParallel','XML',
                      'earth', 'googledrive','cumstats','dummy','knitr','xts','reshape2','mboost','glmnet','broom','recipes'
                      ,'caret','cluster','factoextra',"HiClimR","rpart","rpart.plot","caret","lubridate",
                      "ranger",'roll','Boruta','glmnet','doSNOW')
load_or_install(Required_Packages)

## Loading Required Functions
sourceDir(paste0(Project_Folder,"/Codes/Functions"))

## Disabling API Warning
options("getSymbols.yahoo.warning" = FALSE)
options("getSymbols.warning4.0" = FALSE)

## General RMD Options
Re_Pull = T
NClusters = 8

## Perfromance Function Parameters
Max_Loss = 0.05
Max_Holding = 0.05
Max_Holding_Live = 0.20

## Cap Preferences (one of All/Mega/Large/Mid/Small)
Cap = "All" 

Hour = hour(Sys.time())

if(Hour < 12){
  ## Pulling Historical Data
  if(Re_Pull){
    ## Fresh Historical Data Pull
    Initial_Pull(Cap = Cap,
                 Source = "Y",
                 PAPER = F)
  }else{
    ## Historical Table Update
    Ticker_Pull_Function(Location = paste0(Project_Folder,"/Data/"),
                         Google_Drive = F)
  }
  
  ## Loading Historical Stock Data
  load(paste0(Project_Folder,"/Data/NASDAQ Historical.RDATA"))
  Combined_Results = Combined_Results %>%
    filter(Date != Sys.Date())
  
  # ## Bear/Bull Calculations
  Market_Ind = Market_Direction(Combined_Results,Plot = F) %>%
    select(-c(Close,Indicator)) %>%
    na.omit()

  # ## Saving Market Indicators
  save(Market_Ind,
       file = paste0(Project_Folder,"/Data/Market Direction.RDATA"))
  load(file = paste0(Project_Folder,"/Data/Market Direction.RDATA"))
  
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
  load(file = paste0(Project_Folder,"/Data/Initial Stats.RDATA"))
  load(file = paste0(Project_Folder,"/Data/Stock_META.RDATA"))
  
  Major_Indexs = c("^GSPC","^IXIC","^DJI")
  Total_Alpha_Slope = PR_Stage %>%
    filter(!Stock %in% Major_Indexs) %>%
    select(Date,Close_Slope_50_Norm) %>%
    group_by(Date) %>%
    summarise(Total_Alpha = mean(Close_Slope_50_Norm,trim = 0.05)) %>%
    ungroup() %>%
    na.omit()
  Sector_Alpha_Slope = BAC_Function(PR_Stage = PR_Stage,
                                    Total_Alpha_Slope = Total_Alpha_Slope,
                                    Group_Columns = "Sector",
                                    width = 50)
  Industry_Alpha_Slope = BAC_Function(PR_Stage = PR_Stage,
                                      Total_Alpha_Slope = Total_Alpha_Slope,
                                      Group_Columns = "Industry",
                                      width = 50)
  Sector_Industry_Alpha_Slope = BAC_Function(PR_Stage = PR_Stage,
                                             Total_Alpha_Slope = Total_Alpha_Slope,
                                             Group_Columns = c("Sector","Industry"),
                                             width = 50)
  Stock_Alpha_Slope = BAC_Function(PR_Stage = PR_Stage,
                                   Total_Alpha_Slope = Total_Alpha_Slope,
                                   Group_Columns = "Stock",
                                   width = 50)
  ## Appending Results
  PR_Stage_R2 = PR_Stage %>%
    left_join(Auto_Stocks,by = c("Stock" = "Symbol")) %>%
    left_join(Total_Alpha_Slope) %>%
    left_join(Sector_Alpha_Slope) %>%
    left_join(Industry_Alpha_Slope) %>%  
    left_join(Sector_Industry_Alpha_Slope) %>%
    left_join(Stock_Alpha_Slope) %>%
    na.omit() %>%
    filter_all(all_vars(!is.infinite(.))) %>%
    filter(Open > 0,
           Open < 5000,
           High > 0,
           High < 5000,
           Low > 0,
           Low < 5000,
           Close > 0,
           Close < 5000,
           Volume > 0) %>%
    select(-c(Name,LastSale,MarketCap,Sector,Industry,New_Cap,Cap_Type)) %>%
    na.omit()
  
  
  ## Removing Dead Stocks Or Baby Stocks
  Time_Stop = max(PR_Stage_R2$Date)
  Time_Start = Time_Stop - 365*5
  Last_Time = PR_Stage_R2 %>% 
    group_by(Stock) %>%
    summarise(Max_Time = max(Date),
              Min_Time = min(Date),
              Count = n()) %>%
    filter(Max_Time == Time_Stop,
           Min_Time <= Time_Start,
           Count > 1000)
  
  ## Calculating Technical Indicators
  Stocks = unique(Last_Time$Stock)
  
  ## Spinning Up Clusters
  pb <- progress_estimated(length(Stocks))
  progress <- function(n) pb$pause(0.1)$tick()$print()
  opts <- list(progress = progress)
  library(doSNOW)
  c1 = makeCluster(NClusters,outfile = "")
  registerDoSNOW(c1)
  
  ## Parallel Execution
  Results = foreach(i = 1:length(Stocks),
                    .errorhandling = "remove",
                    .inorder = F,
                    .packages = c("tidyverse",
                                  "quantmod",
                                  "lubridate",
                                  "TTR"),
                    .verbose = F,
                    .options.snow = opts) %dopar% {
                      ## Subsetting Data
                      TMP = PR_Stage_R2 %>%
                        filter(Stock == Stocks[i])
                      
                      ## Calculating Technical Indicators
                      Stat_Appendage_Function(DF = TMP)
                    }
  
  ## Spinning Down Clusters
  stopCluster(c1)
  registerDoSEQ()
  
  ## Consolidating Results
  PR_Stage_R3 = plyr::ldply(Results,data.frame)
  
  ## Saving Results
  save(PR_Stage_R3,
       file = paste0(Project_Folder,"/Data/Normalized Historical and Technical Indicators.RDATA"))
  load(file = paste0(Project_Folder,"/Data/Normalized Historical and Technical Indicators.RDATA"))
  
  ## Initial Data ##
  ID_DF = PR_Stage_R3 %>%
    left_join(select(Auto_Stocks,Symbol,Sector,Industry),
              by = c("Stock" = "Symbol")) %>%
    left_join(Market_Ind) %>%
    mutate(Sector = as.factor(Sector),
           Industry = as.factor(Industry)) %>%
    mutate(WAD_Delta = WAD - lag(WAD,1),
           Close_PD = (Close - lag(Close,1))/lag(Close,1),
           SMI_Delta = (SMI - lag(SMI,1)),
           SMI_Sig_Delta = (SMI_Signal - lag(SMI_Signal,1)),
           CCI_Delta = (CCI - lag(CCI,1)),
           VHF_Delta = (VHF - lag(VHF,1)),
           RSI_Delta = (RSI - lag(RSI,1)))
  
  ## Building Models ##
  Models = Modeling_Function(ID_DF = ID_DF,
                             Max_Date = max(ID_DF$Date),
                             Short_Time = 15,
                             Long_Time = 50)
  
  TODAY = ID_DF %>%
    filter(Date == max(Date))
  
  RESULT = Prediction_Function(Models = Models,
                               TODAY = TODAY,
                               FinViz = T)
  
  write.csv(x = RESULT$TOTAL,
            file = str_c(Project_Folder,
                         "/Data/Return_Predictions/",
                         as_date(now()),".csv"))
  
  ## Saving Results
  save(RESULT,TODAY,Models,
       file = paste0(Project_Folder,"/data/Report Outputs.RDATA"))
}else{
  ## Loading Daily Decision Data
  load(file = paste0(Project_Folder,"/data/Report Outputs.RDATA"))
  load(file = paste0(Project_Folder,"/Data/Market Direction.RDATA"))
  load(file = paste0(Project_Folder,"/Data/Normalized Historical and Technical Indicators.RDATA"))
  load(paste0(Project_Folder,"/Data/NASDAQ Historical.RDATA"))
}  
load(file = paste0(Project_Folder,"/Data/Stock_META.RDATA"))
  
## Running Position Setting Function (Paper and Live)
ALPACA_Performance_Function(TODAY = TODAY,
                            RESULT = RESULT,
                            Auto_Stocks = Auto_Stocks,
                            Project_Folder = Project_Folder,
                            Max_Holding = Max_Holding,
                            PAPER = T)

ALPACA_Performance_Function(TODAY = TODAY,
                            RESULT = RESULT,
                            Auto_Stocks = Auto_Stocks,
                            Project_Folder = Project_Folder,
                            Max_Holding = Max_Holding_Live,
                            PAPER = F)

## Running Back Test To Check For New Rules ##
Runs = 10
p = progress_estimated(Runs)
Results = list()
for(i in 1:Runs){
  Results[[i]] = try(BACKTEST_Rule_Generator(Max_Holding = Max_Holding,
                                             Max_Loss = Max_Loss,
                                             ID_DF = ID_DF,
                                             Auto_Stocks = Auto_Stocks,
                                             Progress = T))
  p$pause(0.1)$tick()$print()
}
save(Results,
     file = paste0(Project_Folder,"/Data/BT_Runs.RDATA"))
load(file = paste0(Project_Folder,"/Data/BT_Runs.RDATA"))
keep = sapply(Results,class) == "list"
Results = Results[keep]
RUNS = plyr::ldply(lapply(Results,'[[',1),data.frame)
Keep = RUNS$Trade_Number > 20
Results = Results[Keep]
RUNS = plyr::ldply(lapply(Results,'[[',1),data.frame)
RULES =  plyr::ldply(lapply(Results,'[[',2),data.frame)
TRADES =  plyr::ldply(lapply(Results,'[[',3),data.frame)

## Summarizing Run Results
psych::describe(RUNS[,3:ncol(RUNS)])
# 
# ## Reducing Rule Set
# RULES_Summary = RULES %>%
#   mutate(Delta = abs(PL-PH),
#          PDM = (MAX - MP)/abs(MP),
#          PD = Delta/MAX,
#          Side = case_when(
#            PH > PL ~ "High",
#            T ~ "Low"
#          ),
#          Value = case_when(
#            Side == "High" ~ VH,
#            T ~ VL)) %>%
#   filter(MAX > MP,
#          PDM > 0.05,
#          VH < VL) %>%
#   group_by(Var,Side) %>%
#   summarise_all(mean) %>%
#   rowwise() %>%
#   mutate(Percent_Kept = case_when(
#     Side == "High" ~  sum(ID_DF[[Var]] > Value,na.rm = T)/nrow(ID_DF),
#     T ~ sum(ID_DF[[Var]] < Value,na.rm = T)/nrow(ID_DF))) %>%
#   arrange(desc(PDM)) %>%
#   filter(Percent_Kept > 0.95) %>%
#   ungroup()
# 
# ## Updating Rules List
# if(nrow(RULES_Summary) > 0){
#   Rule_File = file(str_c(Project_Folder,"/Codes/Functions/BUY_POS_FILTER.R")) 
#   (TXT = readLines(Rule_File,warn = F))
#   for(i in 1:nrow(RULES_Summary)){
#     (TMP = RULES_Summary[i,])
#     rule = str_c("DF = DF[DF[['",TMP$Var,"']] ",
#                  ifelse(TMP$Side == "Low","< ","> "),
#                  TMP$Value,",]")
#     TXT_Top = TXT[1]
#     TXT_Bottom = TXT[which(str_detect(str_trim(TXT),"^return")):length(TXT)]
#     TXT_Middle = setdiff(setdiff(TXT,TXT_Top),TXT_Bottom)
#     if(is_empty(TXT_Middle)){
#       TXT = c(TXT_Top,rule,TXT_Bottom)
#     }else{
#       TXT = c(TXT_Top,sort(c(TXT_Middle,rule)),TXT_Bottom)
#     }
#   }
#   writeLines(text = TXT,
#              con = Rule_File)
#   source(Rule_File)
# }
