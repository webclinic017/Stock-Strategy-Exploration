Project_Folder = rprojroot::find_rstudio_root_file()

library(EmersonDataScience)

devtools::install_github("jagg19/AlpacaforR")
devtools::install_github("sboysel/fredr")

## Loading and Installing Packages if necessacary
Required_Packages = c('tidyverse','installr','psych','quantmod','lubridate','dygraphs','doParallel','XML',
                      'earth', 'googledrive','cumstats','dummy','knitr','xts','reshape2','mboost','glmnet','broom','recipes'
                      ,'caret','cluster','factoextra',"HiClimR","rpart","rpart.plot","caret","lubridate",
                      "ranger",'roll','Boruta','glmnet','doSNOW',"AlpacaforR","iterators","pbapply","forecast","websocket")
load_or_install(Required_Packages)

## Loading Required Functions
sourceDir(paste0(Project_Folder,"/Codes/Functions"))

## General RMD Options
Run_Analysis = T
## Modeling Information
Max_Single_Investment = 1000
Min_Single_Investment = 10
## Cap Preferences (one of All/Mega/Large/Mid/Small)
Cap = c("All") 
## Perfromance Function Parameters
Max_Loss = 0.05
Max_Holding = 0.05
Max_Holding_Live = 0.10
## Disabling API Warning
options("getSymbols.yahoo.warning" = FALSE)
options("getSymbols.warning4.0" = FALSE)

# ## Pulling Economic Data If Needed
# Economic_Data_Location = str_remove(Project_Folder,"[^\\/]+$")
# Reference_Data = readRDS(file = str_c(Economic_Data_Location,"Economic_Data.RDS"))
# if(as.numeric(difftime(Sys.Date(),max(ymd(Reference_Data$Date)),tz = "UTC",units = "days")) >= 62){
#   Reference_Data = Reference_Economic_Pull(Time_Start = "2019-01-01")
#   colnames(Reference_Data)[1] = "Date"
#   saveRDS(Reference_Data,file = str_c(Economic_Data_Location,"Economic_Data.RDS"))
# }

## Pulling Historical Data
if(Run_Analysis){
  ## Fresh Historical Data Pull
  Initial_Pull(Cap = Cap,
               Source = "Y",
               Max_Single_Investment = Max_Single_Investment,
               Min_Single_Investment = Min_Single_Investment,
               Required_Packages = Required_Packages,
               Debug_Save = T)
  
  
  ## Loading Historical Stock Data
  load(paste0(Project_Folder,"/Data/NASDAQ Historical.RDATA"))
  Combined_Results = Combined_Results %>%
    filter(Date != Sys.Date())
  
  # ## Bear/Bull Calculations
  Market_Ind = Market_Direction(Combined_Results,Plot = T) %>%
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
                          Required_Packages = Required_Packages)
  Sys.time() - Start
  
  ## Saving Results
  save(PR_Stage,
       file = paste0(Project_Folder,"/Data/Initial Stats.RDATA"))
  rm(Combined_Results)
  
  ## Compairing Performance to Major Indexs
  load(file = paste0(Project_Folder,"/Data/Initial Stats.RDATA"))
  load(file = paste0(Project_Folder,"/Data/Stock_META.RDATA"))
  
  Market_Ticker = PR_Stage %>%
    group_by(Date) %>%
    summarise_all(mean,trim = 0.05,na.rm = T) %>%
    mutate(Stock = "Total_Market")
  
  
  Major_Indexs = c("^GSPC","^IXIC","^DJI")
  Total_Alpha_Slope = PR_Stage %>%
    filter(!Stock %in% Major_Indexs) %>%
    select(Date,Close) %>%
    group_by(Date) %>%
    summarise(Average_Close = mean(Close,trim = 0.05)) %>%
    mutate(Total_Alpha = (Average_Close - lag(Average_Close,1))/lag(Average_Close,1)) %>%
    ungroup() %>%
    na.omit()
  
  MIN_ALPHA = 0
  MAX_P_VAL = 0.20
  TIMEFRAME = 10
  
  Sector_Alpha_Slope = BAC_Function(PR_Stage = PR_Stage,
                                    Total_Alpha_Slope = Total_Alpha_Slope,
                                    Group_Columns = "Sector",
                                    Required_Packages,
                                    width = TIMEFRAME)
  
  Alpha_Filter = Sector_Alpha_Slope %>%
    group_by(Sector) %>%
    filter(Date == max(Date),
           Alpha_Sector > MIN_ALPHA,
           P_Alpha_Sector < MAX_P_VAL) %>%
    select(Sector)
  
  Sector_Alpha_Slope = Sector_Alpha_Slope %>%
    inner_join(Alpha_Filter) %>%
    mutate(Beta_Sector = ifelse(P_Beta_Sector > MAX_P_VAL,0,Beta_Sector))
  
  Industry_Alpha_Slope = BAC_Function(PR_Stage = PR_Stage,
                                      Total_Alpha_Slope = Total_Alpha_Slope,
                                      Group_Columns = "Industry",
                                      Required_Packages,
                                      width = TIMEFRAME)
  
  Alpha_Filter = Industry_Alpha_Slope %>%
    group_by(Industry) %>%
    filter(Date == max(Date),
           Alpha_Industry > MIN_ALPHA,
           P_Alpha_Industry < MAX_P_VAL) %>%
    select(Industry)
  
  Industry_Alpha_Slope = Industry_Alpha_Slope %>%
    inner_join(Alpha_Filter) %>%
    mutate(Beta_Industry = ifelse(P_Beta_Industry > MAX_P_VAL,0,Beta_Industry))
  
  Sector_Industry_Alpha_Slope = BAC_Function(PR_Stage = PR_Stage,
                                             Total_Alpha_Slope = Total_Alpha_Slope,
                                             Group_Columns = c("Sector","Industry"),
                                             Required_Packages,
                                             width = TIMEFRAME)
  
  Alpha_Filter = Sector_Industry_Alpha_Slope %>%
    group_by(Sector,Industry) %>%
    filter(Date == max(Date),
           Alpha_Sector_Industry > MIN_ALPHA,
           P_Alpha_Sector_Industry < MAX_P_VAL) %>%
    select(Sector,Industry)
  
  Sector_Industry_Alpha_Slope = Sector_Industry_Alpha_Slope %>%
    inner_join(Alpha_Filter) %>%
    mutate(Beta_Sector_Industry = ifelse(P_Beta_Sector_Industry > MAX_P_VAL,0,Beta_Sector_Industry))
  
  Stock_Alpha_Slope = BAC_Function(PR_Stage = PR_Stage,
                                   Total_Alpha_Slope = Total_Alpha_Slope,
                                   Group_Columns = "Stock",
                                   Required_Packages,
                                   width = TIMEFRAME)
  
  Alpha_Filter = Stock_Alpha_Slope %>%
    group_by(Stock) %>%
    filter(Date == max(Date),
           Alpha_Stock > MIN_ALPHA,
           P_Alpha_Stock < MAX_P_VAL) %>%
    select(Stock)
  
  Stock_Alpha_Slope = Stock_Alpha_Slope %>%
    inner_join(Alpha_Filter) %>%
    mutate(Beta_Stock = ifelse(P_Beta_Stock > MAX_P_VAL,0,Beta_Stock))
  
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
    bind_rows(Market_Ticker) %>%
    select(-c(Name,LastSale,MarketCap,Sector,Industry,New_Cap,Cap_Type)) 
  PR_Stage_R2[is.na(PR_Stage_R2)] = 0
  
  save(PR_Stage_R2,
       file = str_c(Project_Folder,"/Data/PR_Stage_R2.RDATA"))
  load(file = str_c(Project_Folder,"/Data/PR_Stage_R2.RDATA"))
  rm(PR_Stage,Sector_Alpha_Slope,Stock_Alpha_Slope,Sector_Industry_Alpha_Slope,Industry_Alpha_Slope,Total_Alpha_Slope)
  
  ## Spinning Up Clusters
  ## Looping All Stocks Through Spline Optimization
  c1 = makeCluster(detectCores())
  registerDoSNOW(c1)
  p <- progress_estimated(length(unique(PR_Stage_R2$Stock)))
  progress <- function(n) p$tick()$print()
  opts <- list(progress = progress)
  
  
  Symbols = isplit(PR_Stage_R2,PR_Stage_R2$Stock)
  
  ## Parallel Execution
  Results = foreach(i = Symbols,
                    .errorhandling = "remove",
                    .inorder = F,
                    .options.snow = opts,
                    .packages = c("tidyverse",
                                  "quantmod",
                                  "lubridate",
                                  "TTR"),
                    .verbose = F) %dopar% {
                      
                      ## Calculating Technical Indicators
                      Stat_Appendage_Function(DF = i$value,
                                              Timeframe = TIMEFRAME)
                    }
  
  ## Spinning Down Clusters
  installr::kill_all_Rscript_s()
  
  ## Consolidating Results
  PR_Stage_R3 = plyr::ldply(Results,data.frame)
  rm(Results)
  
  ## Saving Results
  save(PR_Stage_R3,
       file = paste0(Project_Folder,"/Data/Normalized Historical and Technical Indicators.RDATA"))
  load(file = paste0(Project_Folder,"/Data/Normalized Historical and Technical Indicators.RDATA"))
  rm(PR_Stage_R2)
  
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
           RSI_Delta = (RSI - lag(RSI,1))) %>%
    na.locf(na.rm = T)
  rm(PR_Stage_R3)
  
  # ## Building Models ##
  # Horizon_Start = 1
  # Horizon_End = TIMEFRAME
  # Market_Performance = ID_DF[ID_DF$Stock == "Total_Market",]
  # 
  # 
  # 
  # plot(Market_Performance$Close)
  # registerDoSEQ()
  # Models = Modeling_Function(ID_DF = Market_Performance,
  #                            Timeframes = seq(Horizon_Start,Horizon_End))
  # 
  # Market_Projection = numeric()
  # Sharpe_Projection = numeric()
  # TODAY = Market_Performance %>%
  #   filter(Date == max(Date))
  # Current_Price = TODAY$Close
  # counter = 0
  # for(i in Horizon_Start:Horizon_End){
  #   counter = counter + 1
  #   Timeframe = i
  #   Volatility = TODAY$Volatility_Klass
  #   Projection = as.numeric(predict(Models[[i]]$Model,
  #                                   TODAY))
  #   Risk_Adjusted_Return = exp((i/365)*log(Projection*Volatility + 1)) - 1
  #   Actual_Return = Risk_Adjusted_Return + (exp(log(1 + 0.02)/(1/(i/365))) - 1)
  #   Market_Projection[counter] = Actual_Return * Current_Price + Current_Price
  #   Sharpe_Projection[counter] = Projection
  # }
  # 
  # ## Visualizing Market Outlook
  # Market_Projection = na.approx(Market_Projection) 
  # plot(Market_Projection,
  #      main = str_c("Current Market Price = ",scales::dollar(TODAY$Close)),
  #      ylab = "Projected Market Price",
  #      xlab = "Future Trading Days\n",
  #      sub = str_c("Current Market Date = ",TODAY$Date))
  # abline(h = TODAY$Close,
  #        col = 'red')
  # text(Market_Projection,as.character(round(Sharpe_Projection,1)),col = "red",pos = 2)
  # 
  # Market_Return = diff(c(Current_Price,Market_Projection))/Market_Projection
  
  ggplot(Market_Performance,aes(x = Date,y = RSI)) +
    geom_line() +
    geom_hline(yintercept = median(Market_Performance$RSI),
               linetype = 1,
               size = 1) +
    geom_hline(yintercept = median(Market_Performance$RSI) + 
                 mad(Market_Performance$RSI),
               color = "red",
               linetype = 2,
               size = 1) +
    geom_hline(yintercept = median(Market_Performance$RSI) - 
                 mad(Market_Performance$RSI),
               color = "red",
               linetype = 2,
               size = 1) +
    labs(title = "Total Market Historical RSI",
         subtitle = str_c("Current MACD Indicator = ",
                          round(tail(Market_Performance$MACD - 
                                 Market_Performance$MACD_Signal,1),2)))
  
  TODAY = ID_DF %>%
    filter(Date == max(Date)) %>%
    FinViz_Meta_Data()
  
  TODAY = TODAY %>%
    filter(Short.Float < 0.05) %>%
    select(Stock,Close,ATR,RSI,RSI_Delta,MACD,MACD_Signal,
           Alpha_Stock,Beta_Stock,Industry,Sector,Short.Float)
  
  Watchlist = TODAY %>%
    mutate(Momentum = MACD - MACD_Signal,
           Strength = RSI,
           Strength_Delta = RSI_Delta) %>%
    select(-c(RSI,RSI_Delta,MACD,MACD_Signal)) %>%
    filter(Momentum > 0)
  
  write.csv(Watchlist,file = "C://users//aayorde//desktop//watchlist.csv")
  
  Total_List = ID_DF %>%
    filter(Date == max(Date)) %>%
    select(Stock,Close,ATR,Alpha_Stock,Beta_Stock,RSI,Industry,Sector)
  
  Alpha_Columns = colnames(TODAY)[str_detect(colnames(TODAY),"^Alpha")]
  Alphas = base::apply(X = as.matrix(TODAY[,Alpha_Columns]),MARGIN = 1,FUN = sum)/length(Alpha_Columns)
  Beta_Columns = colnames(TODAY)[str_detect(colnames(TODAY),"^Beta")]
  Betas = base::apply(X = as.matrix(TODAY[,Beta_Columns]),MARGIN = 1,FUN = sum)/length(Beta_Columns)
  Projections = Alphas + (matrix(data = Betas,ncol = 1) %*% Market_Return)
  row.names(Projections) = TODAY$Stock
  
  Target_Profit = base::apply(X = Projections,MARGIN = 1,FUN = mean)

  RESULT = Target_Profit %>%
    as.data.frame() %>%
    mutate(Stock = TODAY$Stock) %>%
    rename("Target_Profit" = ".") %>%
    mutate(ATR = TODAY$ATR,
           Trailing_Stop = ATR/2,
           Potential_Gain = ATR*2/TODAY$Close,
           B_Pcent = TODAY$B_Pct,
           RSI = TODAY$RSI,
           RSI_Delta = TODAY$RSI_Delta,
           Industry = TODAY$Industry,
           Sector = TODAY$Sector) %>%
    filter(Target_Profit >= Potential_Gain) %>%
    arrange(desc(Target_Profit-Potential_Gain)) %>%
    group_by(Industry,Sector) %>%
    filter(Target_Profit-Potential_Gain == max(Target_Profit-Potential_Gain))
  
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
  

Alpha_Signal = mean(TODAY$Total_Alpha) > 0

if(Alpha_Signal){
  ## Running Position Setting Function (Paper and Live)
  ALPACA_Performance_Function(TODAY = TODAY,
                              RESULT = RESULT,
                              Auto_Stocks = Auto_Stocks,
                              Project_Folder = Project_Folder,
                              Max_Holding = Max_Holding,
                              PAPER = T,
                              Rebalance = T)
  
  # ALPACA_Performance_Function(TODAY = TODAY,
  #                             RESULT = RESULT,
  #                             Auto_Stocks = Auto_Stocks,
  #                             Project_Folder = Project_Folder,
  #                             Max_Holding = Max_Holding_Live,
  #                             PAPER = F,
  #                             Rebalance = T)
  
  ALPACA_Performance_Function(TODAY = TODAY,
                              RESULT = RESULT,
                              Auto_Stocks = Auto_Stocks,
                              Project_Folder = Project_Folder,
                              Max_Holding = Max_Holding,
                              PAPER = T,
                              Rebalance = T)
  
  # ALPACA_Performance_Function(TODAY = TODAY,
  #                             RESULT = RESULT,
  #                             Auto_Stocks = Auto_Stocks,
  #                             Project_Folder = Project_Folder,
  #                             Max_Holding = Max_Holding_Live,
  #                             PAPER = F,
  #                             Rebalance = T)
}else{
  Alpaca_Failsafe(PAPER = T)
  Alpaca_Failsafe(PAPER = F)
}
  
# ## Running Back Test To Check For New Rules ##
# Runs = 10
# p = progress_estimated(Runs)
# Results = list()
# for(i in 1:Runs){
#   Results[[i]] = try(BACKTEST_Rule_Generator(Max_Holding = Max_Holding,
#                                              Max_Loss = Max_Loss,
#                                              ID_DF = ID_DF,
#                                              Auto_Stocks = Auto_Stocks,
#                                              Progress = T))
#   p$pause(0.1)$tick()$print()
# }
# save(Results,
#      file = paste0(Project_Folder,"/Data/BT_Runs.RDATA"))
# load(file = paste0(Project_Folder,"/Data/BT_Runs.RDATA"))
# keep = sapply(Results,class) == "list"
# Results = Results[keep]
# RUNS = plyr::ldply(lapply(Results,'[[',1),data.frame)
# Keep = RUNS$Trade_Number > 20
# Results = Results[Keep]
# RUNS = plyr::ldply(lapply(Results,'[[',1),data.frame)
# RULES =  plyr::ldply(lapply(Results,'[[',2),data.frame)
# TRADES =  plyr::ldply(lapply(Results,'[[',3),data.frame)
# 
# ## Summarizing Run Results
# psych::describe(RUNS[,3:ncol(RUNS)])
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
