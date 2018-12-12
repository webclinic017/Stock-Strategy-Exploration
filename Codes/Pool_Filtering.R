
######################## Functions ########################
is_installed = function(mypkg){
  is.element(mypkg, installed.packages()[, 1])}
load_or_install <- function(package_names)
{
  for (package_name in package_names)
  {
    if (!is_installed(package_name))
    {
      install.packages(package_name, dependencies = TRUE)
    }
    library(
      package_name,
      character.only = TRUE,
      quietly = TRUE,
      verbose = FALSE
    )
  }
}
sourceDir <- function(path, trace = TRUE, ...) {
  for (nm in list.files(path, pattern = "\\.[RrSsQq]$")) {
    if(trace) cat(nm,":")           
    source(file.path(path, nm), ...)
    if(trace) cat("\n")
  }
}
##########################################################################
Required_Packages = c('tidyverse','installr','psych','quantmod','lubridate','dygraphs','doParallel','finreportr')
load_or_install(Required_Packages)

## Checking for R updates
updateR(fast = F,
        browse_news = F,
        install_R = T,
        copy_packages = F,
        copy_Rprofile.site = T,
        keep_old_packages = F,
        update_packages = F,
        start_new_R = F,
        quit_R = T,
        print_R_versions = T,
        GUI = F,
        to_checkMD5sums = T,
        keep_install_file = F)

Root_Folder = getwd()

## Loading Required Functions
sourceDir(paste0(getwd(),'/Codes/Functions/'))

## Pulling in Data
NASDAQ_Stocks = read.csv(paste0(Root_Folder,"/Data/NASDAQ.csv"))
load("//climsidfs07/RefEng/1 Ref. Engineering (SH, Scroll & IPD)/13) Analytics/Small Projects/Stocks/Data/Opt_PR_Results.RDATA")

## Combining Results
Examine = left_join(Pool_Results,NASDAQ_Stocks,by = c("Stock" = "Symbol")) %>%
  select(-c(X,Summary.Quote,LastSale)) %>%
  group_by(Sector) %>%
  filter(Price_Growth >= 15,
         Opt >= 0,
         Median_Hold <= 90,
         Ratio >= 1,
         Volume_Trajectory >= 0) %>%
  mutate(Reward_Mean_Score = dense_rank(desc(Reward_Mean)),
         Risk_Mean_Score = dense_rank(desc(Risk_Mean)),
         Price_Growth_Score = dense_rank(desc(Price_Growth)),
         Volume_Norm_Score = dense_rank(desc(Volume_Norm))) %>%
  filter(Days_History > 365) %>%
  # filter(Reward_Mean_Score <= 3 | 
  #          Risk_Mean_Score <= 3 | 
  #          Price_Growth_Score <= 3 |
  #          Volume_Norm_Score <= 3) %>%
  na.omit()
  
## Pulling FinViz Metrics
FinViz_Metric_Pull = function(Ticker){

  url <- paste0("http://finviz.com/quote.ashx?t=", Ticker)
  webpage <- readLines(url)
  html <- htmlTreeParse(webpage, useInternalNodes = TRUE, asText = TRUE)
  tableNodes <- getNodeSet(html, "//table")
  
  # ASSIGN TO STOCK NAMED DFS
  assign(Ticker, readHTMLTable(tableNodes[[9]], 
                          header= c("data1", "data2", "data3", "data4", "data5", "data6",
                                    "data7", "data8", "data9", "data10", "data11", "data12")))
  
  # ADD COLUMN TO IDENTIFY STOCK 
  df <- get(Ticker)
  df['stock'] <- Ticker
  assign("TMP", df)
  
  ## Names and Values
  Names = TMP %>%
    select(data1,data3,data5,data7,data9,data11) %>%
    as.matrix() %>%
    as.vector()
  Values = TMP %>%
    select(data2,data4,data6,data8,data10,data12) %>%
    as.matrix() %>%
    as.vector() %>%
    t() %>%
    as.data.frame()

  colnames(Values) = Names 
  Values$Stock = Ticker
  
  # MOVE STOCK ID TO FIRST COLUMN
  Values <- Values[, c(ncol(Values), 1:ncol(Values)-1)]
  return(Values)
}

storage = list()
p = progress_estimated(nrow(Examine))
for(i in 1:nrow(Examine)){
  Ticker = Examine$Stock[i]
  TMP = try(FinViz_Metric_Pull(Ticker),
            silent = T)
  storage[[i]] = TMP
  p$pause(0.5)$tick()$print()
}
storage2 = storage[sapply(storage,class) %in% "data.frame"]
Metrics = plyr::ldply(storage2,data.frame)

Examine_Metrics = Examine %>%
  left_join(Metrics) %>%
  mutate(Profit.Margin = as.numeric(str_remove(Profit.Margin,"%"))/100,
         Oper..Margin = as.numeric(str_remove(Oper..Margin,"%"))/100,
         Change = as.numeric(str_remove(Change,"%"))/100,
         Price = as.numeric(as.character(Price)),
         Prev.Close = as.numeric(as.character(Prev.Close)),
         ATR = as.numeric(as.character(ATR)),
         Beta = as.numeric(as.character(Beta)),
         Perf.YTD = as.numeric(str_remove(Perf.YTD,"%"))/100,
         Perf.Year = as.numeric(str_remove(Perf.Year,"%"))/100,
         Perf.Half.Y = as.numeric(str_remove(Perf.Half.Y,"%"))/100,
         Perf.Quarter = as.numeric(str_remove(Perf.Quarter,"%"))/100,
         Perf.Month = as.numeric(str_remove(Perf.Month,"%"))/100,
         Perf.Week = as.numeric(str_remove(Perf.Week,"%"))/100,
         RSI..14. = as.numeric(as.character(RSI..14.)),
         EPS..ttm. = as.numeric(str_remove(EPS..ttm.,"%"))/100,
         EPS.next.Y = as.numeric(str_remove(EPS.next.Y,"%"))/100,
         EPS.next.Q = as.numeric(str_remove(EPS.next.Q,"%"))/100,
         EPS.this.Y = as.numeric(str_remove(EPS.this.Y,"%"))/100,
         EPS.next.Y.1 = as.numeric(str_remove(EPS.next.Y.1,"%"))/100,
         EPS.next.5Y = as.numeric(str_remove(EPS.next.5Y,"%"))/100,
         EPS.past.5Y = as.numeric(str_remove(EPS.past.5Y,"%"))/100,
         Sales.past.5Y = as.numeric(str_remove(Sales.past.5Y,"%"))/100,
         Sales.Q.Q = as.numeric(str_remove(Sales.Q.Q,"%"))/100,
         EPS.Q.Q = as.numeric(str_remove(EPS.Q.Q,"%"))/100,
         ROA = as.numeric(str_remove(ROA,"%"))/100,
         ROE = as.numeric(str_remove(ROE,"%"))/100,
         ROI = as.numeric(str_remove(ROI,"%"))/100,
         X52W.High = as.numeric(str_remove(X52W.High,"%"))/100,
         X52W.Low = as.numeric(str_remove(X52W.Low,"%"))/100,
         Target.Price = as.numeric(as.character(Target.Price)),
         Short.Ratio = as.numeric(as.character(Short.Ratio)),
         Short.Float = as.numeric(str_remove(Short.Float,"%"))/100,
         Payout = as.numeric(str_remove(Payout,"%"))/100,
         Gross.Margin = as.numeric(str_remove(Gross.Margin,"%"))/100,
         Inst.Trans = as.numeric(str_remove(Inst.Trans,"%"))/100,
         Inst.Own = as.numeric(str_remove(Inst.Own,"%"))/100,
         Insider.Trans = as.numeric(str_remove(Insider.Trans,"%"))/100,
         Insider.Own = as.numeric(str_remove(Insider.Own,"%"))/100,
         X52W.Low = as.numeric(str_remove(X52W.Low,"%"))/100,
         LT.Debt.Eq = as.numeric(as.character(LT.Debt.Eq)),
         Debt.Eq = as.numeric(as.character(Debt.Eq)),
         Current.Ratio = as.numeric(as.character(Current.Ratio)),
         Quick.Ratio = as.numeric(as.character(Quick.Ratio)),
         P.FCF = as.numeric(as.character(P.FCF)),
         P.C = as.numeric(as.character(P.C)),
         P.B = as.numeric(as.character(P.B)),
         P.S = as.numeric(as.character(P.S)),
         PEG = as.numeric(as.character(PEG)),
         Forward.P.E = as.numeric(as.character(Forward.P.E)),
         P.E = as.numeric(as.character(P.E)),
         Recom = as.numeric(as.character(Recom)),
         Employees = as.numeric(as.character(Employees)),
         Target.Price = as.numeric(as.character(Target.Price)),
         Target.Price = as.numeric(as.character(Target.Price)),
         Target.Price = as.numeric(as.character(Target.Price))
         ) %>%
  select(-c(Volume,Avg.Volume,Rel.Volume,SMA200,SMA50,SMA20)) %>%
  filter(Profit.Margin >= 0,
         Oper..Margin >= 0,
         EPS.next.Y >= 0,
         ROA >= 0,
         ROE >= 0,
         ROI >= 0)
write.csv(Examine_Metrics,
          file = "C://Users//aayorde//desktop//Selection_Pool.csv")

TMP = as.numeric(str_remove(Metrics$Volatility,"%"))/100








