BACKTEST_Rule_Generator = function(Starting_Money,
                                   Max_Holding,
                                   Max_Stocks,
                                   Max_Loss,
                                   Projection,
                                   ID_DF,
                                   PR_Stage_R3,
                                   PR_Stage_R4,
                                   Combined_Results,
                                   Itterations = 100,
                                   PARALLEL = T){
  
  pb <- txtProgressBar(max = Itterations, style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  if(PARALLEL){
    library(doSNOW)
    c1 = makeCluster(min(c(detectCores(),2)),outfile = "")
    registerDoSNOW(c1)
  }else{
    registerDoSEQ()
  }  
  
  RESULTS = foreach(LOOP = 1:Itterations,
                    .inorder = F,
                    .errorhandling = "remove",
                    .packages = c("tidyverse",
                                  "quantmod",
                                  "lubridate",
                                  "scales",
                                  "EmersonDataScience",
                                  "TTR"),
                    .export = c("Modeling_Function",
                                "Prediction_Function",
                                "Performance_Function"), 
                    .verbose = T,
                    .options.snow = opts) %dopar% 
    {
      ## Loop To Ensure Good Start / End Dates
      MR = character(0)
      while(length(MR) == 0){
        Start = sample(seq(1,length(unique(PR_Stage_R4$Date)) - 260),1)
        Delta = as.numeric(max(PR_Stage_R3$Date) - max(PR_Stage_R4$Date))
        Dates = sort(unique(PR_Stage_R4$Date))[Start:(Start+252)]
        IP = Combined_Results$Close[Combined_Results$Stock == "^GSPC" &
                                      Combined_Results$Date == Dates[1]+ Delta]
        FP = Combined_Results$Close[Combined_Results$Stock == "^GSPC" &
                                      Combined_Results$Date == Dates[length(Dates)] + Delta]
        (MR = scales::percent((FP-IP)/IP))
      }
      
      ## Copy for Liquidity Check
      ID_DF_2 = ID_DF %>%
        filter(Date <= Dates[1] + Delta,
               Date >= Dates[1]+Delta-365)
      
      ## Removing Junk / Baby Stocks
      CHECK = ID_DF_2 %>%
        group_by(Stock) %>%
        na.locf() %>%
        na.omit() %>%
        summarise(O = sum(Open == 0),
                  H = sum(High == 0),
                  L = sum(Low == 0),
                  C = sum(Close == 0),
                  V = sum(Volume < 100),
                  V_AVG = median(Volume),
                  C_AVG = median(Close),
                  DV = V_AVG * C_AVG) %>%
        filter(O == 0,
               H == 0,
               L == 0,
               C == 0,
               V == 0,
               V_AVG >= 400000,
               C_AVG < Starting_Money*Max_Holding,
               DV >= 20000000)
      
      ID_DF_2 = ID_DF %>%
        filter(Stock %in% CHECK$Stock)
      
      ## Building Initial Models
      Models = Modeling_Function(PR_Stage_R4 = filter(PR_Stage_R4,
                                                      Stock %in% CHECK$Stock,
                                                      Close < Starting_Money*Max_Holding),
                                 Max_Date = ymd(as.character(Dates[1])))
      
      
      
      ## Initializing Counter / Progress Bar
      counter = 0
      MH = -9e9
      ML = 9e9
      for(i in 1:length(Dates)){
        ## Adjusted To Not Include Trained Information
        Current_Date = as.character(Dates[i]+Delta)
        
        ## Subsetting to Current Day Performance
        TODAY = ID_DF_2 %>%
          filter(Date == Current_Date)
        
        ## Periodically Checking Positions
        if(nrow(TODAY) > 0){
          Preds = Prediction_Function(Models,
                                      TODAY = TODAY,
                                      FinViz = F)
          RESULT = Preds$RESULT %>%
            BUY_POS_FILTER()
          FUTURES = Preds$FUTURES
          SHORTS = Preds$SHORTS
          
          if(nrow(RESULT) > 0 & counter == 0){
            counter = counter + 1
            History_Table = 
              Performance_Function(PR_Stage_R3 = PR_Stage_R3,
                                   RESULT = RESULT,
                                   FUTURES = FUTURES,
                                   SHORTS = SHORTS,
                                   Starting_Money = Starting_Money,
                                   Max_Holding = Max_Holding,
                                   Max_Stocks = Max_Stocks,
                                   Max_Loss = Max_Loss,
                                   Current_Date = Current_Date,
                                   Projection = Projection,
                                   Initial_History = ifelse(counter == 1,T,F),
                                   Save_Hist = F,
                                   Load_Hist = F,
                                   History_Location = paste0(Project_Folder,"/data/Back Test.RDATA"))
          }else{
            counter = counter + 1
            Result =  tryCatch({
              History_Table = 
                Performance_Function(PR_Stage_R3 = PR_Stage_R3,
                                     RESULT = RESULT,
                                     FUTURES = FUTURES,
                                     SHORTS = SHORTS,
                                     Starting_Money = Starting_Money,
                                     Max_Holding = Max_Holding,
                                     Max_Stocks = Max_Stocks,
                                     Max_Loss = Max_Loss,
                                     Current_Date = Current_Date,
                                     Projection = Projection,
                                     Save_Hist = F,
                                     Load_Hist = F,
                                     Initial_History = ifelse(counter == 1,T,F),
                                     History_Location = paste0(Project_Folder,
                                                               "/data/Back Test.RDATA"))
            }, warning = function(w){
              print(w)
            }, error = function(e){
              print(e)
            }, finally = {
            })
            if(all(class(Result) == "data.frame")){
              History_Table = Result
            }
          }
        }
        Profit = sum(History_Table$Profit) + 
          sum(History_Table$Buy.Price[is.na(History_Table$Sell.Date)] *
                History_Table$Pcent.Gain[is.na(History_Table$Sell.Date)] *
                History_Table$Number[is.na(History_Table$Sell.Date)])
        if(Profit > MH){MH = Profit}
        if(Profit < ML){ML = Profit}
      }
    
      History_Table = History_Table %>%
        select(Prob,Delta,everything())
      
      ## Adding Additional Technical Indicators
      Explore = History_Table %>%
        filter(Pcent.Gain != 0,
               !is.na(Sell.Date)) %>%
        left_join(Market_Ind) %>%
        left_join(Fear_Ind) %>%
        mutate(Positive = ifelse(Pcent.Gain > 0,1,0),
               Pcent_Adj = Pcent.Gain/Time.Held,
               Good = case_when(
                 Max.Price > Buy.Price ~ 1,
                 T ~ 0
               ),
               SL = case_when(
                 Pcent.Gain == -Max_Loss ~ 1,
                 T ~ 0
               )) %>%
        select_if(is.numeric) %>%
        select(-contains("MAX_")) %>%
        filter(!is.infinite(Pcent_Adj)) %>%
        select(Good,Pcent_Adj,Positive,SL,everything())
      
      STORE = data.frame(Var = character(),
                         VL = numeric(),
                         PL = numeric(),
                         VH = numeric(),
                         PH = numeric())
      Vars = colnames(Explore)
      for(Var in Vars){
        counter = 0
        for(i in seq(min(Explore[,Var]),max(Explore[,Var]),length.out = 25)){
          counter = counter + 1
          Keep_Low = which(Explore[,Var] < i)
          Keep_High = which(Explore[,Var] > i)
          TMP_Low = History_Table[Keep_Low,]
          P_TL = sum(TMP_Low$Profit) + 
            sum(TMP_Low$Buy.Price[is.na(TMP_Low$Sell.Date)] *
                  TMP_Low$Pcent.Gain[is.na(TMP_Low$Sell.Date)] *
                  TMP_Low$Number[is.na(TMP_Low$Sell.Date)])
          TMP_High = History_Table[Keep_High,]
          P_TH = sum(TMP_High$Profit) + 
            sum(TMP_High$Buy.Price[is.na(TMP_High$Sell.Date)] *
                  TMP_High$Pcent.Gain[is.na(TMP_High$Sell.Date)] *
                  TMP_High$Number[is.na(TMP_High$Sell.Date)])
          if(counter == 1){
            PL = P_TL
            VL = i
            PH = P_TH
            VH = i
          }else{
            if(P_TL > PL){
              PL = P_TL
              VL = i
            }
            if(P_TH > PH){
              PH = P_TH
              VH = i
            }
          }
        }
        TMP = data.frame(
          Var = Var,
          VL = round(VL, 4),
          PL = PL,
          VH = round(VH, 4),
          PH = PH
        )
        STORE = bind_rows(STORE,TMP)
      }
      
      Method_Profit = sum(History_Table$Profit) + 
        sum(History_Table$Buy.Price[is.na(History_Table$Sell.Date)] *
              History_Table$Pcent.Gain[is.na(History_Table$Sell.Date)] *
              History_Table$Number[is.na(History_Table$Sell.Date)])
      
      STORE = STORE %>%
        rowwise() %>%
        mutate(MAX = max(c(PL,PH))) %>%
        arrange(desc(MAX)) %>%
        filter(MAX > Method_Profit)
      
      ## Storing_Results
      RUN_OUT = data.frame(Time_Start = Dates[1] + Delta,
                                   Time_End = Dates[length(Dates)] + Delta,
                                   Starting_Money = Starting_Money,
                                   Market_Return = MR,
                                   Method_Return = scales::percent(Profit/Starting_Money),
                                   Max_Gain = scales::percent(MH/Starting_Money),
                                   Max_Loss = scales::percent(ML/Starting_Money),
                                   Trade_Number = nrow(History_Table))
      RULE_OUT = STORE
      
      list(RUN_OUT = RUN_OUT,
           RULE_OUT = RULE_OUT,
           TRADES_OUT = History_Table)
    }
  
  if(PARALLEL){
    stopCluster(c1)
    registerDoSEQ()
  }
  return(RESULTS)
}