BACKTEST_Rule_Generator = function(Starting_Money,
                                   Max_Holding,
                                   Max_Loss,
                                   Projection,
                                   ID_DF,
                                   Fear_Ind,
                                   Market_Ind,
                                   Auto_Stocks,
                                   PR_Stage_R3,
                                   PR_Stage_R4,
                                   Combined_Results,
                                   Itterations = 100,
                                   PARALLEL = T,
                                   NCores = 2){
  pb <- progress_estimated(Itterations)
  progress <- function(n) pb$pause(0.1)$tick()$print()
  opts <- list(progress = progress)
  threads = floor(detectCores()/2/NCores)
  setMKLthreads(threads)
  print(getMKLthreads())
  if(PARALLEL){
    require(doSNOW)
    ## Creating Clusters
    c1 = makeCluster(min(c(detectCores(),NCores)))
    registerDoSNOW(c1)
    ## Spins Down Cluster After Execution or Error
    on.exit({
      kill_all_Rscript_s()
      stopCluster(c1)
      registerDoSEQ()}
    )
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
                                    "TTR",
                                  "RevoUtilsMath"),
                      .export = c("Modeling_Function",
                                  "Rule_Generator",
                                  "Prediction_Function",
                                  "Performance_Function",
                                  "BUY_POS_FILTER"), 
                      .verbose = F,
                      .options.snow = opts) %dopar% 
      {
        setMKLthreads(threads)
        ## Loop To Ensure Good Start / End Dates
        MR = character(0)
        while(length(MR) == 0){
          Start = sample(seq(1,length(unique(PR_Stage_R4$Date)) - 260),1)
          Delta = as.numeric(max(PR_Stage_R3$Date) - max(PR_Stage_R4$Date))
          Dates = sort(unique(PR_Stage_R4$Date))[Start:(Start+252)]
          IP = Combined_Results$Close[Combined_Results$Stock == "^GSPC" &
                                        Combined_Results$Date == Dates[1]+ Delta] %>%
            na.omit()
          FP = Combined_Results$Close[Combined_Results$Stock == "^GSPC" &
                                        Combined_Results$Date == Dates[length(Dates)] + Delta] %>%
            na.omit()
          (MR = scales::percent((FP-IP)/IP))
        }
        
        ## Copy for Liquidity Check
        ID_DF_2 = ID_DF %>%
          filter(Date <= Dates[1] + Delta,
                 Date >= Dates[1] + Delta-365)
        
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
        
        ## Pausing to stagger CPU draw
        Sys.sleep(runif(n = 1,min = 5,max = 60))
        CPU_Check = T
        while(CPU_Check){
          (Loading = sum(as.numeric(str_extract_all(system("wmic cpu get loadpercentage",
                                                          intern = T),
                                                   "\\d",
                                                   simplify = T)),
                        na.rm = T))
          if(Loading < 90){
            CPU_Check = F
          }else{
            Sys.sleep(30)
          }
        }
        
        ## Building Initial Models
        Models = Modeling_Function(PR_Stage_R4 = filter(PR_Stage_R4,
                                                        Stock %in% CHECK$Stock,
                                                        Close < Starting_Money*Max_Holding),
                                   Max_Date = ymd(as.character(Dates[1])))
        
        
        
        ## Initializing Counter / Progress Bar
        counter = 0
        MH = -9e9
        ML = 9e9
        p = progress_estimated(length(Dates))
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
              BUY_POS_FILTER() %>%
              left_join(Auto_Stocks,by = c("Stock" = "Symbol")) %>%
              dplyr::select(Sector,Industry,Decider,everything()) %>%
              group_by(Sector,Industry) %>%
              filter(Decider == max(Decider)) %>%
              ungroup()
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
                                     Max_Loss = Max_Loss,
                                     Fear_Ind = Fear_Ind,
                                     Market_Ind = Market_Ind,
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
                                       Max_Loss = Max_Loss,
                                       Fear_Ind = Fear_Ind,
                                       Market_Ind = Market_Ind,
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
          p$pause(0.1)$tick()$print()
        }
        
        History_Table = History_Table %>%
          dplyr::select(Prob,Delta,everything())
        
        ## Calculating Overall Profit
        Method_Profit = sum(History_Table$Profit) + 
          sum(History_Table$Buy.Price[is.na(History_Table$Sell.Date)] *
                History_Table$Pcent.Gain[is.na(History_Table$Sell.Date)] *
                History_Table$Number[is.na(History_Table$Sell.Date)])
        
        ## Storing_Results
        RUN_OUT = data.frame(Time_Start = Dates[1] + Delta,
                             Time_End = Dates[length(Dates)] + Delta,
                             Starting_Money = Starting_Money,
                             Market_Return = MR,
                             Method_Return = (Method_Profit/Starting_Money),
                             Max_Gain = (MH/Starting_Money),
                             Max_Loss = (ML/Starting_Money),
                             Trade_Number = nrow(History_Table),
                             Typical_Holding = round(mean(History_Table$Time.Held,na.rm = T))
        )
        
        ## Optimizing Rule Set
        RULE_OUT = Rule_Generator(History_Table)
        
        ## Storing Run Results
        list(RUN_OUT = RUN_OUT,
             RULE_OUT = RULE_OUT,
             TRADES_OUT = History_Table)
      }
    return(RESULTS)
}