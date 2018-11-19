load(file = "C:/Users/aayorde/Desktop/NASDAQ Historical.RDATA")

Tickers = as.character(unique(Combined_Results$Stock))
Total_Results = list()
p = progress_estimated(n = length(Tickers),min_time = 3)
for(i in 1:length(Tickers)){
  p$pause(0.1)$tick()$print()
  Stock_Loop = Tickers[i]
  DF = Combined_Results %>%
    filter(Stock == Stock_Loop)
  
  OptSplineParameter = try(optimize(PR_Cost_Function, c(0,1), maximum = TRUE,tol = 0.0001),
                           silent = T)
  if("try-error" %in% class(OptSplineParameter)){
    TMP = data.frame(Stock = paste(Stock_Loop,"Opt_Error"),
                     PR = 0)
  }else{
  TMP = data.frame(Stock = Stock_Loop,
                   PR = OptSplineParameter$objective)
  }
  Total_Results[[i]] = TMP
}
Output = plyr::ldply(Total_Results,data.frame)
write.csv(Output,
          file = "C://Users//aayorde//desktop//Opt_PR_Results.csv")
