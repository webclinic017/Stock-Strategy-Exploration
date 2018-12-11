## Pulling Additional Company Reference Information
############### Sample Data #####################
Ticker = "EMR"


Financial_Data_Pull = function(Ticker,Start_Year){
require(finreportr)
Basic_Info = CompanyInfo(Ticker)  
Reports = AnnualReports(Ticker)
Income = GetIncome(Ticker,2018)
Balance = GetBalanceSheet(Ticker,2018)
CashFlow = GetCashFlow(Ticker,2018)
}

