# Paul Fullenkamp / Abram Yorde

######################## Functions ########################
is_installed = function(mypkg)
  is.element(mypkg, installed.packages()[, 1])
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
##########################################################################
Required_Packages = c("tidyverse", "zoo", "scales", "quantmod", "lubridate","gridExtra")
load_or_install(Required_Packages)


################# Eample Data ###############
ticker = "EMR"
daysback = 900
##############################################

# Specify period of time we are interested in
startDate = Sys.Date() - daysback
endDate = Sys.Date()

#Download the stock history (for all ticker)
stockData = (getSymbols(
  ticker,
  src = "yahoo",
  from = startDate,
  to = endDate,
  auto.assign = FALSE
)) %>% as.data.frame()
colnames(stockData) = str_replace_all(colnames(stockData),ticker,"Stock")


Plot_DF = stockData %>%
  mutate(
    Date = ymd(rownames(.)),
    Return = (Stock.Adjusted - lag(Stock.Adjusted,1))/lag(Stock.Adjusted,1),
    SD_2 = 2 * rollapply(Stock.Adjusted, 
                         50, 
                         sd, 
                         fill = NA, 
                         align = "right"),
    MA200 = rollapply(Stock.Adjusted, 
                      200,
                      mean, 
                      fill = NA, 
                      align = "right"),
    MA100 = rollapply(Stock.Adjusted, 
                      100, 
                      mean, 
                      fill = NA, 
                      align = "right"),
    MA50 = rollapply(Stock.Adjusted, 
                     50, 
                     mean, 
                     fill = NA, 
                     align = "right"),
    Diff = Stock.Adjusted - lag(Stock.Adjusted, 1),
    OBV = 0,
    OBV = lag(OBV, 1) + case_when(Diff > 0 ~ Stock.Volume,
                                  Diff == 0 ~ 0,
                                  Diff < 0 ~ -Stock.Volume),
    OBV = scale(OBV, center = T),
    Gain = ifelse(Diff >= 0, Diff, NA),
    Loss = ifelse(Diff < 0, -Diff, NA),
    AVG_Gain = rollapply(
      Gain,
      14,
      mean,
      na.rm = T,
      fill = NA,
      align = "right"
    ),
    AVG_Loss = rollapply(
      Loss,
      14,
      mean,
      na.rm = T,
      fill = NA,
      align = "right"
    ),
    RS = AVG_Gain / AVG_Loss,
    RSI = 100 - 100 / (1 + AVG_Gain / AVG_Loss),
    RSI = scale(RSI, center = T),
    UB = Stock.Adjusted + SD_2,
    LB = Stock.Adjusted - SD_2
  ) %>%
  select(-c(Diff, Gain, Loss)) %>%
  na.omit()



DY_MI = Plot_DF %>%
  select(OBV, RSI)

Rownames = Plot_DF$Date
rownames(DY_MI) = Rownames
DY_MI = as.xts(DY_MI)

dygraph(DY_MI, main = paste(ticker, "Momentum Indicators"), group = "Stock") %>%
  dyLimit(0, color = "black") %>%
  dySeries("RSI", drawPoints = T,  strokeWidth = 0, color = "blue", label = "Relative Strength Index") %>%
  dySeries("OBV", color = "red", label = "On Balance Volume")


DY_BA = Plot_DF %>%
  select(Stock.Close, MA50, MA100, MA200, UB, LB)
rownames(DY_BA) = Rownames
DY_BA = as.xts(DY_BA)

dygraph(DY_BA, main = paste(ticker, "Stock Trending Performance"), group = "Stock") %>%
  dySeries("UB", color = "red", label = "Bolinger Upper", strokeWidth = 2, strokePattern = "dashed") %>%
  dySeries("LB", color = "red", label = "Bolinger Upper", strokeWidth = 2, strokePattern = "dashed") %>%
  dySeries("Stock.Close", color = "blue", label = "Adjusted Closing Price") %>%
  dySeries("MA50", color = "green", label = "50 Day Moving Average") %>%
  dySeries("MA100", color = "purple", label = "100 Day Moving Average") %>%
  dySeries("MA200", color = "brown", label = "200 Day Moving Average")