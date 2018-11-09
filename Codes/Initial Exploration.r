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

## Market Indicators
MI = ggplot(Plot_DF, aes(Date, OBV)) +
  geom_line(aes(color = "On Balance Volume")) +
  geom_point(aes(y = RSI, color = "Relative Stregnth Index")) +
  geom_hline(
    yintercept = 0,
    size = 2,
    linetype = 3,
    color = "black"
  ) +
  scale_x_date(breaks = pretty_breaks(n = 12)) +
  labs(color = "Centered / Scaled\nIndicators",
       title = paste(ticker, "Momentum Indicators")) +
  theme(legend.position = c(0.05, 0.15),
        axis.title = element_blank())


# Bands and Averages
BA = ggplot(Plot_DF, aes(Date, Stock.Close)) +
  geom_line(aes(color = "Adjusted Closing Price")) +
  geom_line(aes(y = MA50, colour = "50 Day Moving Average"), size = 1) +
  geom_line(aes(y = MA100, colour = "100 Day Moving Average"), size = 1) +
  geom_line(aes(y = MA200, colour = "200 Day Moving Average"), size = 1) +
  geom_line(aes(y = UB, colour = "Bolinger Bands"),
            linetype = 2,
            size = 1) +
  geom_line(aes(y = LB, colour = "Bolinger Bands"),
            linetype = 2,
            size = 1) +
  scale_x_date(breaks = pretty_breaks(n = 12)) +
  labs(title = paste(ticker, "Stock Trending Performance")) +
  theme(
    legend.position = c(0.05, 0.85),
    legend.title = element_blank(),
    axis.title = element_blank()
  )

## Combined Plot
grid.arrange(MI,BA,nrow = 2)
