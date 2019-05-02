Market_Direction = function(Combined_Results){
  
  ## Defining Market Status Based on Rolling Quarterly Performance
  Market_DF = Combined_Results %>%
    filter(Stock %in% c("^GSPC","^IXIC","^DJI")) %>%
    group_by(Stock) %>%
    mutate(Indicator = runMax(Adjusted,90),
           Delta = (Adjusted -Indicator)/Indicator,
           Market_Extent = scales::rescale(Delta,to = c(0,1))) %>%
    na.omit() %>%
    mutate(Market_Status = factor(
      case_when(
        Delta <= -0.20 ~ "Bear",
        Delta <= -0.10 ~ "Correction",
        Delta < -0.05 ~ "Pullback",
        Delta >= -0.05 ~ "Bull"
      ),
      levels = c("Bull",
                 "Pullback",
                 "Correction",
                 "Bear"))) %>%
    ungroup() %>%
    mutate(Stock = case_when(
      Stock == "^GSPC" ~ "S&P 500",
      Stock == "^IXIC" ~ "NASDAQ",
      Stock == "^DJI" ~ "Dow Jones"
    ))
  
  Market_Extent = Market_DF %>%
    group_by(Date) %>%
    summarise(Market_Extent = min(Market_Extent),
              Market_Delta = min(Delta)) %>%
    mutate(Market_Delta_50 = rollapply(Market_Delta,
                                       width = 50,
                                       FUN = mean,
                                       na.rm = T,
                                       fill = NA,
                                       align = "right"),
           Market_Extent_50 = rollapply(Market_Extent,
                                        width = 50,
                                        FUN = mean,
                                        na.rm = T,
                                        fill = NA,
                                        align = "right"))
  
  ## Summarizing to Conservative Status For Modeling Purposes
  Market_Ind = Market_DF %>%
    select(Date,Stock,Market_Status) %>%
    spread(key = Stock, value = Market_Status) %>%
    mutate(Market_Status = case_when(
      `S&P 500` == "Bear" | `NASDAQ` == "Bear" | `Dow Jones` == "Bear" ~ "Bear",
      `S&P 500` == "Correction" | `NASDAQ` == "Correction" | `Dow Jones` == "Correction" ~ "Correction",
      `S&P 500` == "Pullback" | `NASDAQ` == "Pullback" | `Dow Jones` == "Pullback" ~ "Pullback",
      TRUE ~ "Bull"
    )) %>%
    select(Date,Market_Status) %>%
    arrange(desc(Date)) %>%
    left_join(Market_Extent)
  
  ## Creating Market Type Data
  Plot_Date = max(Market_DF$Date) %m-% months(6)
  MIND_DF = Market_DF %>%
    group_by(Stock) %>%
    mutate(SMA50 = rollapply(Adjusted,
                             width = 50,
                             FUN = mean,
                             na.rm = T,
                             align = "right",
                             fill = NA)) %>%
    filter(Date >= Plot_Date)
  
  ## Pulling Current Status Information
  Current_Status = Market_Ind %>%
    arrange(Date) %>%
    mutate(Days = sequence(rle(Market_Status)$lengths)) %>%
    arrange(desc(Date)) %>%
    head(1)
  
  ## Plot Examining Market Direction Designation
  p1 = ggplot(MIND_DF,aes(x = Date,y = Adjusted)) +
    geom_point(aes(color = Market_Status)) +
    geom_line(aes(y = SMA50),size = 1.5,linetype = 2) +
    scale_x_date(breaks = scales::pretty_breaks(9)) +
    labs(x = "Date",
         y = "Adjusted",
         title = "Market Status of Past 6 Months",
         subtitle = paste0("Current status = ",Current_Status$Market_Status," :: Current Date = ",Current_Status$Date,
                           " :: Status for Past ",Current_Status$Days," Days"),
         color = "Market Status") +
    facet_wrap(Stock~.,nrow = 3,ncol = 1,scales = "free_y")
  print(p1)
  
  return(Market_Ind) 
}