Market_Direction = function(Combined_Results,Plot = T){
  
  ## Defining Market Status Based on Rolling Quarterly Performance
  Market_DF = Combined_Results %>%
    group_by(Date) %>%
    summarise(Close = mean(Close,trim = 0.05,na.rm = T)) %>%
    na.locf() %>%
    ungroup() %>%
    mutate(Indicator = runMax(Close,90),
           Delta = (Close -Indicator)/Indicator) %>%
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
    mutate(Market_Delta_50 = rollapply(Delta,
                                       width = 50,
                                       FUN = mean,
                                       na.rm = T,
                                       fill = NA,
                                       align = "right")) %>%
  mutate(Days = sequence(rle(as.numeric(Market_Status))$lengths))
  
  ## Creating Market Type Data
  Plot_Date = max(Market_DF$Date) %m-% months(6)
  MIND_DF = Market_DF %>%
    mutate(SMA50 = rollapply(Close,
                             width = 50,
                             FUN = mean,
                             na.rm = T,
                             align = "right",
                             fill = NA)) %>%
    filter(Date >= Plot_Date) %>%
    arrange(Date)
  
  ## Pulling Current Status Information
  Current_Status = Market_DF %>%
    na.omit() %>%
    arrange(desc(Date)) %>%
    head(1)
  
  ## Plot Examining Market Direction Designation
  if(Plot){
    p1 = ggplot(MIND_DF,aes(x = Date,y = Close)) +
      geom_point(aes(color = Market_Status)) +
      geom_line(aes(y = SMA50),size = 1.5,linetype = 2) +
      scale_x_date(breaks = scales::pretty_breaks(9)) +
      labs(x = "Date",
           y = "Close",
           title = "Market Status of Past 6 Months",
           subtitle = paste0("Current status = ",
                             Current_Status$Market_Status,
                             " :: Date = ",Current_Status$Date,
                             " :: Status for Past ",Current_Status$Days," Days",
                             "\n50 Day Slope = ",
                             scales::percent(
                               (MIND_DF$SMA50[nrow(MIND_DF)] -  MIND_DF$SMA50[nrow(MIND_DF)-1])/
                                     MIND_DF$SMA50[nrow(MIND_DF)-1])),
           color = "Market Status")
    print(p1)
  }
  
  return(Market_DF) 
}