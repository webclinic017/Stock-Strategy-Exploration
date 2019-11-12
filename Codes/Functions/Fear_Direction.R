Fear_Direction = function(Combined_Results,Market_Ind,Plot = T){
  ## Defining Fear Status
  Fear_DF = Combined_Results %>%
    filter(Stock %in% c("^VIX","^VXN")) %>%
    group_by(Stock) %>%
    mutate(Close_50_SMA = rollapply(data = Close,
                                    width = 50,
                                    FUN = mean,
                                    na.rm = T,
                                    fill = NA,
                                    align = "right"),
           Close_Slope_Inst = (Close - lag(Close,1)),
           Close_Slope_50 = rollapply(data = Close_Slope_Inst,
                                      width = 50,
                                      FUN = mean,
                                      na.rm = T,
                                      fill = NA,
                                      align = "right"),
           Close_Slope_50_Norm = Close_Slope_50/Close_50_SMA) %>%
    ungroup() %>%
    na.omit() %>%
    select(-c(Close_50_SMA,Close_Slope_Inst,Close_Slope_50)) %>%
    mutate(Market_Type = case_when(
      Close_Slope_50_Norm >= 0 ~ "Seller's Market",
      TRUE ~ "Buyer's Market"),
      Fear_Extent = scales::rescale(Close_Slope_50_Norm,to = c(0,1)))
  
  Fear_Extent = Fear_DF %>%
    group_by(Date) %>%
    summarise(Fear_Extent = max(Fear_Extent),
              Fear_Slope = max(Close_Slope_50_Norm))
  
  ## Summarizing to Conservative Status
  Fear_Ind = Fear_DF %>%
    select(Date,Stock,Market_Type) %>%
    spread(key = Stock, value = Market_Type) %>%
    mutate(Market_Type = case_when(
      `^VIX` == "Seller's Market" | `^VXN` == "Seller's Market" ~ "Seller's Market",
      TRUE ~ "Buyer's Market"
    )) %>%
    select(Date,Market_Type) %>%
    left_join(Fear_Extent)
  
  ## Creating Fear Index Data
  Plot_Date = max(Fear_DF$Date) %m-% months(6)
  VOLT_DF = Fear_DF %>%
    mutate(Stock = case_when(
      Stock == "^VIX" ~ "S&P 500 Fear Index",
      Stock == "^VXN" ~ "NASDAQ Fear Index"
    )) %>%
    group_by(Stock) %>%
    mutate(SMA50 = rollapply(Close,
                             width = 50,
                             FUN = mean,
                             na.rm = T,
                             align = "right",
                             fill = NA)) %>% 
    filter(Date >= Plot_Date) %>%
    left_join(Market_Ind,by = "Date")
  
  ## Determining Current Market Status and Type  
  Current_Type = Fear_Ind %>%
    arrange(Date) %>%
    mutate(Days = sequence(rle(Market_Type)$lengths)) %>%
    arrange(desc(Date)) %>%
    head(1)
  
  ## Plot Examining Fear Indexs
  VOLT_DF$Market_Status = factor(VOLT_DF$Market_Status,
                                 levels = c("Bull","Pullback","Correction","Bear"),
                                 ordered = F)
  
  if(Plot){
    p2 = ggplot(VOLT_DF,aes(x = Date,y = Close)) +
      geom_point(aes(color = Market_Status)) +
      geom_line(aes(y = SMA50),size = 1.5,linetype = 2) +
      scale_x_date(breaks = scales::pretty_breaks(9)) +
      labs(x = "Date",
           y = "Close",
           title = "Market Fear of Past 6 Months",
           subtitle = paste0("Current type = ",Current_Type$Market_Type," :: Current Date = ",Current_Type$Date,
                             " :: Type for Past ",Current_Type$Days," Days"),
           color = "Market Status") +
      facet_wrap(Stock~.,nrow = 3,ncol = 1,scales = "free_y")
    print(p2)
  }  
  return(Fear_Ind)
}