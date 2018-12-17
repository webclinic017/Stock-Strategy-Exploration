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