stockSymbols = function(exchange = c("AMEX", "NASDAQ", "NYSE"), 
                        sort.by = c("Exchange", 
                                    "Symbol"), quiet = FALSE) 
{
  symbols <- NULL
  symbols.colnames <- c("Symbol", "Name", "LastSale", "MarketCap", 
                        "IPOyear", "Sector", "Industry", "Exchange")
  exchange <- match.arg(exchange, several.ok = TRUE)
  sort.by <- match.arg(sort.by, symbols.colnames, several.ok = TRUE)
  for (i in exchange) {
    flush.console()
    url <- paste("https://old.nasdaq.com/screening/companies-by-name.aspx", 
                 "?letter=0&exchange=", i, "&render=download", sep = "")
    exch <- read.csv(url, header = TRUE, as.is = TRUE, na = "n/a")
    col.loc <- sapply(symbols.colnames, grep, names(exch), 
                      ignore.case = TRUE)
    exch <- exch[, c(col.loc, recursive = TRUE)]
    exch <- data.frame(exch, Exchange = i, stringsAsFactors = FALSE)
    colnames(exch) <- symbols.colnames
    exch$Symbol <- gsub("[[:space:]]", "", exch$Symbol)
    if (i == "AMEX") {
      exch$Symbol <- gsub("/WS$", "-WT", exch$Symbol)
      exch$Symbol <- gsub("/WS/", "-WT", exch$Symbol)
      exch$Symbol <- gsub("/U", "-U", exch$Symbol)
      exch$Symbol <- gsub("\\^", "-P", exch$Symbol)
      exch$Symbol <- gsub("/", "-", exch$Symbol)
      drop <- c(grep("\\.", exch$Symbol), grep("\\$", exch$Symbol), 
                grep(":", exch$Symbol))
      if (NROW(drop) != 0) {
        exch <- exch[-drop, ]
      }
    }
    else if (i == "NYSE") {
      exch$Symbol <- gsub("/WS$", "-WT", exch$Symbol)
      exch$Symbol <- gsub("/WS/", "-WT", exch$Symbol)
      exch$Symbol <- gsub("\\^", "-P", exch$Symbol)
      exch$Symbol <- gsub("/", "-", exch$Symbol)
      drop <- c(grep("\\$", exch$Symbol), grep(":", exch$Symbol), 
                grep("~", exch$Symbol))
      if (NROW(drop) != 0) {
        exch <- exch[-drop, ]
      }
    }
    symbols <- rbind(symbols, exch)
  }
  symbols <- symbols[do.call("order", symbols[, sort.by]), 
                     ]
  rownames(symbols) <- 1:NROW(symbols)
  return(symbols)
}