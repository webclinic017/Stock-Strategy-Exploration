DCF_Update = function(Symbol){
  require(httr)
  res <- httr::GET(url = paste0('https://financialmodellingprep.com/api/v3/company/discounted-cash-flow/',Symbol))
  json = content(res) 
  if("dcf" %in% names(json)){
    DCF = json[["dcf"]]
    Current_Price = json[["Stock Price"]]
    # try(print(str_c(Symbol,":",round(DCF,2),":",round(Current_Price,2))),silent = T)
    Ratio = try((DCF-Current_Price)/Current_Price,silent = T)
    if(!"try-error" %in% class(Ratio)){
      if(Ratio > 0.02 & Ratio < 1){
        DCF = T
      }else{
        DCF = F
      }
    }else{
      DCF = NA
    }
  }else{
    DCF = NA
  }
  return(DCF)
}