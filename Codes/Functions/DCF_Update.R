DCF_Update = function(Symbol){
  require(httr)
  res <- httr::GET(url = paste0('https://financialmodellingprep.com/api/v3/company/discounted-cash-flow/',Symbol), 
                   httr::add_headers(.headers=c(`Upgrade-Insecure-Requests` = '1')), 
                   query = list(`datatype` = 'json' ))
  json = content(res,as="parsed") 
  if("DCF" %in% names(json)){
    DCF = json[["DCF"]]
  }else{
    DCF = 0
  }
  return(DCF)
}