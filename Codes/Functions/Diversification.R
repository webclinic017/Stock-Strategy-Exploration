## Diversification
Diversification = function(RESULT,Sector_Ind_DF){ 
  if(!"try-error" %in% class(Sector_Ind_DF)){
    ## Sector & Industry Filter
    TMP = RESULT %>%
      filter(!Industry %in% Sector_Ind_DF$Industry,
             !Sector %in% Sector_Ind_DF$Sector)
    
    if(nrow(TMP) != 0){
      return(TMP)
    }
    
    ## Sector Filter
    TMP = RESULT %>%
      filter(!Sector %in% Sector_Ind_DF$Sector)
    
    if(nrow(TMP) != 0){
      return(TMP)
    }
    
    ## Sector or Industry Filter
    TMP = RESULT %>%
      filter(!Industry %in% Sector_Ind_DF$Industry |
               !Sector %in% Sector_Ind_DF$Sector)
    return(TMP)
  }
  return(RESULT)
}