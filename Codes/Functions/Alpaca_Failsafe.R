Alpaca_Failsafe = function(PAPER = T){
  ########################## Setting API Keys ##########################
  KEYS = read.csv(paste0(Project_Folder,"/Data/Keys/Paper API.txt"))
  Sys.setenv('APCA-PAPER-API-KEY-ID' = KEYS$Key.ID)
  Sys.setenv('APCA-PAPER-API-SECRET-KEY' = KEYS$Secret.Key)
  KEYS = read.csv(paste0(Project_Folder,"/Data/Keys/Live API.txt"))
  Sys.setenv('APCA-LIVE-API-KEY-ID' = as.character(KEYS$Key.ID))
  Sys.setenv('APCA-LIVE-API-SECRET-KEY' = as.character(KEYS$Secret.Key))
  if(PAPER){
    Report_CSV = paste0(Project_Folder,"/data/Decison Tracking/Paper Choices.csv")
  }else{
    Report_CSV = paste0(Project_Folder,"/data/Decison Tracking/Live Choices.csv")
  }
  
  Current_Orders = try(get_orders(status = 'all',live = !PAPER) %>%
                         filter(status == "new"),
                       silent = T)
  
  for(i in 1:nrow(Current_Orders)){
    cancel_order(ticker_id = Current_Orders$id[i])
  }
  
  close_all_positions(live = !PAPER)
  
  
}