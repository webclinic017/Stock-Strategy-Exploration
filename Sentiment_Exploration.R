library(AlpacaforR)

Project_Folder = rprojroot::find_rstudio_root_file()

########################## Setting API Keys ##########################
KEYS = read.csv(paste0(Project_Folder,"/Data/Keys/Paper API.txt"))
Sys.setenv('APCA-PAPER-API-KEY-ID' = KEYS$Key.ID)
Sys.setenv('APCA-PAPER-API-SECRET-KEY' = KEYS$Secret.Key)
KEYS = read.csv(paste0(Project_Folder,"/Data/Keys/Live API.txt"))
Sys.setenv('APCA-LIVE-API-KEY-ID' = as.character(KEYS$Key.ID))
Sys.setenv('APCA-LIVE-API-SECRET-KEY' = as.character(KEYS$Secret.Key))


## News for sentiment analysis 
Ticker = "MSFT"

News = get_meta(ticker = Ticker,
                endpoint = "news")



