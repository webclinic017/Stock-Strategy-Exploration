## Required Packages
import alpaca_trade_api as tradeapi
import pandas as pd
import numpy as np
import os

## Live or Paper Execution
Live = False

## Key Definitions
Live_Key = 'AKUTDEQBBURQO275DXAH'
Live_Secret = '5S47hjoBBd1CZvjQ/5MYVD/dF1KsMV5Y0usncm1v'
Paper_Key = 'PKS3NB8CLF738ZV0DZ0W'
Paper_Secret = 'FHqbBUGWO4Q8YTiovjLfzsjPhwxqPeqFI9Slpvsy'

## Defining API Enviornment Variables
if Live:
    os.environ['APCA_API_KEY_ID'] = Live_Key
    os.environ['APCA_API_SECRET_KEY'] = Live_Secret
    os.environ['APCA_API_BASE_URL'] ='https://api.alpaca.markets'
else:
    os.environ['APCA_API_KEY_ID'] = Paper_Key
    os.environ['APCA_API_SECRET_KEY'] = Paper_Secret
    os.environ['APCA_API_BASE_URL'] ='https://paper-api.alpaca.markets'

## Initializing API
api = tradeapi.REST()


## Pulling Historical Data
assets = api.list_assets(status = 'active')
good_assets = [a.symbol for a in assets if 
    a.marginable and a.shortable and a.tradable and a.marginable and a.easy_to_borrow]

bar_data = pd.DataFrame(columns = ('s','o','h','l','c','t','v'))
for i in range(len(good_assets)):
    s = good_assets[i]
    bars = api.get_barset(s,'day',limit = 5)
    tmp = [np.array([s,a.o,a.h,a.l,a.c,a.t,a.v]) for a in bars[s]]
    bar_data = bar_data.append(pd.DataFrame(tmp,columns = bar_data.columns))





account = api.get_account()


print(account)
