## Required Packages
import alpaca_trade_api as tradeapi
import pandas as pd
import numpy as np
import os
from tqdm import tqdm

## Input Parameters
min_price = 15
save_loc = 'C://Users//aayorde//documents//github//stock-strategy-exploration/Data/'
historical_amount = 90

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

os.environ['APCA_RETRY_WAIT'] = 6


## Initializing API
api = tradeapi.REST(api_version='v2')


## Pulling Historical Data
assets = api.list_assets(status = 'active')
good_assets = [a.symbol for a in assets if 
    a.marginable and 
    a.shortable and 
    a.tradable and 
    a.marginable and 
    a.easy_to_borrow]

## Loading Existing File If Exists
if os.path.exists(save_loc + "Historical_Data.pickle"):
    bar_data = pd.read_pickle(save_loc + "Historical_Data.pickle")
    after = max(bar_data.t).isoformat()
else:
    bar_data = pd.DataFrame(columns = ('s','o','h','l','c','t','v'))
    after = (pd.Timestamp.today(tz = 'America/New_York') - pd.Timedelta(days = historical_amount)).isoformat()

## Pulling New Data
for i in tqdm(range(len(good_assets))):
    s = good_assets[i]
    bars = api.get_barset(s,'day',limit = historical_amount,after = after)
    tmp = [[s,a.o,a.h,a.l,a.c,a.t,a.v] for a in bars[s]]
    if tmp[0][1] < min_price:
        next
    bar_data = bar_data.append(pd.DataFrame(tmp,columns = bar_data.columns))

## Saving / Loading Historical Data
bar_data.to_pickle(save_loc + "Historical_Data.pickle")
bar_data = pd.read_pickle(save_loc + "Historical_Data.pickle")

## Loading Account Information
account = api.get_account()
print(account)
positions = api.list_positions()
print(positions)
orders = api.list_orders()
print(orders)