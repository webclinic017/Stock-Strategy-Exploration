#!/usr/bin/env python
# coding: utf-8

# ## Automated Stock Trading

# In[42]:


## Warning Handling
import warnings
warnings.filterwarnings("ignore", message="divide by zero encountered in double_scalars")
warnings.filterwarnings("ignore", message="Mean of empty slice.")
warnings.filterwarnings("ignore", message="invalid value encountered in double_scalars")
warnings.filterwarnings("ignore", message="divide by zero encountered in log")

## Logging Setup
import sys
# sys.stdout = open("Investment_Logs.txt", "w")


# In[43]:


## API Library Setup
import os

# ## Robinhood API Setup
# import robin_stocks as rs
# # Connecting to RobinHood
# rs.login(
#     username = os.getenv("RH_LOGIN"),
#     password = os.getenv("RH_PASS"),
#     expiresIn = 3600*24*7,
#     by_sms = True
# )

## Alpaca API Setup
import alpaca_trade_api as tradeapi
# Connecting to Alpaca
api = tradeapi.REST(os.getenv("AP_KEY"),os.getenv("AP_SECRET"), api_version='v2')
apip = tradeapi.REST(os.getenv("APP_KEY"),os.getenv("APP_SECRET"), api_version='v2',base_url='https://paper-api.alpaca.markets')


# In[67]:


N_DAYS_AGO = 52*5
OLS_Window = 5
min_list_years = 5
min_volume = 400000
min_investment = 15
leveraged_etfs = ['TQQQ','SQQQ','SPXU','UPRO','UDOW','SDOW']

## Account ## (rh = Robin hood, ap = Alpaca Live, app = Alpaca Paper)
Account = "app"


# In[110]:


## Installing Required Packages
import sys
from time import sleep
import math
import pickle
import pandas as pd
import numpy as np
from datetime import datetime, timedelta, date, time
from collections import defaultdict
from tqdm.auto import tqdm
import matplotlib.pyplot as plt
import statsmodels.api as sm
import statsmodels.formula.api as smf
from statsmodels.regression.rolling import RollingOLS
from pypfopt.efficient_frontier import EfficientFrontier
from pypfopt import risk_models
from pypfopt import expected_returns
from pypfopt import plotting
from scipy import stats
from sklearn import tree 
from sklearn.tree import DecisionTreeRegressor
from sklearn.naive_bayes import GaussianNB
from sklearn.model_selection import train_test_split
import yfinance as yf

## Setting Project Folder
Project_Folder = "C://Users//" + os.getlogin() + "//documents//github//Stock-Strategy-Exploration//"

## Reading In Functions
Function_Files = os.listdir(Project_Folder + '//python//' + 'Functions')
for File in Function_Files:
    if '.py' in File:
        print(File)
        Full_File =Project_Folder + '//python//' +  'Functions/' + File
        exec(open(Full_File).read())
        
def years_listed(d1):
    d1 = datetime.strptime(d1, "%Y-%m-%d")
    d2 = datetime.today()
    return abs((d2 - d1).days/365)

max_investment = Get_Equity(Account)*0.2
max_investment


# ### Historical Data Pull

# In[70]:


## Pulling All Available Alpaca Symbols
assets = api.list_assets("active")
Final_Assets = [i._raw['symbol'] for i in assets                 if i._raw['tradable']                     & i._raw['shortable']                     & i._raw['easy_to_borrow']]
Final_Assets.extend(leveraged_etfs)

## Pulling All Bar Data
s_inc = 100
print("Starting Initial Data Pull")
Splits = [0]
while Splits[len(Splits)-1] < (len(Final_Assets)-1):
    if Splits[len(Splits)-1] + s_inc > (len(Final_Assets)-1):
        Splits.append((len(Final_Assets)-1))
    else:
        Splits.append(Splits[len(Splits)-1] + s_inc)
Initial_Bars = {}
for i in tqdm(range(1,len(Splits))):
    Initial_Bars.update(api.get_barset(symbols = Final_Assets[Splits[i-1]:Splits[i]],
                                      timeframe = 'day',
                                      limit = N_DAYS_AGO))

print("Filtering Based On Average Price and Volume")
## Filtering Based On Price And Volume Criteria
Stocks = []
for i in tqdm(Final_Assets):
    try:
        Price_Data = Initial_Bars[i].df
        Price = np.mean(Price_Data['close'])
        Volume = np.mean(Price_Data['volume'])
        if Price < max_investment         and Price > min_investment         and i in Final_Assets         and Volume > min_volume:
            Stocks.append(i)
    except:
        print("Skipped",i,"No Data")

print("Pulling Company Data")
## Pulling Polygon Company Data
Splits = [0]
while Splits[len(Splits)-1] < (len(Stocks)-1):
    if Splits[len(Splits)-1] + 50 > (len(Stocks)-1):
        Splits.append((len(Stocks)-1))
    else:
        Splits.append(Splits[len(Splits)-1] + 50)
Company_Data = {}
for i in tqdm(range(1,len(Splits))):
    Company_Data.update(api.polygon.company(Stocks[Splits[i-1]:Splits[i]]))

## Filtering Based on Company Information
Updated_Stocks = list(Company_Data.keys())
Final_Stocks = []
for i in Updated_Stocks:
    if not Company_Data[i].active:
        continue
    if years_listed(Company_Data[i].listdate) > min_list_years:
        Final_Stocks.append(i)
Final_Stocks.extend(leveraged_etfs)
        
## Amount of Historical Data to Pull
start_date = datetime.now() - timedelta(days=N_DAYS_AGO)
Historical_Data = defaultdict(pd.DataFrame)

print("Formatting Final Dataset")
## Formatting Final Dataset
counter = 0
for i in tqdm(Final_Stocks):
    TMP = Initial_Bars[i].df
    TMP['date'] = TMP.index
    if i in leveraged_etfs:
        TMP['sector'] = 'Leveraged Market'
        TMP['industry'] = 'Leveraged Market'
    else:
        TMP['sector'] = Company_Data[i].sector
        TMP['industry'] = Company_Data[i].industry
    Historical_Data[i] = TMP 

## Adding Ticker Column
for s in Historical_Data:
    Historical_Data[s].insert(0, 'stock', [s]*len(Historical_Data[s]))

## Combining Data To Single Data Frame
Combined_Data = pd.concat(Historical_Data.values())  
Combined_Data = Combined_Data[Combined_Data['sector'] != '']
Combined_Data = Combined_Data[pd.notnull(Combined_Data['sector'])]
Combined_Data = Combined_Data[Combined_Data['industry'] != '']
Combined_Data = Combined_Data[pd.notnull(Combined_Data['industry'])]
## Saving Data
pickle.dump(Combined_Data, open(Project_Folder + "Data//Historical_Data.p" , "wb" ) )


# In[72]:


Indexes = ["^VIX","^GSPC","^NDX","^DJI"]
Index_Data = defaultdict(pd.DataFrame)

for ind in Indexes:
    ticker = yf.Ticker(ind)
    data = ticker.history(period = str(N_DAYS_AGO) + "d")
    data = data.loc[:,["Open","High","Low","Close","Volume"]]
    data.columns = ['open','high','low','close','volume']
    data.index = data.index.rename('date')
    Index_Data[ind] = Stock_Consolidator(data)
    
## Adding Ticker Column
for s in Index_Data:
    Index_Data[s].insert(0, 'stock', [s]*len(Index_Data[s]))

## Combining Data To Single Data Frame
Combined_Index_Data = pd.concat(Index_Data.values()) 


# In[73]:


Combined_Index_Data


# In[74]:


## Loading Stored Data
Combined_Data = pickle.load(open(Project_Folder + "Data//Historical_Data.p" , "rb" ))
# Combined_Data.to_csv(Project_Folder + "Data//Historical_Data_Stocks.csv")

### Exploring Total Market Performance
Combined_Data = Combined_Data.drop_duplicates()
if datetime.now().hour < 16:
    Combined_Data = Combined_Data[(datetime.now() - timedelta(days = N_DAYS_AGO)).strftime("%Y-%m-%d"):(datetime.now()- timedelta(days = 1)).strftime("%Y-%m-%d")]
else:
    Combined_Data = Combined_Data[(datetime.now() - timedelta(days = N_DAYS_AGO)).strftime("%Y-%m-%d"):datetime.now().strftime("%Y-%m-%d")]

Total_Market = Stock_Consolidator(Combined_Data)
Total_Market = Total_Market.loc[Total_Market.RSI > 0,:]

## Run to Update Total Market Data
# Total_Market.to_csv(Project_Folder + "Data//Historical_Data_Total.csv")

Total_Market.tail(10)


# In[75]:


Plot_Data = Total_Market
Plot_Data['RSI'] = pd.qcut(Plot_Data.RSI,100,labels = False)
# Plot_Data['RSI'] = ((Plot_Data['RSI'] -  np.min(Plot_Data['RSI'])) / (np.max(Plot_Data['RSI']) - np.min(Plot_Data['RSI'])))*100
Plot_Data['MACD'] = pd.qcut(Plot_Data.MACD,100,labels = False)/100
# Plot_Data['MACD'] = (1 + 1)*((Plot_Data['MACD'] -  np.min(Plot_Data['MACD'])) / (np.max(Plot_Data['MACD']) - np.min(Plot_Data['MACD']))) - 1

Plot_Data['date'] = Plot_Data.index.get_level_values('date')
Plot_Data = Plot_Data.reset_index(drop = True)
x = Plot_Data['date']

fig, axs = plt.subplots(3,1,gridspec_kw={'height_ratios':[3,1,1]})
axs[0].plot(x,Plot_Data['close'])
axs[0].plot(x,Plot_Data['sma'])
axs[1].plot(x,Plot_Data['RSI'])
axs[1].axhline(y = 70,c = 'black',dashes = (5,2))
axs[1].axhline(y = 30,c = 'black',dashes = (5,2))
axs[2].plot(x,Plot_Data['MACD'])
axs[2].axhline(y = 0.50,c = 'black',dashes = (5,2))

axs[0].set(title = "Mean Consolidated Total Market Closing Past "+ str(N_DAYS_AGO) +" Days\nSMA is " + str(OLS_Window) +" Day Moving Average",
      ylabel = "Mean Close Across All Tickers",
      xlabel = "")
axs[1].set(ylabel = "RSI",
      xlabel = "",
      ylim = [0,100])
axs[2].set(
    ylabel = "MACD",
    xlabel = "",
    ylim = [0,1]
)
fig.set_size_inches(16,9)


# In[131]:


print("Running Bayesian Parameter Optimization")

## Inititalizing Storage Lists
Return = []; DD = []; nt = []; p = []; oc = [];

## Defining Search Area
pcts = np.arange(0.50,0.95,0.05)
sell = ['open','close']

## Running Brute Force Search
for pct in tqdm(pcts):
    for time in sell:
            Results = Bayesian_Leveraged(
                Combined_Data,
                Combined_Index_Data,
                Index = "^NDX",
                test_window = 90,
                open_close=time,
                pct = pct,
                print_results = False
            )
            Return.append(Results['ret'])
            DD.append(Results['dd'])
            nt.append(Results['nt'])
            p.append(pct)
            oc.append(time)

## Combining Results
Bayes_Results = pd.DataFrame({'+- Bull %':p,'Cumulative Return':Return,'Max Drawdown':DD,'# Trades':nt,'Sell Time':oc}).     query('`Cumulative Return` > 1').     assign(Decider = lambda x: x['Cumulative Return'] - x['Max Drawdown']).     sort_values('Decider',ascending=False)
Bayes_Results.head(10)    


# In[134]:


Positive_Prob = np.round(float(Bayes_Results['+- Bull %'].head(1)),2)
Sell_Time = list(Bayes_Results['Sell Time'].head(1))[0]
Optimized_Results = Bayesian_Leveraged(
    Combined_Data,
    Combined_Index_Data,
    Index = "^NDX",
    test_window = 90,
    open_close=Sell_Time,
    pct = Positive_Prob,
    print_results = True
)


# In[128]:


pos_prob = Optimized_Results['pp']/100

## Pulling Relevent Account Information
Account_Equity = Get_Equity(Account)

## Pulling Open Orders
Open_Orders = Get_Open_Orders(Account)

## Pulling Current Stock Holdings
Current_Holdings = Get_Holdings(Account)

## Pulling Current Buying Power
Buying_Power = Get_Buying_Power(Account)

## Pulling Current Prices
TQQQ_Current_Price = api.get_last_trade('TQQQ').price;
T_Buy_Quantity = np.floor(Buying_Power/TQQQ_Current_Price);
SQQQ_Current_Price = api.get_last_trade('SQQQ').price; 
S_Buy_Quantity = np.floor(Buying_Power/SQQQ_Current_Price);

if datetime.now().hour >= 16 & datetime.now().hour < 18:
## Checking Probability Conditions
    if pos_prob > Positive_Prob:
        if 'SQQQ' in list(Current_Holdings.columns):
            print("Closing SQQQ Position")
            Limit_Order(
                s = 'SQQQ',
                Quantity = Current_Holdings.loc['quantity','SQQQ'],
                Limit = SQQQ_Current_Price,
                side = 'sell',
                Account = Account
            )
            sleep(15)
        if 'TQQQ' in list(Current_Holdings.columns):
            print("Maintaining Position in TQQQ")
        else:
            print("Opening TQQQ Position") 
            Limit_Order(
                s = 'TQQQ',
                Quantity = T_Buy_Quantity,
                Limit = TQQQ_Current_Price,
                side = 'buy',
                Account = Account
            )
    elif 1 - pos_prob > Positive_Prob:
        if 'TQQQ' in list(Current_Holdings.columns):
            print("Closing TQQQ Position Moving to Short")
            Limit_Order(
                s = 'TQQQ',
                Quantity = Current_Holdings.loc['quantity','TQQQ'],
                Limit = TQQQ_Current_Price,
                side = 'sell',
                Account = Account
            )
            sleep(15)
        if 'SQQQ' in list(Current_Holdings.columns):
            print("Maintaining Position in SQQQ")
        else:
            print("Opening SQQQ Position")    
            Limit_Order(
                s = 'SQQQ',
                Quantity = S_Buy_Quantity,
                Limit = SQQQ_Current_Price,
                side = 'buy',
                Account = Account
            )
    else:
        print("+- Probability Not Met, No Investments Today") 
        ## Closing Any Open Investments
        if 'TQQQ' in list(Current_Holdings.columns):
            print("Closing TQQQ Position")
            Limit_Order(
                s = 'TQQQ',
                Quantity = Current_Holdings.loc['quantity','TQQQ'],
                Limit = TQQQ_Current_Price,
                side = 'sell',
                Account = Account
            )
        if 'SQQQ' in list(Current_Holdings.columns):
                print("Closing SQQQ Position")
                Limit_Order(
                    s = 'SQQQ',
                    Quantity = Current_Holdings.loc['quantity','SQQQ'],
                    Limit = SQQQ_Current_Price,
                    side = 'sell',
                    Account = Account
                )
elif datetime.now().hour >= 9 & datetime.now().hour < 16:
    print("Closing Any Current Holdings") 
    ## Closing Any Open Investments
    if 'TQQQ' in list(Current_Holdings.columns):
        print("Closing TQQQ Position")
        Limit_Order(
            s = 'TQQQ',
            Quantity = Current_Holdings.loc['quantity','TQQQ'],
            Limit = TQQQ_Current_Price,
            side = 'sell',
            Account = Account
        )
    if 'SQQQ' in list(Current_Holdings.columns):
            print("Closing SQQQ Position")
            Limit_Order(
                s = 'SQQQ',
                Quantity = Current_Holdings.loc['quantity','SQQQ'],
                Limit = SQQQ_Current_Price,
                side = 'sell',
                Account = Account
            )
else:
    print("Outside of Tradable Market Hours")

