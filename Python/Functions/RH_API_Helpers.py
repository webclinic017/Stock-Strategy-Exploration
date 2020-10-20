## Automated Helper Functions
def Get_Open_Orders():
    Open_Orders = rs.orders.get_all_open_stock_orders()
    # Appending Stock Symbols
    for item in Open_Orders:
        item['symbol'] = rs.get_symbol_by_url(item['instrument'])
    return Open_Orders

def Cancel_Open_Orders(s):
    # Canceling Any Open Orders
    if s in [item['symbol'] for item in Open_Orders]:
        Order_Ids = [item['id'] for item in Open_Orders if item['symbol'] == s]
        for ID in Order_Ids:
            print("Canceling Existing Order:",ID)
            rs.orders.cancel_stock_order(ID)
            sleep(5)

def Get_Holdings():
    my_stocks = rs.build_holdings()
    Current_Holdings = pd.DataFrame({key:value for key,value in my_stocks.items()})
    return Current_Holdings

def Rebalance_Lower(s):
    print("\nRebalancing",s,": Currently =",Pct_Holding,", Target =",Rcm_Holding)

    # Canceling Any Open Orders
    Cancel_Open_Orders(s)

    # Placing Sell Order
    Sell_Amount = float(Current_Holdings[s]['equity']) - Account_Equity*Rcm_Holding/100
    print("Selling ",np.round(Sell_Amount))
    Order_Info = rs.orders.order_sell_fractional_by_price(
        symbol = s,
        amountInDollars = Sell_Amount,
        timeInForce = 'gfd',
        extendedHours = False
    )
    print("Order ID:",Order_Info['id'],"placed")

def Rebalance_Higher(s):
    Buying_Power = float(rs.profiles.load_account_profile('buying_power'))
    print("\nRebalancing",s,": Currently =",Pct_Holding,", Target =",Rcm_Holding)

    # Canceling Any Open Orders
    Cancel_Open_Orders(s)

     # Placing Buy Order
    Buy_Amount = Account_Equity*Rcm_Holding/100 - float(Current_Holdings[s]['equity'])
    print("Buying ",np.round(Buy_Amount))

    if Buy_Amount <= Buying_Power and Buy_Amount > 1:
        Order_Info = rs.orders.order_buy_fractional_by_price(
            symbol = s,
            amountInDollars = Buy_Amount,
            timeInForce = 'gfd',
            extendedHours = False
        )
        print("Order ID:",Order_Info['id'],"placed")
    else:
        if Buy_Amount < 1:
            print("Buy Amount Less Than Minimum (1$) :",np.round(Buy_Amount))
        else:
            print("Not Enough Buying Power")
            
def Close_Position(s):
    print("\nClosing",s,"Position")
    
    # Canceling Any Open Orders
    Cancel_Open_Orders(s)
    
    # Placing Order
    Order_Info = rs.orders.order_sell_fractional_by_quantity(
        symbol = s,
        quantity = Current_Holdings[s]['quantity'],
        timeInForce = 'gfd',
        extendedHours = False
    )
    print("Order ID:",Order_Info['id'],"placed")

def Open_Position(s):
    Buying_Power = float(rs.profiles.load_account_profile('buying_power'))
    print("\nOpening",s,"Position")
    
    ## Checking Current Price
    Current_Price = api.get_barset(s,timeframe = 'day',limit=1).df[s]['close']
    Buy_Amount = Account_Equity*Rcm_Holding/100
    Buy_Quantity = math.floor(Buy_Amount/Current_Price)
    
    
    if Buy_Amount <= Buying_Power and Buy_Amount > 1 and Buy_Quantity > 0:
        
        # Canceling Any Open Orders
        Cancel_Open_Orders(s)
        
        # Determinig Limit Price
        Stock_Info = Group_Consolidator(
            Combined_Data = Combined_Data,
            groups = [s],
            column = 'stock'
        )
        Limit_Buy = float(Stock_Info.last_price + float(Stock_Info.mu_day_down))
        
        # Placing Order
        Order_Info = rs.orders.order_buy_limit(
            symbol = s,
            quantity = Buy_Quantity,
            limitPrice = Limit_Buy,
            timeInForce = 'gtc',
            extendedHours = True
        )
        
    else:
        if Buy_Amount < 1:
            print("Buy Amount Less Than Minimum (1$) :",np.round(Buy_Amount))
        elif Buy_Quantity < 1:
            print("Unable To Limit Buy < 1 Share")
        else:
            print("Not Enough Buying Power")