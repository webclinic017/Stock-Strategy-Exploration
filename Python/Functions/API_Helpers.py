## Automated Helper Functions
def Get_Equity(Account = 'rh'):
    if Account == 'rh':
        equity = float(rs.build_user_profile()['equity'])
    elif Account == 'ap':
        account = api.get_account()
        equity = float(account.equity)
    elif Account == 'app':
        account = apip.get_account()
        equity = float(account.equity)
    return equity

def Get_Holdings(Account = 'rh'):
    if Account == 'rh':
        my_stocks = rs.build_holdings()
        holdings = pd.DataFrame({key:value for key,value in my_stocks.items()})
    elif Account == 'ap':
        holdings = api.list_positions()
    elif Account == 'app':
        holdings = apip.list_positions()
    return holdings

def Get_Open_Orders(Account = 'rh'):
    if Account == 'rh':
        Open_Orders = rs.orders.get_all_open_stock_orders()
        # Appending Stock Symbols
        for item in Open_Orders:
            item['symbol'] = rs.get_symbol_by_url(item['instrument'])
    elif Account == 'ap':
        Open_Orders = api.list_orders(status = 'open')
        Symbols = [ord.symbol for ord in Open_Orders]
        IDs = [ord.id for ord in Open_Orders]
        Open_Orders = pd.DataFrame({'symbol':Symbols,'id':IDs})
    elif Account == 'app':
        Open_Orders = apip.list_orders(status = 'open')
        Symbols = [ord.symbol for ord in Open_Orders]
        IDs = [ord.id for ord in Open_Orders]
        Open_Orders = pd.DataFrame({'symbol':Symbols,'id':IDs})
    return Open_Orders

def Cancel_Open_Orders(s,Account = 'rh'):
    if Account == 'rh':
        # Canceling Any Open Orders
        if s in [item['symbol'] for item in Open_Orders]:
            Order_Ids = [item['id'] for item in Open_Orders if item['symbol'] == s]
            for ID in Order_Ids:
                print("\nCanceling Existing Order:",ID)
                rs.orders.cancel_stock_order(ID)
                sleep(5)
    elif Account == 'ap':
        if s in list(Open_Orders.symbol):
            Order_Ids = list(Open_Orders.loc[Open_Orders.symbol == s,'id'])
            for ID in Order_Ids:
                print("\nCanceling Existing Order:",ID)
                api.cancel_order(ID)
                sleep(5)
    elif Account == 'app':
        if s in list(Open_Orders.symbol):
            Order_Ids = list(Open_Orders.loc[Open_Orders.symbol == s,'id'])
            for ID in Order_Ids:
                print("\nCanceling Existing Order:",ID)
                apip.cancel_order(ID)
                sleep(5)

def Rebalance_Lower(s,q = 0.20,Account = 'rh'):
    print("\n\nRebalancing",s,": Currently =",Pct_Holding,", Target =",Rcm_Holding)
    
    # Determinig Stop Loss / Take Profit
    Stock_Info = Group_Consolidator(
        Combined_Data = Combined_Data,
        groups = [s],
        column = 'stock',
        q = q
    )

    # Placing Sell Order
    Sell_Amount = float(Current_Holdings[s]['equity']) - Account_Equity*Rcm_Holding/100
    Sell_Amount = float(np.floor(Sell_Amount/float(Stock_Info['last_price'])))
    Limit_Price = float(Stock_Info.last_price) + float(Stock_Info.quant_day_up)*float(Stock_Info.last_price)
    
    if Sell_Amount < 1:
        print("\nCan Only Rebalance In Whole Share Amounts")
    else:
        print("\nSelling ",np.round(Sell_Amount))
        
        # Canceling Any Open Orders
        Cancel_Open_Orders(s,Account)
        
        try:
            if Account == 'rh':
                Order_Info = rs.orders.order(
                    symbol = s,
                    quantity = Sell_Amount,
                    orderType = 'limit',
                    trigger = 'stop',
                    stopPrice = Limit_Price,
                    limitPrice = Limit_Price,
                    side = 'sell',
                    timeInForce = 'gfd',
                    extendedHours = False
                )
            elif Account == 'ap':
                Order_Info = api.submit_order(
                    symbol = s,
                    side = 'sell',
                    type = 'stop_limit',
                    qty = str(Sell_Amount),
                    time_in_force= 'day',
                    order_class = 'simple',
                    limit_price = str(Limit_Price),
                    stop_price = str(Limit_Price)
                )
            elif Account == 'app':
                Order_Info = apip.submit_order(
                    symbol = s,
                    side = 'sell',
                    type = 'stop_limit',
                    qty = str(Sell_Amount),
                    time_in_force= 'day',
                    order_class = 'simple',
                    limit_price = str(Limit_Price),
                    stop_price = str(Limit_Price)
                )
            sleep(5)
            print("\nOrder ID:",Order_Info['id'],"placed")
        except:
            print("\nOrder Failed For",s)
            print(Order_Info)

def Rebalance_Higher(s,q = 0.20,Account = 'rh'):
    if Account == 'rh':
        Buying_Power = float(rs.profiles.load_account_profile('buying_power'))
    elif Account == 'ap':
        account = api.get_account()
        Buying_Power = float(account.buying_power)
    elif Account == 'app':
        account = apip.get_account()
        Buying_Power = float(account.buying_power) 
    
    if float(Current_Holdings[s]['quantity']) == 0:
        print("\nSkipping Rebalance of",s,"Limit Buy Order In Place")
    else:
        print("\n\nRebalancing",s,": Currently =",Pct_Holding,", Target =",Rcm_Holding)

        # Determinig Stop Loss / Take Profit
        Stock_Info = Group_Consolidator(
            Combined_Data = Combined_Data,
            groups = [s],
            column = 'stock',
            q = q
        )

        # Placing Buy Order
        Buy_Amount = Account_Equity*Rcm_Holding/100 - float(Current_Holdings[s]['equity'])
        Buy_Amount = float(np.floor(Buy_Amount/float(Stock_Info['last_price'])))
        Purchase_Price = Buy_Amount*Stock_Info['last_price']
        Limit_Price = float(Stock_Info.last_price) + float(Stock_Info.quant_day_down)*float(Stock_Info.last_price)
        
        if Buy_Amount < 1:
            print("\nCan Only Rebalance In Whole Share Amounts")
        else:
            if float(Purchase_Price) <= float(Buying_Power):
                print("\nBuying ",Buy_Amount)
                    
                # Canceling Any Open Orders
                Cancel_Open_Orders(s,Account)
                
                try:
                    if Account == 'rh':
                        Order_Info = rs.orders.order(
                            symbol = s,
                            quantity = Buy_Amount,
                            orderType = 'limit',
                            trigger = 'immediate',
                            limitPrice = Limit_Price,
                            side = 'buy',
                            timeInForce = 'gfd',
                            extendedHours = False
                        )
                    elif Account == 'ap':
                        Order_Info = api.submit_order(
                            symbol = s,
                            side = 'buy',
                            type = 'limit',
                            qty = str(Buy_Amount),
                            time_in_force= 'day',
                            order_class = 'simple',
                            limit_price = str(Limit_Price)
                        )
                    elif Account == 'app':
                        Order_Info = apip.submit_order(
                            symbol = s,
                            side = 'buy',
                            type = 'limit',
                            qty = str(Buy_Amount),
                            time_in_force= 'day',
                            order_class = 'simple',
                            limit_price = str(Limit_Price)
                        )
                    sleep(5)
                    print("\nOrder ID:",Order_Info['id'],"placed")
                except:
                    print("\nOrder Failed For",s)
                    print(Order_Info)
            else:
                print("\nNot Enough Buying Power")

def Close_Position(s,Account = 'rh'):
    print("\n\nClosing",s,"Position")
    
    # Canceling Any Open Orders
    Cancel_Open_Orders(s,Account)
    
    try:
        if Account == 'rh':
            # Placing Order
            Order_Info = rs.orders.order_sell_fractional_by_quantity(
                symbol = s,
                quantity = Current_Holdings[s]['quantity'],
                timeInForce = 'gfd',
                extendedHours = False
            )
        elif Account == 'ap':
            Order_Info = api.submit_order(
                symbol = s,
                side = 'sell',
                type = 'market',
                qty = Current_Holdings[s]['quantity'],
                time_in_force= 'day',
                order_class = 'simple'
            )
        elif Account == 'app':
            Order_Info = apip.submit_order(
                symbol = s,
                side = 'sell',
                type = 'market',
                qty = Current_Holdings[s]['quantity'],
                time_in_force= 'day',
                order_class = 'simple'
            )
        sleep(5)
        print("\nOrder ID:",Order_Info['id'],"placed")
    except:
        print("\nOrder Failed For",s)
        print(Order_Info)

def Open_Position(s,q = 0.20,Account = 'rh'):
    if Account == 'rh':
        Buying_Power = float(rs.profiles.load_account_profile('buying_power'))
    elif Account == 'ap':
        account = api.get_account()
        Buying_Power = float(account.buying_power)
    elif Account == 'app':
        account = apip.get_account()
        Buying_Power = float(account.buying_power) 
    
    ## Checking Current Price
    Current_Price = api.get_barset(s,timeframe = 'day',limit=1).df[s]['close']
    Buy_Amount = Account_Equity*Rcm_Holding/100
    Buy_Quantity = math.floor(Buy_Amount/Current_Price)
    
    if Buy_Amount <= Buying_Power and Buy_Amount > 1 and Buy_Quantity > 0:
        # Canceling Any Open Orders
        Cancel_Open_Orders(s,Account)
        
        print("\n\nOpening",s,"Position")
        
        # Determinig Limit Price
        Stock_Info = Group_Consolidator(
            Combined_Data = Combined_Data,
            groups = [s],
            column = 'stock',
            q = q
        )
        Limit_Buy = float(Stock_Info.last_price) + float(Stock_Info.quant_day_down)*float(Stock_Info.last_price)
        
        try:
            if Account == 'rh':
                # Placing Order
                Order_Info = rs.orders.order_buy_limit(
                    symbol = s,
                    quantity = Buy_Quantity,
                    limitPrice = Limit_Buy,
                    timeInForce = 'gfd',
                    extendedHours = True
                )
                sleep(5)
                print("\nOrder ID:",Order_Info['id'],"placed")
            elif Account == 'ap':
                Order_Info = api.submit_order(
                    symbol = s,
                    side = 'buy',
                    type = 'limit',
                    qty = str(Buy_Quantity),
                    time_in_force= 'day',
                    order_class = 'simple',
                    limit_price = str(Limit_Buy)
                )
            elif Account == 'app':
                Order_Info = apip.submit_order(
                    symbol = s,
                    side = 'buy',
                    type = 'limit',
                    qty = str(Buy_Quantity),
                    time_in_force= 'day',
                    order_class = 'simple',
                    limit_price = str(Limit_Buy)
                )
        except:
            print("\nOrder Failed For",s)
            print(Order_Info)
        
    else:
        if Buy_Amount < 1:
            print("\nBuy Amount Less Than Minimum (1$) :",np.round(Buy_Amount))
        elif Buy_Quantity < 1:
            print("\nUnable To Limit Buy < 1 Share")
        else:
            print("\nNot Enough Buying Power")
            
def Exit_Orders(s,q = 0.95,Account = 'rh'):
            # Determinig Stop Loss / Take Profit
            Stock_Info = Group_Consolidator(
                Combined_Data = Combined_Data,
                groups = [s],
                column = 'stock',
                q = q
            )

            Pct_Return = float(Current_Holdings[s]['percent_change'])
            Quantity_Held = np.floor(float(Current_Holdings[s]['quantity']))

            if Pct_Return > 0 and Quantity_Held > 0:
                Take_Profit = float(Stock_Info.last_price) + float(Stock_Info.quant_day_up)*float(Stock_Info.last_price)
                print("\nSetting Take Profit For",s,"At:",np.round(Take_Profit,2))

                try: 
                    Order_Info = rs.orders.order(
                        symbol = s,
                        quantity = Quantity_Held,
                        orderType = 'limit',
                        trigger = 'immediate',
                        limitPrice = Take_Profit,
                        side = 'sell',
                        timeInForce = 'gfd',
                        extendedHours = False
                    )
                    sleep(5)
                    print("\nOrder ID:",Order_Info['id'],"placed")

                except:
                    print("\nOrder Failed For",s)
                    print(Order_Info)
            
            elif Pct_Return < 0 and Quantity_Held > 0:
                Stop_Loss =  float(Stock_Info.last_price) + float(Stock_Info.quant_day_down)*float(Stock_Info.last_price)
                print("\nSetting Stop Loss For",s,"At:",np.round(Stop_Loss,2))

                try:
                    Order_Info = rs.orders.order(
                        symbol = s,
                        quantity = Quantity_Held,
                        orderType = 'limit',
                        trigger = 'stop',
                        stopPrice = Stop_Loss,
                        limitPrice = Stop_Loss,
                        side = 'sell',
                        timeInForce = 'gfd',
                        extendedHours = False
                    )
                    sleep(5)
                    print("\nOrder ID:",Order_Info['id'],"placed")
                except:
                    print("\nOrder Failed For",s)
                    print(Order_Info)
            else:
                print("\nCan Only Set Loss/Profit On Whole Quantities")