def Bayesian_Leveraged(
    Combined_Data,
    Combined_Index_Data,
    Index = "^NDX",
    test_window = 20,
    pct = 0.75,
    open_close = "close",
    print_results = False
):
    ## Defining Leveraged ETFs
    if Index == "^NDX":
        bull = "TQQQ"
        bear = "SQQQ"
    elif Index == "^GSPC":
        bull = "UPRO"
        bear = "SPXU"
    elif Index == "^DJI":
        bull = "UDOW"
        bear = "SDOW"
    else:
        raise ValueError("Index must be one of ^NDX (NASDAQ), ^GSPC (S&P 500), ^DJI (Dow Jones)")
    
    ## Pulling Ticker Data
    Bull_Data = Stock_Consolidator(Combined_Data.query('stock == @bull'))
    Bear_Data = Stock_Consolidator(Combined_Data.query('stock == @bear'))

    ## Creating Model Dataframes
    X = Combined_Index_Data.loc[Combined_Index_Data.stock == Index,['RSI','MACD','AD','Running_Up','Running_Down','close_diff']]
    X = X. \
        assign(MACD_Delta = X.MACD - X.MACD.shift(1)). \
        assign(RSI_Delta = X.RSI - X.RSI.shift(1)). \
        query('RSI > 0 & RSI_Delta == RSI_Delta')
    ## Adding in VIX Index Changes
    X.index = X.index.date
    VIX_Add = Combined_Index_Data.loc[Combined_Index_Data.stock == "^VIX",['close','close_diff']]
    VIX_Add.columns = ['VIX_Close','VIX_Diff']
    X.join(VIX_Add)

    
    Bull_Data = Bull_Data.tail(len(X.index))
    Bear_Data = Bear_Data.tail(len(X.index))
    if open_close == "close":
        y1 = list(Bull_Data.close_diff.shift(-1) > 0)
        ## Creating Returns Vectors
        Bull_Change = (Bull_Data.close.shift(-1) - Bull_Data.close)/Bull_Data.close
        Bull_Change = Bull_Change.tail(len(y1))
        Bear_Change = (Bear_Data.close.shift(-1) - Bear_Data.close)/Bear_Data.close
        Bear_Change = Bear_Change.tail(len(y1))
    elif open_close == "open":
        y1 = list(Bull_Data.open_pclose_diff.shift(-1) > 0)
        ## Creating Returns Vectors
        Bull_Change = (Bull_Data.open.shift(-1) - Bull_Data.close)/Bull_Data.close
        Bull_Change = Bull_Change.tail(len(y1))
        Bear_Change = (Bear_Data.open.shift(-1) - Bear_Data.close)/Bear_Data.close
        Bear_Change = Bear_Change.tail(len(y1))
    
    ## Defining Initial Learning Interval
    learning = len(y1) - test_window
    
    ## Reducing X/Y To Learning Period
    X_ini = X.head(learning)
    y1_ini = y1[0:learning]

    ## Initializing Bayesian Models
    gnb1 = GaussianNB()
    fit1 = gnb1.fit(X_ini,y1_ini)

    ## Initialzing Tracking Variables
    num_trades = 0
    max_drawdown = 0
    ret = 1 
    
    ## Looping Through Test Window (-2)
    for i in range(learning+2,len(y1)):
        ## Updating Models
        fit1 = gnb1.partial_fit(X.iloc[[i-1]],[y1[i-1]])
        ## Producing Probabilities
        prob1 = fit1.predict_proba(X.iloc[[i]])
        
        ## Running Position Logic
                
        # Prob of +ve TQQQ Movement
        if prob1[0][1] > pct:
            mult = 1 + Bull_Change[i]
            num_trades += 1

        # Prob of -ve TQQQ Movement
        elif prob1[0][0] > pct:
            mult = 1 + Bear_Change[i]
            num_trades += 1

        # Skip Investing For Today
        else:
            mult = 1
            
        # Updatng Return
        if not math.isnan(mult):
            ret = ret * mult
            
        # Updating max drawdown
        if 1 - ret > max_drawdown:
            max_drawdown = 1 - ret

    if print_results:
        print("\nCumulative Return @ Learning Period of",learning,":",np.round(ret,2))
        print("Min Index +- Prob:",pct*100)
        print("Period Evaluated:",test_window-2)
        print("Max Drawdown:",np.round(max_drawdown,2))
        print("Number of Trades:",num_trades)
        print("Prob of +ve Index Movement:",np.round(prob1[0][1]*100))
        print("Selling @ Market",open_close)
    return {
        'ret':np.round(ret,3),
        'dd':np.round(max_drawdown,3),
        'nt':num_trades,
        'pp':np.round(prob1[0][1]*100)
    }