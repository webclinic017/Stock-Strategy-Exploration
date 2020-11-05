def Bayesian_Leveraged(
    Combined_Data,
    Minimum_Move = 0.01,
    test_window = 20,
    vol_pct = 0.25,
    pct = 0.75,
    print_results = False
):
    ## Pulling Ticker Data
    TQQQ_Data = Stock_Consolidator(Combined_Data.query('stock == "TQQQ"'))
    SQQQ_Data = Stock_Consolidator(Combined_Data.query('stock == "SQQQ"'))

    ## Creating Model Dataframes
    X = Total_Market.loc[:,['RSI','MACD','AD','Running_Up','Running_Down','close_diff']]. \
        assign(MACD_Delta = Total_Market.MACD - Total_Market.MACD.shift(1)). \
        assign(RSI_Delta = Total_Market.RSI - Total_Market.RSI.shift(1)). \
        query('RSI > 0 & RSI_Delta == RSI_Delta')
    TQQQ_Data = TQQQ_Data.tail(len(X.index))
    SQQQ_Data = SQQQ_Data.tail(len(X.index))
    y1 = list(TQQQ_Data.close_diff.shift(-1) > 0)
    y2 = list(abs(TQQQ_Data.close_diff.shift(-1)) > Minimum_Move)
    
    ## Defining Initial Learning Interval
    learning = len(y1) - test_window
    
    ## Creating Returns Vectors
    TQQQ_Change = (TQQQ_Data.close.shift(-1) - TQQQ_Data.close)/TQQQ_Data.close
    TQQQ_Change = TQQQ_Change.tail(len(y1))
    SQQQ_Change = (SQQQ_Data.close.shift(-1) - SQQQ_Data.close)/SQQQ_Data.close
    SQQQ_Change = SQQQ_Change.tail(len(y1))

    ## Reducing X/Y To Learning Period
    X_ini = X.head(learning)
    y1_ini = y1[0:learning]
    y2_ini = y2[0:learning]

    ## Initializing Bayesian Models
    gnb1 = GaussianNB()
    gnb2 = GaussianNB()
    fit1 = gnb1.fit(X_ini,y1_ini)
    fit2 = gnb2.fit(X_ini,y2_ini)

    ## Initialzing Tracking Variables
    num_trades = 0
    max_drawdown = 0
    ret = 1 
    
    ## Looping Through Test Window (-2)
    for i in range(learning+2,len(y1)):
        ## Updating Models
        fit1 = gnb1.partial_fit(X.iloc[[i-1]],[y1[i-1]])
        fit2 = gnb2.partial_fit(X.iloc[[i-1]],[y2[i-1]])
        ## Producing Probabilities
        prob1 = fit1.predict_proba(X.iloc[[i]])
        prob2 = fit2.predict_proba(X.iloc[[i]])
        
        ## Running Position Logic
        # Prob of Min Abs Move > Threshold
        if prob2[0][1] > vol_pct:
                
            # Prob of +ve TQQQ Movement
            if prob1[0][1] > pct:
                mult = 1 + TQQQ_Change[i]
                num_trades += 1
                
            # Prob of -ve TQQQ Movement
            elif prob1[0][0] > pct:
                mult = 1 + SQQQ_Change[i]
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
        print("Minimum Movement of:",Minimum_Move)
        print("Min Volatility Prob:",vol_pct)
        print("Min TQQQ +- Prob:",pct)
        print("Period Evaluated:",test_window-2)
        print("Max Drawdown:",np.round(max_drawdown,2))
        print("Number of Trades:",num_trades)
        print("Prob of > |Minimum Movement|:",np.round(prob1[0][1]*100))
        print("Prob of +ve TQQQ Movement:",np.round(prob2[0][1]*100))
    return {
        'ret':np.round(ret,3),
        'dd':np.round(max_drawdown,3),
        'nt':num_trades,
        'p_vol':np.round(prob1[0][1]*100),
        'pp':np.round(prob2[0][1]*100)
    }