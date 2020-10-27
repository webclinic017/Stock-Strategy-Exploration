def Group_Consolidator(Combined_Data,
                       groups,
                       column,
                       q = 0.20,
                       min_last_ret = -0.10,
                       max_rsi = 100,
                       min_macd = -10,
                       min_risk_ratio = 0,
                       min_alpha = -2,
                       max_alpha_p = 1
                      ):
    
    Group_Data = defaultdict(pd.DataFrame)
    for i in groups:
        TMP = Stock_Consolidator(Combined_Data[Combined_Data[column] == i])
        mask = [i in TMP.index.values for i in Total_Market.index.values]
        TM_TMP = Total_Market.iloc[mask,:]

        Stock_Count = int(TMP['count'].tail(1))
        TMP['Market_Return'] = TM_TMP['close_diff']

        ## Rolling OLS Regression
        rols = RollingOLS(TMP['close_diff'],sm.add_constant(TMP['Market_Return']),window = OLS_Window).fit()

        ## Pulling Relevent Information
        alpha_pvalue = list(pd.Series(np.around(rols.pvalues[:,0],2)))
        beta_pvalue = list(pd.Series(np.around(rols.pvalues[:,1],2)))
        alpha = list(rols.params['const'])
        beta = list(rols.params['Market_Return'])
        
        last_price = TMP['close']
        ret = TMP['close_return']
        rsi = TMP['RSI']
        macd = TMP['MACD']
        
        ## Calculating Various Risk Metrics
        sd_ret = np.round(np.std(TMP['close_diff'][TMP['close_diff'] > 0].tail(OLS_Window*3)),6)
        sd_loss = np.round(np.std(TMP['close_diff'][TMP['close_diff'] <= 0].tail(OLS_Window*3)),6)
        risk_ratio = sd_ret/sd_loss
        mu_ret = np.mean(TMP['close_diff'].tail(OLS_Window*3))
        
        ## Daily Movement For Loss Orders
        TMP['close_yesterday'] = TMP['close'].shift(1)
        up_data = (TMP['high'].tail(OLS_Window*3) - TMP['close_yesterday'].tail(OLS_Window*3))/TMP['close_yesterday'].tail(OLS_Window*3)
        down_data = (TMP['low'].tail(OLS_Window*3) - TMP['close_yesterday'].tail(OLS_Window*3))/TMP['close_yesterday'].tail(OLS_Window*3)
        ## Various Daily Movement Statistics
        sd_day_up = np.std(up_data)
        mu_day_up = np.mean(up_data)
        quant_day_up = np.quantile(up_data,q)
        sd_day_down = np.std(down_data)
        mu_day_down = np.mean(down_data)
        quant_day_down = np.quantile(down_data,1-q)

        Group_Data[i] = pd.DataFrame(data = {'last_period_return':ret,
                                             'last_price':last_price,
                                             'quant_day_up':quant_day_up,
                                             'mu_day_up':mu_day_up,
                                             'sd_day_up':sd_day_up,
                                             'quant_day_down':quant_day_down,
                                             'mu_day_down':mu_day_down,
                                             'sd_day_down':sd_day_down,
                                             'sd_ret':sd_ret,
                                             'sd_loss':sd_loss,
                                             'risk_ratio': risk_ratio,
                                             'mu_ret':mu_ret,
                                             'rsi':rsi,
                                             'macd':macd,
                                             'alpha':alpha,
                                             'alpha_p':alpha_pvalue,
                                             'beta':beta,
                                             'beta_p':beta_pvalue}).tail(1)
    for s in Group_Data:
        Group_Data[s].insert(0, column, [s]*len(Group_Data[s]))

    Combined_Group = pd.concat(Group_Data.values())  
    
    Group_Summary = Combined_Group. \
        groupby(column). \
        mean(). \
        sort_values(by = ['alpha','beta'],ascending = [0,1])
    
    Group_Summary = Group_Summary[Group_Summary.risk_ratio > min_risk_ratio]
    Group_Summary = Group_Summary[Group_Summary.last_period_return > min_last_ret]
    Group_Summary = Group_Summary[Group_Summary.rsi < max_rsi]
    Group_Summary = Group_Summary[Group_Summary.macd > min_macd]
    
    Group_Summary = Group_Summary[Group_Summary.alpha > min_alpha]
    Group_Summary = Group_Summary[Group_Summary.alpha_p < max_alpha_p]
    
    return Group_Summary