def Group_Consolidator(Combined_Data,
                       groups,
                       column,
                       min_stock_count,
                       min_alpha = 0,
                       max_alpha_p = 0.50,
                       min_alphav = 0,
                       max_alphav_p = 0.50,
                       min_beta = 0.75,
                       max_beta = 1.5,
                       max_beta_p = 0.50
                      ):
    Group_Data = defaultdict(pd.DataFrame)
    for i in groups:
        TMP = Stock_Consolidator(Combined_Data[Combined_Data[column] == i])
        mask = [i in TMP.index.values for i in Total_Market.index.values]
        TM_TMP = Total_Market.iloc[mask,:]

        Stock_Count = int(TMP['count'].tail(1))
        TMP['Market_Return'] = TM_TMP['close_diff']
        TMP['Market_Volume'] = TM_TMP['volume_diff']

        ## Rolling OLS Regression
        rols = RollingOLS(TMP['close_diff'],sm.add_constant(TMP['Market_Return']),window = OLS_Window).fit()
        rolsv = RollingOLS(TMP['volume_diff'],sm.add_constant(TMP['Market_Volume']),window = OLS_Window).fit()

        ## Pulling Relevent Information
        alpha_pvalue = list(pd.Series(np.around(rols.pvalues[:,0],2)))
        beta_pvalue = list(pd.Series(np.around(rols.pvalues[:,1],2)))
        alpha = list(rols.params['const'])
        beta = list(rols.params['Market_Return'])

        alphav_pvalue = list(pd.Series(np.around(rolsv.pvalues[:,0],2)))
        betav_pvalue = list(pd.Series(np.around(rolsv.pvalues[:,1],2)))
        alphav = list(rolsv.params['const'])
        betav = list(rolsv.params['Market_Volume'])

        date = list(TM_TMP.index.values)

        Group_Data[i] = pd.DataFrame(data = {'date':date,
                                              'stock_count':Stock_Count,
                                              'alpha_pvalue':alpha_pvalue,
                                              'alpha':alpha,
                                              'beta_pvalue':beta_pvalue,
                                              'beta':beta,
                                              'alphav_pvalue':alphav_pvalue,
                                              'alphav':alphav,
                                              'betav_pvalue':betav_pvalue,
                                              'betav':betav}).tail(5)
    for s in Group_Data:
        Group_Data[s].insert(0, column, [s]*len(Group_Data[s]))

    Combined_Group = pd.concat(Group_Data.values())  
    Group_Summary = Combined_Group. \
        groupby(column). \
        mean(). \
        sort_values(by = ['alpha','alpha_pvalue','beta','beta_pvalue'],ascending = [0,1,1,0])
    Group_Summary = Group_Summary[Group_Summary.stock_count >= min_stock_count]
    Group_Summary = Group_Summary[Group_Summary.alpha > min_alpha]
    Group_Summary = Group_Summary[Group_Summary.alpha_pvalue <= max_alpha_p]
    Group_Summary = Group_Summary[Group_Summary.alphav > min_alphav]
    Group_Summary = Group_Summary[Group_Summary.alphav_pvalue <= max_alphav_p]
    Group_Summary = Group_Summary[Group_Summary.beta > min_beta]
    Group_Summary = Group_Summary[Group_Summary.beta < max_beta]
    Group_Summary = Group_Summary[Group_Summary.beta_pvalue <= max_beta_p]
    return Group_Summary