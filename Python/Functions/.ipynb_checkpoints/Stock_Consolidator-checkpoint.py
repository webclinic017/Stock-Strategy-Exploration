def Stock_Consolidator(df):
    df = df.groupby('date').agg(
        close = pd.NamedAgg(column='close', aggfunc= np.mean),
        high = pd.NamedAgg(column='high', aggfunc= np.mean),
        low = pd.NamedAgg(column='low', aggfunc= np.mean),
        open = pd.NamedAgg(column='open', aggfunc= np.mean),
        volume = pd.NamedAgg(column='volume', aggfunc= np.mean),
        count = pd.NamedAgg(column='close',aggfunc = len)
    )

    def Col_Diff_Lagger(df,col_name,lag = 1,diff_name = "_diff"):
        df[col_name+'_prev'] = df[col_name].shift(lag)
        df[col_name+diff_name] = (df[col_name] - df[col_name+'_prev'])/df[col_name+'_prev']
        df = df.drop(columns = [col_name+'_prev'])
        return df

    df = Col_Diff_Lagger(df,'close',1)
    df = Col_Diff_Lagger(df,'close',5,diff_name = "_return")
    df = Col_Diff_Lagger(df,'volume',1)
    df['sma'] = df['close'].rolling(OLS_Window).mean()
    df['RSI'] = RSI(df['close'],14)
    df['MACD'] = MACD(df['close'])
    df['AD'] = AD_Ind(df['close'],df['low'],df['high'],df['volume'])
    
    Running_Up = [0]
    Running_Down = [0]
    for i in range(1,len(df.close)):
        if df.close[i] - df.close[i-1] > 0:
            Running_Up.append(Running_Up[i-1] + 1)
            Running_Down.append(0)
        else:
            Running_Down.append(Running_Down[i-1] + 1)
            Running_Up.append(0)
    df = df. \
        assign(Running_Up = Running_Up,
               Running_Down = Running_Down)
    
    
    return df