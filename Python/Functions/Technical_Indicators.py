def EMA(x,Days,Smoothing_Factor = 2):
    ema_value = [x[0]]
    for i in range(1,len(x)):
            val = x[i]*(Smoothing_Factor/(1+Days)) + ema_value[i-1]*(1 - (Smoothing_Factor/(1+Days)))
            ema_value.append(val)
    return ema_value
            
def MACD(x,Long = 26,Short = 12,Signal = 9,Smoothing_Factor = 2):
    x = np.nan_to_num(x,0)
    long_ema = EMA(x = x,Days = Long,Smoothing_Factor = Smoothing_Factor)
    short_ema = EMA(x = x,Days = Short,Smoothing_Factor = Smoothing_Factor)
    macd = []
    for i in range(len(short_ema)):
        macd.append(short_ema[i] - long_ema[i])
    signal_ema = EMA(x = macd,Days = Signal,Smoothing_Factor = Smoothing_Factor)
    macd_value = []
    for i in range(len(macd)):
        macd_value.append(macd[i] - signal_ema[i])
    return macd_value

def RSI(x,Period = 14):
    import numpy as np
    rsi_value = np.zeros(len(x))
    x_diff = np.diff(x,n = 1)
    for i in range(Period,len(x_diff)):
        tmp = x_diff[(i-Period):i]
        pos = np.mean(tmp[tmp > 0])
        neg = -1*np.mean(tmp[tmp <= 0])
        rsi_value[i+1] = 100 - (100/(1 + (pos/neg)))
    return list(rsi_value)

def sharpe(returns, rf, days=252):
    volatility = returns.std() * np.sqrt(days)
    sharpe_ratio = (returns.mean() - rf) / volatility
    return sharpe_ratio
                    
    