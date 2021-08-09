def add_binary_prediction(df1):
    binary_signal = []
    for index, row in df1.iterrows():
    #print(row[1])
        if row[1] > 0.5:
            binary_signal.append('1')
        else:
            binary_signal.append('0')
    df1['binary_prediction'] = binary_signal
    return df1