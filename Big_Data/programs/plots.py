import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
import seaborn as sns
sns.set_theme()
plt.rcParams['figure.figsize'] = (12,5)
plt.rcParams['figure.dpi'] = 80


df = pd.DataFrame(data={

'01': 115217.34262530117,      
'02': 115811.11016347304,      
'03': 115801.70906242428,      
'04': 115880.79261856287,      
'05': 116648.94543734947,      
'06': 106514.02059940119,      
'07': 105799.95474790418,      
'08': 151882.14258484848,      
'09': 184176.85309277117,      
'10': 169984.75482891573,      
'11': 164121.39139212118,      
'12': 169704.57312574852,      
'13': 177061.38221437126,      
'14': 178386.57954311377,      
'15': 162398.93439573172,      
'16': 166989.15328614446,      
'17': 173546.53333353295,      
'18': 171684.80188614456,      
'19': 132489.87956848484,      
'20': 125311.93179090915,      
'21': 125420.91426325304,      
'22': 125730.92196969697,      
'23': 121866.72329515149}, index=[1] ).T
df = df / 10**6

# df.plot()
plt.step(x=np.arange(1,24,1), y=df[1])
plt.xlabel('Hour of the day (HH)')
plt.ylabel('Average Hourly Total Electric Demand Power (MW/H)')
plt.xticks(np.arange(0,24,1))
plt.ylim((0,0.2))
plt.savefig('q4_plot.pdf')
plt.show()