import matplotlib.pyplot as plt
import pandas as pd
import sys

data = pd.read_csv(sys.stdin)

# scatter plot
data.plot(x='n', y='millis')
  
# set the title
plt.title('Union Find Benchmark')

# show the plot
plt.show()