import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

lst = pd.read_csv('output.txt', sep=',', header=None)
for i in range(100):
  plt.ylim(0.0,1.2)
  plt.plot(np.arange(0,1,0.1), lst.ix[i][:])
  plt.savefig("images/%03d.png" % i)
  plt.close()
