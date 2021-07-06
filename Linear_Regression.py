#!/usr/bin/env python

import matplotlib.pyplot as plt         # import matplotlib library for plotting, this library has the plotting tools
import pandas as pd                     # import pandas for reading and writing files
import sys

arg = sys.argv[1]
# print("Reading {}".format(arg)
df = pd.read_csv(arg)                   # importing data from csv file with name from input argument

x = df.x                                # Assigning x values from the data
y = df.y                                # Assigning y values from the data

plt.plot(x,y,'ro')
plt.xlabel('x'); plt.ylabel('y')
plt.savefig('py_orig.png')              # Saving output of the plot to a png file

from scipy import stats                                                                     # import statistics tool from scipy
slope, intercept, r_value, p_value, std_err = stats.linregress(x,y)                         # calling linregress function
plt.plot(x,y,'ro',label='scatter')                                                          # Plotting the scatter
plt.plot(x,x*slope+intercept,'b',label='Linear fit, r_value={:.2f}'.format(r_value))        # Plotting the linear fit on top
plt.xlabel('x'); plt.ylabel('y')
plt.legend()
plt.savefig('py_lm.png')                # Saving output of the plot to a png file
