# -*- coding: utf-8 -*-

import glob, os, sys
import matplotlib.pyplot as plt
import numpy as np
import matplotlib.style as style
import seaborn as sns
import pandas as pd
from collections import OrderedDict
from scipy import interpolate
from matplotlib.gridspec import GridSpec

sns.set_palette("colorblind")
sns.set_color_codes("colorblind")
# sns.set_style("darkgrid",  {"axes.facecolor": ".85"})
sns.set_style("darkgrid")

path = "/home/karpouzi/Research/Eukaryote-mountdir/resultsNaturePopPSE2/population7300.csv"
df = pd.read_csv(path)

print(df.loc[(df["N"]>0.5) & (df["P"]>200)])

fig, axs = plt.subplots(nrows=1,ncols=1)

# df1 = df.loc[(df["P"]>0.0)]

# palette = sns.light_palette("seagreen", as_cmap=True)
# palette = sns.color_palette("Greens", as_cmap=True)
palette = sns.cubehelix_palette(start=2, rot=.25, dark=0, light=.8, as_cmap=True)

minSamples = 0

sns.scatterplot(x='N',y='P',hue='evolution$samples',s=50,palette='flare',data=df.loc[df['evolution$samples']>minSamples])
plt.legend(title='Samples')
axs.set_xlabel('Fraction of Natural Land')
axs.set_ylabel('Population Size')

# sns.scatterplot(x='N',y='P',data=df1)
plt.savefig('naturePopPSE.pdf', format='pdf', dpi = 600, bbox_inches='tight')

plt.show()
