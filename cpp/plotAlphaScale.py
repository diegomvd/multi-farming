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
# sns.set_style("darkgrid",  {"axes.facecolor": ".8"})
sns.set_style("darkgrid")
# sns.set_style("whitegrid")
# sns.set_style("white")

path = "/home/karpouzi/Research/Eukaryote-mountdir/experimentAlphaScale-310821.csv"

df = pd.read_csv(path, sep=",", header=0)

df['Farm Size']=1/(df['nF'])
df['Farm Scale']=1/np.sqrt(df['nF'])
df['Connectivity Scale']=df['dES']/40.0
df['Scale mismatch']= df['Farm Scale']/df['Connectivity Scale']
df['maxNorm']=df['maxN']/1600/df['N']
df['a'] = np.around(df['a'], decimals=1)

# scatterplot of scale against p,n,nmax with sparing in color

fig, axs = plt.subplots(nrows=3,ncols=1,figsize=(10,10))

sns.scatterplot(x='Scale mismatch',y='maxNorm', hue='a', edgecolor='.2', palette='flare', alpha=0.8, ax = axs[0], data=df)
sns.scatterplot(x='Farm Scale',y='maxNorm', hue='a', edgecolor='.2', palette='flare', alpha=0.8, ax = axs[1], data=df)
sns.scatterplot(x='Pstd',y='Nstd', hue='Scale mismatch', edgecolor='.2', palette='flare', alpha=0.8, ax = axs[2], data=df.loc[df['a']>=0.0])
axs[0].set_xscale("log")
axs[1].set_xscale("log")
axs[2].set_xscale("log")
axs[2].set_yscale("log")

plt.show()

# scaleList=df['Scale mismatch'].unique()

# print(scaleList)

# wList=df['w'].unique()
# a0Mat=np.zeros((len(aList)*len(wList),3))
#
#
# ix=0
# for a in aList:
#     for w in wList:
#         df1 = df.loc[(df['a']==a) & (df['w']==w)]
#         a0 = df1['a0'].min()
#         a0Mat[ix,0]=float(w)
#         a0Mat[ix,1]=float(a)
#         a0Mat[ix,2]=1-a0
#         ix = ix+1
#
# dfA0 = pd.DataFrame(data=a0Mat, index=None, columns=['w','a','a0'])
