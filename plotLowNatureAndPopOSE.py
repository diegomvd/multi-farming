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
# sns.set_style("darkgrid",  {"axes.facecolor": ".1"})
sns.set_style("darkgrid")

path = "/home/karpouzi/Research/Eukaryote-mountdir/resultsLowNaturePopOSE3/population10300.csv"
df = pd.read_csv(path)

#categorize sharing-sparing parameter
df['Management']= pd.cut(df['a'], bins=[0,1/3,2/3, float('Inf')], labels=['Most Sharing','Mixed','Most Sparing'])
df['wSEff'] = df['wS']*(df['nF']-1)/df['nF']

df['Average Farm Diameter']=1/np.sqrt(df['nF'])

# .div(df['natFraction']-1))

# fig, axs = plt.subplots(nrows=1,ncols=3)

# sns.scatterplot(x='wS',y='a',size='nF',palette="flare", ax = axs[0], data=df.loc[df['mS']<0.75])
# sns.scatterplot(x='wS',y='a',size='nF',palette="flare", ax = axs[1], data=df.loc[(df['mS']>=0.75) & (df['mS']<1.0)])
# sns.scatterplot(x='wS',y='a',size='nF',palette="flare", ax = axs[2], data=df.loc[df['mS']>=1.0])
# palette=sns.diverging_palette(145, 300, s=60, as_cmap=True)
palette1=sns.diverging_palette(145, 300, s=60)
palette = sns.color_palette("blend:g,m",n_colors=3,desat=0.8)
# blend:<color>,<color>

# fig, axs = plt.subplots(nrows=1,ncols=1)
# sns.scatterplot(x='nF',y='mS', size='wSEff', hue='a', palette=palette, sizes=(15, 100), ax = axs, data=df)
# kwargs = {'s':50}
# sns.scatterplot(x='nF',y='mS', hue='catA', palette=palette,alpha=0.8, ax = axs, data=df, **kwargs)

minSamples=15

g = sns.JointGrid(x='nF',y='mS', hue='Management', data=df.loc[df['evolution$samples']>minSamples], height = 8, ratio=4)
g.plot_joint(sns.scatterplot,alpha=.8, edgecolor='.2', linewidth=.5, s=50, palette=palette)
# g.plot_marginals(sns.kdeplot, alpha=.2, fill='True',  palette=palette)
g.plot_marginals(sns.histplot, alpha=.6, edgecolor='.2', fill='True', multiple="stack", bins=4, palette=palette)
g.set_axis_labels('Number of Farms','Mean Sensitivity to Demand')
g.ax_joint.set(xscale="log")

g.savefig('naturePopOSE.pdf', format='pdf', dpi = 600, bbox_inches='tight')

fig, axs = plt.subplots(nrows=1,ncols=1)
# sns.scatterplot(x='a',y='P', hue='nF', size='mS', palette='flare', alpha=0.8, ax = axs[0], data=df.loc[df['evolution$samples']>minSamples])
sns.scatterplot(x='wSEff',y='mS', hue='a', size='nF',palette='flare', alpha=0.8, ax = axs, data=df.loc[df['evolution$samples']>minSamples])
# sns.scatterplot(x='N',y='P', hue='a', size='nF', palette='flare', alpha=0.8, sizes=(15, 200), ax = axs, data=df.loc[df['evolution$samples']>minSamples])
# sns.scatterplot(x='nF',y='a', hue='wSEff', palette='flare', s=50,alpha=0.8, sizes=(15, 200), ax = axs[1], data=df.loc[df['evolution$samples']>minSamples])
# axs[1].set_xscale('log')
# sns.scatterplot(x='wS',y='P', hue='nF', palette='flare', alpha=0.8, ax = axs[0,1], data=df)
# sns.scatterplot(x='wS',y='N', hue='nF', palette='flare', alpha=0.8, ax = axs[1,1], data=df)

# g = sns.jointplot(x='nF',y='mS', hue='catA', palette=palette, alpha=0.8, data=df, **kwargs)
# g.ax_joint.set(xscale="log")
# g.ax_joint.set(xlabel="Number of Farms")
# g.ax_joint.set(ylabel="Mean Sensitivity to Demand")
# g.legend(title="")
# sns.scatterplot(x='nF',y='mS',size='P', hue='a', palette=palette, ax = axs, data=df)
# plt.legend(title='')
# axs.set_xlabel('Number of Farms')
# axs.set_ylabel('Mean Sensitivity to Demand')
# axs.set_xscale('log')

plt.show()
