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
sns.set_style("darkgrid")
# sns.set_style("white")

path = "/home/karpouzi/Research/Eukaryote-mountdir/resultsHighNaturePopOSE-connecDist/population27000.csv"
df = pd.read_csv(path)

# transformation of number of farms to farm scale
df['Farm Size']=1/(df['nF'])
df['Farm Scale']=1/np.sqrt(df['nF'])
df['Connectivity Scale']=df['dES']/40.0
df['Scale mismatch']= df['Farm Scale']/df['Connectivity Scale']

# categorization
df['Management strategies']= pd.cut(df['a'], bins=[0,1/3,2/3, float('Inf')], labels=['Most Sharing','Mixed','Most Sparing'])
df['Mean Farm Area']= pd.cut(df['Farm Size'], bins=[0,0.01,0.1, float('Inf')], labels=['Small','Medium','Large'])
df['Mean Responsiveness'] = pd.cut(df['mS'], bins=[0.25,0.5,0.75, float('Inf')], labels=['Low','Intermediate','High'])

# variable adjustment
df['N'] = -df['N']
df['P'] = -df['P']

palette_sparing = sns.color_palette("blend:g,m",n_colors=3,desat=1.0)

minSamples=15

x_vars=["Farm Size", "Farm Scale", "Connectivity Scale", "Scale mismatch", "a", "nF"]
y_vars=["Farm Size", "Farm Scale", "Connectivity Scale", "Scale mismatch", "a", "nF"]

# g1 = sns.pairplot(data=df.loc[(df['evolution$samples']>minSamples) ], hue='Mean Responsiveness', x_vars=x_vars,y_vars=y_vars, palette='flare', diag_kind='hist', corner=False, diag_kws={'multiple':'stack','bins':5})

g2 = sns.JointGrid(x='Scale mismatch',y='a', hue='Mean Responsiveness',data=df.loc[ (df['evolution$samples']>minSamples)  ], height = 6, ratio=4)
g2.plot_joint(sns.scatterplot,alpha=.8, edgecolor='.2', linewidth=.5, s=50, palette='flare')
g2.plot_marginals(sns.histplot, alpha=.6, log_scale=(True,False), edgecolor='.2', fill='True', multiple="stack", bins=5, palette='flare')
g2.set_axis_labels('Farm scale relative to ecological scale','Fraction Sparing')
g2.ax_joint.set(xscale="log")

fig1, axs1 = plt.subplots(nrows=2,ncols=1,figsize=(5,7))
alpha = 0.8
sns.scatterplot(x='Scale mismatch',y='N', hue='Management strategies', edgecolor='.2', palette=palette_sparing, alpha=alpha,  ax = axs1[0], legend=False, data=df.loc[df['evolution$samples']>minSamples])
sns.scatterplot(x='Scale mismatch',y='P', hue='Management strategies', edgecolor='.2', palette=palette_sparing, alpha=alpha,  ax = axs1[1], data=df.loc[df['evolution$samples']>minSamples])
axs1[0].set_xscale("log")
axs1[1].set_xscale("log")
axs1[0].set_xlabel('')
axs1[1].set_xlabel('Farm scale relative to ecological scale')
axs1[0].set_ylabel('Natural land fraction')
axs1[1].set_ylabel('Population size')



g3 = sns.JointGrid(x='Scale mismatch',y='mS', hue='Management strategies', data=df.loc[ (df['evolution$samples']>minSamples)  ], height = 8, ratio=4)
g3.plot_joint(sns.scatterplot,alpha=.8, edgecolor='.2', linewidth=.5, s=50, palette='flare')
g3.plot_marginals(sns.histplot, alpha=.6, log_scale=(True,False), edgecolor='.2', fill='True', multiple="stack", bins=5, palette='flare')
g3.set_axis_labels('Scale mismatch','Mean Responsiveness')
g3.ax_joint.set(xscale="log")
#
# g4 = sns.JointGrid(x='a',y='mS', hue='Mean Farm Area', data=df.loc[ (df['evolution$samples']>minSamples)  ], height = 8, ratio=4)
# g4.plot_joint(sns.scatterplot,alpha=.8, edgecolor='.2', linewidth=.5, s=50, palette='flare')
# g4.plot_marginals(sns.histplot, alpha=.6, edgecolor='.2', fill='True', multiple="stack", bins=5, palette='flare')
# g4.set_axis_labels('Fraction Sparing','Mean Responsiveness')
#
g5 = sns.JointGrid(x='Farm Scale',y='Connectivity Scale', hue='Management strategies', data=df.loc[ (df['evolution$samples']>minSamples)  ], height = 8, ratio=4)
g5.plot_joint(sns.scatterplot,alpha=.8, edgecolor='.2', linewidth=.5, s=50, palette='flare')
g5.plot_marginals(sns.histplot, alpha=.6, edgecolor='.2', fill='True', multiple="stack", bins=5, palette='flare')
g5.ax_joint.set(xscale="log")
g5.ax_joint.set(yscale="log")
# g5.ax_marginals.set(xscale="log")
# g5.ax_marginals.set(yscale="log")
#
# g6 = sns.JointGrid(x='Farm Scale',y='Connectivity Scale', hue='Mean Responsiveness', data=df.loc[ (df['evolution$samples']>minSamples)  ], height = 8, ratio=4)
# g6.plot_joint(sns.scatterplot,alpha=.8, edgecolor='.2', linewidth=.5, s=50, palette='flare')
# g6.plot_marginals(sns.histplot, alpha=.6, edgecolor='.2', fill='True', multiple="stack", bins=5, palette='flare')
# g6.ax_joint.set(xscale="log")
# g6.ax_joint.set(yscale="log")
# # g6.ax_marginals.set(xscale="log")
# # g6.ax_marginals.set(yscale="log")

plt.show()
