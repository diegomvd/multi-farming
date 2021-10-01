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
# sns.set_style("darkgrid")
# sns.set_style("whitegrid")
sns.set_style("white")

# path2 = "/home/karpouzi/Research/Eukaryote-mountdir/resultsHighNaturePopOSE/population5000.csv"
# path2 = "/home/karpouzi/Research/Eukaryote-mountdir/resultsHighNaturePopOSE4/population20000.csv"
path2 = "/home/karpouzi/Research/Eukaryote-mountdir/resultsHighNaturePopOSE-coarse/population40000.csv"
# path2 = "/home/karpouzi/Research/Eukaryote-mountdir/resultsHighNaturePopOSE5/population19600.csv"
# path2 = "/home/karpouzi/Research/Eukaryote-mountdir/resultsHighNaturePopOSE2/population16000.csv"
# df1 = pd.read_csv(path1)
df = pd.read_csv(path2)
# df = df1.append(df2, ignore_index=True)

path = "/home/karpouzi/Research/Eukaryote-mountdir/resultsNaturePopPSE2/population7300.csv"
dfpse = pd.read_csv(path)

path = "/home/karpouzi/Research/Eukaryote-mountdir/manScale-moranIfertLoss.csv"
dfmoran = pd.read_csv(path)
dfmoran['Management scale']=1/(dfmoran['nF'])
dfmoran['Fraction of agricultural land']=np.round(dfmoran['a0'], decimals=1)
dfmoran['Fertility loss propensity']=dfmoran['fertLossTime']
dfmoran['Fraction sparing']=dfmoran['a']
dfmoran['Moran\'s I']=dfmoran['moranI']
# print(dfmoran)

df['Mean farm area']=1/(df['nF'])
df['Responsiveness Width'] = df['wS']*(df['nF']-1)/df['nF']

#categorize parameters
df['Management strategies'] = pd.cut(df['a'], bins=[0,1/3,2/3, float('Inf')], labels=['Most sharing','Mixed','Most sparing'])
df['Management scale'] = pd.cut(df['Mean farm area'], bins=[0,0.01,0.1, float('Inf')], labels=['Small','Medium','Large'])
df['Mean responsiveness'] = pd.cut(df['mS'], bins=[0.25,0.5,0.75, float('Inf')], labels=['Low','Intermediate','High'])

df['wSEff2'] = pd.cut(df['Responsiveness Width'], bins=[0,1/3,2/3, float('Inf')], labels=['Low','Intermediate','High'])

df['N'] = -df['N']
df['P'] = -df['P']

minSamples=19
minSamples2 = 0

# palette_sparing =sns.diverging_palette(250, 18, s=100, l=57, n=3, center="light")
# palette_sparing =sns.color_palette("mako", n_colors=3)
palette_sparing = sns.color_palette("blend:g,m",n_colors=3,desat=1.0)
# palette=sns.diverging_palette(250, 30, l=65, center="dark",n_colors=3)
palette_response = sns.color_palette("flare", n_colors=3)
# palette_response = sns.color_palette("Reds", as_cmap=True)

g12 = sns.JointGrid(x='mS',y='Responsiveness Width', hue='Management strategies',data=df.loc[(df['evolution$samples']>minSamples) ], height = 5, ratio=4)
g12.plot_joint(sns.scatterplot,alpha=.8, edgecolor='.2', linewidth=.5, s=50, palette=palette_sparing)
g12.plot_marginals(sns.histplot, alpha=.6, edgecolor='.2', fill='True', multiple="stack", bins=5, palette=palette_sparing)
g12.set_axis_labels('Mean responsiveness to demand','Responsiveness width')

g6 = sns.JointGrid(x='Mean farm area',y='a', hue='Mean responsiveness',data=df.loc[(df['evolution$samples']>minSamples) ], height = 5, ratio=4)
g6.plot_joint(sns.scatterplot,alpha=.8, edgecolor='.2', linewidth=.5, s=50, palette=palette_response)
g6.plot_marginals(sns.histplot, alpha=.6, log_scale=(True,False), edgecolor='.2', fill='True', multiple="stack", bins=5, palette=palette_response)
g6.set_axis_labels('Management scale','Fraction of land-sparing farms')
g6.ax_joint.set(xscale="log")

fig, axs = plt.subplots(nrows=1,ncols=2,figsize=(10,5))
plt.tight_layout
sns.scatterplot(x='N',y='P', hue='Management strategies', size='Management scale',size_order=['Large', 'Medium', 'Small'], edgecolor='0.1', palette=palette_sparing, alpha=0.8, sizes=[225,75,25], ax = axs[1], data=df.loc[df['evolution$samples']>minSamples])
sns.scatterplot(x='N',y='P',hue='evolution$samples',s=50,palette='flare',edgecolor='.2',alpha=0.8,ax = axs[0],data=dfpse.loc[df['evolution$samples']>minSamples2])
# axs[1].legend(loc='upper right')
axs[0].legend(title='Samples',loc='upper right')
axs[0].set_xlabel('Natural land fraction')
axs[1].set_xlabel('Natural land fraction')
axs[1].set_ylabel('')
axs[0].set_ylabel('Population Size')

fig1, axs1 = plt.subplots(nrows=2,ncols=2,figsize=(9,7))
alpha = 0.8
sns.scatterplot(x='Mean farm area',y='N', hue='Management strategies', edgecolor='.2', palette=palette_sparing, alpha=alpha,  ax = axs1[0,0], legend=False, data=df.loc[df['evolution$samples']>minSamples])
sns.scatterplot(x='Mean farm area',y='P', hue='Management strategies', edgecolor='.2', palette=palette_sparing, alpha=alpha,  ax = axs1[1,0],  legend=False,data=df.loc[df['evolution$samples']>minSamples])
sns.boxplot(x="Management scale", y="N", hue="Management strategies",  palette=palette_sparing,  ax = axs1[0,1],  data=df.loc[(df['evolution$samples']>minSamples) ])
sns.boxplot(x="Management scale", y="P", hue="Management strategies",  palette=palette_sparing,  ax = axs1[1,1], data=df.loc[(df['evolution$samples']>minSamples) ])

axs1[0,0].set_xscale("log")
axs1[1,0].set_xscale("log")
axs1[0,0].set_xlim(0,0.1)
axs1[1,0].set_xlim(0,0.1)


axs1[1,1].set_xlabel('Management scale')
axs1[1,0].set_xlabel('Management scale')
axs1[0,0].set_xlabel('')
axs1[0,1].set_xlabel('')

axs1[0,0].set_ylabel('Natural land fraction')
axs1[1,0].set_ylabel('Population size')
axs1[0,1].set_ylabel('')
axs1[1,1].set_ylabel('')

g0 = sns.FacetGrid(dfmoran, row="Fraction of agricultural land", hue="Fraction sparing", palette="mako", height=2.5, aspect=1.5)
g0.map(sns.scatterplot, "Management scale", "Fertility loss propensity", edgecolor='0.2', alpha=0.8)
g0.set(xscale='log')

g00 = sns.FacetGrid(dfmoran, row="Fraction of agricultural land", hue="Fraction sparing", palette="mako", height=2.5, aspect=1.5)
g00.map(sns.scatterplot, "Management scale", "Moran\'s I", edgecolor='0.2', alpha=0.8)
g00.add_legend()
g00.set(xscale='log')

# .div(df['natFraction']-1))

# fig, axs = plt.subplots(nrows=1,ncols=3)

# sns.scatterplot(x='wS',y='a',size='nF',palette="flare", ax = axs[0], data=df.loc[df['mS']<0.75])
# sns.scatterplot(x='wS',y='a',size='nF',palette="flare", ax = axs[1], data=df.loc[(df['mS']>=0.75) & (df['mS']<1.0)])
# sns.scatterplot(x='wS',y='a',size='nF',palette="flare", ax = axs[2], data=df.loc[df['mS']>=1.0])
# palette=sns.diverging_palette(145, 300, s=60, as_cmap=True)
palette1=sns.diverging_palette(145, 300, s=60)
palette = sns.color_palette("blend:g,m",n_colors=3,desat=0.8)
# palette = sns.color_palette("flare",n_colors=3)
# blend:<color>,<color>


# fig1, axs1 = plt.subplots(nrows=1,ncols=1)
# sns.scatterplot(x='a',y='wSEff', hue='mS', palette='flare', sizes=(15, 100), ax = axs1, data=df.loc[(df['evolution$samples']>minSamples) ])
# # kwargs = {'s':50}
# sns.scatterplot(x='nF',y='mS', hue='catA', palette=palette,alpha=0.8, ax = axs, data=df, **kwargs)
#
# g1 = sns.JointGrid(x='nF',y='mS', hue='Management',data=df.loc[(df['evolution$samples']>minSamples) ], height = 8, ratio=4)
# g1.plot_joint(sns.scatterplot,alpha=.8, edgecolor='.2', linewidth=.5, s=50, palette=palette)
# # g1.plot_marginals(sns.kdeplot, alpha=.2, fill='True',  palette=palette)
# g1.plot_marginals(sns.histplot, log_scale=True, alpha=.6, edgecolor='.2', fill='True', multiple="stack", bins=5, palette=palette)
# g1.set_axis_labels('Number of Farms','Mean responsiveness to demand')
# g1.ax_joint.set(xscale="log")
#
# g1.savefig('naturePopOSE-coarse-nFmS.pdf', format='pdf', dpi = 600, bbox_inches='tight')
#
# g2 = sns.JointGrid(x='nF',y='a', hue='mS2',data=df.loc[ (df['evolution$samples']>minSamples)  ], height = 8, ratio=4)
# g2.plot_joint(sns.scatterplot,alpha=.8, edgecolor='.2', linewidth=.5, s=50, palette=palette)
# # g2.plot_marginals(sns.kdeplot, alpha=.2, fill='True',  palette=palette)
# g2.plot_marginals(sns.histplot, alpha=.6, edgecolor='.2', fill='True', multiple="stack", bins=5, palette=palette)
# g2.set_axis_labels('Number of Farms','Fraction Sparing')
# g2.ax_joint.set(xscale="log")
#
# g2.savefig('naturePopOSE-coarse-nFa.pdf', format='pdf', dpi = 600, bbox_inches='tight')
#
# g3 = sns.JointGrid(x='a',y='mS', hue='Farms',data=df.loc[(df['evolution$samples']>minSamples) ], height = 8, ratio=4)
# g3.plot_joint(sns.scatterplot,alpha=.8, edgecolor='.2', linewidth=.5, s=50, palette=palette)
# # g3.plot_marginals(sns.kdeplot, alpha=.2, fill='True',  palette=palette)
# g3.plot_marginals(sns.histplot, alpha=.6, edgecolor='.2', fill='True', multiple="stack", bins=5, palette=palette)
# g3.set_axis_labels('Fraction Sparing','Average responsiveness to demand')
# # g3.ax_joint.set(xscale="log")
#
# g3.savefig('naturePopOSE-coarse-amS.pdf', format='pdf', dpi = 600, bbox_inches='tight')
#
# g4 = sns.JointGrid(x='mS',y='wSEff', hue='Farms',data=df.loc[(df['evolution$samples']>minSamples) ], height = 8, ratio=4)
# g4.plot_joint(sns.scatterplot,alpha=.8, edgecolor='.2', linewidth=.5, s=50, palette=palette)
# # g4.plot_marginals(sns.kdeplot, alpha=.2, fill='True',  palette=palette)
# g4.plot_marginals(sns.histplot, alpha=.6, edgecolor='.2', fill='True', multiple="stack", bins=5, palette=palette)
# g4.set_axis_labels('Average responsiveness to demand','Width of responses')
# # g4.ax_joint.set(xscale="log")
#
# g4.savefig('naturePopOSE-coarse-mSwS.pdf', format='pdf', dpi = 600, bbox_inches='tight')
#
# g5 = sns.pairplot(data=df.loc[(df['evolution$samples']>minSamples) ], hue='Farms', x_vars=["a", "mS", "wSEff"],y_vars=["a", "mS", "wSEff"], palette='flare', diag_kind='hist', corner=False, diag_kws={'multiple':'stack','bins':5})
#
# fig, axs = plt.subplots(nrows=1,ncols=1)
# # sns.scatterplot(x='a',y='P', hue='nF', size='mS', palette='flare', alpha=0.8, ax = axs[0], data=df.loc[df['evolution$samples']>minSamples])
# # sns.scatterplot(x='nF',y='mS', hue='a', size='wSEff',palette='flare', alpha=0.8, ax = axs[0], data=df.loc[df['evolution$samples']>minSamples])
# sns.scatterplot(x='N',y='P', hue='a', size='nF', palette='flare', alpha=0.8, sizes=(15, 200), ax = axs, data=df.loc[df['evolution$samples']>minSamples])
# # sns.scatterplot(x='nF',y='a', hue='wSEff', palette='flare', s=50,alpha=0.8, sizes=(15, 200), ax = axs[1], data=df.loc[df['evolution$samples']>minSamples])
# # axs[1].set_xscale('log')
# # sns.scatterplot(x='wS',y='P', hue='nF', palette='flare', alpha=0.8, ax = axs[0,1], data=df)
# # sns.scatterplot(x='wS',y='N', hue='nF', palette='flare', alpha=0.8, ax = axs[1,1], data=df)
#
# fig1, axs1 = plt.subplots(nrows=2,ncols=2)
# sns.scatterplot(x='nF',y='N', hue='Management', palette=palette, alpha=0.8,  ax = axs1[0,0], data=df.loc[df['evolution$samples']>minSamples])
# sns.scatterplot(x='nF',y='P', hue='Management', palette=palette, alpha=0.8,  ax = axs1[1,0], data=df.loc[df['evolution$samples']>minSamples])
# sns.scatterplot(x='a',y='N', hue='Farms', palette=palette, alpha=0.8,  ax = axs1[0,1], data=df.loc[df['evolution$samples']>minSamples])
# sns.scatterplot(x='a',y='P', hue='Farms', palette=palette, alpha=0.8,  ax = axs1[1,1], data=df.loc[df['evolution$samples']>minSamples])
# axs1[0,0].set_xscale("log")
# axs1[1,0].set_xscale("log")


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
