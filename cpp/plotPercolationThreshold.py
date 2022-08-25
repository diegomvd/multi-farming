# -*- coding: utf-8 -*-

import glob, os, sys
import matplotlib.pyplot as plt
from matplotlib.colors import LogNorm
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
sns.set_style("ticks")
# sns.set_style("whitegrid")
# sns.set_style("white")

path = "/home/karpouzi/Research/Eukaryote-mountdir/experimentPercThreshold-070921-2.csv" #sharing and sparing
path2 = "/home/karpouzi/Research/Eukaryote-mountdir/experimentPercThreshold-080921.csv" # mixed

df = pd.read_csv(path, sep=",", header=0)
df2 = pd.read_csv(path2, sep=",", header=0)

df = pd.concat([df,df2],ignore_index=True)

df['Giant component']=df['maxN']/1600/df['N']
df['Connectivity scale'] = df['dES']/40.0
df['Management scale'] = np.sqrt(1/df['nF'])

# separate in sharing and sparing
dfSharing = df.loc[df['a']==0.0]
dfSparing = df.loc[df['a']==1.0]

# identify a, connectivity and management scale
aList = df['a'].unique()
connectivityList = df['Connectivity scale'].unique()
managementList = df['Management scale'].unique()

fig,axs = plt.subplots(nrows=1,ncols=3, figsize=(8,3))
fig2,axs2 = plt.subplots(nrows=1,ncols=connectivityList.size, figsize=(8,8))

# start to iterate over strategies and over connectivity and management scale
jx=0
for a in aList:
    thMat=np.zeros((len(connectivityList)*len(managementList),3))
    ix=0
    kx=0
    for c in connectivityList:
        for m in managementList:
            giantComp = np.array(df.loc[(df['a']==a) & (df['Connectivity scale']==c) & (df['Management scale']==m)]['Giant component'])
            nLand = np.array(df.loc[(df['a']==a) & (df['Connectivity scale']==c) & (df['Management scale']==m)]['N'])

            sns.lineplot(x=nLand, y=giantComp, label=str(m),ax=axs2[kx])
            axs2[kx].set_title(str(c))

            vec = np.stack((nLand,giantComp))
            vec = np.sort(vec, axis=-1, kind='stable')

            dCompo = np.abs(np.diff(vec[1,:]))
            meanDCompo = np.mean(dCompo)
            stdDCompo = np.std(dCompo)
            maxDCompo = np.max(dCompo)

            if(maxDCompo > meanDCompo+3*stdDCompo):
                thIndex = np.argmax(dCompo)
                percTh = (vec[0,thIndex+1]+vec[0,thIndex])*0.5
            else:
                percTh = 0

            thMat[ix,0] = c
            thMat[ix,1] = m
            thMat[ix,2] = percTh

            ix=ix+1
        kx+=1

    plt.show()
#     # out of the scales loop now is plot time
#     df2plot = pd.DataFrame(data=thMat, index=None, columns=['Connectivity scale','Management scale','Percolation threshold'])
#     df2plot['Connectivity scale'] = np.around(df2plot['Connectivity scale'],decimals=2)
#     df2plot['Management scale'] = np.around(df2plot['Management scale'],decimals=2)
#     df2plot2 = df2plot.pivot("Connectivity scale", "Management scale", 'Percolation threshold')
#     if jx==2:
#         cbar=True
#         yticklabels=False
#     else:
#         cbar=False
#         yticklabels=True
#
#     palette = sns.color_palette("mako_r", as_cmap=True)
#     sns.heatmap(data=df2plot2,cmap=palette,norm=LogNorm(),cbar_kws={'label': 'Percolation threshold'},ax=axs[jx],center=0.6,alpha=1.0, cbar=cbar, yticklabels=yticklabels, vmin=0, vmax=0.7)
#     axs[jx].invert_yaxis()
#     # Z = np.array(df2plot2)
#     # X = np.sort(np.array(df2plot['Management scale'].unique()))
#     # Y = np.sort(np.array(df2plot['Connectivity scale'].unique()))
#     # print(Z)
#     # print(X)
#     # print(Y)
#     # axs[jx].contourf(X, Y, Z, cmap='crest')
#     # axs[jx].set_xlabel('Management scale')
#     # axs[jx].set_ylabel('Connectivity scale')
#     # axs[jx].set_xscale('log')
#     # axs[jx].set_yscale('log')
#     jx+=1
#
# axs[1].set_ylabel('')
# plt.show()
