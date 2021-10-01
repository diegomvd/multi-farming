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

pre_path = "/home/karpouzi/Research/Eukaryote-mountdir/replications"

exploratory_param = 'a'
exploratory_values = ['0.0','0.25','0.5','0.75','1.0']
# exploratory_param = 'nF'
# exploratory_values = ['1.0','1024.0']
# exploratory_param = 'dES'
# exploratory_values = ['2.1','3.1','4.1']
# parameters = {'T':'2000.0','n':'40.0','a0':'0.1','d0':'0.1','nF':'1.0','a':'1.0','mS':'1.25','wS':'0.0','z':'0.25','dES':'1.1','y0':'0.2','y1':'1.2','sFL':'0.01','sR':'0.1','sD':'0.01','dtSave':'2.0'}
parameters = {'T':'1500.0','n':'40.0','a0':'0.15','d0':'0.0','nF':'2.0','a':'1.0','mS':'2.0','wS':'0.0','z':'0.25','dES':'1.1','ye':'0.5','k0':'15.0','sFL':'0.01','sR':'0.1','sD':'0.01','dtSave':'1.0'}

# build the figure
fig, axs = plt.subplots(nrows=2,ncols=len(exploratory_values))

# set colors for plot
land_types=["N","D","A0","A1"]
land_colors=["g", "r", "y","m"]

for ix, val in enumerate(exploratory_values):

    # build the filename to get the data
    parameters[exploratory_param] = val
    specific_path = ""
    for name in parameters:
        specific_path += "_"+name+"_"+parameters[name]
    specific_path += ".csv"
    path = pre_path + specific_path
    df = pd.read_csv(path, delimiter=" ", header=1)

    df.columns = ['t', 'P', 'N', 'D', 'A0', 'A1','','','','','','','','','']
    print(df)
    # round the time for the lineplot with seaborn
    df['t'] = np.around(df['t'], decimals=0)

    # df.drop(df.tail(1).index,inplace=True) # drop last n rows

    # plot the population on top of the landscape composition
    sns.lineplot(x="t",y="P", color='b', ax=axs[0,ix], data=df)
    for jx, land_type in enumerate(land_types):
        sns.lineplot(x='t',y=land_type,color=land_colors[jx],label=land_type,ax=axs[1,ix], data=df)
    axs[0,ix].set_title(exploratory_param+" = "+ val)

plt.show()
