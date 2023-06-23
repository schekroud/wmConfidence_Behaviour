# -*- coding: utf-8 -*-
"""
Created on Thu Jun 22 16:07:38 2023

@author: sammirc
"""

import numpy as np
import scipy as sp
from scipy import stats
import pandas as pd
import statsmodels as sm
import statsmodels.api as sma
import os
import os.path as op
import seaborn as sns
from matplotlib import pyplot as plt
%matplotlib

def se(x):
    return np.std(x)/np.power(len(x), 0.5)


wd = 'C:/Users/sammirc/Desktop/phd/wmConfidence_Behaviour/Experiment1'
os.chdir(wd)


figpath = op.join(wd, 'figures_python')
if not op.exists(figpath):
    os.mkdir(figpath) #make this folder if it doesn't currently exist


fname = op.join(wd, 'data', 'wmSelection_BehaviouralData_All_Preprocessed.csv')
df = pd.read_csv(fname)

cuedcol = '#3182bd' #blue, use for cued
neutcol = '#bdbdbd' #light-ish grey, use for neutral
diffcol = '#B2DF8A' #for colouring of the bars of difference plots, third distinct colour


nsubs = df.subid.unique().size
subs        = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22]
subs2use    = [1, 2, 3, 4, 5, 6, 7,    9, 10, 11,     13, 14, 15, 16, 17, 18, 19, 20, 21, 22]

#can look quickly at how many trials people didnt respond to, or are excluded based on RTs
df.assign(clickresp = np.multiply(-1, np.subtract(df.clickresp, 1)))[['subid', 'clickresp', 'DTcheck', 'cond']].groupby(
    ['subid', 'cond'], as_index=True).agg(
        {'clickresp':'sum', 'DTcheck':'sum'})
#prints the numer of trials where they did *not* click to respond on a trial
# prints the number of trials that are excluded, per condition, in that subject based on being more than 2.5 SDs
# of the within subject, within condition mean

df = df.query('subid in @subs2use')
# df = df.query('clickresp == 1 and DTcheck == 0') #exclude trials where they didnt click to confirm response, and were abnormally fast/slow

df_dt = df.query('clickresp == 1 and DTcheck == 0').groupby(['subid', 'cond'], as_index=False).agg({'DT':'mean'})

df_dt_plot = df_dt.groupby(['cond'], as_index = False).agg({'DT':['mean', sp.stats.sem]})
df_dt_plot.columns = ['condition', 'meanDT', 'semDT']

#   condition    meanDT     semDT
# 0      cued  0.678194  0.063017
# 1   neutral  0.922051  0.085698

#t-test to test difference in RT between cue conditions, across subjects
sp.stats.ttest_rel(df_dt.query('cond == "cued"').DT, df_dt.query('cond == "neutral"').DT)

#TtestResult(statistic=-4.626888088986789, pvalue=0.0001839268029033608, df=19)

#%%
#second way
fig = plt.figure(figsize = (5,5))
ax = fig.add_subplot(111)
ax.bar(x = df_dt_plot.condition, height = df_dt_plot.meanDT, color = [cuedcol, neutcol], width = 0.7)
ax.errorbar(x = df_dt_plot.condition, y = df_dt_plot.meanDT, yerr = df_dt_plot.semDT,
            fmt = 'none', ecolor = '#000000', elinewidth = 2, capsize = 7)
ax.set_ylabel('mean decision time (s)')
ax.set_axisbelow(True)
ax.grid(True, color = '#bdbdbd')

#%%
def wrap(x):
    return (x+180)%360 - 180
def wrap90(x):
    return (x+90)%180 - 90


df_mad = df.query('clickresp == 1').groupby(['subid', 'cond'], as_index=False).agg({'absrdif':'mean'})
df_mad_plot = df_mad.groupby('cond', as_index = False).agg({'absrdif':['mean', sp.stats.sem]})
df_mad_plot.columns = ['condition', 'meanMAD', 'semMAD']

df_mad_plot
#   condition    meanMAD    semMAD
# 0      cued  10.343770  0.504487
# 1   neutral  11.540396  0.492699

#t-test to test difference in response error between cue conditions, across subjects
sp.stats.ttest_rel(df_mad.query('cond == "cued"').absrdif, df_mad.query('cond == "neutral"').absrdif)
# TtestResult(statistic=-3.264553445195169, pvalue=0.004078080478498403, df=19)

fig = plt.figure(figsize = (5,5))
ax = fig.add_subplot(111)
ax.bar(x = df_mad_plot.condition, height = df_mad_plot.meanMAD, color = [cuedcol, neutcol], width = 0.7)
ax.errorbar(x = df_mad_plot.condition, y = df_mad_plot.meanMAD, yerr = df_mad_plot.semMAD,
            fmt = 'none', ecolor = '#000000', elinewidth = 2, capsize = 7)
ax.set_ylabel('(mean) mean absolute deviation (°)')
ax.set_axisbelow(True)
ax.grid(True, color = '#bdbdbd')

#%%
df_acc = df.assign(rabsrdif = np.deg2rad(df.absrdif)).query('clickresp == 1').groupby(
    ['subid', 'cond'], as_index = False).agg(
        {'rabsrdif':sp.stats.circstd})
df_acc_plot = df_acc.groupby('cond', as_index = False).agg({'rabsrdif':['mean', sp.stats.sem]})
df_acc_plot.columns = ['condition', 'meanSD', 'semSD']

df_acc_plot
#   condition    meanSD     semSD
# 0      cued  0.167343  0.009375
# 1   neutral  0.186513  0.010349

#t-test to test difference in response error between cue conditions, across subjects
sp.stats.ttest_rel(df_acc.query('cond == "cued"').rabsrdif, df_acc.query('cond == "neutral"').rabsrdif)
# TtestResult(statistic=-2.1534397359577406, pvalue=0.044338689227273756, df=19)

fig = plt.figure(figsize = (5,5))
ax = fig.add_subplot(111)
ax.bar(x = df_acc_plot.condition, height = df_acc_plot.meanSD, color = [cuedcol, neutcol], width = 0.7)
ax.errorbar(x = df_acc_plot.condition, y = df_acc_plot.meanSD, yerr = df_acc_plot.semSD,
            fmt = 'none', ecolor = '#000000', elinewidth = 2, capsize = 7)
ax.set_ylabel('group mean response variability (SD)')
ax.set_axisbelow(True)
ax.grid(True, color = '#bdbdbd')
