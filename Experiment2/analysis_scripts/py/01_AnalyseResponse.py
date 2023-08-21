# -*- coding: utf-8 -*-
"""
Created on Fri Jun 23 13:51:04 2023

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


wd = 'C:/Users/sammirc/Desktop/phd/wmConfidence_Behaviour/Experiment2'
wd = '/Users/sammi/Desktop/Experiments/wmConfidence_Behaviour/Experiment2'
os.chdir(wd)


figpath = op.join(wd, 'figures_python')
if not op.exists(figpath):
    os.mkdir(figpath) #make this folder if it doesn't currently exist


fname = op.join(wd, 'data', 'wmConfidence_BehaviouralData_All.csv')
df = pd.read_csv(fname)

cuedcol = '#3182bd' #blue, use for cued
neutcol = '#bdbdbd' #light-ish grey, use for neutral
diffcol = '#B2DF8A' #for colouring of the bars of difference plots, third distinct colour

#look at how many trials weren't confirmed with a response, and were outside normal RT ranges
df.assign(clickresp = np.multiply(-1, np.subtract(df.clickresp, 1)))[['subid', 'clickresp', 'DTcheck', 'cond']].groupby(
    ['subid', 'cond'], as_index = False).agg({'clickresp':'mean', 'DTcheck':'mean'})
#clickresp shows proportion of trials where they *did not* click to confirm their response
#DTcheck shows proportion of trials where the decision time is more than 2.5 SDs of the within subject+condition mean decision time (too fast or too slow)

df  = df.query('clickresp == 1 and DTcheck == 0') #exclude trials now for all analyses
df.shape #9608 trials across all subjects


#%% look at decision time - does it vary between cue conditions?

df_dt = df.groupby(['subid', 'cond'], as_index=False).agg({'DT':'mean'})

df_dt_plot = df_dt.groupby(['cond'], as_index = False).agg({'DT':['mean', sp.stats.sem]})
df_dt_plot.columns = ['condition', 'meanDT', 'semDT']

#   condition    meanDT     semDT
# 0      cued  0.528792  0.041476
# 1   neutral  0.774952  0.059691

#t-test to test difference in RT between cue conditions, across subjects
sp.stats.ttest_rel(df_dt.query('cond == "cued"').DT, df_dt.query('cond == "neutral"').DT)

#TtestResult(statistic=-8.143477519625808, pvalue=1.2857679172109906e-07, df=19)


# visualise this
fig = plt.figure(figsize = (5,5))
ax = fig.add_subplot(111)
ax.bar(x = df_dt_plot.condition, height = df_dt_plot.meanDT, color = [cuedcol, neutcol], width = 0.7)
ax.errorbar(x = df_dt_plot.condition, y = df_dt_plot.meanDT, yerr = df_dt_plot.semDT,
            fmt = 'none', ecolor = '#000000', elinewidth = 2, capsize = 7)
ax.set_ylabel('mean decision time (s)')
ax.set_axisbelow(True)
ax.grid(True, color = '#bdbdbd')

#%% look at response error - are people better in the cued condition on this wm recall task?

def wrap(x):
    return (x+180)%360 - 180
def wrap90(x):
    return (x+90)%180 - 90


df_mad = df.query('clickresp == 1').groupby(['subid', 'cond'], as_index=False).agg({'absrdif':'mean'})
df_mad_plot = df_mad.groupby('cond', as_index = False).agg({'absrdif':['mean', sp.stats.sem]})
df_mad_plot.columns = ['condition', 'meanMAD', 'semMAD']

df_mad_plot
#   condition    meanMAD    semMAD
# 0      cued   9.224149  0.539341
# 1   neutral  10.367385  0.699515

#t-test to test difference in response error between cue conditions, across subjects
sp.stats.ttest_rel(df_mad.query('cond == "cued"').absrdif, df_mad.query('cond == "neutral"').absrdif)
# TtestResult(statistic=-4.0798083657246655, pvalue=0.0006385184090710034, df=19)

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
# 0      cued  0.148977  0.010430
# 1   neutral  0.167423  0.011682

#t-test to test difference in response error between cue conditions, across subjects
sp.stats.ttest_rel(df_acc.query('cond == "cued"').rabsrdif, df_acc.query('cond == "neutral"').rabsrdif)
# TtestResult(statistic=-2.335755331441612, pvalue=0.030618225561454798, df=19)

#can do this nonparametrically if concerned about the distribution of response error distribution standard deviations not being normal across participants
#gives similar effect of significant diff: median sd is lower in cued condition vs neutral
sp.stats.wilcoxon(df_acc.query('cond == "cued"').rabsrdif, df_acc.query('cond == "neutral"').rabsrdif)
# WilcoxonResult(statistic=37.0, pvalue=0.009435653686523438)

fig = plt.figure(figsize = (5,5))
ax = fig.add_subplot(111)
ax.bar(x = df_acc_plot.condition, height = df_acc_plot.meanSD, color = [cuedcol, neutcol], width = 0.7)
ax.errorbar(x = df_acc_plot.condition, y = df_acc_plot.meanSD, yerr = df_acc_plot.semSD,
            fmt = 'none', ecolor = '#000000', elinewidth = 2, capsize = 7)
ax.set_ylabel('group mean response variability (SD)')
ax.set_axisbelow(True)
ax.grid(True, color = '#bdbdbd')











