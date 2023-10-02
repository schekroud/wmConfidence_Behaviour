# -*- coding: utf-8 -*-
"""
Created on Thu Sep 28 16:38:47 2023

@author: sammirc
"""
import numpy as np
import scipy as sp
import pandas as pd
import statsmodels.api as sma
import statsmodels as sm
import sklearn as skl
import os.path as op
import os
import sys
from matplotlib import pyplot as plt
import seaborn as sns
import pycircstat as circstat
import progressbar
from numpy import matlib

np.set_printoptions(suppress=True)
%matplotlib
np.random.seed(1)

sys.path.insert(0, 'C:/Users/sammirc/Desktop/phd/pyCircMixModel')
import CircMixModel as cmm

def wrap(x):
    return (x+180)%360 - 180
def wrap90(x):
    return (x+90)%180 - 90

cuedcol = '#3182bd' #blue, use for cued
neutcol = '#bdbdbd' #light-ish grey, use for neutral
diffcol = '#B2DF8A' #for colouring of the bars of difference plots, third distinct colour

wd = 'C:/Users/sammirc/Desktop/phd/wmConfidence_Behaviour'
os.chdir(wd)
#%%

E1dat = pd.read_csv(op.join(wd, 'Experiment1', 'data', 'wmSelection_BehaviouralData_All_Preprocessed.csv'))
subs2use    = [1, 2, 3, 4, 5, 6, 7,    9, 10, 11,     13, 14, 15, 16, 17, 18, 19, 20, 21, 22]
E1dat = E1dat.query('subid in @subs2use')
nsubs = E1dat.subid.unique().size
nsims = 10000
sim_corrs = np.empty(shape = (nsubs, 2, nsims))
observed_corrs = np.empty(shape = (nsubs,2))

count = -1
for isub in E1dat.subid.unique():
    count += 1
    # print('- - - - - - working on sub %02d - - - - - -'%isub)
    tmpsub = E1dat.copy().query('subid == @isub')
    tmpsub_neutral = tmpsub.query('cond == "neutral"') #get response error for neutral trials
    tmpsub_cued    = tmpsub.query('cond == "cued"')
    
    eneut = tmpsub_neutral.copy().absrdif.to_numpy()
    ecued = tmpsub_cued.copy().absrdif.to_numpy()
    cneut = tmpsub_neutral.copy().confwidth.to_numpy()
    ccued = tmpsub_cued.copy().confwidth.to_numpy()
    
    observed_corrs[count, 0], _  = sp.stats.spearmanr(eneut, cneut)
    observed_corrs[count, 1], _  = sp.stats.spearmanr(ecued, ccued)
    with progressbar.ProgressBar(max_value = nsims).start() as bar:
        for isim in range(nsims):
            bar.update(isim)
            
            #randomly shuffle confidence in neutral and cued trials
            #this makes trialwise confidence unlinked to the trial, and solely linked to the across trial confidence distribution
            #effectively we can repeatedly do this to build the null distribution of error ~ confidence correlations per subject
            np.random.shuffle(cneut)
            np.random.shuffle(ccued)
            
            irh_neut, _ = sp.stats.spearmanr(eneut, cneut)
            irh_cued, _ = sp.stats.spearmanr(ecued, ccued)
            
            sim_corrs[count, 0, isim], sim_corrs[count, 1, isim] = irh_neut, irh_cued

#%%
#get metrics about these simulations in a slightly more useful format (getting quantiles per subject and condition)
corrs = pd.DataFrame()
count = -1
for isub in E1dat.subid.unique():
    count += 1
    tmpdat = np.quantile(sim_corrs[count], [0.05, 0.25, 0.5, 0.75, 0.95], axis = 1).T
    tmpdf = pd.DataFrame(tmpdat, columns = ['5', '25', '50', '75', '95'])
    tmpdf = tmpdf.assign(observed = observed_corrs[count],
                         subid = int(isub),
                         cond = ['neutral', 'cued'])
    tmpdf = tmpdf.assign(meansim =sim_corrs[count].mean(axis=1),
                         stdsim = sim_corrs[count].std(axis=1),
                         sesim  = sp.stats.sem(sim_corrs[count], axis=1, ddof=0))
    #this outputs a 2 x 5 array where first row is neutral quantiles, second row is for cued
    corrs = pd.concat([corrs, tmpdf])
    

#%%

fig = plt.figure(figsize = (14, 7))
ax = fig.add_subplot(111)
count = -1
boxes = []
for isub in E1dat.subid.unique():
    count += 1
    tmpneut = corrs.query('subid == @isub and cond == "neutral"')
    tmpcued = corrs.query('subid == @isub and cond == "cued"')
    boxes.append(
        {
            'label' : "sub "+str(isub)+ ' neut',
            'whislo': tmpneut['5'],    # Bottom whisker position
            'q1'    : tmpneut['25'],    # First quartile (25th percentile)
            'med'   : tmpneut['50'],    # Median         (50th percentile)
            'q3'    : tmpneut['75'],    # Third quartile (75th percentile)
            'whishi': tmpneut['95'],    # Top whisker position
            'fliers': tmpneut['observed']        # Outliers
        }
        )
    
    boxes.append(
        {
            'label' : "sub"+str(isub)+ ' cued',
            'whislo': tmpcued['5'],    # Bottom whisker position
            'q1'    : tmpcued['25'],    # First quartile (25th percentile)
            'med'   : tmpcued['50'],    # Median         (50th percentile)
            'q3'    : tmpcued['75'],    # Third quartile (75th percentile)
            'whishi': tmpcued['95'],    # Top whisker position
            'fliers': tmpcued['observed']        # Outliers
        }
        )
    
pos = np.array([x for x in np.arange(1, nsubs*3) if x%3 != 0])
ax.bxp(boxes, positions = pos, vert = True, showfliers=False)
ax.set_xticklabels(ax.get_xticklabels(), rotation=45)

count = -1
for isub in E1dat.subid.unique():
    count += 1
    ax.scatter(pos[count], corrs.query('subid == @isub and cond == "neutral"')['observed'],
               color = '#bdbdbd', s = 15)
    count += 1
    ax.scatter(pos[count], corrs.query('subid == @isub and cond == "cued"')['observed'],
               color = '#3182bd', s = 15)
ax.set_ylabel("spearmans r")
ax.set_xlabel('participant & condition')
fig.savefig(op.join(wd, 'PermutationControl_boxplot_perSubCondition.eps'), format = 'eps', dpi = 300)
fig.savefig(op.join(wd, 'PermutationControl_boxplot_perSubCondition.pdf'), format = 'pdf', dpi = 300)
#%%

count = -1
fig = plt.figure()
ax = fig.add_subplot(111)
for isub in E1dat.subid.unique():
    count += 1
    tmpsub = corrs.copy().query('subid == @isub')
    ax.scatter([pos[count], pos[count+1]], tmpsub.meansim,
                label = ['neutral', 'cued'], color = ['#bdbdbd', '#3182bd'])
    # ax.errorbar(x = [pos[count], pos[count+1]], y = tmpsub.meansim,
    #             yerr = tmpsub.sesim, ls = 'none', ecolor = ['#bdbdbd', '#3182bd'])
    ax.errorbar(x = [pos[count], pos[count+1]], y = tmpsub['50'],
                yerr = np.abs(np.array([tmpsub['5'].to_numpy(), tmpsub['95'].to_numpy()])),
                ls = 'none', ecolor = ['#bdbdbd', '#3182bd'])
    ax.scatter([pos[count], pos[count+1]], tmpsub.observed, label = 'observed',
               color = ['#bdbdbd','#3182bd'], marker ='x')
    count+=1
ax.set_xlabel('participant & condition')
ax.set_ylabel("spearman's r")
fig.savefig(op.join(wd, 'PermutationControl_pointrange_perSubCondition.eps'), format = 'eps', dpi = 300)
fig.savefig(op.join(wd, 'PermutationControl_pointrange_perSubCondition.pdf'), format = 'pdf', dpi = 300)
#%%

#can do a paired-samples t-test on the observed correlation vs mean permuted correlation
#to examine the reliability with which the observed corr > null hypothesis correlation

testdat = corrs.copy()[['meansim', 'observed', 'subid', 'cond']]

#calculate the fisher transform of both the null corr and the observed corr so you can t-test them
#fisher transform is used to normalise the bounded spearmans corr [-1 1] so it fits normality assumption
#basically stretches values closer to the boundary, but largely preserves the original value of data towards the middle of the range
testdat = testdat.assign(
    fisher_null = np.arctanh(testdat.meansim),
    fisher_observed = np.arctanh(testdat.observed)
    )

sp.stats.ttest_rel(testdat.query('cond == "neutral"').fisher_observed,
                   testdat.query('cond == "neutral"').fisher_null,
                   alternative = 'greater') #test whether the observed correlations are larger than the null correlations
#TtestResult(statistic=7.266832593078108, pvalue=3.3881970764564526e-07, df=19)
                   
sp.stats.ttest_rel(testdat.query('cond == "cued"').fisher_observed,
                   testdat.query('cond == "cued"').fisher_null,
                   alternative = 'greater') #test whether the observed correlations are larger than the null correlations
#TtestResult(statistic=6.291018963717266, pvalue=2.4336535788621724e-06, df=19)

sp.stats.ttest_rel(testdat.query('cond == "cued"').fisher_observed,
                   testdat.query('cond == "neutral"').fisher_observed,
                   alternative = 'greater')

testdat.groupby('cond', as_index=False).agg({'observed':['mean', 'sem'], 'meansim':['mean', 'sem']})
#       cond  observed             meansim          
#                 mean       sem      mean       sem
# 0     cued  0.178639  0.027793  0.000148  0.000201
# 1  neutral  0.189095  0.025763  0.000002  0.000176

"""

the premise of this analysis is to engage with the idea that we don't have insight into performance on a given trial
- if we knew how wrong we were (or imprecise the representation was), we wouldn't make the error in the first place
if people don't have insight into the single-trial representation but instead are just noisly sampling confidence from their belief about performance *across* trials
then we would expect to see a correlation between error and confidence if we randomly shuffle confidence
such that trial N is not associated with confidence on trial N, but instead on a different trial
- this preserves the across trial confidence distribution but kills the link between single-trial error and confidence

This generates a (null) distribution of correlations that we would expect if there was no single-trial introspection.
We can test whether the observed correlation is larger than the expected correlation under this null

to test this, you use the fisher transform (arctanh) of the correlations (to normalise them) and do a paired-samples t-test
between observed and expected-permuted correlation to see how reliably (across subjects) the observed correlation is larger


"""