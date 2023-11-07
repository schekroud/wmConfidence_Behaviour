# -*- coding: utf-8 -*-
"""
Created on Fri Nov  3 12:05:03 2023

@author: sammirc
"""

#permutation random effects analysis
#shuffle confidence, single subject glm, across subject t-test, store t values, get null distribution of t-values
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

pvals = []
tvals = []
nsims = 10000
simbetas = np.empty(shape = (4, nsims))

count = -1
if not op.exists(op.join(wd, 'RandomEffectsControl_Permutations_10k_ttestOut.npy')):
    with progressbar.ProgressBar(max_value = nsims).start() as bar:
        for isim in range(nsims):       
            bar.update(isim)
            #loop oversubjects and shuffle confidence response
            #add shuffled data back into dataframe
            allBetas = pd.DataFrame()
            for isub in subs2use:
                tmpdf = E1dat.copy().query('subid == @isub') #get subject data
                tmpdf_neut = tmpdf.query('cond == "neutral"')
                tmpdf_cued = tmpdf.query('cond == "cued"')
                
                #get just confidence
                obsconf_neut = tmpdf_neut.confwidth.copy().to_numpy()
                obsconf_cued = tmpdf_cued.confwidth.copy().to_numpy()
                
                
                #shuffle confidence within condition
                #with the condition that the shuffled confidence cannot be the same as the observed confidence
                while np.equal(tmpdf_neut.confwidth.to_numpy(),  obsconf_neut).all():
                    #keep shuffling this for as long as the two arrays are equal (we need to ensure the shuffled data arent the same as observed data)
                    np.random.shuffle(obsconf_neut)
                
                while np.equal(tmpdf_cued.confwidth.to_numpy(), obsconf_cued).all():
                    np.random.shuffle(obsconf_cued)
                    
                #reassign back in, and bind dataframe
                tmpdf_neut = tmpdf_neut.assign(confwidth = obsconf_neut)
                tmpdf_cued = tmpdf_cued.assign(confwidth = obsconf_cued)
                subdat = pd.concat([tmpdf_neut, tmpdf_cued])
                
                
                subdat = subdat.assign(confwidth = np.where(subdat.confwidth == 0, 0.1, subdat.confwidth)) #cant log a 0, so set this to a similarly small value instead
                subdat = subdat.assign(logcw = np.log(subdat.confwidth))
                
                logcw = subdat.logcw.to_numpy()
                cuetype = subdat.cond.to_numpy()
                cuetype = np.where(cuetype == 'neutral', -1, 1) #set neutral trials to -1, so this is a contrast regressor of cued - neutral
                error = subdat.absrdif.to_numpy()
                error = np.where(error == 0, 0.1, error) #zero error wouldn't get modelled, so set it to similarly small error to be able to include in model
                errXcue = np.multiply(error, cuetype) #code the interaction
                intercept = np.ones(logcw.size)
                
                desMat = np.array([intercept, cuetype, error, errXcue]).T
                
                model = sm.regression.linear_model.OLS(endog = logcw,
                                                              exog = pd.DataFrame(desMat, columns = ['intercept', 'cuetype', 'error', 'cueXerror']))
                # model = sm.regression.linear_model.OLS.from_formula(data = subdat,
                #                                                     formula = 'logcw ~ error*C(cond, Treatment(reference="neutral"))')
                
                fit = model.fit()
                fit.summary()
                betas = pd.DataFrame(fit.params).T
                betas = betas.assign(subid = isub)
                
                allBetas = pd.concat([allBetas, betas]) #this contains the glm output betas for each single subject glm, in one dataframe
            
            allBetas = pd.melt(allBetas, id_vars = 'subid', value_vars = ['intercept', 'cuetype', 'error', 'cueXerror'], var_name = 'regressor', value_name = 'beta')
            allBetas = allBetas.query('regressor != "intercept"') #drop intercept as not that useful to see
    
            t, p = sp.stats.ttest_1samp(allBetas.query('regressor == "cueXerror"').beta, popmean = 0, alternative = 'two-sided')
            pvals.append(p)
            tvals.append(t)
    pvals = np.array(pvals)
    tvals = np.array(tvals)
    np.save(file = op.join(wd, 'RandomEffectsControl_Permutations_10k_ttestOut.npy'), arr = tvals)
else:
    tvals = np.load(op.join(wd, 'RandomEffectsControl_Permutations_10k_ttestOut.npy'))
#%%

# observed for the random effects analysis (across subject t-test on output from single subject glms) -> t = 2.689, p = 0.0145

fig = plt.figure()
ax = fig.add_subplot(111)
sns.histplot(x = tvals, bins = 100, alpha = 0.7, kde = True,
             color = '#1c9099', edgecolor = 'none', ax = ax)
ax.axvline(x = 2.689, lw = 1, color = '#000000', ls = 'dashed')
ax.set_xlabel('t-value')
ax.set_ylabel('count')
ax.set_title('null distribution of t-values from permutation')
fig.savefig(op.join(wd, 'supplementary_analysis_figures', 'permutation_analyses', 'RandomEffectsControl_tval_NullDistribution.eps'), format = 'eps', dpi = 300)
fig.savefig(op.join(wd, 'supplementary_analysis_figures', 'permutation_analyses', 'RandomEffectsControl_tval_NullDistribution.pdf'), format = 'pdf', dpi = 300)

np.greater(tvals, 2.689).sum()/tvals.size #percentage of tvalues with an effect size greater than our observed effect
# 0.0066 <- pvalue for the observed t-value under this null distribution of effect sizes


