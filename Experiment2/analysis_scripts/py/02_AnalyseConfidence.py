#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Jun 26 12:17:01 2023

@author: sammi
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


nsubs = df.subid.unique().size

#look across subjects to see how often people didn't click to confirm their confidence response
df.assign(confclicked = np.multiply(-1, np.subtract(df.confclicked, 1)))[['subid', 'confclicked', 'cond']].groupby(
    ['subid', 'cond'], as_index=True).agg(
        {'confclicked':'sum'})


#take only usable data (clicked to confirm response + confidence, and not too slow or fast)
df = df.query('clickresp == 1 and DTcheck == 0 and confclicked == 1')

#%%

#%% look at mean reported confidence width across subjects

df_confdist = df.groupby(['subid', 'cond'], as_index= False).agg({'confwidth':'mean'})
df_confdist_plot = df_confdist.groupby(['cond'], as_index=False).agg(
    {'confwidth':['mean', sp.stats.sem]})
df_confdist_plot.columns = ['condition', 'meanConf', 'semConf']

df_confdist_plot
#   condition   meanConf   semConf
# 0      cued  17.549642  1.204232
# 1   neutral  18.367572  1.301768

#t-test to look at confidence wedge width between cue conditions
sp.stats.ttest_rel(df_confdist.query('cond == "cued"').confwidth, df_confdist.query('cond == "neutral"').confwidth)
# TtestResult(statistic=-4.224129847332724, pvalue=0.00045932647027278, df=19)

#can do this non-parametrically too if you are concerned about the distribution of mean confidence wedge widths not being normal across subjects
sp.stats.wilcoxon(df_confdist.query('cond == "cued"').confwidth, df_confdist.query('cond == "neutral"').confwidth)
# WilcoxonResult(statistic=13.0, pvalue=0.0001678466796875)


fig = plt.figure(figsize = (5,5))
ax = fig.add_subplot(111)
ax.bar(x = df_confdist_plot.condition, height = df_confdist_plot.meanConf, color = [cuedcol, neutcol], width = 0.7)
ax.errorbar(x = df_confdist_plot.condition, y = df_confdist_plot.meanConf, yerr = df_confdist_plot.semConf,
            fmt = 'none', ecolor = '#000000', elinewidth = 2, capsize = 7)
ax.set_ylabel('mean confidence width (°)')
ax.set_axisbelow(True)
ax.grid(True, color = '#bdbdbd')
ax.set_ylim([10, 20])
#%%

#%% look at absolute confidence error  (ignoring over- vs under-confidence)

df_confmu = df.assign(absconfdiff = np.abs(df.confdiff)).groupby(['subid', 'cond'], as_index = False).agg(
    {'absconfdiff':'mean'})

df_confmu_plot = df_confmu.groupby('cond', as_index = False).agg({'absconfdiff':['mean', sp.stats.sem]})
df_confmu_plot.columns = ['condition', 'meanAbscdiff', 'semAbscdiff']

df_confmu_plot
#   condition  meanAbscdiff  semAbscdiff
# 0      cued     10.769475     0.814613
# 1   neutral     11.228211     0.845833


#t-test to look at absolute confidence error between cue conditions
sp.stats.ttest_rel(df_confmu.query('cond == "cued"').absconfdiff, df_confmu.query('cond == "neutral"').absconfdiff)
# TtestResult(statistic=-2.5869090243579027, pvalue=0.01808298964217226, df=19)

#can do this non-parametrically too if you are concerned about the distribution of mean absolute confdidence error not being normal across subjects
sp.stats.wilcoxon(df_confmu.query('cond == "cued"').absconfdiff, df_confmu.query('cond == "neutral"').absconfdiff)
# WilcoxonResult(statistic=40.0, pvalue=0.013616561889648438)


#%% look at relationship between response error and confidence width
# within subject do a glm, then look at across subject effects in a random effects analysis
subs2use = df.subid.unique().tolist()
allBetas = pd.DataFrame()
allfits = []
df_glms = pd.DataFrame()
for i in subs2use: #loop over subjects
    subdat = df.copy().query('subid == @i').reset_index()
    subdat = subdat.assign(confwidth = np.where(subdat.confwidth == 0, 0.1, subdat.confwidth)) #cant log a 0, so set this to a similarly small value instead
    subdat = subdat.assign(logcw = np.log(subdat.confwidth))
    
    logcw = subdat.logcw.to_numpy()
    cuetype = subdat.cue.to_numpy()
    cuetype = np.where(cuetype == 0, -1, cuetype) #set neutral trials to -1, so this is a contrast regressor of cued - neutral
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
    betas = betas.assign(subid = i)
    
    residuals = fit.resid
    subdat = subdat.assign(residual = residuals,
                           fittedlogcw = fit.fittedvalues)
    
    df_glms = pd.concat([df_glms, subdat])
    allfits.append(fit)
    allBetas = pd.concat([allBetas, betas])

#random effects analysis of these betas - e.g., are they significantly different from 0 across the group?
# inference here would be whether a new subject would likely have the same effect
# inference at subject level, rather than observation (trial) level

allbetas = pd.melt(allBetas, id_vars = 'subid', value_vars = ['intercept', 'cuetype', 'error', 'cueXerror'], var_name = 'regressor', value_name = 'beta')
allbetas = allbetas.query('regressor != "intercept"') #drop intercept as not that useful to see
sns.barplot(allbetas, x = 'regressor', y = 'beta', estimator = 'mean', errorbar = 'se', hue = 'regressor')


#t-tests on these betas to test for significant effects across subjects
sp.stats.ttest_1samp(allbetas.query('regressor == "cuetype"').beta, popmean = 0, alternative = 'two-sided')
# TtestResult(statistic=-3.343468144736726, pvalue=0.003413809179756467, df=19)


sp.stats.ttest_1samp(allbetas.query('regressor == "error"').beta, popmean = 0, alternative = 'two-sided')
# TtestResult(statistic=7.629779051450365, pvalue=3.3630204166516365e-07, df=19)



sp.stats.ttest_1samp(allbetas.query('regressor == "cueXerror"').beta, popmean = 0, alternative = 'two-sided')
# TtestResult(statistic=1.0187624911181468, pvalue=0.32111357116876726, df=19)
#non significant interaction between cue type and error on confidence width across subjects

sp.stats.wilcoxon(allbetas.query('regressor == "cueXerror"').beta)
#WilcoxonResult(statistic=73.0, pvalue=0.24548721313476562) #also non sig here


#plot these regressor coefficients across subjects
plotbetas = allbetas.groupby('regressor', as_index = False).agg({'beta':['mean', sp.stats.sem]})
plotbetas.columns = ['regressor', 'meanBeta', 'semBeta']
plotbetas = plotbetas.sort_values(by = 'regressor', ascending = False)

fig = plt.figure(figsize = (4,4))
ax = fig.add_subplot(111)
ax.bar(x = plotbetas.regressor, height = plotbetas.meanBeta, color = ['#1b9e77', '#d95f02', '#7570b3'], width = 0.5)
ax.errorbar(x = plotbetas.regressor, y = plotbetas.meanBeta, yerr = plotbetas.semBeta,
            fmt = 'none', ecolor = '#000000', elinewidth = 2, capsize = 7)
ax.set_ylabel('beta (AU)')
ax.set_axisbelow(True)
ax.grid(True, color = '#bdbdbd')
fig.tight_layout()
fig.savefig(fname = op.join(figpath, 'gave_betas_singleSubGlms_logcw_errXcueineraction.eps'), format = 'eps', dpi = 300)
fig.savefig(fname = op.join(figpath, 'gave_betas_singleSubGlms_logcw_errXcueineraction.pdf'), format = 'pdf', dpi = 300)

#%% can we visualise this across subjects easily? maybe not

#get mean intercept and the mean beta for
allBetas = allBetas.assign(errorVsInt   = np.subtract(allBetas.error, allBetas.cueXerror),
                           errorPlsInt  = np.add(allBetas.error, allBetas.cueXerror))

subdat = df_glms.query('subid == 1')
#%%
fig = plt.figure()
ax = fig.add_subplot(111)
sns.scatterplot(subdat, x = 'absrdif', y = 'logcw', hue = 'cond', palette = [neutcol, cuedcol], hue_order = ['neutral', 'cued'], ax = ax)
ax.axline(xy1 = (0, allBetas.iloc[0,0]), slope = allBetas.iloc[0, 5], color = neutcol)
ax.axline(xy1 = (0, allBetas.iloc[0,0]), slope = allBetas.iloc[0, 6], color = cuedcol)

#%%

#visualising this interaction across subjects -- ask jill for help because im stupid
df_glms = df_glms.assign(fitted_exp = np.exp(df_glms.fittedlogcw))

fig =plt.figure()
ax = fig.add_subplot(111)
sns.scatterplot(df_glms, x = 'absrdif', y = 'fittedlogcw', hue = 'cond',
                palette = [neutcol, cuedcol], hue_order = ['neutral', 'cued'], ax = ax)
ax.axline(xy1 = (0, allBetas.intercept.mean()), slope = allBetas.errorVsInt.mean(), color = neutcol)
ax.axline(xy1 = (0, allBetas.intercept.mean()), slope = allBetas.errorPlsInt.mean(), color = cuedcol)


#%%

'''
for each subject we fit a model that attempts to predict their confidence based on their response error, condition and interaction
we get residuals in this model - variability in confidence that isn't explained by the model
can look at the distribution of these residuals to look basically at the model precision
if there is greater variability in residuals, it means there is more variability around the regression line
cognitively, this lets you look at whether the distribution of confidence changes as error changes
    - it's a way of looking at conditional distributions
'''

#look at variability around the regression line across subjects, per condition


df_resids = df_glms.groupby(['subid', 'cond'], as_index = False).agg({'residual':['std']})
df_resids.columns = ['subid', 'cond', 'std_residual']

fig = plt.figure()
ax = fig.add_subplot(111)
sns.kdeplot(df_resids, x = 'std_residual', hue = 'cond',ax=ax)
sns.rugplot(df_resids, x = 'std_residual', hue = 'cond',ax=ax) 

#t-test on diffrence in standard deviation of the residuals between conditions
sp.stats.ttest_rel(a = df_resids.query('cond == "cued"').std_residual.to_numpy(),
                   b = df_resids.query('cond == "neutral"').std_residual.to_numpy(),
                   alternative = 'two-sided')
# TtestResult(statistic=-1.6584302083403246, pvalue=0.11364856921011005, df=19)

#non-parametric alternative if the distribution of mean residuals (across participants) is not normal (even if residual distribution is)
sp.stats.wilcoxon(x = df_resids.query('cond == "cued"').std_residual.to_numpy(),
                  y = df_resids.query('cond == "neutral"').std_residual.to_numpy())
# WilcoxonResult(statistic=54.0, pvalue=0.058258056640625)


# no across-subject difference in variability of residuals between conditions
