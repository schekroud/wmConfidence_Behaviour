# -*- coding: utf-8 -*-
"""
Created on Wed Aug 16 10:00:24 2023

@author: sammi
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
np.set_printoptions(suppress=True)
%matplotlib

def wrap(x):
    return (x+180)%360 - 180
def wrap90(x):
    return (x+90)%180 - 90

cuedcol = '#3182bd' #blue, use for cued
neutcol = '#bdbdbd' #light-ish grey, use for neutral
diffcol = '#B2DF8A' #for colouring of the bars of difference plots, third distinct colour


#create a parent distribution that is smooth, and a subject is a sample of this distribution
error_parent = wrap90(np.round(np.rad2deg(np.random.vonmises(0, 6, 1000)).astype(int)))
abserr_parent = np.abs(error_parent.copy())

cuederror_parent = wrap90(np.round(np.rad2deg(np.random.vonmises(0, 9, 1000)).astype(int)))

#visualise the signed and absolute distributions for this parent distribution
fig = plt.figure(figsize = (10,5))
ax = fig.add_subplot(1, 2, 1)
ax.hist(error_parent, color = neutcol, alpha = 0.7, label = 'neutral', bins=20)
ax.hist(cuederror_parent, color = cuedcol, alpha = 0.7, label = 'cued', bins=20)
ax = fig.add_subplot(1,2,2)
ax.hist(np.abs(error_parent), color = neutcol, alpha = 0.7, bins = 20, label = 'neutral')
ax.hist(np.abs(cuederror_parent), color = cuedcol, alpha = 0.7, bins = 20, label = 'cued')
ax.legend(loc = 'upper right')

ntrls = 128

#simulate a response error distribution by sampling from this parent distribution
err = np.random.choice(error_parent, size = ntrls, replace = True) #sample with replacement from this parent distribution
abserr = np.abs(err)

cuederr = np.random.choice(cuederror_parent, size = ntrls, replace = True) #sample cued errors with replacement from its parent distribution
#scenario 1:
# no insight on single trial error (or would not make the error)
# instead, perfect insight to their across trial performance (distribution)
# which means confidence is instead sampled from the same distribution as error, so that across trial insight is accurate to the performance distribution
neut_conf = np.random.choice(abserr_parent, size = ntrls, replace = True) #sample with replacement from the same distribution
cued_conf = np.random.choice(np.abs(cuederr), size = ntrls, replace = True)



cond = np.concatenate([np.tile(np.array(['neutral']), ntrls), np.tile(np.array(['cued']),ntrls)])
suberrors = np.concatenate([err, cuederr])
subconfs = np.concatenate([neut_conf, cued_conf])

df = pd.DataFrame(np.array([cond, suberrors, subconfs]).T, columns = ['condition', 'error', 'confidence'])
df.error = df.error.astype(int)
df.confidence = df.confidence.astype(int)
df = df.assign(absrdif = np.abs(df.error))
df = df.assign(confidence = np.where(df.confidence == 0, 0.001, df.confidence))
df = df.assign(logconf = np.log(df.confidence))
sns.lmplot(data = df,
           x = 'absrdif', y = 'logconf', hue = 'condition')

sns.lmplot(data = df,
           y = 'absrdif', x = 'confidence', hue = 'condition', logx = True)

#set up linear model
lm = sm.regression.linear_model.OLS.from_formula(data = df,
                                                 formula = 'logconf ~ absrdif*C(condition, Sum("neutral"))')
lmfit = lm.fit()
lmfit.summary()


#%% need to figure out a way of operationalising this to run the simulation with a number of subjects
nsubs = 20
ntrls = 




