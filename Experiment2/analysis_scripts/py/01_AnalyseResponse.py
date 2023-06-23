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
os.chdir(wd)