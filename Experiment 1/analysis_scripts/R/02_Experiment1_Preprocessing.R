#import libraries
library(tidyverse) # general data manipulation & plotting
library(magrittr)  # allows use of more pipes

wd <- '/Users/sammi/Desktop/Experiments/DPhil/wmSelection'
setwd(wd)

dpath <- paste0(wd, '/data') #path to folder with behavioural data
figpath <- paste0(wd, '/figures')

#path to file with all subjects collated data
fpath <- paste0(dpath, '/wmSelection_BehaviouralData_All.csv')

df <- read.csv(fpath, header = T, as.is = T, sep = ',') 

if('X' %in% colnames(df)){ df <- df %>% dplyr::select(-starts_with('X') )}

#mark trials where decision time (time to press space bar) outside 2.5 SDs of condition specific mean
df %<>%
  dplyr::mutate(cond = ifelse(cue ==0, 'neutral', 'cued')) %>%
  dplyr::mutate(cond = as.factor(cond)) %>%
  dplyr::mutate(cond = relevel(cond, 'neutral')) %>%
  dplyr::mutate(DTcheck = 1) %>% #make new variable to approve discards
  dplyr::group_by(subid, cue) %>%
  dplyr::mutate(DTcheck = ifelse(DT <= ( mean(DT) + 2.5*sd(DT)) & DT >= (mean(DT) - 2.5*sd(DT)), 0, DTcheck)) %>%
  as.data.frame()


library(circular)
wrap <- function(x) (x+180)%%360 - 180 #function to wrap data between +/- 90 degrees. keeps sign of the response (-ve leftwards, +ve rightwards)
wrap90 <- function(x) (x+90)%%180 - 90

#bound response deviation between -90 and 90 because of symmetry
df %<>% dplyr::mutate(rdif = wrap90(resp-targori))

#compute some things about the confidence judgement
df %<>%
  dplyr::mutate(confwidth = abs(wrap90(confang-resp))) %>% #get width of confidence interval
  dplyr::mutate(absrdif = abs(rdif)) %>% #get absolute deviation
  dplyr::mutate(confdiff = abs(rdif) - confwidth) %>% #get the difference between confidence width, and response deviation (i.e. whether target inside or outside confidence interval)
  dplyr::group_by(subid) %>% #group by subject
  dplyr::mutate(prevtrlconfdiff = lag(confdiff)) %>% #get previous trial confidence error
  dplyr::mutate(prevtrlcw       = lag(confwidth)) %>% #get previous trial confidence width
  dplyr::mutate(prevtrlabsrdif  = lag(absrdif))  %>%#get previous trial error
  as.data.frame()


write.csv(df, file = paste0(dpath, '/wmSelection_BehaviouralData_All_Preprocessed.csv'), sep = ',', eol = '\n', dec = '.', col.names = T) #save preprocessed data with new name
