#import libraries
library(tidyverse) # general data manipulation & plotting
library(magrittr)  # allows use of more pipes

wd <- '/Users/sammi/Desktop/Experiments/DPhil/wmConfidence'
setwd(wd)

datapath <- paste0(wd, '/data/datafiles/collated_data')
outpath  <- paste0(wd, '/data/datafiles/preprocessed_data')


#get full subject list
sublist <- seq( 1, 26, by = 1)


#preprocessing of the data is done on all trials within-subject in this script.
for(sub in sublist){
  
  #subjects 1, 2, 3, 10 and 19 only did 1 session of the task.
  if(sub <= 3 | sub == 10 | sub == 19){
    fpath <- paste0(datapath, sprintf('/wmConfidence_S%02d_allData.csv', sub))
    df <- read.csv(fpath, header = T, as.is = T, sep = ',') 
    if('X' %in% colnames(df)){ df <- df %>% dplyr::select(-starts_with('X')) }
    
    
    df %<>%
      dplyr::mutate(cond = ifelse(cue == 0, 'neutral', 'cued')) %>%
      dplyr::mutate(cond = as.factor(cond)) %>%
      dplyr::mutate(cond = relevel(cond, 'neutral')) %>%
      dplyr::mutate(session = 'a') %>% #force to 'a', as only one session in these subjects, corrects fact that no session var in subs 1&2
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
      dplyr::mutate(prevtrlconfdiff = lag(confdiff)) %>% #get previous trial confidence error
      dplyr::mutate(prevtrlcw       = lag(confwidth)) %>% #get previous trial confidence width
      dplyr::mutate(prevtrlabsrdif  = lag(absrdif)) #get previous trial error
    
    detach("package:circular", unload=TRUE)
    
    fname <- paste0(outpath, sprintf('/wmConfidence_S%02d_allData_preprocessed.csv', sub))
    write.csv(df, file = fname, eol = '\n', col.names = T)
  }
  
  #other subjects did 2 sessions, so we need to load in 2 two datafiles per subject
  if(sub > 3 & sub != 10 & sub != 19){
    for(part in c('a', 'b')){
      
      fpath <- paste0(datapath, sprintf('/wmConfidence_S%02d%s_allData.csv', sub, part))
      df <- read.csv(fpath, header = T, as.is = T, sep = ',')# %>% select(-X)
      
      if('X' %in% colnames(df)){ df <- df %>% dplyr::select(-starts_with('X')) }
      
      df %<>%
        dplyr::mutate(cond = ifelse(cue == 0, 'neutral', 'cued')) %>%
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
        dplyr::mutate(absrdif = abs(rdif)) %>%                   #get absolute response error
        dplyr::mutate(confdiff = abs(rdif) - confwidth)  %>%     #get confidence error (i.e. whether target inside or outside confidence interval)
        dplyr::mutate(prevtrlconfdiff = lag(confdiff)) %>%       #get previous trial confidence error
        dplyr::mutate(prevtrlcw       = lag(confwidth)) %>%      #get previous trial confidence width
        dplyr::mutate(prevtrlabsrdif  = lag(absrdif))            #get previous trial error
      
      detach("package:circular", unload=TRUE)
      
      
      fname <- paste0(outpath, sprintf('/wmConfidence_S%02d%s_allData_preprocessed.csv', sub,part)) #write out the preprocessed data as a new file
      write.csv(df, file = fname, eol = '\n', col.names = T)
      
      
    }
  }
  
}
