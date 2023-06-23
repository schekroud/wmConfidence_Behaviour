library(tidyverse)
getwd()
# dir <- '/home/sammirc/Desktop/DPhil/wmConfidence'
# dir <- '/Users/sammi/Desktop/Experiments/DPhil/wmConfidence'
dir <- 'C:/Users/sammirc/Desktop/phd/wmConfidence_Behaviour/Experiment2'
setwd(dir)

datapath <- paste0(dir, '/data/datafiles')

#list of all subject IDs
sublist <- seq(4, 26, by = 1)
subs2use = c(4, 5, 6, 7, 8, 9, 11, 12, 13, 14, 15, 16, 17, 18, 20, 21, 22, 24, 25, 26)

for(sub in subs2use){
  sess1path = sprintf('%s/s%02d/a', datapath, sub)
  sess2path = sprintf('%s/s%02d/b', datapath, sub)
  
  sess1files = sort(list.files(path = sess1path, pattern = '.csv', full.names = T)) #get full paths for all participant data files
  sess2files = sort(list.files(path = sess2path, pattern = '.csv', full.names = T)) #get full paths for all participant data files

  sess1 = purrr::map_dfr(sess1files, function(x) read.csv(x, header = T, as.is = T,
                                                          row.names= ,sep = ',')) %>% #read and rowbind into one dataframe
          dplyr::mutate(session = 'a')
  sess2 = purrr::map_dfr(sess2files, function(x) read.csv(x, header = T, as.is = T,
                                                          row.names= ,sep = ',')) %>% #read and rowbind into one dataframe
          dplyr::mutate(session = 'b')
  
  collated = sess1 %>% dplyr::bind_rows(sess2)
  collated_fname = sprintf('%s/collated_data/wmConfidence_s%02d_allData.csv', datapath, sub)
  write.csv(collated, file = collated_fname, col.names = T, row.names = F)
  
  #do some preprocessing of these data files, separately for each session per subject
  
  sess1 %<>%
    dplyr::mutate(cond = ifelse(cue == 0, 'neutral', 'cued')) %>%
    dplyr::mutate(cond = as.factor(cond)) %>%
    dplyr::mutate(cond = relevel(cond, 'neutral')) %>%
    dplyr::mutate(DTcheck = 1) %>% #make new variable to approve discards
    dplyr::group_by(subid, cue) %>%
    dplyr::mutate(DTcheck = ifelse(DT <= ( mean(DT) + 2.5*sd(DT)) & DT >= (mean(DT) - 2.5*sd(DT)), 0, DTcheck)) %>%
    as.data.frame()
  
  sess2 %<>%
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
  sess1 %<>% dplyr::mutate(rdif = wrap90(resp-targori))
  sess2 %<>% dplyr::mutate(rdif = wrap90(resp-targori))
  
    
  #compute some things about the confidence judgement
  sess1 %<>%
    dplyr::mutate(confwidth = abs(wrap90(confang-resp))) %>% #get width of confidence interval
    dplyr::mutate(absrdif = abs(rdif)) %>%                   #get absolute response error
    dplyr::mutate(confdiff = abs(rdif) - confwidth)  %>%     #get confidence error (i.e. whether target inside or outside confidence interval)
    dplyr::mutate(prevtrlconfdiff = lag(confdiff)) %>%       #get previous trial confidence error
    dplyr::mutate(prevtrlcw       = lag(confwidth)) %>%      #get previous trial confidence width
    dplyr::mutate(prevtrlabsrdif  = lag(absrdif))            #get previous trial error
  
  sess2 %<>%
    dplyr::mutate(confwidth = abs(wrap90(confang-resp))) %>% #get width of confidence interval
    dplyr::mutate(absrdif = abs(rdif)) %>%                   #get absolute response error
    dplyr::mutate(confdiff = abs(rdif) - confwidth)  %>%     #get confidence error (i.e. whether target inside or outside confidence interval)
    dplyr::mutate(prevtrlconfdiff = lag(confdiff)) %>%       #get previous trial confidence error
    dplyr::mutate(prevtrlcw       = lag(confwidth)) %>%      #get previous trial confidence width
    dplyr::mutate(prevtrlabsrdif  = lag(absrdif))            #get previous trial error
  
  detach("package:circular", unload=TRUE)
  
  
  df = sess1 %>% dplyr::bind_rows(sess2)
  fname = sprintf('%s/preprocessed_data/wmConfidence_s%02d_allData_preprocessed.csv', datapath, sub)
  write.csv(df, file = fname, col.names = T, row.names = F)  
}

#after gathering all block data into single subject files, combine these single subject files into one dataframe for easier analysis
filePaths <- paste0(datapath, '/preprocessed_data')
dfiles = list.files(path = filePaths, pattern = '.csv', full.names = T) #get full paths for all participant data files

dframe <- purrr::map_dfr(dfiles, function(x) read.csv(x, header = T, as.is = T,
                                                      sep = ',')) #read and rowbind into one dataframe


fname <- paste0(dir, '/data/wmConfidence_BehaviouralData_All.csv')
write.csv(dframe, file = fname, sep = ',', eol = '\n', dec = '.', col.names = T, row.names = F)




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# below is old code


# 
# #data was saved per block of the task, in case subjects withdrew partway through. This script collates them into one datafile per subject
# for(sub in sublist){
#   
#   #subjects 1 and 2 only completed 1 session
#   if(sub %in% c(1,2)){
#     subpath <- paste0(datapath, sprintf('/s%02d/', sub))
#     fileList  <- sort(list.files(path = subpath, pattern = sprintf('wmConfidence_s%02d_', sub))) #.csv))
#     dataFiles <- list(NULL)
#     count <- 1
#     for(i in fileList){
#       path <- paste0(subpath, '/', i)
#       tmp <- read.csv(path, header = T, as.is = T, sep = ',')
#       dataFiles[[count]] <- tmp
#       count <- count + 1
#     }
#     df <- do.call('rbind', dataFiles)
#     fname <- paste0(datapath, '/collated_data/', sprintf('wmConfidence_S%02d_allData.csv', sub))
#     write.csv(df, file = fname, eol = '\n',col.names = T)
#   }
#   
#   #subjects 3, 10 and 19 only completed one session
#   if(sub %in% c(3,10, 19)){
#     subpath <- paste0(datapath, sprintf('/s%02d/', sub))
#     fileList  <- sort(list.files(path = subpath, pattern = sprintf('wmConfidence_s%02da_', sub))) #.csv))
#     dataFiles <- list(NULL)
#     count <- 1
#     for(i in fileList){
#       path <- paste0(subpath, '/', i)
#       tmp <- read.csv(path, header = T, as.is = T, sep = ',')
#       dataFiles[[count]] <- tmp
#       count <- count + 1
#     }
#     df <- do.call('rbind', dataFiles)
#     df$session <- 'a'
#     fname <- paste0(datapath, '/collated_data/', sprintf('wmConfidence_S%02d_allData.csv', sub))
#     write.csv(df, file = fname, eol = '\n',col.names = T)
#   }
#   
#   #for all other subjects, there should be 2 sessions x 8 blocks of data
#   if(sub > 3 & sub != 10 & sub != 19){
#     for(part in c('a', 'b')){
#       subpath <- paste0(datapath, sprintf('/s%02d/%s', sub,part))
#       fileList  <- sort(list.files(path = subpath, pattern = sprintf('wmConfidence_s%02d%s_', sub,part))) #.csv))
#       dataFiles <- list(NULL)
#       count <- 1
#       for(i in fileList){
#         path <- paste0(subpath, '/', i)
#         tmp <- read.csv(path, header = T, as.is = T, sep = ',')
#         dataFiles[[count]] <- tmp
#         count <- count + 1
#       }
#       df <- do.call('rbind', dataFiles)
#       df$session <- part
#       fname <- paste0(datapath, '/collated_data/', sprintf('wmConfidence_S%02d%s_allData.csv', sub,part))
#       write.csv(df, file = fname, eol = '\n',col.names = T)
#     }
#     
#   }
# }

