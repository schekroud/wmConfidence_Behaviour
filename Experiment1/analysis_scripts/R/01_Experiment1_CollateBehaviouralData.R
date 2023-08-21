library(dplyr)
getwd()
#dir <- '/Users/sammi/Desktop/Experiments/DPhil/wmSelection'
dir <- 'C:/Users/sammirc/Desktop/phd/wmConfidence_Behaviour/Experiment1'
setwd(dir)

datapath <- paste0(dir, '/data/datafiles')


#loop across each subject, and collate the data from all blocks into one file (the 'allBlocks' file saved at end of task isnt good to use!!)

subs <- seq(1,22,by=1)
for(sub in subs){
  subpath <- paste0(datapath, sprintf('/s%02d/', sub))
  fileList <- sort(list.files(path = subpath, pattern = '.csv'))
  dataFiles = list(NULL)
  count <- 1

  for (i in fileList){
    path <- paste0(subpath, i)
    tmp <- read.csv(path, header = T, as.is = T, sep = ',')
    dataFiles[[count]] <- tmp
    count <- count+1
  }

  data <- do.call('rbind', dataFiles)
  fname <- paste0(datapath, '/collated_data/', sprintf('wmSelection_s%02d_allData.csv', sub))
  write.csv(data, file = fname, sep = ',', eol = '\n', dec = '.', col.names = T, row.names = F)
}



#after gathering all block data into single subject files, combine these single subject files into one dataframe for easier analysis
filePaths <- paste0(datapath, '/collated_data')
dfiles = list.files(path = filePaths, pattern = '.csv', full.names = T) #get full paths for all participant data files

df <- purrr::map_dfr(dfiles, function(x) read.csv(x, header = T, as.is = T,
                                                  row.names= ,sep = ',')) #read and rowbind into one dataframe


fname <- paste0(dir, '/data/wmSelection_BehaviouralData_All.csv')
write.csv(df, file = fname, sep = ',', eol = '\n', dec = '.', col.names = T, row.names = F)


# - - - - - - - - Do some preprocessing of this data, getting some information that we want later on in the analyses - - - - - - - - - - - #

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


write.csv(df, file = paste0(dir, '/data', '/wmSelection_BehaviouralData_All_Preprocessed.csv'),
          sep = ',', eol = '\n', dec = '.', col.names = T, row.names =F) #save preprocessed data with new name


