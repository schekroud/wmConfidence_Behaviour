library(tidyverse)
library(magrittr)
library(lubridate)

#get demographics information for participants in Experiment 1

wd <- "/Users/sammi/Desktop/Experiments/DPhil/wmSelection"
setwd(wd)

infodir <- paste0(wd, '/data/info')
fnames  <- sort(list.files(path = infodir, pattern = '.csv'))
files  <- list(NULL)
count <- 1
for(i in fnames){
  path <- paste0(infodir, '/', i)
  tmp  <- read.csv(path, header = F, as.is = T, sep = ',')
  files[[count]] <- tmp
  count <- count+1
}

dat <- do.call('rbind', files)
dat %<>% 
  dplyr::rename('subid' = V1,
                'age'   = V2,
                'sex'   = V3,
                'handedness' = V4,
                'date'  = V5) %>%
  dplyr::select(-date) %>%
  dplyr::mutate(sex = as.factor(sex), handedness = as.factor(handedness))
summary(dat)
head(dat)
subs2use <- c(1,2,3,4,5,6,7,9,10,11,13,14,15,16,17,18,19,20,21,22)
dat %<>% dplyr::filter(subid %in% subs2use)
dim(dat)
summary(dat) #print out demographics information
