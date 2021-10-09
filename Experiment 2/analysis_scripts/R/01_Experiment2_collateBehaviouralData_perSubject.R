library(dplyr)
getwd()
dir <- '/home/sammirc/Desktop/DPhil/wmConfidence'
dir <- '/Users/sammi/Desktop/Experiments/DPhil/wmConfidence'
setwd(dir)

datapath <- paste0(dir, '/data/datafiles')

#list of all subject IDs
sublist <- seq( 1, 26, by = 1)


#data was saved per block of the task, in case subjects withdrew partway through. This script collates them into one datafile per subject
for(sub in sublist){
  
  #subjects 1 and 2 only completed 1 session
  if(sub %in% c(1,2)){
    subpath <- paste0(datapath, sprintf('/s%02d/', sub))
    fileList  <- sort(list.files(path = subpath, pattern = sprintf('wmConfidence_s%02d_', sub))) #.csv))
    dataFiles <- list(NULL)
    count <- 1
    for(i in fileList){
      path <- paste0(subpath, '/', i)
      tmp <- read.csv(path, header = T, as.is = T, sep = ',')
      dataFiles[[count]] <- tmp
      count <- count + 1
    }
    df <- do.call('rbind', dataFiles)
    fname <- paste0(datapath, '/collated_data/', sprintf('wmConfidence_S%02d_allData.csv', sub))
    write.csv(df, file = fname, eol = '\n',col.names = T)
  }
  
  #subjects 3, 10 and 19 only completed one session
  if(sub %in% c(3,10, 19)){
    subpath <- paste0(datapath, sprintf('/s%02d/', sub))
    fileList  <- sort(list.files(path = subpath, pattern = sprintf('wmConfidence_s%02da_', sub))) #.csv))
    dataFiles <- list(NULL)
    count <- 1
    for(i in fileList){
      path <- paste0(subpath, '/', i)
      tmp <- read.csv(path, header = T, as.is = T, sep = ',')
      dataFiles[[count]] <- tmp
      count <- count + 1
    }
    df <- do.call('rbind', dataFiles)
    df$session <- 'a'
    fname <- paste0(datapath, '/collated_data/', sprintf('wmConfidence_S%02d_allData.csv', sub))
    write.csv(df, file = fname, eol = '\n',col.names = T)
  }
  
  #for all other subjects, there should be 2 sessions x 8 blocks of data
  if(sub > 3 & sub != 10 & sub != 19){
    for(part in c('a', 'b')){
      subpath <- paste0(datapath, sprintf('/s%02d/%s', sub,part))
      fileList  <- sort(list.files(path = subpath, pattern = sprintf('wmConfidence_s%02d%s_', sub,part))) #.csv))
      dataFiles <- list(NULL)
      count <- 1
      for(i in fileList){
        path <- paste0(subpath, '/', i)
        tmp <- read.csv(path, header = T, as.is = T, sep = ',')
        dataFiles[[count]] <- tmp
        count <- count + 1
      }
      df <- do.call('rbind', dataFiles)
      df$session <- part
      fname <- paste0(datapath, '/collated_data/', sprintf('wmConfidence_S%02d%s_allData.csv', sub,part))
      write.csv(df, file = fname, eol = '\n',col.names = T)
    }
    
  }
}

