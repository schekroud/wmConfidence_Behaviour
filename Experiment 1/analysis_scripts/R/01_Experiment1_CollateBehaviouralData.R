library(dplyr)
getwd()
dir <- '/Users/sammi/Desktop/Experiments/DPhil/wmSelection'
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
  fname <- paste0(datapath, '/collated_data/', sprintf('wmSelection_S%02d_allData.csv', sub))
  write.csv(data, file = fname, sep = ',', eol = '\n', dec = '.', col.names = T)
}



#now true collated data is done for each subjects, combine all subjects into one file again like normal


fileList <- sort(list.files(path = paste0(datapath, '/collated_data'), pattern = '.csv'))


dataFiles =list(NULL)
count <- 1

for(i in fileList){
  path <- paste0(datapath, '/collated_data/', i)
  tmp <- read.csv(path, header = T, as.is = T, sep = ',') #%>% dplyr::mutate(subid = count) #dirty hack
  dataFiles[[count]] <- tmp
  count <- count+1
}

data <- do.call('rbind', dataFiles)

fname <- paste0(dir, '/data/wmSelection_BehaviouralData_All.csv')
write.csv(data, file = fname, sep = ',', eol = '\n', dec = '.', col.names = T)
