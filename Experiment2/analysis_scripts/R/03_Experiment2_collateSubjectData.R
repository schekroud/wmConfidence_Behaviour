library(dplyr)
library(magrittr)
getwd()
dir <- '/home/sammirc/Desktop/DPhil/wmConfidence'
dir <- '/Users/sammi/Desktop/Experiments/DPhil/wmConfidence'
setwd(dir)

datapath <- paste0(dir, '/data/datafiles/preprocessed_data')

sublist <- seq( 1, 26, by = 1)


#this will collate all parts of the same subjects into one file per subject
dataFiles = list(NULL)
for(sub in sublist){
  
  #these subjects only completed 1 session, so we aren't combining multiple files.
  if(sub <= 3 | sub == 10 | sub == 19){
    fname <- paste0(datapath, sprintf('/wmConfidence_S%02d_allData_preprocessed.csv', sub))
    df <- read.csv(fname, header = T, as.is = T, sep = ',')
    
    if('X' %in% colnames(df)){df <- df %>% dplyr::select(-starts_with('X'))}
    outpath <- sprintf('%s/data/datafiles/s%02d/wmConfidence_S%02d_gathered_preprocessed.csv', dir, sub, sub)
    write.csv(df, file = outpath, sep = ',', eol = '\n', dec = '.', col.names = T, row.names = F)
  }
  
  #all remaining subjects had two sessions of data, so we need to load in 2 datafiles and collate them
  if(sub>3 & sub != 10 & sub != 19) { #only two parts to subjects after subject 3, so only need to concatenate two files from sub4 onwards
    parts <- c('a', 'b')
    fname1 <- paste0(datapath, sprintf('/wmConfidence_S%02d%s_allData_preprocessed.csv', sub, parts[1]))
    fname2 <- paste0(datapath, sprintf('/wmConfidence_S%02d%s_allData_preprocessed.csv', sub, parts[2]))
    
    df1 <- read.csv(fname1, header = T, as.is = T, sep = ',') #%>% dplyr::select(-starts_with('X'))
    df2 <- read.csv(fname2, header = T, as.is = T, sep = ',') #%>% dplyr::select(-starts_with('X'))
    
    
    if('X' %in% colnames(df1)){ df1 <- df1 %>% dplyr::select(-starts_with('X')) }
    if('X' %in% colnames(df2)){ df2 <- df2 %>% dplyr::select(-starts_with('X')) }
    
    
    df <- df1 %>% dplyr::bind_rows(df2)
    
    outpath <- sprintf('%s/data/datafiles/s%02d/wmConfidence_S%02d_gathered_preprocessed.csv',dir, sub, sub) #write out one file per subject that contains all data
    
    write.csv(df, file = outpath, sep = ',', eol = '\n', dec = '.', col.names = T, row.names = F)
  }
}

#now collate all subjects and parts into one csv file
dataFiles = list(NULL)
for(sub in sublist){
  fname <- sprintf('%s/data/datafiles/s%02d/wmConfidence_S%02d_gathered_preprocessed.csv',dir,sub,sub)
  df <- read.csv(fname, header = T, as.is=T, sep = ',') 
  if('X' %in% colnames(df)){
    df %<>% dplyr::select(-starts_with('X'))
  }
  #%>% select(-X)
  dataFiles[[sub]] <- df
}

data <- do.call('rbind', dataFiles)

#write out all the behavioural data per subject + session into one large dataframe to use in analysis.
outname <- sprintf('%s/data/datafiles/wmConfidence_BehaviouralData_All.csv', dir)
write.csv(data, file=outname, sep = ',', eol = '\n', dec = '.', col.names = T, row.names = F)
