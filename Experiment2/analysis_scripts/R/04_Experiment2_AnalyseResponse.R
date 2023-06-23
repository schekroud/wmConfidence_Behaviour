#import libraries
library(tidyverse) # general data manipulation & plotting
library(magrittr)  # allows use of more pipes
library(afex)      # anovas etc


#this just gives access to some more fonts when making figures
loadfonts = T
if(loadfonts){
  library(extrafont)
  font_import() #yes to this
  # font_import(paths='/home/sammirc/Desktop/fonts')
  fonts()
  loadfonts(device = "pdf");
  loadfonts(device = 'postscript')
}

#set theme for plots to standardise them and make cleaner
theme_set(theme_bw() +
            theme(
              strip.background = element_blank(),
              axis.text    = element_text(family = 'Source Sans Pro', colour = '#000000', size = 18),
              axis.title   = element_text(family = 'Source Sans Pro', colour = '#000000', size = 24),
              panel.border = element_rect(size = 1, color = '#000000'),
              legend.title = element_text(family = 'Source Sans Pro', colour = '#000000', size = 16),
              legend.text  = element_text(family = 'Source Sans Pro', colour = '#000000', size = 14),
              strip.text   = element_text(family = 'Source Sans Pro', colour = '#000000', size = 16) 
            ) 
)
cuedcol <- '#3182bd' #blue, use for cued
neutcol <- '#bdbdbd' #light-ish grey, use for neutral
diffcol <- '#B2DF8A' #for colouring of the bars of difference plots, third distinct colour
se <- function(x) sd(x)/sqrt(length(x))




wd <- '/Users/sammi/Desktop/Experiments/DPhil/wmConfidence'
wd <- 'C:/Users/sammirc/Desktop/phd/wmConfidence_Behaviour/Experiment2'
setwd(wd)

dpath <- paste0(wd, '/data') #path to folder with behavioural data
figpath <- paste0(wd, '/figures/behaviour')
fpath <- paste0(dpath, '/wmConfidence_BehaviouralData_All.csv')

#load in the data
df <- read.csv(fpath, header = T, as.is = T, sep = ',') # str(df) if you want to see the columns and values etc
nsubs <- length(unique(df$subid)) #get the number of subjects


#just quickly look at how many trials aren't clickresped/DTchecked
df %>%
  dplyr::select(subid, clickresp, DTcheck, cond) %>%
  group_by(subid, cond) %>%
  dplyr::mutate(clickresp = (clickresp -1)*-1) %>%
  dplyr::summarise_at(.vars = c('clickresp', 'DTcheck'), .funs = c('sum')) %>%
  as.data.frame() %>%
  dplyr::mutate(clickresp = ifelse(subid %in% c(1,2), (clickresp/320)*100, clickresp)) %>%
  dplyr::mutate(clickresp = ifelse(subid %in% c(3,10, 19), (clickresp/256)*100, clickresp)) %>%
  dplyr::mutate(clickresp = ifelse(subid >3 & subid != 10 & subid != 19, (clickresp/512)*100, clickresp)) %>%
  dplyr::mutate(clickresp = round(clickresp,1) )
# clickresp -- percentage of trials in condition where they didn't click to respond
# DTcheck   -- number of trials outside 2.5 SDs of the within condition mean

df %>%
  dplyr::group_by(subid) %>% count(.) %>% as.data.frame() #get trial numbers for each subject (before removing trials for outlying behaviours)

df %>% dplyr::group_by(subid) %>% count(.) %>% as.data.frame() %>% dplyr::filter(n != 512)
df %>% dplyr::group_by(subid) %>% count(.) %>% as.data.frame() %>% dplyr::filter(n == 512)
#this shows which subjects didn't do two full sessions (256 trials per full session)


#note:
#subjects 1 & 2 have 320 trials (1 session of 10 blocks)
#subject 3 has one session of 8 blocks (256 trials)
#subjects 10 and 19 only did one session of 8 blocks (256 trials) as they withdrew after the first session
#subject 23 was removed due to noise in EEG data arising from disengaging in the task.
#all other subjects have 2 sessions of 8 blocks (512 trials in total)

subs2use <- c(4, 5, 6, 7, 8, 9, 11, 12, 13, 14, 15, 16, 17, 18, 20, 21, 22, 24, 25, 26) #these are the final subjects to analyse

df %<>% dplyr::filter(subid %in% subs2use) %>% #keep only subjects safe to use based on clicking to confirm response (this is a failsafe in case it's not coded later on)
  dplyr::filter(clickresp == 1) %>%
  dplyr::filter(DTcheck == 0) #hard code this now

dim(df) # 9608 x 47 at this point



#visualise decision time distributions per subject and condition
df %>%
  ggplot(aes(x = DT, fill = cond)) +
  geom_histogram(aes(y = ..count../sum(..count..)),stat = 'bin', binwidth = .1) + #DT is in seconds, so .1 bin width is 100ms bins (this line does normalised bins rel to trial number)
  #geom_histogram(stat = 'bin', binwidth = .2) +
  scale_fill_manual(values = c('neutral' = neutcol, 'cued' = cuedcol)) +
  labs(x = 'Decision Time (s)') +
  facet_wrap(subid~cond, ncol = 6, scales= 'free')


#density plot of decision times (time taken to press space, after the probe appears) per subject (separate density curves for cue types)
df %>%
  ggplot(aes(x = DT, fill = cond)) +
  geom_density(alpha = .5, adjust = 2, outline.type = 'upper') + #adjust sets the kernel width of the gaussian smoothing
  scale_fill_manual(values = c('neutral' = neutcol, 'cued' = cuedcol)) +
  labs(x = 'Decision Time (s)') +
  facet_wrap(~subid, scales = 'free')


#get average decision time per condition and subject time (time taken to initiate response)
df.dt <- df %>%
  dplyr::group_by(subid, cond) %>%
  dplyr::summarise_at(.vars = 'DT', .funs = 'mean') #gets within subject, within condition averages


df.dt %>% #not changing df.dt - will use dframe for stats, just mutating in place for plotting
  dplyr::group_by(cond) %>%
  dplyr::summarise_at(.vars = 'DT', .funs = c('mean', 'se')) %>% #gets group average for conditions
  ggplot(aes(x = cond, y = mean, ymin = mean-se, ymax = mean+se, fill = cond)) +
  geom_bar(stat = 'identity', width = .7) +
  geom_errorbar(stat = 'identity', width = .2, size = 1) +
  #geom_point(inherit.aes=F, data = df.dt, aes(x = cond, y = DT), size = 1) +
  #geom_line(inherit.aes=F, data = df.dt, aes(x = cond,y = DT, group = subid), size = .5) +
  scale_fill_manual(values = c('neutral' = neutcol, 'cued' = cuedcol)) +
  labs(x = '', y = 'mean decision time (s)') +
  theme(legend.position = 'none')
ggsave(filename = paste0(figpath, '/DT_groupaverage_20subs.eps'), device = cairo_ps, dpi = 600, height = 9, width = 9)
ggsave(filename = paste0(figpath, '/DT_groupaverage_20subs.pdf'), device = cairo_pdf, dpi = 600, height = 9, width = 9)




library(circular)
wrap <- function(x) (x+180)%%360 - 180 #function to wrap data between +/- 90 degrees. keeps sign of the response (-ve leftwards, +ve rightwards)
wrap90 <- function(x) (x+90)%%180 - 90


#look at mean response error 

#get average absolute response error per condition per subject
df.mad <- df %>%
  dplyr::filter(subid %in% subs2use) %>%
  dplyr::filter(clickresp == 1) %>%
  dplyr::mutate(absrdif = abs(rdif)) %>%
  dplyr::group_by(subid, cond) %>%
  summarise_at(.vars = c('absrdif'), .funs = c('mean')) %>% as.data.frame()


#now plot mean absolute response error in subjects 
df.mad %>%
  dplyr::group_by(cond) %>%
  dplyr::summarise_at(.vars = c('absrdif'), .funs = c('mean', 'se')) %>%
  ggplot(aes(x = cond, y = mean, ymin = mean-se, ymax = mean+se, fill = cond)) +
  geom_bar(stat = 'identity', width = .7) +
  geom_errorbar(stat = 'identity', width = .2, size = 1) +
  scale_fill_manual(values = c('neutral' = neutcol, 'cued' = cuedcol)) +
  labs(x = '', y = 'error (degrees)') +
  coord_cartesian(ylim = c(5, 12)) +
  theme(legend.position = 'none')
ggsave(filename = paste0(figpath, '/MAD_groupaverage_20subs.eps'), device = cairo_ps, dpi = 600, height = 9, width = 9)
ggsave(filename = paste0(figpath, '/MAD_groupaverage_20subs.pdf'), device = cairo_pdf, dpi = 600, height = 9, width =9)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#get mean response variability (SD of the response error distribution) per subject and condition
df.acc <- df %>% 
  dplyr::filter(subid %in% subs2use) %>%
  dplyr::filter(clickresp == 1) %>%
  dplyr::mutate(rabsrdif = rad(abs(rdif))) %>%
  dplyr::group_by(subid, cond) %>%
  summarise_at(.vars = c('rabsrdif'), .funs = c(sd.circular)) %>% as.data.frame() %>%
  dplyr::mutate(acc = rabsrdif) %>% #get standard deviation of responses for condition
  dplyr::select(-rabsrdif) #remove duplicate column


#plot group average response variability
df.acc %>%
  dplyr::group_by(cond) %>%
  dplyr::summarise_at(.vars = 'acc', .funs = c('mean', 'se')) %>%
  ggplot(aes(x = cond, y = mean, ymin = mean-se, ymax = mean+se, fill = cond)) +
  geom_bar(stat = 'identity', width = .7) +
  geom_errorbar(stat = 'identity', width = .35, size = 1) +
  scale_fill_manual(values = c('neutral' = neutcol, 'cued' = cuedcol)) +
  labs(x = '', y = 'error variability (SD)') +
  coord_cartesian(ylim = c(0.1, 0.2)) +
  theme(legend.position = 'none')
ggsave(filename = paste0(figpath, '/errorvar_groupaverage_20subs.eps'), device = cairo_ps, dpi = 600, height = 8, width = 8)
ggsave(filename = paste0(figpath, '/errorvar_groupaverage_20subs.pdf'), device = cairo_pdf, dpi = 600, height = 8, width = 8)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
####statistical analyses below
library('afex')


#one-way anova on decision time (time taken to initiate response) by cue type
anova_dt  <- afex::aov_ez(id = 'subid', data = df.dt , 'DT' , within = 'cond')

#print anova table
nice(anova_dt, es = 'pes') #significant main effect of condition (cue) on decision time
# Anova Table (Type 3 tests)
# 
# Response: DT
# Effect    df  MSE         F pes p.value
# 1   cond 1, 19 0.01 66.32 *** .78  <.0001
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘+’ 0.1 ‘ ’ 1

#t-test to show direction of the difference
t.test(DT~cond, data = df.dt, paired = T) 
# Paired t-test
# 
# data:  DT by cond
# t = -8.1435, df = 19, p-value = 1.286e-07
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.3094271 -0.1828920
# sample estimates:
#   mean of the differences 
# -0.2461595 


#analysis of variance on response error, by cue type
anova_mad <- afex::aov_ez(id = 'subid', data = df.mad, 'absrdif', within = 'cond')

#print anova table
nice(anova_mad, es = 'pes') #signif main effect of condition (cue) on response error
# Anova Table (Type 3 tests)
# 
# Response: absrdif
# Effect    df  MSE         F pes p.value
# 1   cond 1, 19 0.79 16.64 *** .47   .0006
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘+’ 0.1 ‘ ’ 1

t.test(absrdif ~ cond, df.mad, paired = T) #t-test to show direction of the difference
# Paired t-test
# 
# data:  absrdif by cond
# t = -4.0798, df = 19, p-value = 0.0006385
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -1.7297401 -0.5567332
# sample estimates:
#   mean of the differences 
# -1.143237 


#one-way anova on response variability (SD of the error distribution)
anova_acc <- afex::aov_ez(id = 'subid', data = df.acc, 'acc', within = 'cond')

nice(anova_acc, es = 'pes') #print the anova table
# Anova Table (Type 3 tests)
# 
# Response: acc
# Effect    df  MSE      F pes p.value
# 1   cond 1, 19 0.00 5.46 * .22     .03
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘+’ 0.1 ‘ ’ 1

t.test(acc ~ cond, df.acc, paired = T) #t-test to show the direction of the effect
# Paired t-test
# 
# data:  acc by cond
# t = -2.3358, df = 19, p-value = 0.03062
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.034973558 -0.001916821
# sample estimates:
#   mean of the differences 
# -0.01844519 


