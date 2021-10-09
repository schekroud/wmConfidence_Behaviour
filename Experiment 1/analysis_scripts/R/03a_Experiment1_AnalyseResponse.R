#import libraries
library(tidyverse) # general data manipulation & plotting
library(magrittr)  # allows use of more pipes
library(afex)      # anovas etc


loadfonts = T
if(loadfonts){
  library(extrafont)
  font_import() #yes to this
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
              strip.text   = element_text(family = 'Source Sans Pro', colour = '#000000', size = 16),
              axis.line    = element_line(colour = '#000000', size = 1),
              axis.ticks   = element_line(colour = '#000000', size = 1),
              axis.ticks.length = unit(.25, 'cm')
            ) 
)
cuedcol <- '#3182bd' #blue, use for cued
neutcol <- '#bdbdbd' #light-ish grey, use for neutral
diffcol <- '#B2DF8A' #for colouring of the bars of difference plots, third distinct colour

se <- function(x) sd(x)/sqrt(length(x))


wd <- '/Users/sammi/Desktop/Experiments/DPhil/wmSelection'
setwd(wd)

dpath <- paste0(wd, '/data') #path to folder with behavioural data
figpath <- paste0(wd, '/figures')


fpath <- '/Users/sammi/Desktop/Experiments/DPhil/wmSelection/data/wmSelection_BehaviouralData_All_Preprocessed.csv'


#get my block of test data
df <- read.csv(fpath, header = T, as.is = T, sep = ',') %>% dplyr::select(-X) # str(df) if you want to see the columns and values etc
nsubs <- length(unique(df$subid))
subs2use <- c(1,2,3,4,5,6,7,9,10,11,13,14,15,16,17,18,19,20,21,22)

#just quickly look at how many trials aren't clickresped/DTchecked
df %>%
  dplyr::select(subid, clickresp, DTcheck, cond) %>%
  dplyr::group_by(subid, cond) %>%
  dplyr::mutate(clickresp = (clickresp -1)*-1) %>%
  dplyr::summarise_at(.vars = c('clickresp', 'DTcheck'), .funs = c('sum')) %>%
  as.data.frame(.) %>%
  dplyr::mutate(clickresp = round((clickresp/128)*100,1))
# clickresp -- percentage of trials in condition where they didn't click to respond
# DTcheck   -- number of trials outside 2.5 SDs of the within condition mean
# may need to replace some subjects on the basis of this, as some subjects are missing a lot of trials (like 30-40% get thrown out because of clickresp)

df %<>% dplyr::filter(subid %in% subs2use)


#this section won't change df in place, just do stuff to it without change
df %>%
  dplyr::filter(DTcheck == 0) %>% dplyr::filter(clickresp == 1) %>%
  ggplot(aes(x = DT, fill = cond)) +
  geom_histogram(aes(y = ..count../sum(..count..)),stat = 'bin', binwidth = .1) + #DT is measure in seconds, not milliseconds, so a .1 bin width is 100ms bins (this line does normalised bins rel to trial number)
  #geom_histogram(stat = 'bin', binwidth = .2) +
  scale_fill_manual(values = c('neutral' = neutcol, 'cued' = cuedcol)) +
  labs(x = 'Decision Time (s)') +
  facet_wrap(subid~cond, ncol = 6)
# ggsave(filename = paste0(figpath, '/DT_hist_11subs_pilot.pdf'),
#        dpi = 600, height = 10, width = 10)

#density plot of decision times (time taken to press space, after the probe appears)
df %>%
  dplyr::filter(DTcheck ==0) %>% dplyr::filter(clickresp == 1) %>%
  ggplot(aes(x = DT, fill = cond)) +
  geom_density(alpha = .5, adjust = 2) + #adjust sets the kernel width of the gaussian smoothing
  scale_fill_manual(values = c('neutral' = neutcol, 'cued' = cuedcol)) +
  labs(x = 'Decision Time (s)') +
  facet_wrap(~subid, scales = 'fixed')
# ggsave(filename = paste0(figpath, '/DT_density_13subs_pilot.pdf'),dpi = 600, height = 10, width = 10)
# ggsave(filename = paste0(figpath, '/DT_density_13subs_pilot.eps'),dpi = 600, height = 10, width = 10)

df.dt <- df %>%
  dplyr::filter(clickresp ==1) %>% dplyr::filter(DTcheck ==0) %>%
  dplyr::group_by(subid, cond) %>%
  dplyr::summarise_at(.vars = 'DT', .funs = 'mean') #gets within subject, within condition averages

df.dt.diffs <- df.dt %>% 
  dplyr::group_by(subid) %>%
  dplyr::summarise(diff = last(DT)- first(DT)) #neutral - cued decision time (positive means lower for cued)

df.dt.diffs %>%
  dplyr::summarise_at(.vars = 'diff', .funs = c('mean', 'se')) %>%
  ggplot(aes(x = 1, y = mean, ymin = mean-se, ymax = mean + se)) +
  geom_bar(stat = 'identity', width = .4, fill = neutcol) + 
  geom_errorbar(stat = 'identity', width = .15, size = 1) +
  geom_point(data = df.dt.diffs, aes(x = 1, y = diff), inherit.aes = F,size = .5, position = position_jitter(.01)) + 
  geom_hline(yintercept = 0, linetype = 'dashed', color = '#000000', size = .2) +
  labs(x = '', y = 'difference in DT between\nneutral and cued condition (s)') +
  xlim(c(0.5,1.5)) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
ggsave(filename = paste0(figpath, '/DT_groupaverage_diffs.eps'), device = cairo_ps, dpi = 600, height = 5, width = 5)
ggsave(filename = paste0(figpath, '/DT_groupaverage_diffs.pdf'), device = cairo_pdf, dpi = 600, height = 5, width = 5)


df.dt %>% #not changing in place (will use dframe for stats, mutating in place for plotting)
  dplyr::group_by(cond) %>%
  dplyr::summarise_at(.vars = 'DT', .funs = c('mean', 'se')) %>% #gets group average for conditions
  ggplot(aes(x = cond, y = mean, ymin = mean-se, ymax = mean+se, fill = cond)) +
  geom_bar(stat = 'identity', width = .7) +
  geom_errorbar(stat = 'identity', width = .3, size = 1) +
  # geom_point(inherit.aes=F, data = df.dt, aes(x = cond, y = DT), size = 1) +
  # geom_line(inherit.aes=F, data = df.dt, aes(x = cond,y = DT, group = subid), size = .5) +
  scale_fill_manual(values = c('neutral' = neutcol, 'cued' = cuedcol)) +
  labs(x = '', y = 'mean decision time (s)') +
  theme(legend.position = 'none')
ggsave(filename = paste0(figpath, '/DT_groupaverage.eps'), device = cairo_ps, dpi = 600, height = 5, width = 5)
ggsave(filename = paste0(figpath, '/DT_groupaverage.pdf'), device = cairo_pdf, dpi = 600, height = 5, width = 5)

df.dt %>% #not changing in place
  dplyr::group_by(cond) %>%
  dplyr::summarise_at(.vars = 'DT', .funs = c('mean', 'se'))
# A tibble: 2 x 3
# cond    mean    se
# <chr>   <dbl>   <dbl>
# cued    0.678   0.0630
# neutral 0.922   0.0857

library(circular)
wrap <- function(x) (x+180)%%360 - 180 #function to wrap data between +/- 90 degrees. keeps sign of the response (-ve leftwards, +ve rightwards)
wrap90 <- function(x) (x+90)%%180 - 90

df %>%
  dplyr::filter(DTcheck == 0) %>%
  dplyr::filter(clickresp == 1) %>%
  ggplot(aes(x = rdif, fill = cond)) +
  geom_histogram(stat = 'bin', binwidth = 3) +
  geom_vline(aes(xintercept = 0), linetype = 'dashed', color = '#000000') +
  scale_fill_manual(values = c('neutral' = neutcol, 'cued' = cuedcol)) +
  labs(x = 'response deviation (degrees)') +
  facet_wrap(cond ~ subid, nrow = 2, ncol = nsubs)

#look at responses relative to non-target orientation
df %>%
  dplyr::filter(subid %in% subs2use) %>%
  dplyr::filter(DTcheck == 0) %>%
  dplyr::filter(clickresp == 1) %>%
  dplyr::mutate(NTresp = wrap90(resp-nontargori)) %>%
  ggplot(aes(x = NTresp, fill = cond)) +
  #geom_density(inherit.aes = T, adjust = 1.5, alpha = .3) +
  geom_histogram(stat = 'bin', binwidth = 5) +
  geom_vline(aes(xintercept = 0), linetype = 'dashed', color = '#000000') +
  scale_fill_manual(values = c('neutral' = neutcol, 'cued' = cuedcol)) +
  labs(x = 'response deviation to non target ori (degrees)') +
  facet_wrap(subid~cond, nrow = 2, ncol = length(subs2use))


#density plot showing distribution of errors relative to target orientation, for both conditions
df %>%
  dplyr::filter(subid %in% subs2use) %>%
  dplyr::filter(clickresp == 1) %>%
  dplyr::filter(DTcheck == 0) %>% #only take trials where DT within 2 sd's of the mean (throw out outliers)
  ggplot(aes(x = rdif, fill = cond)) +
  geom_density(alpha = .4, adjust = 1.5) +
  geom_vline(xintercept = 0, linetype = 'dashed', color = '#000000', size = .2) +
  scale_fill_manual(values = c('neutral' = neutcol, 'cued' = cuedcol)) +
  labs(x = 'response deviation (degrees)') +
  facet_wrap(~subid,  nrow = 5, ncol = 4)
ggsave(filename = paste0(figpath, '/rdif_density_facetSubjects.eps'), device = cairo_ps, dpi = 600, height = 10, width = 10)
ggsave(filename = paste0(figpath, '/rdif_density_facetSubjects.pdf'), device = cairo_pdf, dpi = 600, height = 10, width = 10)


#look at mean abs dev & accuracy (1/SD) measures too

df.mad <- df %>%
  dplyr::filter(subid %in% subs2use) %>%
  dplyr::filter(clickresp == 1) %>%
  dplyr::mutate(absrdif = abs(rdif)) %>%
  dplyr::group_by(subid, cond) %>%
  summarise_at(.vars = c('absrdif'), .funs = c('mean')) %>% as.data.frame()

df.mad.diffs <- df.mad %>%
  dplyr::group_by(subid) %>%
  dplyr::summarise(diff = last(absrdif) - first(absrdif)) #neutral - cued (negative values where cued MAD worse than neutral MAD)

df.mad.diffs %>%
  dplyr::summarise_at(.vars = 'diff', .funs = c('mean', 'se')) %>%
  ggplot(aes(x = 1, y = mean, ymin = mean-se, ymax = mean + se)) +
  geom_bar(stat = 'identity', width = .4, fill = neutcol) + 
  geom_errorbar(stat = 'identity', width = .1, size = 1) +
  geom_point(data = df.mad.diffs, aes(x = 1, y = diff), inherit.aes = F, size = 1, position = position_jitter(.01)) + 
  geom_hline(yintercept = 0, linetype = 'dashed', color = '#000000', size = 1) +
  labs(x = '', y = 'difference in MAD between\nneutral and cued condition (degrees)') +
  xlim(c(0.5,1.5)) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
ggsave(filename = paste0(figpath, '/MAD_groupaverage_diffs.eps'), device = cairo_ps, dpi = 600, height = 5, width = 5)
ggsave(filename = paste0(figpath, '/MAD_groupaverage_diffs.pdf'), device = cairo_pdf, dpi = 600, height = 5, width = 5)


df.mad %>%
  dplyr::group_by(cond) %>%
  dplyr::summarise_at(.vars = c('absrdif'), .funs = c('mean', 'se')) %>%
  ggplot(aes(x = cond, y = mean, ymin = mean-se, ymax = mean+se, fill = cond)) +
  geom_bar(stat = 'identity', width = .7) +
  geom_errorbar(stat = 'identity', position = position_dodge(), width = 0.3, size = 1) +
  geom_point(inherit.aes=F, data = df.mad, aes(x = cond, y = absrdif), size = 1) +
  geom_line(inherit.aes=F, data = df.mad, aes(x = cond,y = absrdif, group = subid), size = .5) +
  scale_fill_manual(values = c('neutral' = neutcol, 'cued' = cuedcol)) +
  labs(x = '', y = '(mean) mean absolute deviation (degrees)') +
  theme(legend.position = 'none')
ggsave(filename = paste0(figpath, '/MAD_groupaverage.eps'), device = cairo_ps,  dpi = 600, height = 5, width = 5)
ggsave(filename = paste0(figpath, '/MAD_groupaverage.pdf'), device = cairo_pdf, dpi = 600, height = 5, width = 5)

df.mad %>%
  dplyr::group_by(cond) %>%
  dplyr::summarise_at(.vars = c('absrdif'), .funs = c('mean', 'se'))
# A tibble: 2 x 3
# cond      mean    se
# <chr>     <dbl>   <dbl>
# cued      10.3    0.504
# neutral   11.5    0.493

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

df.acc <- df %>% 
  dplyr::filter(subid %in% subs2use) %>%
  dplyr::filter(clickresp == 1) %>%
  dplyr::mutate(rabsrdif = rad(abs(rdif))) %>%
  dplyr::group_by(subid, cond) %>%
  summarise_at(.vars = c('rabsrdif'), .funs = c(sd.circular)) %>% as.data.frame() %>%
  dplyr::mutate(acc = rabsrdif) %>% #get standard deviation of responses for condition
  dplyr::select(-rabsrdif) #get rid of radians of absolute error


#plot response variability
#plot as a single difference value rather than separate bars
df.acc.diffs <- df.acc %>%
  dplyr::group_by(subid) %>%
  dplyr::summarise(diff = last(acc) - first(acc)) #neutral - cued (negative values where cued MAD worse than neutral MAD)

df.acc.diffs %>%
  dplyr::summarise_at(.vars = 'diff', .funs = c('mean', 'se')) %>%
  ggplot(aes(x = 1, y = mean, ymin = mean-se, ymax = mean + se)) +
  geom_bar(stat = 'identity', width = .4, fill = neutcol) + 
  geom_errorbar(stat = 'identity', width = .1, size = 1) +
  geom_point(data = df.acc.diffs, aes(x = 1, y = diff), inherit.aes = F,size = 1, position = position_jitter(0.02)) + 
  geom_hline(yintercept = 0, linetype = 'dashed', color = '#000000', size = 1) +
  labs(x = '', y = 'difference in response variability (SD)\nbetween neutral and cued condition ') +
  xlim(c(0.5,1.5)) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
ggsave(filename = paste0(figpath, '/acc_groupaverage_diffs.eps'), device = cairo_ps, dpi = 600,  height = 5, width = 5)
ggsave(filename = paste0(figpath, '/acc_groupaverage_diffs.pdf'), device = cairo_pdf, dpi = 600, height = 5, width = 5)



#plot group average
df.acc %>%
  dplyr::group_by(cond) %>%
  dplyr::summarise_at(.vars = 'acc', .funs = c('mean', 'se')) %>%
  ggplot(aes(x = cond, y = mean, ymin = mean-se, ymax = mean+se, fill = cond)) +
  geom_bar(stat = 'identity', width = .7) +
  geom_errorbar(stat = 'identity', width = .3, size = 1) +
  # geom_point(inherit.aes=F, data = df.acc, aes(x = cond, y = acc), size = 1) +
  # geom_line(inherit.aes=F, data = df.acc, aes(x = cond,y = acc, group = subid), size = .5) +
  scale_fill_manual(values = c('neutral' = neutcol, 'cued' = cuedcol)) +
  labs(x = '', y = 'group mean response variability (SD)') +
  theme(legend.position = 'none')
ggsave(filename = paste0(figpath, '/acc_groupaverage.eps'), device = cairo_ps,  dpi = 600, height = 5, width = 5)
ggsave(filename = paste0(figpath, '/acc_groupaverage.pdf'), device = cairo_pdf, dpi = 600, height = 5, width = 5)

df.acc %>%
  dplyr::group_by(cond) %>%
  dplyr::summarise_at(.vars = 'acc', .funs = c('mean', 'se')) 
# # A tibble: 2 x 3
# cond    mean    se
# <chr>   <dbl>   <dbl>
# cued    0.167   0.00937
# neutral 0.187   0.0103 

#one-way anova on decision time
anova_dt  <- afex::aov_ez(id = 'subid', data = df.dt , 'DT' , within = 'cond')
nice(anova_dt, es = 'pes') #significant main effect of condition (cue) on decision time
# Anova Table (Type 3 tests)
# Response: DT
# Effect    df  MSE         F  pes p.value
# 1   cond 1, 19 0.03 21.41 *** .530   <.001
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '+' 0.1 ' ' 1
t.test(DT~cond, data = df.dt, paired = T) #mean diff output here
# Paired t-test
# 
# data:  DT by cond
# t = -4.6269, df = 19, p-value = 0.0001839
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.3541692 -0.1335459
# sample estimates:
#   mean of the differences 
# -0.2438576 


anova_mad <- afex::aov_ez(id = 'subid', data = df.mad, 'absrdif', within = 'cond')
nice(anova_mad, es = 'pes') #signif main effect of condition (cue) on mean absolute deviation
# Anova Table (Type 3 tests)
# Response: absrdif
# Effect    df  MSE        F  pes p.value
# 1   cond 1, 19 1.34 10.66 ** .359    .004
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '+' 0.1 ' ' 1

t.test(absrdif ~ cond, df.mad, paired = T)
# Paired t-test
# 
# data:  absrdif by cond
# t = -3.2646, df = 19, p-value = 0.004078
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -1.9638249 -0.4294252
# sample estimates:
#   mean of the differences 
# -1.196625 

#one-way anova on accuracy(sd measure)
anova_acc <- afex::aov_ez(id = 'subid', data = df.acc, 'acc', within = 'cond')
nice(anova_acc, es = 'pes') 
# Anova Table (Type 3 tests)
# Response: acc
# Effect    df  MSE      F  pes p.value
# 1   cond 1, 19 0.00 4.64 * .196    .044
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '+' 0.1 ' ' 1

t.test(acc ~ cond, df.acc, paired = T)
# Paired t-test
# 
# data:  acc by cond
# t = -2.1534, df = 19, p-value = 0.04434
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.0378021838 -0.0005378227
# sample estimates:
#   mean of the differences 
# -0.01917