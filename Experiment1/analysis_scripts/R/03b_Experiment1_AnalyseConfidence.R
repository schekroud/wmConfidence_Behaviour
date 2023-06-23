#import libraries
library(tidyverse) # general data manipulation & plotting
library(magrittr)  # allows use of more pipes
library(afex)      # anovas etc
#library(RePsychLing)
library(MASS)

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
wd <- 'C:/Users/sammirc/Desktop/phd/wmConfidence_Behaviour/Experiment1'
setwd(wd)

dpath <- paste0(wd, '/data') #path to folder with behavioural data
figpath <- paste0(wd, '/figures')

#load in the data
fpath <- paste0(dpath, '/wmSelection_BehaviouralData_All_Preprocessed.csv')


df <- read.csv(fpath, header = T, as.is = T, sep = ',') #%>% dplyr::select(-X) # str(df) if you want to see the columns and values etc
nsubs = length(unique(df$subid))
subs2use <- c(1,2,3,4,5,6,7,9,10,11,13,14,15,16,17,18,19,20,21,22)


df %>%
  dplyr::select(subid, confclicked, cond) %>%
  group_by(subid, cond) %>%
  dplyr::mutate(confclicked = (confclicked -1)*-1) %>%
  dplyr::summarise_at(.vars = c('confclicked'), .funs = c('sum')) %>%
  as.data.frame() %>%
  dplyr::mutate(clickresp = round((confclicked/128)*100,1))


df %<>% dplyr::filter(subid %in% subs2use) %>% #keep only the subjects from the final sample to analyse
  dplyr::filter(clickresp == 1) %>% #only keep trials where they clicked to confirm response
  dplyr::filter(confclicked == 1) %>% #and where they clicked to confirm their confidence
  dplyr::filter(DTcheck == 0) # and their decision times were not outliers (outside 2 SDs of mean per subject and condition)

dim(df) #output dimensions of the dataframe

#this just shows the spread, per subject, of confidence widths (i.e. the angles people reported across trials)
df %>%
  ggplot(aes(x = confwidth, fill = cond)) +
  geom_density(alpha = .5, adjust = 1) +
  scale_fill_manual(values = c('neutral' = neutcol, 'cued' = cuedcol)) +
  labs(x = 'confidence report width') +
  facet_wrap(~subid)



#get the average reported confidence width , per condition, per subject
df.confdist <- df %>%
  dplyr::group_by(subid, cond) %>%
  dplyr::summarise_at(.vars = 'confwidth', .funs=c('mean', 'sd')) %>%
  ungroup() %>% as.data.frame(.) %>%
  dplyr::mutate(cond = as.factor(cond))


#visualise grand-average reported confidence width, for neutral and cued trials 
df.confdist %>%
  dplyr::group_by(cond) %>% dplyr::summarise_at(.vars = 'mean', .funs = c('se', 'mean')) %>%
  ggplot(aes(x = cond, fill = cond, y = mean, ymin = mean - se, ymax = mean + se)) +
  geom_bar(stat = 'identity', width = .7, position = position_dodge(.6) ) + 
  scale_fill_manual(values = c('neutral' = neutcol, 'cued' = cuedcol)) +
  geom_errorbar(position = position_dodge(.6), width = .3, size = 1) +
  #geom_point(inherit.aes = F, data = df.confdist, aes(x = cond, y = mean), size = 1) +
  #geom_line(inherit.aes = F, data = df.confdist, aes(x = cond, y = mean, group = subid), size = .5) +
  labs(x = '', y = 'average confidence width (degrees)') + theme(legend.position = 'none') +
  coord_cartesian(ylim = c(7, 13))
ggsave(filename = paste0(figpath, '/gave_confwidth_mean.eps'), device = cairo_ps,  dpi = 600, height = 5, width = 5)
ggsave(filename = paste0(figpath, '/gave_confwidth_mean.pdf'), device = cairo_pdf, dpi = 600, height = 5, width = 5)

#analysis of variance to look at differences in raw confidence width as a function of cue
anova_confwidth <- afex::aov_ez(id = 'subid', data = df.confdist, 'mean', within = 'cond')
nice(anova_confwidth, es = 'pes') # 
# Anova Table (Type 3 tests)
# Response: mean
# Effect    df  MSE    F  pes p.value
# 1   cond 1, 19 0.43 0.57 .029    .459
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '+' 0.1 ' ' 1


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

library(circular)

#get the average (absolute) confidence error per subject and condition
df.confmu <- df %>%
  dplyr::mutate(cond = as.factor(cond)) %>%
  dplyr::mutate(absconfdiff = abs(confdiff)) %>% #need this to get mean absolute confidence error
  dplyr::group_by(subid, cond) %>%
  dplyr::summarise_at(.vars = 'absconfdiff', .funs = mean)

df.confmu.separate <- df %>%
  dplyr::mutate(cond = as.factor(cond)) %>%
  dplyr::mutate(targinconf = ifelse(confdiff <= 0, 'Target Inside', 'Target Outside')) %>%
  dplyr::mutate(targinconf = as.factor(targinconf)) %>%
  dplyr::mutate(absconfdiff = abs(confdiff)) %>% #need this to get mean absolute confidence error
  dplyr::group_by(subid, cond, targinconf) %>%
  dplyr::summarise_at(.vars = 'absconfdiff', .funs = mean)


#visualise mean (absolute) confidence error
df.confmu %>%
  dplyr::group_by(cond) %>%
  dplyr::summarise_at(.vars = 'absconfdiff', .funs = c('mean', 'se')) %>% #average across subjects
  ggplot(aes(x = cond, y = mean, ymin = mean-se, ymax = mean+se, fill = cond)) +
  geom_bar(stat = 'identity', width = .7) +
  geom_errorbar(stat = 'identity', width = .3, size = 1) +
  #geom_point(inherit.aes = F, data = df.confmu, aes(x = cond, y = absconfdiff), size = .5) +
  #geom_line(inherit.aes = F, data = df.confmu, aes(x = cond, y = absconfdiff, group = subid), size = .1) +
  scale_fill_manual(values = c('neutral' = neutcol, 'cued' = cuedcol)) +
  labs(x = '', y = '(group) mean confidence error') +
  theme(legend.position = 'none')
ggsave(filename = paste0(figpath, '/gave_conferr.eps'), device = cairo_ps,  dpi = 600, height = 5, width = 5)
ggsave(filename = paste0(figpath, '/gave_conferr.pdf'), device = cairo_pdf, dpi = 600, height = 5, width = 5)


#get values for average per condition, across subjects
df.confmu %>%
  dplyr::group_by(cond) %>%
  dplyr::summarise_at(.vars = 'absconfdiff', .funs = c('mean', 'se')) 
# # A tibble: 2 x 3
# cond     mean  se
# <fct>    <dbl> <dbl>
# cued     7.16  0.313
# neutral  8.16  0.396


#analysis of variance on mean absolute confidence width, across cue types
anova_confmu <- afex::aov_ez(id = 'subid', data = df.confmu, 'absconfdiff', within = c('cond'))

#print anova table
nice(anova_confmu, es = 'pes') #significant main effect of cue (i.e. significant condition difference in confmu)
# Anova Table (Type 3 tests)
# Response: absconfdiff
# Effect    df  MSE         F  pes p.value
# 1   cond 1, 19 0.39 25.54 *** .573   <.001
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '+' 0.1 ' ' 1

#t-test between neutral and cued to show direction of the difference
t.test(data = df.confmu, absconfdiff ~ cond, paired = TRUE)
# Paired t-test
# 
# data:  absconfdiff by cond
# t = -5.0541, df = 19, p-value = 7.046e-05
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -1.4155171 -0.5864565
# sample estimates:
#   mean of the differences 
# -1.000987 

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#linear mixed effects model to investigate the relationship between error and confidence, and how it may be influenced by attention


lmm.data <- df %>%
  dplyr::filter(subid != 12 & subid != 8) %>% droplevels(.) %>%
  dplyr::mutate(confwidth = rad(confwidth), absrdif = rad(absrdif)) %>% #get confidence width and error as radians
  dplyr::filter(clickresp == 1, DTcheck == 0, confclicked == 1) %>% #just to be sure we exclude the right trials
  dplyr::mutate(subid = as.factor(subid)) %>% #coerce some things to factors
  dplyr::mutate(cond = as.factor(cond)) %>%
  dplyr::mutate(cond = relevel(cond, ref = 'neutral'))


contrasts(lmm.data$cond) <- contr.sum(2) #specify a contrast between neutral and cued

# are my DV normally distr
# determine lambda for confwidth
# temp fix for 0 values in the confwidth
#lmm.data$confwidth[lmm.data$confwidth==0] <- 0.0001
lmm.data$confwidth[lmm.data$confwidth == 0] <- 0.1
lambdaList <- boxcox(confwidth~cond, data=lmm.data)
(lambda <- lambdaList$x[which.max(lambdaList$y)])

# - - - - - - - - - - - - - - - - - - - - - - 
fullmodel_log <- lme4::lmer(data = lmm.data, log(confwidth) ~ absrdif+ cond + absrdif:cond +  (1 + absrdif + cond + absrdif:cond|subid))
minmodel_log  <- lme4::lmer(data = lmm.data, log(confwidth) ~ absrdif + cond + absrdif:cond  + (1|subid))

summary(lme4::rePCA(fullmodel_log))#check if model is degenerate (spoiler: it is!)

anova(fullmodel_log, minmodel_log) #full model would be chosen (although the random effects structure is degenerate)

model2_log <- lmerTest::lmer(data = lmm.data, log(confwidth) ~ absrdif+ cond + absrdif:cond +  (1 + absrdif + cond|subid))
summary(lme4::rePCA(model2_log))#check if model is degenerate (spoiler: it is!)

model3_log <- lmerTest::lmer(data = lmm.data, log(confwidth) ~ absrdif+ cond + absrdif:cond +  (1 + absrdif|subid))
summary(lme4::rePCA(model3_log))#check if model is degenerate (seems to be good)

anova(fullmodel_log, model3_log) #rejects the full model over model3_log lmm
anova(minmodel_log, model3_log) #and chooses model3_log over the minimal model


#print out the results table for the chosen model.
summary(model3_log) 
# Fixed effects:
#                 Estimate    Std. Error  df            t value   Pr(>|t|)    
# (Intercept)     -1.90678    0.08483     19.06870      -22.478   3.47e-15 ***
# absrdif          0.65120    0.09421     19.96390      6.913     1.04e-06 ***
# cond1            0.03356    0.01115     4506.61354    3.009     0.00263 ** 
# absrdif:cond1   -0.13591    0.04347     4469.88549    -3.127    0.00178 ** 
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


library(remef)
fit <- keepef(model3_log, fix = 'absrdif:cond1', grouping = TRUE)

lmm.data$fitted <- fit


#visualise the relationship between error and fitted confidence
lmm.data %>%
  dplyr::mutate(cond = ifelse(cond=='neutral', 'Neutral', 'Cued')) %>%
  ggplot(aes(x = absrdif, y = exp(fitted))) +
  # geom_point(size = 1, aes(colour = cond)) + #this is a lot of data points and can take a long time, and make the files massive
  geom_ribbon(stat = 'smooth', method = 'lm', aes(fill = cond)) +
  geom_line(stat = 'smooth', method = 'lm', aes(colour=cond), size = 1) +
  scale_fill_manual(values = c('Neutral' = '#d9d9d9', 'Cued' = '#d9d9d9')) +
  scale_color_manual('Condition', values = c('Neutral' = neutcol, 'Cued' = cuedcol)) +
  labs(x = 'absolute deviation of response from target (radians)',
       y = 'value of confidence interval width from the model (radians)') +
  geom_abline(intercept = 0, slope = 1, linetype = 'dashed', color = '#636363', size = 1) +
  coord_cartesian(xlim = c(0, 1.6), ylim = c(0,0.75)) + theme(legend.position = 'top')
ggsave(filename = paste0(figpath, '/absrdif~confwidth_fitted.eps'), device = cairo_ps, dpi = 600, height = 7, width = 7)
ggsave(filename = paste0(figpath, '/absrdif~confwidth_fitted.pdf'), device = cairo_pdf, dpi = 600, height = 7, width = 7)

