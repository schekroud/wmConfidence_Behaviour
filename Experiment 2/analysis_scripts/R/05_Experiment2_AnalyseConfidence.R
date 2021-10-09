#import libraries
library(tidyverse) # general data manipulation & plotting
library(magrittr)  # allows use of more pipes
library(afex)      # anovas etc
library(MASS)


#this just loads in some extra fonts for figure making
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
              axis.title   = element_text(family = 'Source Sans Pro', colour = '#000000', size = 18),
              panel.border = element_rect(size = 2, color = '#000000'),
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
setwd(wd)

dpath <- paste0(wd, '/data') #path to folder with behavioural data
figpath <- paste0(wd, '/figures/behaviour')

#get behavioural data
fpath <- paste0(dpath, '/datafiles/wmConfidence_BehaviouralData_All.csv')



df <- read.csv(fpath, header = T, as.is = T, sep = ',') 
nsubs <- length(unique(df$subid))


df %>%
  dplyr::select(subid, confclicked, cond) %>%
  group_by(subid, cond) %>%
  dplyr::mutate(confclicked = (confclicked -1)*-1) %>%
  dplyr::summarise_at(.vars = c('confclicked'), .funs = c('sum')) %>%
  as.data.frame() %>%
  dplyr::mutate(confclicked = ifelse(subid %in% c(1,2), (confclicked/160)*100, confclicked)) %>%
  dplyr::mutate(confclicked = ifelse(subid == 3, (confclicked/128)*100, confclicked)) %>%
  dplyr::mutate(confclicked = ifelse(subid > 3, (confclicked/256)*100, confclicked)) %>%
  dplyr::mutate(confclicked = round(confclicked,1) )
#confclicked is the percentage of trials, within condition, where the participant did not click to confirm their confidence response

subs2use <- c(4, 5, 6, 7, 8, 9, 11, 12, 13, 14, 15, 16, 17, 18, 20, 21, 22, 24, 25, 26) #these subs are included in eeg analyses, so just work with these for behavioural analyses too
#these subs are included in eeg analyses, so just work with these for behavioural analyses too


df %<>% dplyr::filter(subid %in% subs2use) %>% #keep only subjects safe to use based on clicking to confirm response
  dplyr::filter(clickresp == 1) %>% #clicked to confirm orientation response
  dplyr::filter(confclicked == 1) %>% #and clicked to confirm their confidence
  dplyr::filter(DTcheck == 0) #and exclude trials that were outliers in response time (over 2.5 SDs from with within subject/condition mean)


dim(df) #get output dataframe dimensions


#visualise raw reported confidence width here
df %>%
  dplyr::group_by(subid, cond) %>% summarise_at(.vars = 'confwidth', .funs = c('mean', 'sd')) %>% #get mean confidence per subject per condition
  dplyr::group_by(cond) %>% summarise_at(.vars = c('mean', 'sd'), .funs = c('mean', 'se')) %>% #average across subjects, to get grand-average confidence per condition
  ggplot(aes(x = cond, y = mean_mean, fill = cond)) + #and lets just plot it
  scale_fill_manual(values = c('neutral' = neutcol, 'cued' = cuedcol)) +
  geom_bar(stat = 'identity', width = .7) +
  geom_errorbar(aes(ymin = mean_mean - mean_se, ymax = mean_mean + mean_se), width = .35, size=1, color = '#000000') +
  labs(y = 'confidence width (degrees)', x = 'cue condition') + 
  coord_cartesian(ylim = c(12,20)) +
  theme(legend.position = 'none')
ggsave(filename = paste0(figpath, '/gave_confwidthmean.pdf'), device = cairo_pdf, dpi = 600, height = 9, width = 9)
ggsave(filename = paste0(figpath, '/gave_confwidthmean.eps'), device = cairo_ps, dpi = 600, height = 9, width = 9)

# get average reported confidence width
df.cwmean <- df %>%
  dplyr::group_by(subid, cond) %>%
  summarise_at(.vars = 'confwidth', .funs = c('mean'))

#analysis of variance on reported confidence width
aov.cwmean <- afex::aov_ez(id = 'subid', data = df.cwmean, 'confwidth', within = 'cond')
nice(aov.cwmean, es = 'pes') #print anova table
aov.cwmean$Anova #get exact values if you want to see them

# Anova Table (Type 3 tests)
# 
# Response: confwidth
# Effect    df  MSE         F pes p.value
# 1   cond 1, 19 0.37 17.84 *** .48   .0005
# ---


t.test(data = df.cwmean, confwidth ~ cond, paired = T) #t-test to show the direction of the difference

# Paired t-test
# 
# data:  confwidth by cond
# t = -4.2241, df = 19, p-value = 0.0004593
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -1.223208 -0.412652
# sample estimates:
#   mean of the differences 
# -0.8179302 

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
library(circular)


# get the average (absolute) confidence error per subject and condition
df.confdiffmu <- df %>%
  dplyr::mutate(cond = as.factor(cond)) %>%
  dplyr::mutate(absconfdiff = abs(confdiff)) %>% #need this to getabsolute confidence error
  dplyr::group_by(subid, cond) %>%
  dplyr::summarise_at(.vars = 'absconfdiff', .funs = mean)


#visualise grand-average (absolute) confidence error for neutral and cued trials
df.confdiffmu %>%
  dplyr::group_by(cond) %>%
  dplyr::summarise_at(.vars = 'absconfdiff', .funs = c('mean', 'se')) %>%
  ggplot(aes(x = cond, y = mean, ymin = mean-se, ymax = mean+se, fill = cond)) +
  geom_bar(stat = 'identity', width = .7) +
  geom_errorbar(stat = 'identity', width = .35, size = 1) +
  scale_fill_manual(values = c('neutral' = neutcol, 'cued' = cuedcol)) +
  labs(x = '', y = 'mean confidence error (degrees)') +
  coord_cartesian(ylim = c(8,13)) +
  theme(legend.position = 'none')
ggsave(filename = paste0(figpath, '/gave_confdiffmu.pdf'), device = cairo_pdf, dpi = 600, height = 9, width = 9)
ggsave(filename = paste0(figpath, '/gave_confdiffmu.eps'), device = cairo_ps,  dpi = 600, height = 9, width = 9)

#analysis of variance to look at differences in confidence error by cue type
anova_confdiffmu <- afex::aov_ez(id = 'subid', data = df.confdiffmu, 'absconfdiff', within = c('cond'))

#print the anova table
nice(anova_confdiffmu, es = 'pes') #significant main effect of cue (i.e. significant condition difference in confmu)
anova_confdiffmu$Anova #exact numbers if you want to see them

# Anova Table (Type 3 tests)
# 
# Response: absconfdiff
# Effect    df  MSE      F pes p.value
# 1   cond 1, 19 0.31 6.69 * .26     .02
# ---

t.test(data = df.confdiffmu, absconfdiff ~ cond, paired = TRUE) #t-test to show the direction of the difference
# Paired t-test
# 
# data:  absconfdiff by cond
# t = -2.5869, df = 19, p-value = 0.01808
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.82989088 -0.08758045
# sample estimates:
#   mean of the differences 
# -0.4587357 

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#linear mixed-effects models to look at the relationship between response error and confidence, and whether it is influenced by attention


#get the data to use for the LMM
lmm.data <- df %>%
  dplyr::mutate(confwidth = rad(confwidth), absrdif = rad(absrdif)) %>%
  dplyr::filter(clickresp == 1, DTcheck == 0, confclicked == 1) %>% #just to be sure we have the data we want
  dplyr::mutate(subid = as.factor(subid)) %>%
  dplyr::mutate(cond = as.factor(cond)) %>%
  dplyr::mutate(cond = relevel(cond, ref = 'neutral'))

#make the plot of *raw* data from here
lmm.data %>%
  ggplot(aes(x = absrdif, y = confwidth)) +
  geom_ribbon(stat = 'smooth', method = 'lm', aes(fill = cond)) +
  geom_line(  stat = 'smooth', method = 'lm', aes(colour = cond)) +
  geom_point(size = .5, aes(colour = cond)) +
  #geom_smooth(method = 'lm', size = .5) +
  scale_color_manual(values = c('neutral' = neutcol, 'cued' = cuedcol)) +
  scale_fill_manual(values = c('neutral' = '#d9d9d9', 'cued' = '#d9d9d9')) +
  geom_abline(intercept = 0, slope = 1, linetype = 'dashed', color = '#636363') +
  facet_wrap(~subid,  nrow = 5)



contrasts(lmm.data$cond) <- contr.sum(2) #specify a contrast to compare neutral and cued trials

# are my DV normally distributed
# determine lambda for confwidth
# temp fix for 0 values in the confwidth
lmm.data$confwidth[lmm.data$confwidth==0] <- 0.0001
lambdaList <- boxcox(confwidth~cond, data=lmm.data)
(lambda <- lambdaList$x[which.max(lambdaList$y)])


# - - - - - - - - - - - - - - - - - - - - - - 
library(lmerTest)

fullmodel_log <- lmerTest::lmer(data = lmm.data, log(confwidth) ~ absrdif + cond + absrdif:cond +
                                  (1 + absrdif + cond + absrdif:cond|subid))
#this model has a singular fit, suggesting is is overfitted/overparameterised

summary(rePCA(fullmodel_log))#check if model is degenerate
#cumulative proportion suggests only two random effects needed

model2 <- lmerTest::lmer(data = lmm.data, log(confwidth) ~ absrdif + cond + absrdif:cond + (1 +absrdif + cond|subid)) #removing the interaction from random effects
#this model also has singular fit
summary(rePCA(model2)) #the third random effect can be dropped from this
anova(fullmodel_log, model2) #model doesn't significantly degrade after  removing this interaction, so we should exclude it

model3 <- lmerTest::lmer(data = lmm.data, log(confwidth) ~ absrdif + cond + absrdif:cond + (1 + absrdif|subid)) #doesn't generate a singular fit (overfitted)
summary(rePCA(model3))
summary(model3)



#should i still compare model 3 (with absrdif in random effects) against the minimal model?
minmodel_log  <- lmerTest::lmer(data = lmm.data, log(confwidth) ~ absrdif + cond + absrdif:cond  + (1|subid)) #model only allowing across participant fitting of intercept
summary(rePCA(minmodel_log))
summary(minmodel_log)

anova(minmodel_log, model3) #model3 is better than minimal model log, and we need absrdif in the random effects structure.
#reporting from this model (model3) onwards


#print the coefficients table from this model
summary(model3) #this is the model that we will report on in the paper
# Fixed effects:
#               Estimate    Std. Error df         t value   Pr(>|t|)    
# (Intercept)   -1.367e+00  6.746e-02  1.901e+01  -20.266   2.47e-14 ***
# absrdif        5.740e-01  7.415e-02  1.816e+01   7.741    3.68e-07 ***
# cond1          2.220e-02  5.537e-03  9.493e+03   4.009    6.14e-05 ***
# absrdif:cond1 -5.266e-02  2.327e-02  9.469e+03  -2.263    0.0236 *  



library(remef)
fit <- keepef(model3, fix = 'absrdif:cond1', grouping = TRUE)

lmm.data$fitted <- fit
lmm.data %<>% dplyr::mutate(fitted = exp(fitted)) #exponent because we modelled the log of confidence with, need to exponent it to get true fitted value)

#visualise relationship between response error and fitted confidence width
lmm.data %>%
  dplyr::mutate(cond = ifelse(cond=='neutral', 'Neutral', 'Cued')) %>%
  ggplot(aes(x = absrdif, y = confwidth)) +
  geom_ribbon(inherit.aes = F, aes(x = absrdif, y = fitted, fill = cond), stat = 'smooth', method = 'lm') +
  geom_line(inherit.aes = F, aes(x = absrdif, y = fitted, color = cond), stat = 'smooth', method = 'lm', size = 1) +
  scale_fill_manual(values = c('Neutral' = '#d9d9d9', 'Cued' = '#d9d9d9'), name = NULL, labels = NULL, guide = "none") +
  scale_color_manual('Condition', values = c('Neutral' = neutcol, 'Cued' = cuedcol)) +
  labs(x = 'absolute reponse error (radians)',
       y = 'confidence interval width (radians)') +
  geom_abline(intercept = 0, slope = 1, linetype = 'dashed', color = '#636363') +
  coord_cartesian(xlim = c(0, 1.6), ylim = c(0,0.75)) + theme(legend.position = 'top')
ggsave(filename = paste0(figpath, '/absrdif~confwidth_fittedline_fromModel3.pdf'), device = cairo_pdf, dpi = 600, height = 10, width = 10)
ggsave(filename = paste0(figpath, '/absrdif~confwidth_fittedline_fromModel3.eps'), device = cairo_ps, dpi = 600, height = 10, width = 10)


#%%  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#is confidence updated across trials based on previous confidence performance (i.e. previous trial confidence errors)

#get the data for this analysis
lmm.trladjcw.data <- df %>%
  dplyr::mutate(prevtrlinconf = ifelse(prevtrlconfdiff <= 0, 1, 0)) %>%
  dplyr::mutate(prevtrlinconf = as.factor(prevtrlinconf)) %>%
  dplyr::mutate(trladj = confwidth - prevtrlcw) %>% # difference in current trials confidence compared to the previous trial
  dplyr::filter(trialnum != 1) %>% #exclude first trial of each session as no prev trial for it (these vals would be NA anyway)
  dplyr::filter(!is.na(prevtrlconfdiff)) #remove any nans from the data (e.g. where there was no "previous trial")

#check correlation between confidence width and the update
# this should be correlated really, because the computation of the adjustment includes the previous trial confidence width anyways
lmm.trladjcw.data %>% ggplot(aes(x = prevtrlcw, y = trladj)) +
  geom_point(size=.3) +
  geom_smooth(method='lm') +
  facet_wrap(~subid) +
  labs(y = 'trialwise adjustment in confidence/n(negative = become more confident)',
       x = 'previous trial confidence width')

contrasts(lmm.trladjcw.data$prevtrlinconf) <- contr.sum(2) #make this a sum contrast to look at the difference

lmm.trladjcw.min  <- lmerTest::lmer(data = lmm.trladjcw.data, trladj ~ prevtrlconfdiff + prevtrlcw + (1| subid)) #get the minimal model
lmm.trladjcw.full <- lmerTest::lmer(data = lmm.trladjcw.data, trladj ~ prevtrlconfdiff + prevtrlcw + (1 + prevtrlconfdiff + prevtrlcw | subid)) # and get the maximal model

summary(lme4::rePCA(lmm.trladjcw.full)) #definitely redundancies in the random effects structure


#what random effects are justified in being there, and which need removing to avoid over-parameterisation
trladjcw.model1 <- lmerTest::lmer(data = lmm.trladjcw.data, trladj ~ prevtrlconfdiff + prevtrlcw + (1 + prevtrlcw | subid))
summary(lme4::rePCA(trladjcw.model1))


trladjcw.model2 <- lmerTest::lmer(data = lmm.trladjcw.data, trladj ~ prevtrlconfdiff + prevtrlcw + (1 + prevtrlconfdiff | subid))
summary(lme4::rePCA(trladjcw.model2)) #redundant to have prevtrlconfdiff in the random effects structure

anova(lmm.trladjcw.min, trladjcw.model1) #prefer the more complex model


#print the coefficients table for the best-fitting model that was chosen
summary(trladjcw.model1)
# Fixed effects:
#                     Estimate    Std. Error  df          t value   Pr(>|t|)    
#   (Intercept)       1.539e+01   1.115e+00   1.920e+01   13.801    2.03e-11 ***
#   prevtrlconfdiff   4.518e-02   7.995e-03   9.468e+03   5.651     1.64e-08 ***
#   prevtrlcw         -8.405e-01  2.153e-02   2.085e+01   -39.048   < 2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

fit.trladjcw <- keepef(trladjcw.model1, fix = c('prevtrlconfdiff', 'prevtrlcw'), grouping=T)
lmm.trladjcw.data$fitted <- fit.trladjcw #get the fitted data

#visualise the fitted trialwise adjustments from this lmm
#this will get the fitted values per trial across subjects, having controlled for the within and between subject variance of the model
lmm.trladjcw.data %>%
  ggplot(aes(x = prevtrlconfdiff, y = trladj)) +
  geom_ribbon(inherit.aes = F, aes(x = prevtrlconfdiff, y = fitted), stat = 'smooth', method = 'lm', fill = '#bdbdbd') +
  geom_line(inherit.aes = F, aes(x = prevtrlconfdiff, y = fitted), stat = 'smooth', method = 'lm', color = '#756bb1', size = 1) +
  labs(y = 'confidence adjustment\n(current trial - previous trial confidence width',
       x = 'previous trial confidence error') +
  coord_cartesian(ylim = c(-80, 80), xlim = c(-80, 80)) #only add theme_bw if changing the device from cairo to normal to export into sketch
ggsave(filename = paste0(figpath, '/trladjustment_confidence_prevtrlconferr_agg.pdf'), device = cairo_pdf, dpi = 600, height = 9, width = 9)
ggsave(filename = paste0(figpath, '/trladjustment_confidence_prevtrlconferr_agg.eps'), device = cairo_ps,  dpi = 600, height = 9, width = 9)


#and save a version of this figure that has the single trial data on it
lmm.trladjcw.data %>%
  ggplot(aes(x = prevtrlconfdiff, y = trladj)) +
  geom_point(aes(x = prevtrlconfdiff, y = fitted), size = 1, alpha = .5, color = '#bdbdbd') +
  geom_ribbon(inherit.aes = F, aes(x = prevtrlconfdiff, y = fitted), stat = 'smooth', method = 'lm', fill = '#bdbdbd') +
  geom_line(inherit.aes = F,   aes(x = prevtrlconfdiff, y = fitted), stat = 'smooth', method = 'lm', color = '#756bb1', size = 1) +
  labs(y = 'confidence adjustment\n(current trial - previous trial confidence width',
       x = 'previous trial confidence error') +
  coord_cartesian(ylim = c(-80, 80), xlim = c(-80, 80)) #only add theme_bw if changing the device from cairo to normal to export into sketch
ggsave(filename = paste0(figpath, '/trladjustment_confidence_prevtrlconferr_agg_singletrls.pdf'), device = cairo_pdf, dpi = 300, height = 9, width = 9)
ggsave(filename = paste0(figpath, '/trladjustment_confidence_prevtrlconferr_agg_singletrls.eps'), device = cairo_ps,  dpi = 300, height = 9, width = 9)

#the regression line is the relationship between prev trl confidence error and the adjustment *from the linear mixed effects model*