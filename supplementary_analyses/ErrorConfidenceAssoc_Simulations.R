library(tidyverse)
library(magrittr)
library(afex)
library(lmerTest)
library(circular)

wrap   <- function(x) (x+180)%%360 - 180
wrap90 <- function(x) (x+90)%%180 - 90
se <- function(x) sd(x)/sqrt(length(x))

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

cuedcol = '#3182bd' #blue, use for cued
neutcol = '#bdbdbd' #light-ish grey, use for neutral
diffcol = '#B2DF8A' #for colouring of the bars of difference plots, third distinct colour

wd = '/Users/sammi/Desktop/Experiments/wmConfidence_Behaviour'
setwd(wd)
#%%

#these are the mean and standard error of modelled kappa concentration parameters of behavioural performance in experiment 1
#these can be used to sample single subject concentration parameters to simulate data
kappase_neut <- 1.7403459686238567
kappamean_neut <- 21.326299997413784
kappase_cued <- 1.6771219142957927
kappamean_cued <- 25.713405876244206


tmp  <- wrap90(round(deg(circular::rvonmises(n = 1000, mu = 0, kappa = kappamean_neut)), 0))
tmp2 <- wrap90(round(deg(circular::rvonmises(n = 1000, mu = 0, kappa = kappamean_cued)), 0))

ggplot() +
  geom_density(aes(x = as.numeric(tmp)),
               color = neutcol, fill = neutcol, alpha = 0.5) +
  geom_density(aes(x = as.numeric(tmp2)),
               color = cuedcol, fill = cuedcol, alpha = 0.5)
#%%


nsubs = 20
ntrls = 128
alldf = data.frame()
for (i in seq(1, nsubs, by=1)){
  
  #sample the concentration parameter of a single simulated participants error (the spread of performance in circular space) for neutral trials
  neutkappa = rnorm(n=1, mean = kappamean_neut, sd = kappase_neut)
  
  #sample the concentration parameter of a single simulated participants error (the spread of performance in circular space) for cued trials
  cuedkappa = rnorm(n=1, mean = kappamean_cued, sd = kappase_cued)
  #note: this assumes that cueing on average reults in an improvement (mostly) but that the amount of difference varies across people
  
  #here we are generating a a parent performance distribution for neutral and cued trials. this lets us assume that each simulated participant has an overall working memory performance
  # and when they do the task, we are taking a sample from this performance. so we can randomly sample from this parent distribution to get their task performance,
  # assuming their overarching "ability" would be constant across task repetitions
  parenterror_neut = as.numeric(wrap90(round(deg(circular::rvonmises(n=1000, mu = 0, kappa = neutkappa)), 0)))
  parenterror_cued = as.numeric(wrap90(round(deg(circular::rvonmises(n=1000, mu = 0, kappa = cuedkappa)), 0)))
  
  visparent = F
  if(visparent){
    ggplot() +
      geom_density(aes(x = parenterror_cued), color = cuedcol, alpha = 0.5, fill = cuedcol) +
      geom_density(aes(x = parenterror_neut), color = neutcol, alpha = 0.5, fill = neutcol) +
      labs(x = 'response error (degrees)', y = 'density')
  }
  
  #sample the parent distributions, with replacement, to simulate the across trial response error distribution
  subjerror_neut = sample(parenterror_neut, size = ntrls, replace = T)
  subjerror_cued = sample(parenterror_cued, size = ntrls, replace = T)
  
  
  
  #scenario 1: no insight into single-trial error, or the error wouldn't be made
  # instead, perfect insight into the across trial performance distribution (how good they are overall)
  # confidence thus sampled from the same distribution as error so that across trial insight is accurate to the overall performance distibution
  
  neutconf = sample(abs(parenterror_neut), size = ntrls, replace = T)
  cuedconf = sample(abs(parenterror_cued), size = ntrls, replace = T)
  
  df = data.frame(c(subjerror_neut, subjerror_cued))
  colnames(df) = 'error'
  df %<>%
    dplyr::mutate(confidence = c(neutconf, cuedconf)) %>%
    dplyr::mutate(trlid = seq(1, nrow(df), by = 1)) %>%
    dplyr::mutate(condition = ifelse(trlid %in% seq(1,ntrls,by=1), 'neutral', 'cued')) %>%
    dplyr::mutate(absrdif = abs(error),
                  conf = ifelse(confidence == 0, 0.001, confidence)) %>%
    dplyr::mutate(logconf = log(conf),
                  subid = i)
  
  alldf %<>% dplyr::bind_rows(df)
  
}

alldf %<>% dplyr::mutate(conferror = error - confidence)


#%%

#do some random effects analyses here to make sure that the simulation can replicate the difference we find in the task data.
#if it can't, the simulation might need tweaking

alldf %>% dplyr::group_by(subid, condition) %>% dplyr::summarise_at(.vars = 'absrdif', .funs = 'mean') %>%
  as.data.frame() %>% dplyr::mutate(condition = as.factor(condition)) -> df.mad

t.test(data = df.mad, absrdif ~ condition, paired = T)
# Paired t-test
# data:  absrdif by condition
# t = -4.9354, df = 19, p-value = 9.185e-05
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -1.3039242 -0.5273258
# sample estimates:
#   mean of the differences 
# -0.915625 

df.mad %>%
  dplyr::group_by(condition) %>%
  dplyr::summarise_at(.vars = 'absrdif', .funs = c('mean', 'se')) %>%
  ggplot(aes(x = condition, y = mean, ymin = mean-se, ymax = mean+se, fill = condition)) +
  geom_bar(stat = 'identity', width = .7) +
  geom_errorbar(stat = 'identity', width = .35, size = 1) +
  scale_fill_manual(values = c('neutral' = neutcol, 'cued' = cuedcol)) +
  labs(x = '', y = 'mean absolute response error (degrees)') +
  coord_cartesian(ylim = c(0,11)) +
  theme(legend.position = 'none')


#random effects analysis on confidence too
alldf %>% dplyr::group_by(subid, condition) %>% dplyr::summarise_at(.vars = 'confidence', .funs = 'mean') %>%
  as.data.frame() %>% dplyr::mutate(condition = as.factor(condition)) -> df.conf

t.test(data = df.conf, confidence ~ condition, paired = T)
# Paired t-test
# data:  confidence by condition
# t = -4.1367, df = 19, p-value = 0.0005608
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -1.3088971 -0.4293842
# sample estimates:
#   mean of the differences 
# -0.8691406 

df.conf %>%
  dplyr::group_by(condition) %>%
  dplyr::summarise_at(.vars = 'confidence', .funs = c('mean', 'se')) %>%
  ggplot(aes(x = condition, y = mean, ymin = mean-se, ymax = mean+se, fill = condition)) +
  geom_bar(stat = 'identity', width = .7) +
  geom_errorbar(stat = 'identity', width = .35, size = 1) +
  scale_fill_manual(values = c('neutral' = neutcol, 'cued' = cuedcol)) +
  labs(x = '', y = 'mean confidence width (degrees)') +
  coord_cartesian(ylim = c(0,11)) +
  theme(legend.position = 'none')


#%% the outstanding question is: in this simulation, is there an interaction in the linear models
# showing that the relationship between error and confidence is improved by cue condition, purely where the only difference is a more precise error distribution



#firstly we can do this as a LMM

lmm.data <- alldf %>%
  dplyr::mutate(confwidth = rad(conf),
                absrdif = rad(absrdif)) %>% #convert these to radians
  dplyr::mutate(condition = as.factor(condition)) %>%
  dplyr::mutate(condition = relevel(condition, ref = 'cued'))
contrasts(lmm.data$condition) = contr.sum(2) #condition is now set up as a [1, -1] contrast to look at the difference between cued and neutral


lmm <- lmerTest::lmer(data = lmm.data,
                      log(confwidth) ~ absrdif + condition + absrdif:condition + (1 + absrdif|subid))
summary(lmm)
# Fixed effects:
#                       Estimate    Std. Error  df            t value   Pr(>|t|)    
# (Intercept)          -2.35930     0.04121     5113.44022    -57.246   < 2e-16 ***
# absrdif               0.06755     0.22757       69.07888    0.297     0.76748    
# condition1           -0.10809     0.04121     5113.05456    -2.623    0.00875 ** 
# absrdif:condition1    0.31797     0.20177     5109.37729    1.576     0.11510    

#at the level of the LMM, this simulation doesn't produce an interaction that would suggest that cueing leads to a difference in introspective calibration
#it does replicate the difference in mean performance between cue conditions though which is good

# if we do this as a random effects analysis, do we get a qualitatively similar pattern?
allbetas = data.frame()
for(i in seq(1, nsubs,by=1)){
  alldf %>% dplyr::filter(subid == i) %>%
    dplyr::mutate(condition = as.factor(condition)) %>%
    dplyr::mutate(condition = relevel(condition, ref='cued')) %>% 
    dplyr::mutate(absrdif = ifelse(absrdif == 0, 0.1, absrdif),
                  confwidth = ifelse(conf == 0, 0.1, conf)) -> tmpsub
  contrasts(tmpsub$condition) <- contr.sum(2) #set [1, -1] [cued, neutral] contrast regressor
  
  glm <- glm(data = tmpsub,
             formula = log(confwidth) ~ absrdif + condition + absrdif:condition)
  
  betas = data.frame(t(glm$coefficients))
  colnames(betas) = c('intercept', 'absrdif', 'cuetype', 'absrdif:cuetype')
  betas %<>% dplyr::mutate(subid = i)
  allbetas %<>% dplyr::bind_rows(betas)
}

#random effects (across subjects) of the betas -- one sample t-tests against zero to check for group level effects in the regression outputs

t.test(allbetas$absrdif)
# One Sample t-test
# data:  allbetas$absrdif
# t = 0.32124, df = 19, p-value = 0.7515
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
#   -0.008001859  0.010903505
# sample estimates:
#   mean of x 
# 0.001450823 

t.test(allbetas$cuetype)
# One Sample t-test
# data:  allbetas$cuetype
# t = -3.0138, df = 19, p-value = 0.00714
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
#   -0.18813703 -0.03392106
# sample estimates:
#   mean of x 
# -0.111029 

t.test(allbetas$`absrdif:cuetype`)
# One Sample t-test
# data:  allbetas$`absrdif:cuetype`
# t = 2.01, df = 19, p-value = 0.05885
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
#   -0.0002575431  0.0127264479
# sample estimates:
#   mean of x 
# 0.006234452 

#slightly non-significant (v much a trend) effect of interaction across subjects


