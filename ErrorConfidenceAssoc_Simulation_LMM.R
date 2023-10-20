#import libraries
library(tidyverse) # general data manipulation & plotting
library(magrittr)  # allows use of more pipes
library(afex)      # anovas etc
library(MASS)
library(circular)


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
wrap <- function(x) (x+180)%%360 - 180 #function to wrap data between +/- 90 degrees. keeps sign of the response (-ve leftwards, +ve rightwards)
wrap90 <- function(x) (x+90)%%180 - 90


wd <- 'C:/Users/sammirc/Desktop/phd/wmConfidence_Behaviour'
wd <- '/Users/sammi/Desktop/Experiments/wmConfidence_Behaviour'
setwd(wd)

nsims <- 10000
nsubs <- 20
ntrls <- 128
#get values from mixture modelling of task data, to be used in generation of simulated data
kappase_neut   <- 1.7403459686238723
kappamean_neut <- 21.326299997413816

kappase_cued   <- 1.6771219142958136
kappamean_cued <- 25.713405876244302

tvals = list()
pvals = list()
simout_df_tstats <- data.frame()
simout_df_betas <- data.frame()

if( file.exists(paste0(wd, '/LMM_simulations_10k_tstats.RDS'))){
  simout_df_tstats <- readRDS(paste0(wd, '/LMM_simulations_10k_tstats.RDS'))
  simout_df_betas  <- readRDS(paste0(wd, '/LMM_simulations_10k_betas.RDS'))
  
} else{ 
  
  for(isim in seq(1, nsims, by=1)){
    #get participants kappa for neutral and cued trials by sampling a normal distribution with the mean/se above as mean/sd
    alldf = data.frame()
    for(isub in seq(1, nsubs, by=1)){
      neutkappa <- rnorm(n=1, mean = kappamean_neut, sd = kappase_neut)
      cuedkappa <- rnorm(n=1, mean = kappamean_cued, sd = kappase_cued)
      
      #get parent distribution for response error
      #wrap this vonmises to be betweeen -90 and +90 degrees (the error limits on the task)
      parentneut <- as.numeric(wrap90(round(deg(rvonmises(n=1000, mu = 0, kappa = neutkappa)),0)))
      parentcued <- as.numeric(wrap90(round(deg(rvonmises(n=1000, mu = 0, kappa = cuedkappa)),0)))
      
      #sample this parent distribution, with replacement, to get the single subject response distribution
      subjerror_neut <- sample(parentneut, size = ntrls, replace = T)
      subjerror_cued <- sample(parentcued, size = ntrls, replace = T)
      
      
      # with no insight into single-trial error, but perfect insight to across-trial error distribution,
      # confidence can be simulated by randomly sampling from the absolute (confidence always positive) of the across-trial error distribution
      
      neutconf <- sample(abs(parentneut), size = ntrls, replace = T)
      cuedconf <- sample(abs(parentcued), size = ntrls, replace = T)
      
      cond <- sort(rep(c('cued', 'neutral'), ntrls)) #generate variable marking cue condition on each simulated 'trial'
      trlid <- seq(1, length(cond), by=1)
      
      suberrors <- c(subjerror_cued, subjerror_neut)
      subconfs  <- c(cuedconf, neutconf)
      df <- data.frame(isub, trlid, cond, suberrors, subconfs)
      colnames(df) <- c('subid', 'trild', 'condition', 'error', 'confidence')
      
      df %<>% 
        dplyr::mutate(absrdif    = abs(error)) %>%
        dplyr::mutate(confidence = ifelse(confidence == 0, 0.001, confidence)) %>%
        dplyr::mutate(logconf    = log(confidence)) %>%
        dplyr::mutate(condition  = as.factor(condition)) %>%
        dplyr::mutate(subid      = as.factor(subid))
      
      alldf %<>% dplyr::bind_rows(df)
    }
    
    #set up the LMM that was run on the empirical data, so we can get the effect size for this simulated dataset of 20 people, simulated under the null hypothesis
    lmm.data <- alldf %>%
      dplyr::mutate(confwidth = rad(confidence),
                    absrdif = rad(absrdif)) %>% #convert to radians
      dplyr::mutate(condition = relevel(condition, ref = 'cued'))
    contrasts(lmm.data$condition) <- contr.sum(2) #specify a contrast to compare cued - neutral trials [1, -1]
    
    # are my DV normally distributed
    # determine lambda for confwidth
    # temp fix for 0 values in the confwidth
    lmm.data$confwidth[lmm.data$confwidth==0] <- 0.001
    
    #this is the LMM structure from the empirical analysis pipeline, we want to look at the null distribution of effects across simulations
    model3 <- lmerTest::lmer(data = lmm.data, log(confwidth) ~ absrdif + condition + absrdif:condition + (1 + absrdif|subid))
    out <- summary(model3)
    tval  = as.numeric(out$coefficients[,4]) #get tvalues for this simulated dataset analysis
    tval <- as.data.frame(t(tval))
    colnames(tval) <- c('intercept', 'absrdif', 'condition1', 'absrdif:condition1')
    tval$simnum <- isim
    
    betas <- as.numeric(out$coefficients[,1]) #get output betas for the simulated dataset
    bvals <- as.data.frame(t(betas))
    colnames(bvals) <- c('intercept', 'absrdif', 'condition1', 'absrdif:condition1')
    bvals$simnum <- isim
    
    simout_df_tstats %<>% dplyr::bind_rows(tval)
    simout_df_betas  %<>% dplyr::bind_rows(bvals)
  }
# save to file so we don't have to repeat the simulations
saveRDS(simout_df_tstats, file = paste0(wd, '/LMM_simulations_10k_tstats.RDS'))
saveRDS(simout_df_betas, file = paste0(wd, '/LMM_simulations_10k_betas.RDS'))
}

#observed effect in experiment 1: b = -0.13591, t = -3.127
#this is for a contrast of neutral - cued, so can just sign flip this

#get proportion of t-values larger than our observed t-value
tmp <- ifelse(simout_df_tstats$`absrdif:condition1` > 3.127, 1, 0)
sum(tmp)/nsims 
#0.0008
#only 8 out of 10k simulations had a t-value larger than our observed relationship

ggplot(simout_df_tstats) +
  geom_histogram(aes(x=`absrdif:condition1`), bins = 100, fill = '#756bb1') +
  geom_vline(xintercept = 3.127, size = 0.5, color = '#000000') +
  labs(x= 't-value', y = 'count')
ggsave(filename = paste0(wd, '/NullDistribution_LMManalysis_tstats.eps'), dpi = 600, height = 4, width = 6, device = cairo_ps)


#can get quantiles for the simulated t-stats
quantile(simout_df_tstats$`absrdif:condition1`, probs = c(0.05, 0.25, 0.5, 0.75, 0.95))
#         5%         25%         50%         75%         95% 
#-1.65630650 -0.70517374 -0.01637486  0.66428465  1.60939075 


sum(ifelse(abs(simout_df_tstats$`absrdif:condition1`)>3.127,1,0))/nsims #this is the two-sided version of the test - still 0.0011,
#so suggests the observed t-value didn't arise under the null hypothesis of this simulation
#this tests how likely a t-value of that *magnitude* is likely under the null, ignoring direction (so just strength of effect)


#should also check the effect size (beta) from simulations, rather than t-value. beta is the effect size estimate,
#t-values are the betas/std err of the beta, so scales beta by the variability in the effect

tmp2 <- ifelse(simout_df_betas$`absrdif:condition1` > 0.13591, 1, 0)
sum(tmp2)/nsims
# 0.2454 -- 2454 simulations out of the 10k gave rise to an effect size larger than what we observed
#suggests that the effect size could be observed under the null, but it wouldn't come out significant (as the tvalues are scaled by variability)

#get quantiles for the simulated beta coefficients
quantile(simout_df_betas$`absrdif:condition1`, probs = c(0.05, 0.25, 0.5, 0.75, 0.95))
#           5%          25%          50%          75%          95% 
# -0.332121414 -0.141607213 -0.003332296  0.133309377  0.322591646




#does this stand if you use the betas (effect size/direction) rather than t-value
#observed, empirical beta: -5.266e-02
  
  
  
  





