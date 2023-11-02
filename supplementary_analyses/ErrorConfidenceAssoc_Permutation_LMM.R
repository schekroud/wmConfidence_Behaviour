#import libraries
library(tidyverse) # general data manipulation & plotting
library(magrittr)  # allows use of more pipes
library(afex)      # anovas etc
library(MASS)
library(circular)
options(scipen=999) #suppress scientific notation


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
setwd(wd)


#read in the data from experiment 1
path <- 'C:/Users/sammirc/Desktop/phd/wmConfidence_Behaviour/Experiment1'
dpath <- paste0(path, '/data') #path to folder with behavioural data
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
dim(df)

#set up the permutation analysis loop


nsims <- 10000
tvals = list()
pvals = list()
simout_df_tstats <- data.frame()
simout_df_betas <- data.frame()
set.seed(123)

if( file.exists(paste0(wd, '/LMM_Permutations_10k_tstats.RDS'))){
  simout_df_tstats <- readRDS(file = paste0(wd, '/LMM_Permutations_10k_tstats.RDS'))
  simout_df_betas <- readRDS(file = paste0(wd, '/LMM_Permutations_10k_betas.RDS'))
} else{ 
  
  for(isim in seq(1, nsims, by=1)){
    # For each simulation we will shuffle the individuals confidence reports, within condition,
    #to create a new error-confidence association that is unrelated to current trial performance
    #we'll then recombine the shuffled data into a dataframe again, and run the LMM.
    
    #by repeating this, we'll create a null distribution of effect sizes (and t-values)
    #for the linear mixed-effects modelling analysis that we can compare our observed effect to

    alldf = data.frame()
    for(isub in subs2use){
      tmpsub <- df %>% dplyr::filter(subid == isub)
      tmpsubneut <- tmpsub %>% dplyr::filter(cond == 'neutral')
      tmpsubcued <- tmpsub %>% dplyr::filter(cond == 'cued')
      
      #get confidences
      neutconf = tmpsubneut$confwidth
      cuedconf = tmpsubcued$confwidth
      
      #shuffle confidence across trials. preserves across trial distribution, but kills link between error & confidence at single trial level
      confneut = sample(neutconf)
      confcued = sample(cuedconf)
      
      #make sure that the shuffled is not the exact same as the original, just to be sure
      #there will be some similarity normally, but we want to make sure its not exactly the same (so we arent replicating the observed finding)
      while (identical(neutconf,confneut)){
        confneut = sample(neutconf)
      }
      
      while(identical(cuedconf, confcued)){
        confcued = sample(cuedconf)
      }
      
      tmpsubneut %<>% dplyr::mutate(confwidth = confneut)
      tmpsubcued %<>% dplyr::mutate(confwidth = confcued)
      
      tmp <- dplyr::bind_rows(tmpsubneut, tmpsubcued)
      
      tmp %<>% 
        dplyr::mutate(absrdif    = abs(rdif)) %>%
        dplyr::mutate(confidence = ifelse(confwidth == 0, 0.001, confwidth)) %>%
        dplyr::mutate(logconf    = log(confwidth)) %>%
        dplyr::mutate(condition  = as.factor(cond)) %>%
        dplyr::mutate(subid      = as.factor(subid))
      
      alldf %<>% dplyr::bind_rows(tmp)
    }
    #set up the LMM that was run on the empirical data, so we can get the effect size for this permuted dataset
    lmm.data <- alldf %>%
      dplyr::mutate(radconf = rad(confidence),
                    absrdif = rad(absrdif)) %>% #convert to radians
      dplyr::mutate(condition = relevel(condition, ref = 'cued'))
    contrasts(lmm.data$condition) <- contr.sum(2) #specify a contrast to compare cued - neutral trials [1, -1]
    
    # are my DV normally distributed
    # determine lambda for confwidth
    # temp fix for 0 values in the confwidth

    #this is the LMM structure from the empirical analysis pipeline, we want to look at the null distribution of effects across simulations
    model3 <- lmerTest::lmer(data = lmm.data, log(confidence) ~ absrdif + condition + absrdif:condition + (1 + absrdif|subid))
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
   saveRDS(simout_df_tstats, file = paste0(wd, '/LMM_Permutations_10k_tstats.RDS'))
   saveRDS(simout_df_betas, file = paste0(wd, '/LMM_Permutations_10k_betas.RDS'))
}

#observed effect in experiment 1: b = -0.13591, t = -3.127
#this is for a contrast of neutral - cued, so can just sign flip this

#get proportion of t-values larger than our observed t-value
tmp <- ifelse(simout_df_tstats$`absrdif:condition1` > 3.127, 1, 0)
sum(tmp)/nsims 
#0.0009
#only 9 out of 10k simulations had a t-value larger than our observed relationship

#can get quantiles for the simulated t-stats
quantile(simout_df_tstats$`absrdif:condition1`, probs = c(0.05, 0.25, 0.5, 0.75, 0.95))
#        5%        25%        50%        75%        95% 
#-1.4986109 -0.5049609  0.1340519  0.7778855  1.7029481 


#lets plot the distribution of simulated t-values
ggplot(simout_df_tstats, aes(x = `absrdif:condition1`)) +
  geom_histogram(fill = '#1c9099', alpha = 0.7, bins = 100) +
  geom_vline(xintercept = 3.127, linetype = 'dashed', size = 1, color = '#000000') +
  labs(x = 't-value', y = 'count')
ggsave(filename = paste0(wd, '/supplementary_analysis_figures', '/permutation_analyses',
                         '/NullDistribution_LMManalysis_tstats.eps'),
       dpi = 300, height = 8, width = 8, device = cairo_ps)
ggsave(filename = paste0(wd, '/supplementary_analysis_figures', '/permutation_analyses',
                         '/NullDistribution_LMManalysis_tstats.pdf'),
       dpi = 300, height = 8, width = 8, device = cairo_pdf())




sum(ifelse(abs(simout_df_tstats$`absrdif:condition1`)>3.127,1,0))/nsims #this is the two-sided version of the test - still 0.002,
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









