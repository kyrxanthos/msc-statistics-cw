rm(list=ls())  #clear loadspace
library(lars)

library(dplyr, warn.conflicts = FALSE)

setwd("/Users/lysi2/Documents/UNI_Imperial/Genetics_Bio/Courseworks/CW1")


dat=read.table("data_1389741.txt",header=TRUE,stringsAsFactors=FALSE)


# Begin EDA
print(nrow(dat))
print(ncol(dat))

no_response = which(is.na(dat[,1]))


q = rowSums(is.na(dat))

q1 = colSums(is.na(dat[,-1]))

# Quality Control

# 5% of the existing SNPs
K = 45

# 5% of cols
K1 = ncol(dat) * 0.05


missing_genotypes = which(q >= K)

missing_snps = which(q1 >= K1)



dat = dat[-c(no_response), ]

print(nrow(dat))
print(ncol(dat))

dat = dat[-c(missing_genotypes),]

print(nrow(dat))
print(ncol(dat))

dat = dat [, -c(missing_snps)]

print(nrow(dat))
print(ncol(dat))

# function for calculating MAF
MAF <- function(col_snp){
  x2 <- (sum(dat[col_snp]==2, na.rm = TRUE))*2
  x1 <- sum(dat[col_snp]==1, na.rm = TRUE)
  x0 <- sum(dat[col_snp]==0, na.rm = TRUE)
  return((x2+x1)/((x0 + x1 + x2/2)*2))
}

all_MAF = sapply(names(dat[,3:ncol(dat)]), MAF)

low_maf = which(all_MAF <=0.05)

high_maf = which(all_MAF >0.5)

# remove all of them that do not satisfy conditions
dat = dat[, -c(low_maf)]

dat = dat[, -c(high_maf)]

print(nrow(dat))
print(ncol(dat))


# function to calculate HWE given a SNP
HWE <- function(col_snp){
  n_minor = sum(dat[col_snp]==2, na.rm = TRUE)
  n_mixed = sum(dat[col_snp]==1, na.rm = TRUE)
  n_major = sum(dat[col_snp]==0, na.rm = TRUE)
  
  x2 <- (n_minor)*2
  x1 <- n_mixed
  x0 <- n_major
  
  # x0 <- sum(dat[col_snp]==0, na.rm = TRUE) - nrow(dat) * (1-all_MAF)^2
  maf = (x2+x1)/((x0 + x1 + x2/2)*2)
  
  ET_AA = nrow(dat) * (1-maf)^2
  ET_AT = nrow(dat) * 2 * maf * (1-maf)
  ET_TT = nrow(dat) * maf^2
  
  X_squared = ((n_major - ET_AA)^2 / ET_AA) + ((n_mixed - ET_AT)^2 / ET_AT) +
    ((n_minor - ET_TT)^2 / ET_TT)
  
  pval = 1 - pchisq(X_squared, df=1)
  
  return(pval)
}


all_p_values_per_snp = sapply(names(dat[,3:ncol(dat)]), HWE)

not_in_HWE = which(p.adjust(all_p_values_per_snp, method = 'BH') < 0.05)

length(not_in_HWE) #this is zero.

# dat = dat[, -c(not_in_HWE)]

print(nrow(dat))
print(ncol(dat))



# Begin Frequentist Analysis

# chisquared test of association for each SNP
chi_snp <- function(SNP){
  X = dat[,SNP]
  y = dat[,1]
  ps = chisq.test(X, y)$p.value
  return(ps)
}


pss = sapply(names(dat[,3:ncol(dat)]), chi_snp)

# adjust using benjamini
adjustedpss = p.adjust(pss, method = 'BH')

# 5% level
length(which(adjustedpss <0.05))
important_fr_SNPs = names(which(adjustedpss <0.05))

sorted_im_fr_snps = names(which(sort(adjustedpss) <0.05))
sorted_im_fr_snps

# sorted p values for chisq test
adjustedpss[sorted_im_fr_snps]



thresholds = seq(0.001, 0.1, 0.001)

#function to test different p values
chi_sq <- function(val){
  return(length(which(adjustedpss <val)))
}

lengths_of_snps = sapply(thresholds, chi_sq)

lengths_of_snps_no_bh = sapply(thresholds, chi_sq)

plot(thresholds, lengths_of_snps, xlab = 'alpha', 
     ylab ='Number of Significant SNPs', type='s')

plot(thresholds, lengths_of_snps_no_bh, type = 'l', xlab = 'alpha', 
     ylab ='Number of Significant SNPs')


#Bayesian

# function that calculates logistic regresssion coefficients,
# ABF and PPA
PPA <- function(SNP, use_maf, W){
  X = dat[,SNP]
  y = dat[,1]
  maf = MAF(SNP)
  lgrg = summary(glm(y ~ X,family='binomial'))
  beta_hat = lgrg$coefficients[2, 1]
  V = lgrg$coefficients[2, 2]^2
  Z_squared = beta_hat^2 / V
  ABF = sqrt((V + W) / V) * exp(- (Z_squared/2) * W / (V + W))
  if (use_maf == TRUE){
    PO = maf / (1 - maf) * (1 / ABF)
  }
  else{
    PO = 0.01 / (1 - 0.01) * (1 / ABF)
  }
  PPA = PO / (1 + PO)
  return(PPA)
}

ppas_maf = sapply(names(dat[,3:ncol(dat)]), PPA,  use_maf = TRUE, W = 0.01)

ppas_other = sapply(names(dat[,3:ncol(dat)]), PPA,  use_maf = FALSE, W = 0.2)

# 5% level
length(which(ppas_maf > 0.95))

length(which(ppas_other> 0.95))

important_ppa_maf_SNPs = names(which(ppas_maf > 0.95))

important_ppa_other_SNPs = names(which(ppas_other> 0.95))

intersect(important_ppa_maf_SNPs, important_ppa_other_SNPs)

length(intersect(important_ppa_maf_SNPs, important_fr_SNPs))


#explore the variation for different W for MAF
variation <- function(W){
  len_ppa = length(which(sapply(names(dat[,3:ncol(dat)]), 
                                PPA,  use_maf = TRUE, W) >0.95))
  return(len_ppa)
}

#explore the variation for different W for pi=0.01
variation2 <- function(W){
  len_ppa = length(which(sapply(names(dat[,3:ncol(dat)]), 
                                PPA,  use_maf = FALSE, W) >0.95))
  return(len_ppa)
}

Ws = seq(0.1, 20, 0.5)
# all of the maf ppas for different Ws
all_lengths_ppa = sapply(Ws, variation)

# all of the 0.01 ppas for different Ws
all_lengths_ppa2 = sapply(Ws, variation2)

# all of the 0.1 ppas for different Ws
all_lengths_ppa3 = sapply(Ws, variation2)


plot(Ws, all_lengths_ppa, type = 'l', ylim = c(15, 35), 
     xlab = 'W', ylab = 'Number of Significant SNPs')
lines(Ws, all_lengths_ppa2, type = 'l', col = 'red')
lines(Ws, all_lengths_ppa3, type = 'l', col = 4)
legend(x='topright', legend = c('MAF', '0.01', '0.1'), 
       lty = c(1,1,1), col= c(1,2,4), title = 'Prior', cex = 0.7)



# same function as above but takes as argument p, the prior
PPA2 <- function(SNP ,p){
  X = dat[,SNP]
  y = dat[,1]
  lgrg = summary(glm(y ~ X,family='binomial'))
  beta_hat = lgrg$coefficients[2, 1]
  V = lgrg$coefficients[2, 2]^2
  Z_squared = beta_hat^2 / V
  W = 0.02
  ABF = sqrt((V + W) / V) * exp(- (Z_squared/2) * W / (V + W))
  PO = p / (1 - p) * (1 / ABF)
  PPA = PO / (1 + PO)
  return(PPA)
}

# explores variation for different priors
variation3 <- function(prior){
  len_ppa = length(which(sapply(names(dat[,3:ncol(dat)]), 
                                PPA2,  p = prior) >0.95))
  return(len_ppa)
}

all_priors = seq(0.01, 0.7, by = 0.02)


all_ppas_diff_priors = sapply(all_priors, variation3)

plot(all_priors, all_ppas_diff_priors, type='s', 
     xlab = 'prior probability', ylab = 'Number of Significant SNPs')


#Q3

# check the interaction using a logistic regression model.
LR_interaction <- function(SNP){
  X = dat[,SNP]
  y = dat[,1]
  E = dat[,2]
  lgrg = summary(glm(y ~ E * X,family='binomial'))
  pval = lgrg$coefficients[4,4]
  # now do bonferoni
  return(pval)
}

ps_interaction = sapply(names(dat[,3:ncol(dat)]), LR_interaction)
length(which(ps_interaction < 0.05))


# adjust using bonferroni
adjust_ps_interaction = p.adjust(ps_interaction, method = 'bonferroni')
length(which(adjust_ps_interaction < 0.05))

interaction_fun_diff_val <- function(val){
  return(length(which(adjust_ps_interaction <val)))
}

all_lengths_interaction = sapply(thresholds, interaction_fun_diff_val)

plot(thresholds, all_lengths_interaction)
  

important_interaction_SNPs = names(which(adjust_ps_interaction < 0.05))

#find the intercetions of all methods
intersect(important_fr_SNPs, important_interaction_SNPs)

intersect(important_ppa_maf_SNPs, important_interaction_SNPs)


