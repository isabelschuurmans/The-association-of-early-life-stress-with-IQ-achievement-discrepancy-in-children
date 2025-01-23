#authors: isabel k. schuurmans & donna de maat

#########################################
######## enviromental risk score ########
#########################################

#----------------------------------------
# Prepping
#----------------------------------------

set.seed(08121996)

# open library
library(lavaan) 
library(semTools)
library(mice)
library(xlsx)
library(magrittr)
library(car)
library(ggplot2)
library(haven)

# read in implist
implist <- readRDS('O:/medewerkers/042647 Schuurmans, I/PROJECT_1_ELS_CM/R Code/imp_data_20210325.rds')
implist_c <- complete(implist, action  = 'long', include = T)
datarisk <- readRDS('O:/medewerkers/042647 Schuurmans, I/PROJECT_1_ELS_CM/R Code/datarisk_20210325.rds')

# add 888 as participant id was accidently set to NA
datarisk[is.na(datarisk$idc), 'idc'] <- 888
implist_c[implist_c$.id == 329, 'idc'] <- 888

# remove participant 102 from analyses as they wanted to be removed from dataset
datarisk <- datarisk[which(!datarisk$idc == 102),]
implist_c <- implist_c[(!implist_c$idc == 102),]

# calculate cumulative ELS scores
implist <- as.mids(implist_c)

#----------------------------------------
# Functions
#----------------------------------------

# function for quick mean and sd
meansd <- function(var){
  mean <- round(mean(var, na.rm=T),2)
  sd <- round(sd(var, na.rm=T),2)
  x <- c(mean, sd)
  return(x)
}

# same but double
meansd_d <- function(var1, var2){
  x1 <- meansd(var1)
  x2 <- meansd(var2)
  x <- c(x1, x2)
  return(x)
}

# library and functions
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

# get estimates mi lm
modelest <- function(mod, n = 2){
  ls <- c(round(summary(pool(mod))[n,2],2), 
           round(summary(pool(mod))[n,2]-1.96*summary(pool(mod))[n,3],2),
           round(summary(pool(mod))[n,2]+1.96*summary(pool(mod))[n,3],2),
           2*pt(summary(pool(mod))[n,4], summary(pool(mod))[n,5]))
  return(ls)}

#----------------------------------------
# DESCRIPTIVES
#----------------------------------------

# TABLE 1: DESCRIPTIVES
row1_2 <- c(nrow(datarisk[which(datarisk$gender == 1),]), 
            round(nrow(datarisk[which(datarisk$gender == 1),])/nrow(datarisk),2))
row1_3 <- c(nrow(datarisk[which(datarisk$gender == 2),]), 
            round(nrow(datarisk[which(datarisk$gender == 2),])/nrow(datarisk),2))

row1_4 <- meansd(datarisk$age)
row1_5 <- meansd(datarisk$agechild13)
row1_6 <- meansd(datarisk$agechild13-datarisk$age)

row1_7 <- c(nrow(datarisk[which(datarisk$ethnicity == 0),]), 
            round(nrow(datarisk[which(datarisk$ethnicity == 0),])/nrow(datarisk),3))
row1_8 <- c(nrow(datarisk[which(datarisk$ethnicity == 1),]), 
           round(nrow(datarisk[which(datarisk$ethnicity == 1),])/nrow(datarisk),3))
row1_9 <- c(nrow(datarisk[which(datarisk$ethnicity == 2),]), 
           round(nrow(datarisk[which(datarisk$ethnicity == 2),])/nrow(datarisk),3))

row1_10 <- c(0,1)
row1_11 <- meansd(datarisk$iq)
row1_12 <- meansd(datarisk$cm)

table1a <- rbind(row1_2, row1_3, row1_4, row1_5, row1_6, row1_7, row1_8, row1_9, row1_10, row1_11, row1_12)

#########################################
# POSTNATAL STUFF
#########################################

fitmeas <- c('chisq', 'df', 'pvalue', 'cfi', 'tli', 'rmsea','rmsea.ci.lower', 'rmsea.ci.upper', 'srmr')

# assess model fit base model
basemodelpost <- 'r1 =~ lc_le + lc_ce + lc_pr + lc_ip + lc_dv '

# model summary with imputations
mymodelfit_postbaseimp<- runMI(basemodelpost, data = implist, fun = "sem", estimator = "MLR", std.lv = T)
postbaseimp <- summary(mymodelfit_postbaseimp, standardized=T, rsq=T)

# model fit + factor loading for figure 1
fig1post <- list(fitmeasures(mymodelfit_postbaseimp)[fitmeas], round(postbaseimp[1:5,11],2))

# predict with missings
predictpost <- sem(basemodelpost, fixed.x = F, missing='fiml', data = datarisk, std.lv=T)
datarisk$riskpost_missings <- predict(predictpost)

# get n and number of imputations
n <- nrow(datarisk)
m <- 30

# get all predictions; do this for each imputed data set seperately
pred <- matrix(nrow=n, ncol=m)
for (i in 1:m){
  model_pred <- sem(basemodelpost, fixed.x = F, missing='fiml', data = complete(implist,action=i), std.lv=T)
  pred[,i] <- predict(model_pred)
}

# get for each row the mean and predict risk without missings
datarisk$riskpost <- apply(pred, 1, mean)

#----------------------------------------
# PRIMARY ANALYSIS: COGNITIVE MISMATCH
#----------------------------------------

# cfa model primary analysis
cfa.model_post1 <- '                                                           

# latent variable modelling 
r1 =~ lc_le + lc_ce + lc_pr + lc_ip + lc_dv

# primary analysis cognitive mismatch
cm ~ r1 + age + gender'

# get model summary with imputations -> report this one
mymodelfit_post1imp<- runMI(cfa.model_post1, data = implist, fun = "sem", estimator = "MLR", std.lv = T)
(post1imp <- summary(mymodelfit_post1imp, standardized=T, rsq=T))

# get fit measures for primary model
fitmeasures(mymodelfit_post1imp)

# get stuff for table2
row2_6_est <- round(post1imp[6,12],2)
row2_6_95l <- round(post1imp[6,12]-1.96*post1imp[6,6],2)
row2_6_95u <- round(post1imp[6,12]+1.96*post1imp[6,6],2)
row2_6_p <- post1imp[6,9]
row2_6 <- cbind(row2_6_est, row2_6_95l, row2_6_95u, row2_6_p)

#----------------------------------------
# SECONDARY ANALYSIS: CITO AND iq
#----------------------------------------

# cfa model
cfa.model_post2 <- '                                                           

# latent variable modelling 
r1 =~ lc_le + lc_ce + lc_pr + lc_ip + lc_dv

# secondary analysis
cito ~ r1 + age + gender
iq ~ r1 + age + gender'

# get model summary with imputations -> report this one
mymodelfit_post2imp<- runMI(cfa.model_post2, data = implist, fun = "sem", estimator = "MLR", std.lv = T)
(post2imp <- summary(mymodelfit_post2imp, standardized=T, rsq=T))

# get fit measures for primary model
fitmeasures(mymodelfit_post2imp)[fitmeas]

# get stuff for table3: academic achievement
row3aa_6_est <- round(post2imp[6,12],2)
row3aa_6_95l <- round(post2imp[6,12]-1.96*post2imp[6,6],2)
row3aa_6_95u <- round(post2imp[6,12]+1.96*post2imp[6,6],2)
row3aa_6_p <- post2imp[6,9]
row3aa_6 <- cbind(row3aa_6_est, row3aa_6_95l, row3aa_6_95u, row3aa_6_p)

# get stuff for table3: cognition/iq
row3iq_6_est <- round(post2imp[9,12],2)
row3iq_6_95l <- round(post2imp[9,12]-1.96*post2imp[9,6],2)
row3iq_6_95u <- round(post2imp[9,12]+1.96*post2imp[9,6],2)
row3iq_6_p <- post2imp[9,9]
row3iq_6 <- cbind(row3iq_6_est, row3iq_6_95l, row3iq_6_95u, row3iq_6_p)

# get row 6 table 3
row3_6 <- cbind(row3aa_6, row3iq_6)

#----------------------------------------
# TERTIAIRY ANALYSIS: SINGLE INDICATORS
#----------------------------------------

# COGNITIVE MISMATCH

# seperate risk domains -> academisch achievement

## le
modle <- with(implist, lm(cm~ scale(lc_le) + age + gender))
lsle <- modelest(modle)
## ce
modce <- with(implist, lm(cm~ scale(lc_ce) + age + gender))
lsce <- modelest(modce)
## ip
modip <- with(implist, lm(cm~ scale(lc_ip) + age + gender))
lsip <- modelest(modip)
## pr
modpr <- with(implist, lm(cm~ scale(lc_pr) + age + gender))
lspr <- modelest(modpr)
## dv
moddv <- with(implist, lm(cm~ scale(lc_dv) + age + gender))
lsdv <- modelest(moddv)

# bind
row2_7to12 <- rbind(lsle, lsce, lsip, lspr, lsdv)
# rename the columns 
colnames(row2_7to12) <- names(row2_6[1,])

# multivariate analysis
modmultipost <- with(implist, lm(cm~scale(lc_le)+scale(lc_ce)+scale(lc_pr)+scale(lc_ip)+scale(lc_dv) + age + gender))
multivabpost <- summary(pool(modmultipost))
multivabpostDisc <- data.frame(multivabpost[2:6,2],
                               multivabpost[2:6,2]-1.96*multivabpost[2:6,3],
                               multivabpost[2:6,2]+1.96*multivabpost[2:6,3],
                               multivabpost[2:6,6])
colnames(multivabpostDisc) <- names(row2_6[1,])

# ACADEMIC ACHIEVEMENT 

## le
modle <- with(implist, lm(cito~ scale(lc_le) + age + gender))
lsle <- modelest(modle)
## ce
modce <- with(implist, lm(cito~ scale(lc_ce) + age + gender))
lsce <- modelest(modce)
## ip
modip <- with(implist, lm(cito~ scale(lc_ip) + age + gender))
lsip <- modelest(modip)
## pr
modpr <- with(implist, lm(cito~ scale(lc_pr) + age + gender))
lspr <- modelest(modpr)
## dv
moddv <- with(implist, lm(cito~ scale(lc_dv) + age + gender))
lsdv <- modelest(moddv)

# bind
row3aa_7to12 <- rbind(lsle, lsce, lsip, lspr, lsdv)
# rename the columns 
names(row3aa_7to12) <- names(row2_6[1,])

# multivariate analysis
modmultipost <- with(implist, lm(cito~scale(lc_le)+scale(lc_ce)+scale(lc_pr)+scale(lc_ip)+scale(lc_dv) + age + gender))
multivabpost <- summary(pool(modmultipost))
multivabpostcito <- data.frame(multivabpost[2:6,2],
                               multivabpost[2:6,2]-1.96*multivabpost[2:6,3],
                               multivabpost[2:6,2]+1.96*multivabpost[2:6,3],
                               multivabpost[2:6,6])
colnames(multivabpostcito) <- names(row2_6[1,])

# COGNITIVE PERFORMANCE

## le
modle <- with(implist, lm(iq~ scale(lc_le) + age + gender))
lsle <- modelest(modle)
## ce
modce <- with(implist, lm(iq~ scale(lc_ce) + age + gender))
lsce <- modelest(modce)
## ip
modip <- with(implist, lm(iq~ scale(lc_ip) + age + gender))
lsip <- modelest(modip)
## pr
modpr <- with(implist, lm(iq~ scale(lc_pr) + age + gender))
lspr <- modelest(modpr)
## dv
moddv <- with(implist, lm(iq~ scale(lc_dv) + age + gender))
lsdv <- modelest(moddv)

# bind
row3iq_7to12 <- rbind(lsle, lsce, lsip, lspr, lsdv)
# rename the columns 
colnames(row3iq_7to12) <- names(row2_6[1,])

# multivariate analysis
modmultipost <- with(implist, lm(iq~scale(lc_le)+scale(lc_ce)+scale(lc_pr)+scale(lc_ip)+scale(lc_dv) + age + gender))
multivabpost <- summary(pool(modmultipost))
multivabpostiq <- data.frame(multivabpost[2:6,2],
                               multivabpost[2:6,2]-1.96*multivabpost[2:6,3],
                               multivabpost[2:6,2]+1.96*multivabpost[2:6,3],
                               multivabpost[2:6,6])
colnames(multivabpostiq) <- names(row2_6[1,])


#########################################
# PRENATAL STUFF
#########################################

# assess model fit base model
basemodelpre <- 'r1 =~ life_events_m + contextual_m + personal_stress_m + interpersonal_m '

# model summary with imputations
mymodelfit_prebaseimp<- runMI(basemodelpre, data = implist, fun = "sem", estimator = "MLR", std.lv = T)
prebaseimp <- summary(mymodelfit_prebaseimp, standardized=T, rsq=T)

# model fit and loadings for figure 1
fig1pre <- list(fitmeasures(mymodelfit_prebaseimp)[fitmeas], round(prebaseimp[1:4,11],2))

# predict with missings
predictpre <- sem(basemodelpre, fixed.x = F, missing='fiml', data = datarisk, std.lv=T)
datarisk$riskpre_missings <- predict(predictpre)

# get all predictions; do this for each imputed data set seperately
pred <- matrix(nrow=n, ncol=m)
for (i in 1:m){
  model_pred <- sem(basemodelpre, fixed.x = F, missing='fiml', data = complete(implist,action=i), std.lv=T)
  pred[,i] <- predict(model_pred)
}

# get for each row the mean and predict risk without missings
datarisk$riskpre <- apply(pred, 1, mean)

#----------------------------------------
# PRIMARY ANALYSIS: COGNITIVE MISMATCH
#----------------------------------------

# cfa model primary analysis
cfa.model_pre1 <- '                                                           

# latent variable modelling 
riskpre =~ life_events_m + contextual_m + personal_stress_m + interpersonal_m

# primary analysis cognitive mismatch
cm ~ riskpre + age + gender
'

# get model summary with imputations -> report this one
mymodelfit_pre1imp<- runMI(cfa.model_pre1, data = implist, fun = "sem", estimator = "MLR", std.lv = T)
(pre1imp <- summary(mymodelfit_pre1imp, standardized=T, rsq=T))

# get fit measures for primary model
fitmeasures(mymodelfit_pre1imp)

# get stuff for table2
row2_1_est <- round(pre1imp[5,12],2)
row2_1_95l <- round(pre1imp[5,12]-1.96*pre1imp[5,6],2)
row2_1_95u <- round(pre1imp[5,12]+1.96*pre1imp[5,6],2)
row2_1_p <- pre1imp[5,9]
row2_1 <- cbind(row2_1_est, row2_1_95l, row2_1_95u, row2_1_p)

#----------------------------------------
# SECONDARY ANALYSIS: CITO AND iq
#----------------------------------------

# cfa model
cfa.model_pre2 <- '                                                           

# latent variable modelling 
r1 =~ life_events_m + contextual_m + personal_stress_m + interpersonal_m

# secondary analysis
cito ~ r1 + age + gender
iq ~ r1 + age + gender'

# get model summary with imputations -> report this one
mymodelfit_pre2imp<- runMI(cfa.model_pre2, data = implist, fun = "sem", estimator = "MLR", std.lv = T)
(pre2imp <- summary(mymodelfit_pre2imp, standardized=T, rsq=T))

# get fit measures for primary model
fitmeasures(mymodelfit_pre2imp)[fitmeas]

# get stuff for table3: academic achievement
row3aa_1_est <- round(pre2imp[5,12],2)
row3aa_1_95l <- round(pre2imp[5,12]-1.96*pre2imp[5,6],2)
row3aa_1_95u <- round(pre2imp[5,12]+1.96*pre2imp[5,6],2)
row3aa_1_p <- pre2imp[5,9]
row3aa_1 <- cbind(row3aa_1_est, row3aa_1_95l, row3aa_1_95u, row3aa_1_p)

# get stuff for table3: cognition/iq
row3iq_1_est <- round(pre2imp[8,12],2)
row3iq_1_95l <- round(pre2imp[8,12]-1.96*pre2imp[8,6],2)
row3iq_1_95u <- round(pre2imp[8,12]+1.96*pre2imp[8,6],2)
row3iq_1_p <- pre2imp[8,9]
row3iq_1 <- cbind(row3iq_1_est, row3iq_1_95l, row3iq_1_95u, row3iq_1_p)

# get row 6 table 3
row3_1 <- cbind(row3aa_1, row3iq_1)

#----------------------------------------
# PERFORM TERTIAIRY ANALYSIS 
#----------------------------------------

# COGNITIVE MISMATCH

# seperate risk domains -> academisch achievement

## le
modle <- with(implist, lm(cm~ scale(life_events_m) + age + gender))
lsle <- modelest(modle)
## ce
modce <- with(implist, lm(cm~ scale(contextual_m) + age + gender))
lsce <- modelest(modce)
## ip
modpr <- with(implist, lm(cm~ scale(personal_stress_m) + age + gender))
lspr <- modelest(modip)
## pr
modip <- with(implist, lm(cm~ scale(interpersonal_m) + age + gender))
lsip <- modelest(modpr)

# bind
row2_2to5 <- rbind(lsle, lsce, lsip, lspr)
# rename the columns 
names(row2_2to5) <- names(row2_6[1,])

# multivariate analysis
modmultipre <- with(implist, lm(cm~scale(life_events_m)+scale(contextual_m)+scale(personal_stress_m)+scale(interpersonal_m) + age + gender))
multivabpre <- summary(pool(modmultipre))
multivabpreDisc <- data.frame(multivabpre[2:5,2],
                               multivabpre[2:5,2]-1.96*multivabpre[2:5,3],
                               multivabpre[2:5,2]+1.96*multivabpre[2:5,3],
                               multivabpre[2:5,6])
names(multivabpreDisc) <- names(row2_6[1,])


# ACADEMIC ACHIEVEMENT 

# seperate risk domains -> academisch achievement

## le
modle <- with(implist, lm(cito~ scale(life_events_m) + age + gender))
lsle <- modelest(modle)
## ce
modce <- with(implist, lm(cito~ scale(contextual_m) + age + gender))
lsce <- modelest(modce)
## ip
modpr <- with(implist, lm(cito~ scale(personal_stress_m) + age + gender))
lspr <- modelest(modip)
## pr
modip <- with(implist, lm(cito~ scale(interpersonal_m) + age + gender))
lsip <- modelest(modpr)

# bind
row3aa_2to5 <- rbind(lsle, lsce, lsip, lspr)
# rename the columns 
names(row3aa_2to5) <- names(row2_6[1,])

# multivariate analysis
modmultipre <- with(implist, lm(cito~scale(life_events_m)+scale(contextual_m)+scale(personal_stress_m)+scale(interpersonal_m) + age + gender))
multivabpre <- summary(pool(modmultipre))
multivabprecito <- data.frame(multivabpre[2:5,2],
                              multivabpre[2:5,2]-1.96*multivabpre[2:5,3],
                              multivabpre[2:5,2]+1.96*multivabpre[2:5,3],
                              multivabpre[2:5,6])
names(multivabprecito) <- names(row2_6[1,])

# COGNITIVE PERFORMANCE

## le
modle <- with(implist, lm(iq~ scale(life_events_m) + age + gender))
lsle <- modelest(modle)
## ce
modce <- with(implist, lm(iq~ scale(contextual_m) + age + gender))
lsce <- modelest(modce)
## ip
modpr <- with(implist, lm(iq~ scale(personal_stress_m) + age + gender))
lspr <- modelest(modip)
## pr
modip <- with(implist, lm(iq~ scale(interpersonal_m) + age + gender))
lsip <- modelest(modpr)

# bind
row3iq_2to5 <- rbind(lsle, lsce, lsip, lspr)
# rename the columns 
names(row3iq_2to5) <- names(row2_6[1,])

# multivariate analysis
modmultipre <- with(implist, lm(iq~scale(life_events_m)+scale(contextual_m)+scale(personal_stress_m)+scale(interpersonal_m) + age + gender))
multivabpre <- summary(pool(modmultipre))
multivabpreiq <- data.frame(multivabpre[2:5,2],
                              multivabpre[2:5,2]-1.96*multivabpre[2:5,3],
                              multivabpre[2:5,2]+1.96*multivabpre[2:5,3],
                              multivabpre[2:5,6])
names(multivabpreiq) <- names(row2_6[1,])

#########################################
# PRENATAL + POSTNATAL
#########################################

# BASE MODEL 

# cfa model
cfa.model_exp1 <- '                                                           

# latent variable modelling 
riskpre =~ life_events_m + contextual_m + personal_stress_m + interpersonal_m
riskpost =~ lc_le + lc_ce + lc_pr + lc_ip + lc_dv

# covariances
riskpre~~riskpost
'

# get model summary with imputations -> report this one
mymodelfit_exp1imp<- runMI(cfa.model_exp1, data = implist, fun = "sem", estimator = "MLR", std.lv = T)
(corprepost <- summary(mymodelfit_exp1imp, standardized=T, rsq=T))

# IQ-ACHIEVEMENT DISCREPANCY 

# cfa model
cfa.model_exp1 <- '                                                           

# latent variable modelling 
riskpre =~ life_events_m + contextual_m + personal_stress_m + interpersonal_m
riskpost =~ lc_le + lc_ce + lc_pr + lc_ip + lc_dv

# covariances
riskpre~~riskpost

# regressions
cm ~ riskpre + riskpost + age + gender
'

# get model summary with imputations 
mymodelfit_cm1imp <- runMI(cfa.model_exp1, data = implist, fun = "sem", estimator = "MLR", std.lv = T)
(multivabdiscrep <- summary(mymodelfit_cm1imp, standardized=T, rsq=T))

# get fit measures for primary model
fitmeasures(mymodelfit_pre1imp)

# COGNTIVE PERFORMANCE + ACADEMIC ACHIEVEMENT

# cfa model
cfa.model_exp1 <- '                                                           

# latent variable modelling 
riskpre =~ life_events_m + contextual_m + personal_stress_m + interpersonal_m
riskpost =~ lc_le + lc_ce + lc_pr + lc_ip + lc_dv

cito ~ riskpre+riskpost+age+gender
iq ~ riskpre+riskpost+age+gender

# covariances
riskpre~~riskpost
'

# get model summary with imputations 
mymodelfit_exp1imp<- runMI(cfa.model_exp1, data = implist, fun = "sem", estimator = "MLR", std.lv = T)
(multivabiqAA <- summary(mymodelfit_exp1imp, standardized=T, rsq=T))

#########################################
# UNDERACHIEVERS VS. OVERACHIEVERS
#########################################

# COGNITIVE MISMATCH

# dichotomize underachievers and overachievers
datarisk$status <- as.factor(recode(datarisk$cm, 'lo:-1=1; 1:hi=3; else=2'))
datarisk$gradient <- pnorm(datarisk$cm)

# standardize
datarisk$riskpre_std <- scale(datarisk$riskpre)
datarisk$riskpost_std <- scale(datarisk$riskpost)

# numbers
table(datarisk$status)

# prenatal
preplot <- ggplot(data=datarisk, aes(x=riskpre_std, y=cm, group=status, color=status))+ 
  stat_smooth(method='lm', formula = y~x) +
  scale_color_discrete(name = "Direction of the discrepancy", 
                       labels = c("Underachievement (n = 341; bottom line)", "Normal achievement (n = 1,729; middle line)", 'Overachievement (n = 331; upper line)')) +
  theme(legend.position = "left") +
#  ylim(-4,4)+
#  xlim(-1.5, 6.5)+
  geom_point(alpha = 0.25) +
  guides(color = guide_legend(reverse = T)) +
  ggtitle("Prenatal early life stress") +
  xlab("Prenatal early life stress") + ylab("IQ-achievement disceprancy") 


# postnatal
postplot <- ggplot(data=datarisk, aes(x=riskpost_std, y=cm, group=status, color=status))+ 
  stat_smooth(method='lm', formula = y~x) +
  scale_color_discrete(name = "Direction of the discrepancy", 
                       labels = c("Underachievement (n = 341; bottom line)", "Normal achievement (n = 1,729; middle line)", 'Overachievement (n = 331; upper line)')) +
  #  ylim(-4,4)+
#  xlim(-1.5, 6.5)+
  geom_point(alpha = 0.25) +
  guides(color = guide_legend(reverse = T)) +
  ggtitle("Postnatal early life stress") +
  xlab("Postnatal early life stress") + ylab("IQ-achievement disceprancy") 

# plot status, r1 and cm
require(gridExtra)
grid.arrange(preplot, postplot, ncol=2)

#----------------------------------------
# S1 and S2 missingsness
#----------------------------------------

missings_prenatal <- round((apply(datarisk[,c("family_member_died","friend_relative_died","family_member_ill","unemployed", "mother_work_study_problems","moved_house",
                                              "victim_robbery","health","pregnancy_planned","blood_loss","admitted_to_hospital","examination","obstetric_care",
                                              "baby_worried","pregnancy_worried","income_reduced","housing_adequacy","financial_problems","housing_basic_living",
                                              "housing_defects","financial_difficulties","gsi_mdich","agem",'edu', "criminal_record_m","forcemdich",
                                              "publicordermdich","difficulties_partner", "difficulties_family_friend","difficulties_contacts","mardich.y",
                                              "famsize","divorce", "family_affection", "family_plans","family_acceptance","family_talk_sadness","family_talk_worries",
                                              "family_acception","family_painful_feelings","family_decisions_problems","family_decisions","family_trust","family_conflict",     
                                              "family_support")], 2, function(x){sum(is.na(x))/length(x)*100})),1)

missings_postnatal <- round(apply(datarisk[,c('gr1065v1.g0201165','gr1065v1.g0200365',"le4", "le5", "le6", "le1", "le2", "le3", "le23", "le17", "le7", "le24", "rep",
                                              'gr1065v1.g0200565','made','fidi','le9','trpa','inon','inch','empon','empch',
                                              'edum','edup','im','psif','dm','df','am','af','agem','agep',
                                              'gr1065v1.g0201965','mari','fams','fad5','fad9mdich','fad9fdich','le11','le12','le13','le16','le14',
                                              'parentingv1.harsh80_m','parentingv1.harsh80_p','bullying','le18','le19','le20','le21','le22')], 2, function(x){sum(is.na(x))/length(x)*100}),1)


#----------------------------------------
# MULTINOMIAL
#----------------------------------------

library(nnet)

# recode status
datarisk$status <- as.factor(recode(datarisk$cm, 'lo:-1=2; 1:hi=3; else=1'))

# multinomal model POSTNATAL
test <- multinom(status ~ riskpost + age + gender, data = datarisk)
(summarypost <- summary(test))

# get p-values
z <- summarypost$coefficients / summarypost$standard.errors
(pmulpost <- 1-pnorm(abs(z), 0,1)) * 2

# put in in the list
multinompost <- list(summarypost$coefficients[,2], exp(summarypost$coefficients[,2]), pmulpost[,2])

# multinomal model PRENATAL
test <- multinom(status ~ riskpre + age + gender, data = datarisk)
(summarypre <- summary(test))

# get p-values
z <- summarypre$coefficients / summarypre$standard.errors
(pmulpre <- 1-pnorm(abs(z), 0,1)) * 2

# put in in the list
multinompre <- list(summarypre$coefficients[,2], exp(summarypre$coefficients[,2]), pmulpre[,2])

# TABLE S3: MEAN NUMBER OF EVENTS

# prenatal
rowS3_1 <- meansd(datarisk$riskpre_std)
rowS3_2 <- meansd(datarisk$life_events_m*15)
rowS3_3 <- meansd(datarisk$contextual_m*6)
rowS3_4 <- meansd(datarisk$personal_stress_m*6)
rowS3_5 <- meansd(datarisk$interpersonal_m*18)

# postnatal
rowS3_6 <- meansd(datarisk$riskpost_std)
rowS3_7 <- meansd(datarisk$lc_le*13)
rowS3_8 <- meansd(datarisk$lc_ce*9)
rowS3_9 <- meansd(datarisk$lc_pr*10)
rowS3_10 <- meansd(datarisk$lc_ip*11)
rowS3_11 <- meansd(datarisk$lc_dv*8)

# demographics
rowS3_12 <- meansd(datarisk$cm)
rowS3_13 <- meansd(datarisk$iq)
rowS3_14 <- meansd(datarisk$cito)
rowS3_15 <- meansd(datarisk$age)

# make cols
tableS3 <- rbind(rowS3_1, rowS3_2, rowS3_3, rowS3_4, rowS3_5, rowS3_6, rowS3_7, rowS3_8, rowS3_9, rowS3_10, rowS3_11, rowS3_12, rowS3_13, rowS3_14, rowS3_15)

# add range
rowS3_1_b <- c(min(datarisk$riskpre_std, na.rm=T), max(datarisk$riskpre_std, na.rm = T))
rowS3_2_b <- c(min(datarisk$life_events_m*15, na.rm=T), max(datarisk$life_events_m*15, na.rm = T))
rowS3_3_b <- c(min(datarisk$contextual_m*6, na.rm=T), max(datarisk$contextual_m*6, na.rm = T))
rowS3_4_b <- c(min(datarisk$personal_stress_m*6, na.rm=T), max(datarisk$personal_stress_m*6, na.rm = T))
rowS3_5_b <- c(min(datarisk$interpersonal_m*18, na.rm=T), max(datarisk$interpersonal_m*18, na.rm = T))
rowS3_6_b <- c(min(datarisk$riskpost_std, na.rm=T), max(datarisk$riskpost_std, na.rm = T))
rowS3_7_b <- c(min(datarisk$lc_le*13, na.rm=T), max(datarisk$lc_le*13, na.rm = T))
rowS3_8_b <- c(min(datarisk$lc_ce*9, na.rm=T), max(datarisk$lc_ce*9, na.rm = T))
rowS3_9_b <- c(min(datarisk$lc_pr*10, na.rm=T), max(datarisk$lc_pr*10, na.rm = T))
rowS3_10_b <- c(min(datarisk$lc_ip*11, na.rm=T), max(datarisk$lc_ip*11, na.rm = T))
rowS3_11_b <- c(min(datarisk$lc_dv*8, na.rm=T), max(datarisk$lc_dv*8, na.rm = T))
rowS3_12_b <- c(min(datarisk$cm, na.rm=T), max(datarisk$cm, na.rm = T))
rowS3_13_b <- c(min(datarisk$iq, na.rm=T), max(datarisk$iq, na.rm = T))
rowS3_14_b <- c(min(datarisk$cito, na.rm=T), max(datarisk$cito, na.rm = T))
rowS3_15_b <- c(min(datarisk$age, na.rm=T), max(datarisk$age, na.rm = T))

# make cols
tableS3b <- rbind(rowS3_1_b, rowS3_2_b, rowS3_3_b, rowS3_4_b, rowS3_5_b, rowS3_6_b, 
                  rowS3_7_b, rowS3_8_b, rowS3_9_b, rowS3_10_b, rowS3_11_b, rowS3_12_b, rowS3_13_b, rowS3_14_b, rowS3_15_b)

# combine
tableS3_a <- cbind(tableS3b, tableS3)

# row and colnames
rownames(tableS3_a) <- c('Prenatal ELS','Pre Life events', 'Pre Contextual risk', 'Pre Parental risk', 'Pre interpersonal risk',
                         'Postnatal ELS','Post Life events', 'Post Contextual risk', 'Post Parental risk', 'Post interpersonal risk', 'Post Direct victimization',
                         'CM', 'IQ', 'Cito', 'Age')
colnames(tableS3_a) <- c( 'min', 'max', 'mean', 'sd')

tableS3_a <- round(tableS3_a, 2)

#----------------------------------------
# COMBINE INFO TO TABLES
#----------------------------------------

# set working directory so you can find the tables
setwd('O:/medewerkers/042647 Schuurmans, I/Project_1_ELS_CM/Tables')

# TABLE 1 # calculate missings ehtnicity
write.xlsx(table1a, 'Proj1_CMandELS_Table1.xlsx')
cogn <-  read_sav("O:/medewerkers/042647 Schuurmans, I/DATA/WISC/20210305/03052021_WISC.sav")
iq <- merge(datarisk, cogn, by.x = 'idc', by.y = 'IDC', all.x = T)
mean(iq$WISC13_FSIQ); sd(iq$WISC13_FSIQ)

# FIGURE 1
order <- c('First prenatal, than postnatal information. Order factor loadings: Life events, Contextual risk, Parental risk, interpersonal risk, (Direct victimization)')
figure1 <- list(order, fig1pre, fig1post)
sink('Proj1_CMandELS_Figure1.txt'); print(figure1); sink()

# TABLE 2
table2 <- rbind(row2_1[1,], row2_2to5, row2_6[1,], row2_7to12)
rownames(table2) <- c('Prenatal risk', 'Pre Life events', 'Pre Contextual risk', 'Pre Parental risk', 'Pre interpersonal risk',
                      'Postnatal risk', 'Post Life events', 'Post Contextual risk', 'Post Parental risk', 'Post interpersonal risk', 'Post Direct victimization')
table2 <- as.data.frame(table2)
table2$p_adjusted <- NA
for (i in 1:length(table2[,4])){table2$p_adjusted[i] <- round(p.adjust(table2[i,4], method = 'BH', n = 13),3)}
write.xlsx(table2, 'Proj1_CMandELS_Table2.xlsx')

# TABLE 3
row3_2to5 <- cbind(row3aa_2to5, row3iq_2to5)
row3_7to12 <- cbind(row3aa_7to12, row3iq_7to12)
table3 <- rbind(row3_1[1,], row3_2to5, row3_6[1,], row3_7to12)
rownames(table3) <- rownames(table2) 
table3 <- as.data.frame(table3)
table3$p_adjusted_AA <- table3$p_adjusted_IQ <- NA
for (i in 1:length(table3[,8])){table3$p_adjusted_IQ[i] <- round(p.adjust(table3[i,8], method = 'BH', n = 13),3)}
for (i in 1:length(table3[,4])){table3$p_adjusted_AA[i] <- round(p.adjust(table3[i,4], method = 'BH', n = 13),3)}
table3_org <- table3[,c(5:9, 1:4, 10)]
write.xlsx(table3_org, 'Proj1_CMandELS_Table3.xlsx')

# SUPPLEMENT

# FIGURE S1 # will not work! you need to run de rmarkdown first
sink('Proj1_CMandELS_FigureS1.txt'); print(figureS1); sink()

# TABLE S3
write.xlsx(tableS3_a, 'Proj1_CMandELS_TableS3_a.xlsx')
# IQ info
mean(iq$WISC13_FSIQ); sd(iq$WISC13_FSIQ)
min(iq$WISC13_FSIQ); max(iq$WISC13_FSIQ)
# Sex info
table(datarisk$gender); prop.table(table(datarisk$gender))  #2 is girl

# TABLE S3
datarisk$sex <- datarisk$gender - 1
tableS3_b <- round(cor(datarisk[,c('riskpre','life_events_m','contextual_m','personal_stress_m','interpersonal_m', 
                'riskpost','lc_le','lc_ce','lc_pr','lc_ip', 'lc_dv', 'cm', 'iq','cito', 'age', 'sex')], use = 'pairwise.complete.obs'), 2)
tableS3_b[upper.tri(tableS3_b, diag = T)] <- NA
write.xlsx(tableS3_b, 'Proj1_CMandELS_TableS3_b.xlsx')

# TABLE S3
pvalue_cor <- function(dat, cortable){
  pvalues <- matrix(NA, nrow(cortable), ncol(cortable))
  for (i in 1:nrow(cortable)) {
    for (j in 1:ncol(cortable)) {
      pvalues[i,j] <- cor.test(datarisk[,rownames(cortable)[i]], 
                               datarisk[,rownames(cortable)[j]])$p.value
    }
  }
  return(pvalues)
}

tableS3_c <- round(pvalue_cor(datarisk, tableS3_b),4)
tableS3_c[upper.tri(tableS3_c, diag = T)] <- NA
write.xlsx(tableS3_c, 'Proj1_CMandELS_TableS3_c.xlsx')

# table S4
pre <- cbind(multivabpreDisc, multivabpreiq, multivabprecito)
post <- cbind(multivabpostDisc, multivabpostiq, multivabpostcito)
all <- rbind(pre, post)
all[,c(1:3, 5:7, 9:11)] <- round(all[,c(1:3, 5:7, 9:11)], 2)
all[,c(4,8,12)] <- round(all[,c(4,8,12)], 3)
all <- as.data.frame(all)
all$p_cm <- apply(as.data.frame(all[,4]), 1, function(x) round(as.numeric(p.adjust(x, method =  "BH", n = 13)),3))
all$p_iq <- apply(as.data.frame(all[,8]), 1, function(x) round(as.numeric(p.adjust(x, method =  "BH", n = 13)),3))
all$p_aa <- apply(as.data.frame(all[,12]), 1, function(x) round(as.numeric(p.adjust(x, method =  "BH", n = 13)),3))
all_org <- all[,c(1:4, 13, 5:8, 14, 9:12, 15)]
write.xlsx(all_org, 'Proj1_CMandELS_TableS4.xlsx')

# S5 is done by rerunning large parts of the code with implist_sens_dutch_20201207


# RESULTS

# correlation between risk prenatal and risk postnatal
corprepost 

# missingsness 
# check out risk item with highest % of missingness manually by opening missings (harsh parenting dad in this case)
missings <- sort(apply(datarisk[,c('le18','le19','le20','le21','le22','parentingv1.harsh80_m','parentingv1.harsh80_p','bullying', 
                                   'edum','edup','agem','agep','im','am','dm','psif','df','af',
                                   'le11','le12','le13','le14','le16','gr1065v1.g0201965','mari','fams','fad5','fad9mdich','fad9fdich',
                                   'gr1065v1.g0200565','made','fidi','le9','trpa','inon','inch','empon','empch',
                                   'le1','le2','le3','le4','le5','le6','le7','rep','le17','le23','le24','gr1065v1.g0201165','gr1065v1.g0200365',
                                   "divorce","moved_house","unemployed","friend_relative_died","family_member_died","family_member_ill",         
                                   "admitted_to_hospital","health","mother_work_study_problems","blood_loss","examination","victim_robbery",         
                                   "pregnancy_planned","baby_worried","obstetric_care","pregnancy_worried","housing_basic_living","housing_defects",          
                                   "housing_adequacy","financial_problems","financial_difficulties","income_reduced","criminal_record_m","mardich.y",                   
                                   "agem","gsi_mdich","publicordermdich","forcemdich","difficulties_partner","difficulties_family_friend","difficulties_contacts",'edu',
                                   "family_affection","famsize","family_decisions_problems","family_acceptance","family_acception","family_trust",              
                                   "family_painful_feelings","family_decisions","family_conflict","family_plans","family_talk_sadness","family_talk_worries",     
                                   "family_support")], 2, function(x){sum(is.na(x))/length(x)*100}))
missings
mean(missings)

# model fit
fig1_pre_fitindices
fig1_post_fitindices

# how many teacher reports cito
table(datarisk$reportcito)

# ages and all for cito and iq
tableS3

# correlation sex and the IQ-achievement discrepancy
cor.test(datarisk$sex, datarisk$cm)

# most risk experiences in area with highest mean (prop)
# 96 assessed risk factors
# sum prenatal
datarisk$sumpre <- datarisk$life_events_m*15 + datarisk$contextual_m*6 + datarisk$personal_stress_m*5 + datarisk$interpersonal_m*18
datarisk$sumpost <- datarisk$lc_le*13 +datarisk$lc_ce*9 +datarisk$lc_ip*11 +datarisk$lc_pr*10 +datarisk$lc_dv*8
sumall <- datarisk$sumpre+datarisk$sumpost
meansd(sumall)

# multinomreg
multinompre # 2 is status = 1 compared to status = 2 
multinompost# first list is coef, second is OR, third is p

# sex interaction
sexintpre <- lm(cm ~ riskpre*gender + age, data = datarisk)
summary(sexintpre)
sexintpost <- lm(cm ~ riskpost*gender + age, data = datarisk)
summary(sexintpost)


