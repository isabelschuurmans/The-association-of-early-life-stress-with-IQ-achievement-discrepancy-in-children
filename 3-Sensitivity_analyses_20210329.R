#----------------------------------------
# Get new implist
#----------------------------------------

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

# select mids object only containing dutch kids
implistcomp_eth <- implist_c

## first delete all kids with missing ehtnicity
for (j in 1:table(is.na(implistcomp_eth$ethnicity))[2]){
  n <- match(NA, implistcomp_eth$ethnicity)
  m <- nrow(implistcomp_eth)/(nrow(datarisk)+1-j)-1
  y <- rep(NA, m)
  y[1] <- n
  for (i in 1:m){
    y[i+1] <- n + i*nrow(implistcomp_eth)/(m+1)
  }
  implistcomp_eth <- implistcomp_eth[-y,] 
}

## then delete all without non-dutch ethnicity
implistcomp_eth <- implistcomp_eth[-which(implistcomp_eth$ethnicity!=0),]

# convert back
implist_eth <- as.mids(implistcomp_eth)

#----------------------------------------
# Postnatal
#----------------------------------------

# LAVAAN --------------------------------

## IQ-achievement discrepancy
mymodelfit_post1imp_eth <- runMI(cfa.model_post1, data = implist_eth, fun = "sem", estimator = "MLR", std.lv = T)
(post1imp_eth  <- summary(mymodelfit_post1imp_eth, standardized=T, rsq=T))

# get stuff for table S5: IQ-achievement discrepancy
rowS5_6_est <- round(post1imp_eth[6,12],2)
rowS5_6_95l <- round(post1imp_eth[6,12]-1.96*post1imp_eth[6,6],2)
rowS5_6_95u <- round(post1imp_eth[6,12]+1.96*post1imp_eth[6,6],2)
rowS5_6_p <- post1imp_eth[6,9]
rowS5_6 <- cbind(rowS5_6_est, rowS5_6_95l, rowS5_6_95u, rowS5_6_p)

## IQ and AA
mymodelfit_post2imp_eth<- runMI(cfa.model_post2, data = implist_eth, fun = "sem", estimator = "MLR", std.lv = T)
(post2imp_eth <- summary(mymodelfit_post2imp_eth, standardized=T, rsq=T))

# get stuff for tableS5: academic achievement
rowS5aa_6_est <- round(post2imp_eth[6,12],2)
rowS5aa_6_95l <- round(post2imp_eth[6,12]-1.96*post2imp_eth[6,6],2)
rowS5aa_6_95u <- round(post2imp_eth[6,12]+1.96*post2imp_eth[6,6],2)
rowS5aa_6_p <- post2imp_eth[6,9]
rowS5aa_6 <- cbind(rowS5aa_6_est, rowS5aa_6_95l, rowS5aa_6_95u, rowS5aa_6_p)

# get stuff for tableS5: cognition/iq
rowS5iq_6_est <- round(post2imp_eth[9,12],2)
rowS5iq_6_95l <- round(post2imp_eth[9,12]-1.96*post2imp_eth[9,6],2)
rowS5iq_6_95u <- round(post2imp_eth[9,12]+1.96*post2imp_eth[9,6],2)
rowS5iq_6_p <- post2imp_eth[9,9]
rowS5iq_6 <- cbind(rowS5iq_6_est, rowS5iq_6_95l, rowS5iq_6_95u, rowS5iq_6_p)

# get row 6 table S5
rowS5_6 <- cbind(rowS5_6, rowS5iq_6, rowS5aa_6)

# LINEAR REGRESSION ---------------------

## IQ-achievement discrepancy
modle <- with(implist_eth, lm(cm~ scale(lc_le) + age + gender))
lsle <- modelest(modle)
modce <- with(implist_eth, lm(cm~ scale(lc_ce) + age + gender))
lsce <- modelest(modce)
modip <- with(implist_eth, lm(cm~ scale(lc_ip) + age + gender))
lsip <- modelest(modip)
modpr <- with(implist_eth, lm(cm~ scale(lc_pr) + age + gender))
lspr <- modelest(modpr)
moddv <- with(implist_eth, lm(cm~ scale(lc_dv) + age + gender))
lsdv <- modelest(moddv)

# bind
rowS5_7to12 <- rbind(lsle, lsce, lsip, lspr, lsdv)
colnames(rowS5_7to12) <- names(row2_6[1,])

## Academic achievement
modle <- with(implist_eth, lm(cito~ scale(lc_le) + age + gender))
lsle <- modelest(modle)
modce <- with(implist_eth, lm(cito~ scale(lc_ce) + age + gender))
lsce <- modelest(modce)
modip <- with(implist_eth, lm(cito~ scale(lc_ip) + age + gender))
lsip <- modelest(modip)
modpr <- with(implist_eth, lm(cito~ scale(lc_pr) + age + gender))
lspr <- modelest(modpr)
moddv <- with(implist_eth, lm(cito~ scale(lc_dv) + age + gender))
lsdv <- modelest(moddv)

# bind
rowS5aa_7to12 <- rbind(lsle, lsce, lsip, lspr, lsdv)
names(rowS5aa_7to12) <- names(row2_6[1,])

## IQ
modle <- with(implist_eth, lm(iq~ scale(lc_le) + age + gender))
lsle <- modelest(modle)
modce <- with(implist_eth, lm(iq~ scale(lc_ce) + age + gender))
lsce <- modelest(modce)
modip <- with(implist_eth, lm(iq~ scale(lc_ip) + age + gender))
lsip <- modelest(modip)
modpr <- with(implist_eth, lm(iq~ scale(lc_pr) + age + gender))
lspr <- modelest(modpr)
moddv <- with(implist_eth, lm(iq~ scale(lc_dv) + age + gender))
lsdv <- modelest(moddv)

# bind
rowS5iq_7to12 <- rbind(lsle, lsce, lsip, lspr, lsdv)
colnames(row3iq_7to12) <- names(row2_6[1,])

#----------------------------------------
# Prenatal
#----------------------------------------

# LAVAAN --------------------------------

## IQ-achievement discrepancy
mymodelfit_pre1imp_eth <- runMI(cfa.model_pre1, data = implist_eth, fun = "sem", estimator = "MLR", std.lv = T)
(pre1imp_eth  <- summary(mymodelfit_pre1imp_eth, standardized=T, rsq=T))

# get stuff for table S5: IQ-achievement discrepancy
rowS5_1_est <- round(pre1imp_eth[5,12],2)
rowS5_1_95l <- round(pre1imp_eth[5,12]-1.96*pre1imp_eth[5,6],2)
rowS5_1_95u <- round(pre1imp_eth[5,12]+1.96*pre1imp_eth[5,6],2)
rowS5_1_p <- pre1imp_eth[5,9]
rowS5_1 <- cbind(rowS5_1_est, rowS5_1_95l, rowS5_1_95u, rowS5_1_p)

## IQ and AA
mymodelfit_pre2imp_eth<- runMI(cfa.model_pre2, data = implist_eth, fun = "sem", estimator = "MLR", std.lv = T)
(pre2imp_eth <- summary(mymodelfit_pre2imp_eth, standardized=T, rsq=T))

# get stuff for tableS5: academic achievement
rowS5aa_1_est <- round(pre2imp_eth[5,12],2)
rowS5aa_1_95l <- round(pre2imp_eth[5,12]-1.96*pre2imp_eth[5,6],2)
rowS5aa_1_95u <- round(pre2imp_eth[5,12]+1.96*pre2imp_eth[5,6],2)
rowS5aa_1_p <- pre2imp_eth[5,9]
rowS5aa_1 <- cbind(rowS5aa_1_est, rowS5aa_1_95l, rowS5aa_1_95u, rowS5aa_1_p)

# get stuff for tableS5: cognition/iq
rowS5iq_1_est <- round(pre2imp_eth[8,12],2)
rowS5iq_1_95l <- round(pre2imp_eth[8,12]-1.96*pre2imp_eth[8,6],2)
rowS5iq_1_95u <- round(pre2imp_eth[8,12]+1.96*pre2imp_eth[8,6],2)
rowS5iq_1_p <- pre2imp_eth[8,9]
rowS5iq_1 <- cbind(rowS5iq_1_est, rowS5iq_1_95l, rowS5iq_1_95u, rowS5iq_1_p)

# get row 1 table S5
rowS5_1 <- cbind(rowS5_1, rowS5iq_1, rowS5aa_1)

# LINEAR REGRESSION ---------------------

## IQ-achievement discrepancy
modle <- with(implist_eth, lm(cm~ scale(life_events_m) + age + gender))
lsle <- modelest(modle)
modce <- with(implist_eth, lm(cm~ scale(contextual_m) + age + gender))
lsce <- modelest(modce)
modpr <- with(implist_eth, lm(cm~ scale(personal_stress_m) + age + gender))
lspr <- modelest(modip)
modip <- with(implist_eth, lm(cm~ scale(interpersonal_m) + age + gender))
lsip <- modelest(modpr)

# bind
rowS5_2to5 <- rbind(lsle, lsce, lsip, lspr)
names(rowS5_2to5) <- names(row2_6[1,])

## Academic achievement
modle <- with(implist_eth, lm(cito~ scale(life_events_m) + age + gender))
lsle <- modelest(modle)
modce <- with(implist_eth, lm(cito~ scale(contextual_m) + age + gender))
lsce <- modelest(modce)
modpr <- with(implist_eth, lm(cito~ scale(personal_stress_m) + age + gender))
lspr <- modelest(modip)
modip <- with(implist_eth, lm(cito~ scale(interpersonal_m) + age + gender))
lsip <- modelest(modpr)

# bind
rowS5aa_2to5 <- rbind(lsle, lsce, lsip, lspr)
names(rowS5aa_2to5) <- names(row2_6[1,])

## IQ
modle <- with(implist_eth, lm(iq~ scale(life_events_m) + age + gender))
lsle <- modelest(modle)
modce <- with(implist_eth, lm(iq~ scale(contextual_m) + age + gender))
lsce <- modelest(modce)
modpr <- with(implist_eth, lm(iq~ scale(personal_stress_m) + age + gender))
lspr <- modelest(modip)
modip <- with(implist_eth, lm(iq~ scale(interpersonal_m) + age + gender))
lsip <- modelest(modpr)

# bind
rowS5iq_2to5 <- rbind(lsle, lsce, lsip, lspr)
names(rowS5iq_2to5) <- names(row2_6[1,])

#----------------------------------------
# Combine to table and write out
#----------------------------------------

# the n
table(datarisk$ethnicity)[1]

# combine to table
rowS5_2to5 <- cbind(rowS5_2to5, rowS5iq_2to5, rowS5aa_2to5)
rowS5_7to12 <- cbind(rowS5_7to12, rowS5iq_7to12, rowS5aa_7to12)
tableS5 <- rbind(rowS5_1[1,], rowS5_2to5, rowS5_6[1,], rowS5_7to12)
tableS5 <- as.data.frame(tableS5)
tableS5$p_cm <- apply(as.data.frame(tableS5[,4]), 1, function(x) round(as.numeric(p.adjust(x, method =  "BH", n = 13)),3))
tableS5$p_iq <- apply(as.data.frame(tableS5[,8]), 1, function(x) round(as.numeric(p.adjust(x, method =  "BH", n = 13)),3))
tableS5$p_aa <- apply(as.data.frame(tableS5[,12]), 1, function(x) round(as.numeric(p.adjust(x, method =  "BH", n = 13)),3))
tableS5_org <- tableS5[,c(1:4, 13, 5:8, 14, 9:12, 15)]
write.xlsx(tableS5_org, 'Proj1_CMandELS_TableS5.xlsx')
