# run this code first!!

######################
#### READ IN DATA ####
######################

# Open library
library(foreign)

# functions

## replace na 
replacenas <- function(dat){
  
  for (i in 1:length(dat)) {
    if (colnames(dat)[i] == "idm" | colnames(dat)[i] == "idc" | colnames(dat)[i] == "mother") {
      dat[,i] <- dat[,i]
    } else {
      dat[,i] <- ifelse(dat[,i] == 777 | dat[,i] == 888 | dat[,i] == 999, NA, dat[,i]) }
  } 
  return(dat)
}
  

## read in data quickly
readquick <- function(path){ # only works for spss
  dataframe <- read.spss(path, use.value.labels = F, to.data.frame = T)
  names(dataframe) <- tolower(names(dataframe))
  return(dataframe)
}

#--------

## Life Events Interview
LifeEvents <- read.spss("O:/Medewerkers/042647 Schuurmans, I/DATA/MOTHERTRAUMAINTERVIEW9_24112016.sav", use.value.labels = FALSE)
LifeEvents <- as.data.frame(LifeEvents)

# Exclude unreliable interviews
LifeEvents <- subset(LifeEvents, LifeEvents$unreliable == 0)

#--------

## BSI - see SyntaxBSI_date 
source("O:/Medewerkers/042647 Schuurmans, I/Project_1_ELS_CM/R Code/SyntaxBSI_20191230.R")

#--------

## FAD - see SyntaxFAD_date (run first!)
source("O:/Medewerkers/042647 Schuurmans, I/Project_1_ELS_CM/R Code/SyntaxFAD_20200104.R")

#--------

# RUN PARENTING BEHAVIOR SYNTAX FIRST

## Parenting behavior
Parentingv1 <- read.spss("O:/Medewerkers/042647 Schuurmans, I/DATA/Parenting 3 years of age_GR1065 F1F6 -GR1066 B1-B5_22112016.sav", use.value.labels = FALSE)
Parentingv1 <- as.data.frame(Parentingv1)

# Construct GR1065
Parenting <- data.frame(Parentingv1$IDC, Parentingv1$harsh80_p, Parentingv1$harsh80_m)
Parenting$IDC <- Parenting$Parentingv1.IDC

#--------

## GR1005-E6
GR1005 <- readquick("O:/Medewerkers/042647 Schuurmans, I/DATA/prenatal/GR1005-E_22112016.sav")

#--------

## GR1005-E6
Smoking <- readquick("O:/Medewerkers/042647 Schuurmans, I/DATA/prenatal/MATERNALSMOKING_22112016.sav")
# dichotomize smoking
Smoking$smoking <- ifelse(Smoking$smoke_all == 2, 3, Smoking$smoke_all)

#--------

## GR1003dep and GR1004dep
GR1003dep <- readquick("O:/Medewerkers/042647 Schuurmans, I/DATA/depression/GR1003-BSI D1_22112016.sav")
GR1004dep <- readquick("O:/Medewerkers/042647 Schuurmans, I/DATA/depression/GR1004-BSI G1_22112016.sav")

#--------

# BMI mother
BMI <- readquick('O:/Medewerkers/042647 Schuurmans, I/DATA/MOTHERANTHROPOMETRY_18022013.sav')

#--------

## GR1065G2
GR1065v1 <- read.spss("O:/Medewerkers/042647 Schuurmans, I/DATA/GR1065-G2_01072012.sav", use.value.labels = FALSE)
GR1065v1 <- as.data.frame(GR1065v1)

# Construct GR1065
GR1065 <- data.frame(GR1065v1$IDC, GR1065v1$G0200365, GR1065v1$G0201165, GR1065v1$G0200765, GR1065v1$G0202365, GR1065v1$G0200565, GR1065v1$G0201965)
GR1065$IDC <- GR1065$GR1065v1.IDC

#--------

## GR1065G3-6
GR1065v1G <- read.spss("O:/Medewerkers/042647 Schuurmans, I/DATA/GR1065-G3-6_01072012.sav", use.value.labels = FALSE)
GR1065v1G <- as.data.frame(GR1065v1G)

# Construct GR1065
GR1065G <- data.frame(GR1065v1G$IDC, GR1065v1G$G0600165, GR1065v1G$G0300165, GR1065v1G$G0400265)
GR1065G$IDC <- GR1065G$GR1065v1G.IDC

#--------

## GR1065X
GR1065v1X <- read.spss("O:/Medewerkers/042647 Schuurmans, I/DATA/GR1065-X_01072012.sav", use.value.labels = FALSE)
GR1065v1X <- as.data.frame(GR1065v1X)

# Construct GR1065
GR1065X <- data.frame(GR1065v1X$IDC, GR1065v1X$X0500165)
GR1065X$IDC <- GR1065X$GR1065v1X.IDC

#--------

## GR1075
GR1075 <- read.spss("O:/Medewerkers/042647 Schuurmans, I/DATA/SESFOLLOWUP_17072015.sav", use.value.labels = FALSE)
GR1075 <- as.data.frame(GR1075)

#--------

## GR1075B3
GR1075v1B3 <- read.spss("O:/Medewerkers/042647 Schuurmans, I/DATA/GR1075-B3_17072015.sav", use.value.labels = FALSE)
GR1075v1B3 <- as.data.frame(GR1075v1B3)

# Construct GR1075
GR1075B3 <- data.frame(GR1075v1B3$IDC, GR1075v1B3$children_household_clean)
GR1075B3$IDC <- GR1075B3$GR1075v1B3.IDC

#--------

## GR1075empl
GR1075v1E <- read.spss("O:/Medewerkers/042647 Schuurmans, I/DATA/PARENTEMPLOYMENT5_13082012.sav", use.value.labels = FALSE)
GR1075v1E <- as.data.frame(GR1075v1E)

# Construct GR1075
GR1075E <- data.frame(GR1075v1E$IDC, GR1075v1E$B0500175_clean, GR1075v1E$B1200175_clean)
GR1075E$IDC <- GR1075E$GR1075v1E.IDC

#--------

## GR1079
GR1079v1 <- read.spss("O:/Medewerkers/042647 Schuurmans, I/DATA/20141027_TRFteacherCleaned.sav", use.value.labels = FALSE)
GR1079v1 <- as.data.frame(GR1079v1)

# Construct GR1080
GR1079 <- data.frame(GR1079v1$IDC, GR1079v1$D0100179, GR1079v1$D0100279, GR1079v1$D0100379)
GR1079$IDC <- GR1079$GR1079v1.IDC

#--------

## GR1080
GR1080v1 <- read.spss("O:/Medewerkers/042647 Schuurmans, I/DATA/GR1080-C10-11_04042017.sav", use.value.labels = FALSE)
GR1080v1 <- as.data.frame(GR1080v1)

# Construct GR1080
GR1080 <- data.frame(GR1080v1$IDC, GR1080v1$Cleaned_C1001280v2)
GR1080$IDC <- GR1080$GR1080v1.IDC

#--------

## GR1080E
GR1080v1E <- read.spss("O:/Medewerkers/042647 Schuurmans, I/DATA/GR1080-E_Bullying_17072015.sav", use.value.labels = FALSE)
GR1080v1E <- as.data.frame(GR1080v1E)
IDC <- GR1080v1E$IDC
# Construct GR1080
GR1080E <- data.frame(GR1080v1E$IDC, GR1080v1E$E0100280, GR1080v1E$E0100380, GR1080v1E$E0100480)
GR1080E$IDC <- GR1080E$GR1080v1E.IDC

#--------

## GR1081 E3-6
GR1081v1 <- read.spss("O:/Medewerkers/042647 Schuurmans, I/DATA/GR1081_E1-3_5-6_08082016.sav", use.value.labels = FALSE)
GR1081v1 <- as.data.frame(GR1081v1)

# Construct GR1081
GR1081 <- data.frame(GR1081v1$IDC, GR1081v1$E0600181_v2, GR1081v1$E0100281_v2, GR1081v1$E0100381_v2, 
                     GR1081v1$E0100481_v2, GR1081v1$E0100581_v2, GR1081v1$E0100681_v2, GR1081v1$E0100781_v2, 
                     GR1081v1$E0100881_v2, GR1081v1$E0101181_v2, GR1081v1$E0200181_v2, GR1081v1$E0200381_v2)
GR1081$IDC <- GR1081$GR1081v1.IDC

#--------

## GR1081 I (tv)
GR1081v1I <- read.spss("O:/Medewerkers/042647 Schuurmans, I/DATA/GR1081_I1-9_08082016.sav", use.value.labels = FALSE)
GR1081v1I <- as.data.frame(GR1081v1I)

# Construct GR1081 I
GR1081I <- data.frame(GR1081v1I$IDC, GR1081v1I$I0100181_cleaned)
GR1081I$IDC <- GR1081I$GR1081v1I.IDC

#--------

## GR1081empl
GR1081v1E <- read.spss("O:/Medewerkers/042647 Schuurmans, I/DATA/GR1081_E4_30082016.sav", use.value.labels = FALSE)
GR1081v1E <- as.data.frame(GR1081v1E)

# Construct GR1075
GR1081E <- data.frame(GR1081v1E$IDC, GR1081v1E$E0400181_v2)
GR1081E$IDC <- GR1081E$GR1081v1E.IDC

#--------

## GR1082
GR1082v1 <- read.spss("O:/Medewerkers/042647 Schuurmans, I/DATA/GR1082_C1-5_22092016.sav", use.value.labels = FALSE)
GR1082v1 <- as.data.frame(GR1082v1)

# Construct GR1082
GR1082 <- data.frame(GR1082v1$IDC, GR1082v1$C0300182_cleaned)
GR1082$IDC <- GR1082$GR1082v1.IDC

#--------

## Predictor data prenatal

# Read in data prenatal risk
riskpre <- readquick("O:/medewerkers/042647 Schuurmans, I/DATA/Prenatal/AllData_getindicators.sav")
riskpre <- riskpre[!is.na(riskpre$idc),]
# gender and ethnicity
gender <- readquick("o:/medewerkers/042647 schuurmans, i/data/child-allgeneraldata_12112020.sav")

# merge
risk <- merge(gender, riskpre, by = 'idc', all = TRUE)

# I need to add education to the prenatal ELS score so I merged it with general data
# Now I dichotomize edu and I add it to the personal stress domain
risk$edu <- ifelse(risk$educm <= 3, yes = 1, no = 0) 
risk$personal_stress <- risk$personal_stress + risk$edu
risk$nmispersonal_stress <- ifelse(is.na(risk$edu), risk$nmispersonal_stress + 1, risk$nmispersonal_stress)
  
# Make rowmeans, but only for those with no missings
risk$life_events_m <- ifelse(risk$nmislife_events == 0, risk$life_events_sum_weighted/15, NA)
risk$contextual_m <- ifelse(risk$nmiscontextual == 0, risk$contextual_sum_weighted/6, NA)
risk$interpersonal_m <- ifelse(risk$nmisinterpersonal == 0, risk$interpersonal_sum_weighted/18, NA)
risk$personal_stress_m <- ifelse(risk$nmispersonal_stress == 0, risk$personal_stress/6, NA)

# ETHNICITIY recode
risk$ethnicity <- risk$ethninfv2
risk$ethnicity[risk$ethninfv2 == 1] <- 0
risk$ethnicity[risk$ethninfv2 == 300] <- 1
risk$ethnicity[risk$ethninfv2 == 500] <- 1
risk$ethnicity[risk$ethninfv2 == 700] <- 1
risk$ethnicity[risk$ethninfv2 == 800] <- 1
risk$ethnicity[risk$ethninfv2 == 2] <- 2
risk$ethnicity[risk$ethninfv2 == 3] <- 2
risk$ethnicity[risk$ethninfv2 == 4] <- 2
risk$ethnicity[risk$ethninfv2 == 5] <- 2
risk$ethnicity[risk$ethninfv2 == 6] <- 2
risk$ethnicity[risk$ethninfv2 == 7] <- 2
risk$ethnicity[risk$ethninfv2 == 200] <- 2
risk$ethnicity[risk$ethninfv2 == 400] <- 2
risk$ethnicity[risk$ethninfv2 == 600] <- 2

#--------

## Outcome data Cito

# read in data cito school report
cito1 <- readquick("o:/medewerkers/042647 schuurmans, i/data/childcito_15122017.sav")

# read in data cito mother report
cito2 <- readquick("o:/medewerkers/042647 schuurmans, i/data/GR1093-F_School_13052020.sav")

# merge both
cito <- merge(cito1, cito2, by = 'idc', all = T)

# make new cito variable
cito$cito <- ifelse(is.na(cito$citotest), yes = cito$f0200293_cleaned_citoscore, no = cito$citotest)
cito$reportcito <- ifelse(is.na(cito$citotest), yes = 'mother', no = 'teacher')
cito$age <- ifelse(is.na(cito$citotest), cito$agechildcito_gr1093, cito$age_cito)

#--------

## Outcome data Wisc
cogn <- readquick("o:/medewerkers/042647 schuurmans, i/data/WISC/20210305/03052021_WISC.sav")
# make an IQ-score (orthogonalize on age + scale, exclude, weight)
cogn$IQ <- cogn$wisc13_fsiq

########################
#### FINAL DATA SET ####
########################

tmp <- LifeEvents 
tmp1 <- merge(GR1080, tmp, by = 'IDC', all = TRUE) 
tmp2 <- merge(GR1082, tmp1, by = 'IDC', all = TRUE) 
tmp3 <- merge(GR1065, tmp2, by = 'IDC', all = TRUE)
tmp4 <- merge(GR1065G, tmp3, by = 'IDC', all = TRUE)
tmp6 <- merge(GR1081, tmp4, by = 'IDC', all = TRUE)
tmp7 <- merge(GR1075, tmp6, by = 'IDC', all = TRUE)
tmp8 <- merge(GR1075E, tmp7, by = 'IDC', all = TRUE)
tmp9 <- merge(GR1081E, tmp8, by = 'IDC', all = TRUE)
tmp11 <- merge(BSI_totalscore, tmp9, by = 'IDC', all = TRUE)
tmp12 <- merge(GR1065X, tmp11, by = 'IDC', all = TRUE)
tmp13 <- merge(GR1075B3, tmp12, by = 'IDC', all = TRUE)
tmp14 <- merge(fad, tmp13, by = 'IDC', all = TRUE)
tmp15 <- merge(GR1080E, tmp14, by = 'IDC', all = TRUE)
tmp16 <- merge(GR1079, tmp15, by = 'IDC', all = TRUE)
tmp17 <- merge(Parenting, tmp16, by = 'IDC', all = TRUE)
tmp18 <- merge(GR1081I, tmp17, by = 'IDC', all = TRUE)
tmp19 <- merge(risk, tmp18, by = 'idc', by.y = 'IDC', all = TRUE)
tmp20 <- merge(cito, tmp19, by = 'idc', all = TRUE)
tmp21 <- merge(cogn, tmp20, by = 'idc', all = TRUE)
tmp23 <- merge(Smoking, GR1005, by = 'idm', all = TRUE)
tmp24 <- merge(GR1004dep, tmp23, by = 'idm', all = TRUE)
tmp25 <- merge(GR1003dep, tmp24, by = 'idm', all = TRUE)
tmp26 <- merge(tmp21, tmp25, by = 'idm', all = TRUE)
tmp5 <- merge(tmp26, BMI, by = 'mother', all = TRUE)

# remove participants with no ids & measurements 
tmp5 <- tmp5[!is.na(tmp5$idc),]
tmp5 <- tmp5[!is.na(tmp5$idm),]

#####################
#### REMOVE DATA ####
#####################

rm(GR1065,GR1065G,GR1065v1,GR1065v1G,GR1065v1X,GR1065X,GR1075,GR1075B3,GR1075E,GR1075v1B3,GR1075v1E,GR1079,GR1079v1,GR1080,GR1080E,GR1080v1,
   GR1080v1E,GR1081,GR1081E,GR1081v1,GR1081v1E,GR1082,GR1082v1,LifeEvents,Parenting,Parentingv1,tmp,tmp1,tmp2,tmp3,tmp4,tmp6,
   tmp7,tmp8,tmp9,tmp11,tmp12,tmp13,tmp14,tmp15,tmp16,GR1081I,GR1081v1I,tmp17,tmp18, tmp19, tmp20, riskpre, cito, cito1, 
   cito2, cogn, fad, gender, BSI_totalscore, risk, tmp21)

setwd('O:/medewerkers/042647 Schuurmans, I/DATA')
# write out
library(xlsx)
tmp5 <- replacenas(tmp5)
saveRDS(tmp5, 'tmp5.rds')
tmp5 <- readRDS('tmp5.rds')
