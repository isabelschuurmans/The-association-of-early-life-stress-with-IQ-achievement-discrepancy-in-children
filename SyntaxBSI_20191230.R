#Author: Isabel K. Schuurmans 

######################################
#### CODE BRIEF SYMPTOM INVENTORY ####
######################################

#-------------------------------------
# Open library
library(foreign)

#-------------------------------------
# Specify path to BSI data set (DO THIS YOURSELF)
briefsymptominventory3 <- "V:/medewerkers/042647 Schuurmans, I/DATA/BSI 3 years of age_GR1065 G1-GR1066 C1_22112016_NOSYNTAX.sav"
briefsymptominventory9 <- "V:/medewerkers/042647 Schuurmans, I/DATA/GR1081-GR1083_D1_BSI_19042017.sav"

# Read data BSI3
BSI3 <- read.spss(briefsymptominventory3, use.value.labels = FALSE)
BSI3 <- as.data.frame(BSI3)
IDC <- BSI3$IDC

# Replace NA 
BSI3[BSI3 == 999] <- NA
BSI3[BSI3 == 888] <- NA
BSI3$IDC <- IDC

# Read data BSI9
BSI9 <- read.spss(briefsymptominventory9, use.value.labels = FALSE)
BSI9 <- as.data.frame(BSI9)
IDC <- BSI9[,1:4]

# Replace NA 
BSI9[BSI9 == 999] <- NA
BSI9[BSI9 == 888] <- NA
BSI9[,1:4] <- IDC

# Merge data
BSI <- merge(BSI3, BSI9, by = 'IDC', all = T)


#-------------------------------------

###############
# 3 YEARS OLD #
###############

# MOTHER REPORT

#Interpersonal Sensitivity: Items 20, 21, 22, and 42
MoIn3 <- data.frame(BSI$G0101065, BSI$G0101165, BSI$G0101265, BSI$G0101765)
NMoIn3 <- rowSums(!is.na(MoIn3))
ibsi3m <- ifelse(NMoIn3 >= 3, yes = (rowSums(MoIn3)/NMoIn3)-1, no = NA)

#Depression: Items 9, 16, 17, 18, 35, and 50
MoDe3 <- data.frame(BSI$G0100365, BSI$G0100665, BSI$G0100765, BSI$G0100865, BSI$G0101365, BSI$G0102165)
NMoDe3 <- rowSums(!is.na(MoDe3))
dbsi3m <- ifelse(NMoDe3 >= 5, yes = (rowSums(MoDe3)/NMoDe3)-1, no = NA)

#Anxiety: Items 1, 12, 19, 38, 45, and 49 
MoAn3 <- data.frame(BSI$G0100165, BSI$G0100465, BSI$G0100965, BSI$G0101465, BSI$G0101865, BSI$G0102065)
NMoAn3 <- rowSums(!is.na(MoAn3))
absi3m <- ifelse(NMoAn3 >= 5, yes = (rowSums(MoAn3)/NMoAn3)-1, no = NA) 

# FATHER REPORT

#Interpersonal Sensitivity: Items 20, 21, 22, and 42
PaIn3 <- data.frame(BSI$C0101066, BSI$C0101166, BSI$C0101266, BSI$C0101766)
NPaIn3 <- rowSums(!is.na(PaIn3))
ibsi3f <- ifelse(NPaIn3 >= (ncol(PaIn3)-1), yes = (rowSums(PaIn3)/NPaIn3)-1, no = NA)

#Depression: Items 9, 16, 17, 18, 35, and 50
PaDe3 <- data.frame(BSI$C0100366, BSI$C0100666, BSI$C0100766, BSI$C0100866, BSI$C0101366, BSI$C0102166)
NPaDe3 <- rowSums(!is.na(PaDe3))
dbsi3f <- ifelse(NPaDe3 >= (ncol(PaDe3)-1), yes = (rowSums(PaDe3)/NPaDe3)-1, no = NA)

#Anxiety: Items 1, 12, 19, 38, 45, and 49 
PaAn3 <- data.frame(BSI$C0100166, BSI$C0100466, BSI$C0100966, BSI$C0101466, BSI$C0101866, BSI$C0102066)
NPaAn3 <- rowSums(!is.na(PaAn3))
absi3f <- ifelse(NPaAn3 >= (ncol(PaAn3)-1), yes = (rowSums(PaAn3)/NPaAn3)-1, no = NA) 

###############
# 9 YEARS OLD #
###############

# MOTHER REPORT

#Interpersonal Sensitivity: Items 20, 21, 22, and 42
MoIn9 <- data.frame(BSI$D0101081_cleaned, BSI$D0101181_cleaned, BSI$D0101281_cleaned, BSI$D0101781_cleaned)
NMoIn9 <- rowSums(!is.na(MoIn9))
ibsi9m <- ifelse(NMoIn9 >= 3, yes = (rowSums(MoIn9)/NMoIn9)-1, no = NA)

#Depression: Items 9, 16, 17, 18, 35, and 50
MoDe9 <- data.frame(BSI$D0100381_cleaned, BSI$D0100681_cleaned, BSI$D0100781_cleaned, BSI$D0100881_cleaned, BSI$D0101381_cleaned, BSI$D0102181_cleaned)
NMoDe9 <- rowSums(!is.na(MoDe9))
dbsi9m <- ifelse(NMoDe9 >= (ncol(MoDe9)-1), yes = (rowSums(MoDe9)/NMoDe9)-1, no = NA)

#Anxiety: Items 1, 12, 19, 38, 45, and 49 
MoAn9 <- data.frame(BSI$D0100181_cleaned, BSI$D0100481_cleaned, BSI$D0100981_cleaned, BSI$D0101481_cleaned, BSI$D0101881_cleaned, BSI$D0102081_cleaned)
NMoAn9 <- rowSums(!is.na(MoAn9))
absi9m <- ifelse(NMoAn9 >= (ncol(MoAn9)-1), yes = (rowSums(MoAn9)/NMoAn9)-1, no = NA) 

# FATHER REPORT

#Interpersonal Sensitivity: Items 20, 21, 22, and 42
PaIn9 <- data.frame(BSI$D0101083_cleaned, BSI$D0101183_cleaned, BSI$D0101283_cleaned, BSI$D0101783_cleaned)
NPaIn9 <- rowSums(!is.na(PaIn9))
ibsi9f <- ifelse(NPaIn9 >= (ncol(PaIn9)-1), yes = (rowSums(PaIn9)/NPaIn9)-1, no = NA)

#Depression: Items 9, 16, 17, 18, 35, and 50
PaDe9 <- data.frame(BSI$D0100383_cleaned, BSI$D0100683_cleaned, BSI$D0100783_cleaned, BSI$D0100883_cleaned, BSI$D0101383_cleaned, BSI$D0102183_cleaned)
NPaDe9 <- rowSums(!is.na(PaDe9))
dbsi9f <- ifelse(NPaDe9 >= (ncol(PaDe9)-1), yes = (rowSums(PaDe9)/NPaDe9)-1, no = NA)

#Anxiety: Items 1, 12, 19, 38, 45, and 49 
PaAn9 <- data.frame(BSI$D0100183_cleaned, BSI$D0100483_cleaned, BSI$D0100983_cleaned, BSI$D0101483_cleaned, BSI$D0101883_cleaned, BSI$D0102083_cleaned)
NPaAn9 <- rowSums(!is.na(PaAn9))
absi9f <- ifelse(NPaAn9 >= (ncol(PaAn9)-1), yes = (rowSums(PaAn9)/NPaAn9)-1, no = NA)

#-------------------------------------
# Construct final dataset
BSI_totalscore <- data.frame(ibsi3m, dbsi3m, absi3m, ibsi3f, dbsi3f, absi3f,
                             ibsi9m, dbsi9m, absi9m, ibsi9f, dbsi9f, absi9f, BSI$IDC, BSI$IDM,
                             BSI$GR1065_filledinby, BSI$GR1066_filledinby, BSI$GR1081_filledinby, BSI$GR1083_filledinby,
                             BSI$age_GR1065, BSI$age_GR1066, BSI$AgeMotherGR1081, BSI$AgeFatherGR1083)
BSI_totalscore$IDC <- BSI_totalscore$BSI.IDC
BSI_totalscore$IDM <- BSI_totalscore$BSI.IDM

#####################
#### REMOVE DATA ####
#####################

rm(BSI)
rm(BSI3)
rm(BSI9)
rm(MoAn3)
rm(MoAn9)
rm(MoDe3)
rm(MoDe9)
rm(MoIn3)
rm(MoIn9)
rm(PaAn3)
rm(PaAn9)
rm(PaDe3)
rm(PaDe9)
rm(PaIn3)
rm(PaIn9)
rm(absi3f)
rm(absi3m)
rm(absi9f)
rm(absi9m)
rm(ibsi3f)
rm(ibsi3m)
rm(ibsi9f)
rm(ibsi9m)
rm(IDC)
rm(briefsymptominventory3)
rm(briefsymptominventory9)
rm(dbsi3f)
rm(dbsi3m)
rm(dbsi9f)
rm(dbsi9m)
rm(NMoAn3)
rm(NMoAn9)
rm(NMoDe3)
rm(NMoDe9)
rm(NMoIn3)
rm(NMoIn9)
rm(NPaAn3)
rm(NPaAn9)
rm(NPaDe3)
rm(NPaDe9)
rm(NPaIn3)
rm(NPaIn9)

