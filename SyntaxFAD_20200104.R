#Author: Isabel K. Schuurmans 

#######################################
#### CODE FAMILY ASSESSMENT DEVICE ####
#######################################

#-------------------------------------
# Open library
library(foreign)

#-------------------------------------
# Specify path to BSI data set (DO THIS YOURSELF)
familyassessmentdevice5 <- "O:/medewerkers/042647 Schuurmans, I/DATA/GR1075-B4_17072015.sav"
familyassessmentdevice9m <- "O:/medewerkers/042647 Schuurmans, I/DATA/GR1081_E1-3_5-6_08082016.sav"
familyassessmentdevice9f <- "O:/medewerkers/042647 Schuurmans, I/DATA/GR1083_E7_19102016.sav"

# Read data FAD5
FAD5 <- read.spss(familyassessmentdevice5, use.value.labels = FALSE)
FAD5 <- as.data.frame(FAD5)
IDC <- FAD5$IDC

# Replace NA 
FAD5[FAD5 == 999] <- NA
FAD5[FAD5 == 888] <- NA
FAD5$IDC <- IDC

# Read data FAD9 mother report
FAD9m <- read.spss(familyassessmentdevice9m, use.value.labels = FALSE)
FAD9m <- as.data.frame(FAD9m)
IDC <- FAD9m$IDC

# Replace NA 
FAD9m[FAD9m == 999] <- NA
FAD9m[FAD9m == 888] <- NA
FAD9m$IDC <- IDC

# Read data FAD9 father report
FAD9f <- read.spss(familyassessmentdevice9f, use.value.labels = FALSE)
FAD9f <- as.data.frame(FAD9f)
IDC <- FAD9f$IDC

# Replace NA 
FAD9f[FAD9f == 999] <- NA
FAD9f[FAD9f == 888] <- NA
FAD9f$IDC <- IDC

#-------------------------------------

####### age 5 #######

#-------------------------------------

# Read total score
fad5 <- FAD5$FAD_5y

#-------------------------------------

####### age 9 #######

#-------------------------------------

### MOTHER REPORT ###

# Recode inverse items
fadloop <- data.frame(FAD9m$E0500181_v2, FAD9m$E0500381_v2, FAD9m$E0500581_v2, FAD9m$E0500781_v2, FAD9m$E0500981_v2, FAD9m$E0501181_v2)
for (i in 1:length(fadloop)) {fadloop[i]<-5-fadloop[i]}

# Merge with other items
dffad9m <- data.frame(fadloop, FAD9m$E0500281_v2, FAD9m$E0500481_v2, FAD9m$E0500681_v2, FAD9m$E0500881_v2, FAD9m$E0501081_v2, FAD9m$E0501281_v2)

### FATHER REPORT ###

# Recode inverse items
fadloop <- data.frame(FAD9f$E0700183v2, FAD9f$E0700383v2, FAD9f$E0700583v2, FAD9f$E0700783v2, FAD9f$E0700983v2, FAD9f$E0701183v2)
for (i in 1:length(fadloop)) {fadloop[i]<-5-fadloop[i]}

# Merge with other items
dffad9f <- data.frame(fadloop, FAD9f$E0700283v2, FAD9f$E0700483v2, FAD9f$E0700683v2, FAD9f$E0700883v2, FAD9f$E0701083v2, FAD9f$E0701283v2) 

#-------------------------------------

### MOTHER REPORT ### 

# Sum items
fadtotal9m <- rowSums(dffad9m)

# Number of endorsed items
nfad9m <- rowSums(!is.na(dffad9m))

# Compute mean score + Do not calculate when missing more than 25% of items
fad9m <- ifelse(nfad9m >= 9, yes = fadtotal9m/nfad9m, no = NA)

### FATHER REPORT ###

# Sum items
fadtotal9f <- rowSums(dffad9f)

# Number of endorsed items
nfad9f <- rowSums(!is.na(dffad9f))

# Compute mean score + Do not calculate when missing more than 25% of items
fad9f <- ifelse(nfad9f >= 9, yes = fadtotal9f/nfad9f, no = NA)

#-------------------------------------
# Construct final dataset
fad <- data.frame(fad5, fad9m, fad9f, FAD5$IDC)
fad$IDC <- fad$FAD5.IDC


#####################
#### REMOVE DATA ####
#####################

rm(dffad9f)
rm(dffad9m)
rm(FAD5)
rm(FAD9f)
rm(FAD9m)
rm(fadloop)
rm(fad5)
rm(fad9f)
rm(fad9m)
rm(fadtotal9f)
rm(fadtotal9m)
rm(i)
rm(familyassessmentdevice5)
rm(familyassessmentdevice9f)
rm(familyassessmentdevice9m)
rm(IDC)
rm(nfad9f)
rm(nfad9m)
