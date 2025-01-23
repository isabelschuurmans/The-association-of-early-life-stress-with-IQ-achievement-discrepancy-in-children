# NON RESPONSE ANALYSIS
# I.K. Schuurmans, 2021-08-30

# open library
library(haven)
library(xlsx)

# read in data
tmp <- readRDS("O:/medewerkers/042647 Schuurmans, I/Project_1_ELS_CM/tmp.rds")
datarisk <- readRDS('O:/medewerkers/042647 Schuurmans, I/PROJECT_1_ELS_CM/R Code/datarisk_20210325.rds')
datarisk[is.na(datarisk$idc), 'idc'] <- 888
datarisk <- datarisk[which(!datarisk$idc == 102),]

variables = c('gender', 'dep')
ids_study = datarisk$idc
ids_full = tmp$idc 
dataset <- tmp

# function
nonresponse <- function(ids_study, ids_full, dataset, variables){
  
  # convert dataset to dataframe
  dataset <- as.data.frame(dataset)[]
  
  # make indicator variable for included and excluded participants
  dataset$indicator <- ids_full %in% ids_study
  
  # get empty table
  table_stats <- matrix(NA, nrow = (length(variables)), ncol = 5)
  
  # compare on the variables
  
  for (i in 1:length(variables)){
   
    # t-test continuous outcomes
    if (is.numeric(dataset[,variables[i]])){
      
      table_stats[i,1] <- paste(round(mean(dataset[dataset$indicator == T,variables[i]], na.rm = T),2), round(sd(dataset[dataset$indicator == T,variables[i]], na.rm = T),2), sep = ' ± ')
      table_stats[i,2] <- paste(round(mean(dataset[dataset$indicator == F,variables[i]], na.rm = T),2), round(sd(dataset[dataset$indicator == F,variables[i]], na.rm = T),2), sep = ' ± ')
      table_stats[i,3] <- round(t.test(dataset[dataset$indicator == T,variables[i]], dataset[dataset$indicator == F,variables[i]])$statistic, 2)
      table_stats[i,4] <- round(t.test(dataset[dataset$indicator == T,variables[i]], dataset[dataset$indicator == F,variables[i]])$parameter, 2)
      table_stats[i,5] <- round(t.test(dataset[dataset$indicator == T,variables[i]], dataset[dataset$indicator == F,variables[i]])$p.value, 3)
      
    } 
    
    # chi-square for categorical outcomes 
    else {
      
      table_stats[i,1] <- paste(table(dataset[dataset$indicator == T,variables[i]])[1], ' (', round(prop.table(table(dataset[dataset$indicator == T,variables[i]]))[1]*100,1), ')', sep = '')
      table_stats[i,2] <- paste(table(dataset[dataset$indicator == F,variables[i]])[1], ' (', round(prop.table(table(dataset[dataset$indicator == F,variables[i]]))[1]*100,1), ')', sep = '') 
      table_stats[i,3] <- round(chisq.test(dataset[,variables[i]], dataset$indicator)$statistic, 2)
      table_stats[i,4] <- round(chisq.test(dataset[,variables[i]], dataset$indicator)$parameter, 2)
      table_stats[i,5] <- round(chisq.test(dataset[,variables[i]], dataset$indicator)$p.value, 3)
      
    }
    
  }
  
  # clean up stats
  colnames(table_stats) <- c('mean study', 'mean remaining','t/X', 'df_t', 'p_t')
  rownames(table_stats) <- variables
  
  return(table_stats)
  
}

## we will check non-response with auxiliary variables and gender

# convert to correct data types GENDER
tmp$gender <- tmp$gender - 1
tmp$smoking <- ifelse(tmp$smoking == 3, 2, tmp$smoking) - 1
tmp[,c('ethnicity', 'smoking', 'gender', 'edum','edup','mari','edu',"mardich.y")] <- 
  apply(tmp[,c('ethnicity', 'smoking', 'gender', 'edum','edup','mari','edu',"mardich.y")], 2, as.factor)
  
# run non-response
tab1 <- nonresponse(ids_study = datarisk$idc, ids_full = tmp$idc, dataset = tmp, 
                   variables = c('gender','ethnicity', 'gestbir', 'weight','parity','smoking', 
                                 'age_m_v2','bmi_1','bmimotherf5', 'dep', 'dep_p', "dbsi3m","dbsi3f",
                                 'edu','edum','edup',"mardich.y", 'mari'))

tab2 <- nonresponse(ids_study = datarisk$idc, ids_full = tmp$idc[which(tmp$visit13==1)], dataset = tmp[which(tmp$visit13==1),], 
                    variables = c('gender','ethnicity', 'gestbir', 'weight','parity','smoking', 
                                  'age_m_v2','bmi_1','bmimotherf5', 'dep', 'dep_p', "dbsi3m","dbsi3f",
                                  'edu','edum','edup',"mardich.y", 'mari'))

write.xlsx(tab1, 'nonresponse_genr_20220107.xlsx')
write.xlsx(tab2, 'nonresponse_f13_20220107.xlsx')
getwd()


