#FUNCTIONS

# Psych, different cutoffs
psychoBSI <- function(x, filledinby, persons, cutoff){
  
 if(length(persons) != length(cutoff)){
   print('Length persons and length cutoff must be equal. Please, provide as many person values as cutoff values')
   break
 }else{
   
  z <- filledinby 
  y <- cutoff[z]
   if ((cutoff[z] == c(999))|(is.na(x))){
    return (NA)
    break
  } else{
  ifelse(x >= y, yes = 1, no = 0)
  }
 }

  }



psychoBSI <- function(x, filledinby, persons, cutoff){
  if(length(persons) != length(cutoff)){
    print('Length persons and length cutoff must be equal. Please, provide as many person values as cutoff values')
    break
  }else{
    if (is.na(x)){
      return (NA)
      break
    }else{
      if (is.na(filledinby)){
        return (NA)
        break
      }else{
        z <- filledinby 
        y <- cutoff[z]
        if (cutoff[z] == c(999)){
          return (NA)
          break
          } else{
            ifelse(x >= y, yes = 1, no = 0)
          }
      }
    }
  }
}


#psycho BSI function
#x = score on BSI
#filledinby = number to specify who filled this in
#person = specify who can fill this in
#cutoff = cut off, must correspond to person!

psychoBSI(x=9, filledinby = 5, person = c(1:5), cutoff = c(10, 11, 11, 12.5, 999))

nep1<- (rep(c(1:3,NA,5), 100))
nep2<- rnorm(500, 12, 1)
nep3<- 1:length(nep1)

df <- data.frame(nep1, nep2, nep3)

for (i in 1:length(df$nep3)){
  df$nep3[i] <- psychoBSI(x = df$nep2[i], filledinby = df$nep1[i], person = c(1:5), cutoff = c(5, 11, 11, 12.5, 999))}




