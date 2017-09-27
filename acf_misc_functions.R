# collection of functions for my phd

## read_my_csv --------------------------------------#

read_my_csv <- function(csv_file) {
  my_csv_file <- read.csv(csv_file, header = TRUE, na.strings = c("NA", "NULL"))
  return(my_csv_file)
}

## binarise.it --------------------------------------#

binarise.it <- function(y) {
  binarise <- (ifelse(is.na(y) == TRUE, 0, 
                      ifelse(is.na(y) == FALSE, 1, NA)))
  return(binarise)
}



## categorise.honos --------------------------------------#

categorise.honos <- function(x) {
categorise <-  (ifelse(x == 0, "No symptoms",
                ifelse(x == 1 | x == 2, "Mild",
                ifelse(x == 3 | x == 4, "Mod to Severe", NA))))
return(categorise)
}


## categorise.symptom.count --------------------------------------#

categorise.symptom.count <- function(x) {
  categorise <- (ifelse(x < 10,"1.Less than 10 times", 
                        ifelse(x >= 10 & x <= 50, "2.10 - 50 times",
                               ifelse(x > 50, "3.More than 50 times", NA))))
  return(categorise)
}


## age.gender.adjusted analysis --------------------------------------#

age.gender.adjusted <- function(y) {
  age.gender.adjustify <- multinom(NameOfAD ~ agegroups + gender_new + I(y), ad_users_grouped_test)
  return(age.gender.adjustify)
}


## factor it --------------------------------------#
factor.it <- function(x) {
  factorise <- as.factor(x)
  return(factorise)
}



## binarising risk assessment --------------------------------------#
binarise.it.RA <- function(y) {
  binarise.RA <- (ifelse(y == "Yes", 1, 
                  ifelse(y == "No", 0, NA)))
  return(binarise.RA)
}
  


## date format --------------------------------------#
dateformatAF <- function(y) {
  dateformat <- as.Date(y, origin="1970-01-01")
  return(dateformat)
}
  


  
