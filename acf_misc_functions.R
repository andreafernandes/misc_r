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
  categorise <- (ifelse(as.numeric(x) == 1,"1.No symptoms", 
                        ifelse(as.numeric(x) == 2 | 
                                 as.numeric(x) == 3, "2.Mild to moderate symptoms",
                               ifelse(as.numeric(x) == 4 | 
                                        as.numeric(x) == 5, "3.Moderate to severe symptoms",
                                      ifelse(as.numeric(as.numeric(x)) == 6, NA,
                                             ifelse(is.na(as.numeric(x)) == TRUE, NA, NA
                                             ))))))
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
  


  
