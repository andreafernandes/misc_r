# collection of functions for my phd

## read_my_csv --------------------------------------#

read_my_csv <- function(csv_file) {
  my_csv_file <- read.csv(csv_file, header = TRUE, na.strings = c("NA", "NULL"))
  return(my_csv_file)
}
  
