
#### Classifying Income Basis Project ####

### CML Data ###







# Import CML data & load packages--------------------------------------------------------

library(haven)
library(dplyr)
library(tidyverse)
library(reshape)
library(taRifx)
library(data.table)
library(readxl)
library(scales)
library(zoo)
library(lubridate)
library(doBy)
library(grid)
library(stringr)
library(forecast)
library(compare)



setwd("D:/R/First Time Buyer Project")

nspl <- fread("Datasets/nspl.csv")

for(i in 2013:2017) {
  assign(paste("cml_", i, sep = ""), 
         value = read_spss(paste("Datasets/CML/CML - ", i, ".sav", sep = "")))
} # Import and assign all CML data sets


# Tidying CML Data --------------------------------------------------------

subset_cml1  <- function(x) {
  x <- x[ , c("TypeBorrower", "LoanAmt", "InitialGrossIntRate", 
              "TypeIntRate", "RepayMethod", "IncomeBasis", 
              "AgeMain", "GrossIncome", "PurchasePrice", 
              "NewDwelling", "TypeDwelling", "NumBedrooms", 
              "PropertyPostCode", "MtgeTerm", "MainEmpStatus", 
              "compdate", "ftb")]
}  # subset variables for 2013-2015 & 2017 dfs
subset_cml2  <- function(x) {
  x <- x[ , c("TypeBorrower", "LoanAmt", "InitialGrossIntRate", 
              "RepayMethod", "AgeMain", "GrossIncome",
              "PurchasePrice", "NewDwelling", "NumBedrooms", 
              "pcode", "MtgeTerm", "compdate",
              "ftb")]
}  # subset variables for 2016 data
rename_cols1 <- function(x) {
  colnames(x) <- c("type_borrower", "loan_amount", "int_rate", 
                   "type_int_rate", "repay_method", "income_basis", 
                   "age_main", "gross_income", "purchase_price", 
                   "new_dwelling", "type_dwelling", "num_bedrooms", 
                   "pcd_no_space", "mortgage_term", "main_emp_status", 
                   "compdate", "ftb")
  x
}  # rename the variables for 2013-2015 & 2017 dfs
rename_cols2 <- function(x) {
  colnames(x) <- c("type_borrower", "loan_amount", "int_rate", 
                   "repay_method", "age_main", "gross_income",
                   "purchase_price", "new_dwelling", "num_bedrooms", 
                   "pcd_no_space", "mortgage_term", "compdate",
                   "ftb")
  x
}  # rename the variables for 2016 data
date_create  <- function(x) {
  x$comp_date <- sprintf("%04d", x$comp_date)
  separate_dates <- function(x) {
    vec <- c()
    vec[1:2] <- substring(x[[1]], seq(1,nchar(x[[1]]),2), seq(2,nchar(x[[1]]),2))
    for(i in 2:length(x)) {
      vec[((i*2)-1):(i*2)] <- substring(x[[i]], seq(1,nchar(x[[i]]),2), seq(2,nchar(x[[i]]),2))
    }
    print(vec)
  }
  vec <- separate_dates(x$comp_date)
  x$month <- vec[seq(1, length(vec), 2)]
  x$year  <- vec[seq(2, length(vec), 2)]
  x$year <- paste0(20, x$year)
  x$date <-  paste(x$year, x$month, sep = "-")
  as.yearmon(x$date)
}  # format comp_date variable into correct date format
space_remove <- function(x) {
  str_replace_all(x, fixed(" "), "")
}  # remove spaces (use for postcodes)

nspl$pcd_no_space <- space_remove(nspl$pcd) # Create new column in nspl with pcds without spaces




# CML 2013
cml_2013_1 <- subset_cml1(cml_2013)
cml_2013_1 <- rename_cols1(cml_2013_1)
cml_2013_1$pcd_no_space <- space_remove(cml_2013_1$pcd_no_space)

# CML 2014
cml_2014_1 <- subset_cml1(cml_2014)
cml_2014_1 <- rename_cols1(cml_2014_1)
cml_2014_1$pcd_no_space <- space_remove(cml_2014_1$pcd_no_space)

# CML 2015 
cml_2015_1 <- subset_cml1(cml_2015)
cml_2015_1 <- rename_cols1(cml_2015_1)
cml_2015_1$pcd_no_space <- space_remove(cml_2015_1$pcd_no_space)

# CML 2016
cml_2016_1 <- subset_cml2(cml_2016)
cml_2016_1 <- rename_cols2(cml_2016_1)
cml_2016_1$pcd_no_space <- space_remove(cml_2016_1$pcd_no_space)

# CML 2017
cml_2017_1 <- subset_cml3(cml_2017)
cml_2017_1 <- rename_cols3(cml_2017_1)
cml_2017_1$pcd_no_space <- space_remove(cml_2017_1$pcd_no_space)


# Create one data frame for first time buyers
cml_list_all <- list(cml_2013_1, cml_2014_1, 
                         cml_2015_1, cml_2016_1, 
                         cml_2017_1)     # Create list with all data frames
cml_all <- merge_all(cml_list_all) # POWER OVERWHELMING


write.csv(cml_all, "CML data - 1.csv")


# Geog lookups and general tidying
cml_all0 <- cml_all
cml_all0$comp_date <- date_create(cml_all0)       # Format dates 
cml_all0 <- cml_all0[ , c(10,6,5,1,2,9,8,4,3,7)]  # Re-arrange variables 
nspl_amend <- nspl[, c(39,26,12,17,16)]                      # Select geographies for lookup in nspl
cml_all1 <- merge(cml_all0, nspl_amend, by = "pcd_no_space") # Lookup geographies
colnames(cml_all1)[11:14] <- c("msoa_code", "la_code", "region_code", "country_code") # Rename geog variables
cml_all1 <- cml_all1[ order(cml_all1[,2]), ]      # Order by Date
rownames(cml_all1) <- 1:nrow(cml_all1)            # Re-number rows

# Removing duplicates
duplicates <- cml_all1[which(duplicated(cml_all1) | duplicated(cml_all1[nrow(cml_all1):1, ])[nrow(cml_all1):1]), ]
cml_all2 <- unique(cml_all1)                          # Remove duplicates from df
duplicated(cml_all2$pcd_no_space)

proportion_missing <- function(x) {
  round(sum(is.na(x)) / length(x) * 100, 5)
}                  # Summarise missing proportions
missing_variable <- sapply(cml_all2, proportion_missing) # Apply to df
missing_variable                                         # Print missing summary

# Add "01" to the beginning of dates, if want to correctly parse as a date variable
cml_all2$comp_date <- paste0("01 ", cml_all2$comp_date)
cml_all2$comp_date <- dmy(cml_all2$comp_date)

# Add a year variable for ease of analysis
years <- c(rep(2013, nrow(cml_all3[cml_all3$comp_date >= "2013-01-01" & cml_all3$comp_date <= "2013-12-01", ])),
           rep(2014, nrow(cml_all3[cml_all3$comp_date >= "2014-01-01" & cml_all3$comp_date <= "2014-12-01", ])),
           rep(2015, nrow(cml_all3[cml_all3$comp_date >= "2015-01-01" & cml_all3$comp_date <= "2015-12-01", ])),
           rep(2016, nrow(cml_all3[cml_all3$comp_date >= "2016-01-01" & cml_all3$comp_date <= "2016-12-01", ])),
           rep(2017, nrow(cml_all3[cml_all3$comp_date >= "2017-01-01" & cml_all3$comp_date <= "2017-12-01", ])))
cml_all3$year <- years # Create years variable -- MAYBE LOOK INTO LOOPING/WRITING FUNCTION FOR THIS??

cml_all4 <- cml_all3
cml_all5 <- cml_all4

# General final tidying
cml_all5 <- cml_all5[ order(cml_all5[ , 3]), ]                    # Order rows by date
cml_all5 <- cml_all5[ , c(3:11,14,13,15,12,16,2,17,1,18)]             # Re-order variables
empty_as_na <- function(x) {
  if("factor" %in% class(x)) x <- as.character(x) # since ifelse wont work with factors
  ifelse(as.character(x)!="", x, NA)
}                                              # Function to replace blanks with NA
cml_all6 <-  cbind(cml_all5$comp_date, 
                       cml_all5[, -1] %>% mutate_all(funs(empty_as_na))) # Replace blanks
colnames(cml_ftb_all6)[1] <- "comp_date"
cml_all6$sole_or_joint[cml_all6$sole_or_joint == "S"] <- "Sole"       # Expand "S" to "Sole"
cml_all6$sole_or_joint[cml_all6$sole_or_joint == "J"] <- "Joint"      # Expand "J" to "Joint"
cml_all6 <- cml_all6[-which.max(cml_all6$initial_gross_int_rate), ]    # Remove record that looks incorrect 





#write.csv(cml_ftb_all6, "FTB - CML data - tidy data, England & Wales.csv")
