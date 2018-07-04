
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
                   "comp_date", "ftb")
  x
}  # rename the variables for 2013-2015 & 2017 dfs
rename_cols2 <- function(x) {
  colnames(x) <- c("type_borrower", "loan_amount", "int_rate", 
                   "repay_method", "age_main", "gross_income",
                   "purchase_price", "new_dwelling", "num_bedrooms", 
                   "pcd_no_space", "mortgage_term", "comp_date",
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


#write.csv(cml_all, "CML data - 1.csv")
cml_all0 <- fread("CML data - 1.csv")
cml_all0 <- cml_all0[ , -1]
colnames(cml_all0)[16] <- "comp_date"


# Geog lookups and general tidying
cml_all0 <- cml_all
cml_all0$comp_date <- date_create(cml_all0)       # Format dates 


                  
                   
""            
           
            

cols <- c("comp_date", "purchase_price" , "gross_income",
          "loan_amount", "int_rate", "type_int_rate",
          "repay_method", "type_borrower", "age_main",
          "main_emp_status", "mortgage_term", "income_basis",
          "ftb", "type_dwelling", "num_bedrooms", "new_dwelling", 
          "pcd_no_space")


cml_all0 <- cml_all0[ , cols, with = FALSE]  # Re-arrange variables 
nspl_amend <- nspl[, c(39,33,34)]                      # Select geographies for lookup in nspl
cml_all1 <- merge(cml_all0, nspl_amend, by = "pcd_no_space") # Lookup geographies
cml_all1 <- cml_all1[ order(cml_all1[,"comp_date"]), ]       # Order by Date
rownames(cml_all1) <- 1:nrow(cml_all1)            # Re-number rows

# Removing duplicates
duplicates <- cml_all1[which(duplicated(cml_all1) | duplicated(cml_all1[nrow(cml_all1):1, ])[nrow(cml_all1):1]), ]
cml_all2 <- unique(cml_all1)   # Remove duplicates from df
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
cml_all3 <- cml_all2

years <- c(rep(2013, nrow(cml_all3[cml_all3$comp_date >= "2013-01-01" & cml_all3$comp_date <= "2013-12-01", ])),
           rep(2014, nrow(cml_all3[cml_all3$comp_date >= "2014-01-01" & cml_all3$comp_date <= "2014-12-01", ])),
           rep(2015, nrow(cml_all3[cml_all3$comp_date >= "2015-01-01" & cml_all3$comp_date <= "2015-12-01", ])),
           rep(2016, nrow(cml_all3[cml_all3$comp_date >= "2016-01-01" & cml_all3$comp_date <= "2016-12-01", ])),
           rep(2017, nrow(cml_all3[cml_all3$comp_date >= "2017-01-01" & cml_all3$comp_date <= "2017-12-01", ])))
cml_all3$year <- years # Create years variable -- MAYBE LOOK INTO LOOPING/WRITING FUNCTION FOR THIS??

cml_all4 <- cml_all3


# General final tidying
cml_all4 <- cml_all4[ order(cml_all4[ , "comp_date"]), ]     # Order rows by date
                            

cols <- c("comp_date", "purchase_price", "gross_income",
          "loan_amount", "int_rate", "type_int_rate",
          "mortgage_term", "repay_method", "type_borrower",
          "age_main", "main_emp_status", "income_basis",
          "ftb", "type_dwelling", "num_bedrooms",
          "new_dwelling", "year", "lat", "long")


cml_all4 <- cml_all4[ , cols, with = FALSE]    # Re-order variables
empty_as_na <- function(x) {
  if("factor" %in% class(x)) x <- as.character(x) # since ifelse wont work with factors
  ifelse(as.character(x)!="", x, NA)
}                                              # Function to replace blanks with NA
cml_all5 <-  cbind(cml_all4$comp_date, 
                   cml_all4[, -1] %>% mutate_all(funs(empty_as_na))) # Replace blanks
colnames(cml_all5)[1] <- "comp_date"
cml_all5$income_basis[cml_all5$income_basis == "S"] <- "Sole"       # Expand "S" to "Sole"
cml_all5$income_basis[cml_all5$income_basis == "J"] <- "Joint"      # Expand "J" to "Joint"

# Redefine type interest rate records
cml_all5$type_int_rate[cml_all5$type_int_rate == "C"] <- "Capped rate"
cml_all5$type_int_rate[cml_all5$type_int_rate == "D"] <- "Dicsounted variable rate"
cml_all5$type_int_rate[cml_all5$type_int_rate == "F"] <- "Fixed rate"
cml_all5$type_int_rate[cml_all5$type_int_rate == "O"] <- "Other"
cml_all5$type_int_rate[cml_all5$type_int_rate == "T"] <- "Tracker"
cml_all5$type_int_rate[cml_all5$type_int_rate == "V"] <- "Standard variable rate"

# Redefine repay method records
cml_all5$repay_method[cml_all5$repay_method == "C"] <- "Capital and interest"
cml_all5$repay_method[cml_all5$repay_method == "E"] <- "Interest only / endowment"
cml_all5$repay_method[cml_all5$repay_method == "I"] <- "Interest only / ISA"
cml_all5$repay_method[cml_all5$repay_method == "M"] <- "Mix of capital and interest and interest only"
cml_all5$repay_method[cml_all5$repay_method == "N"] <- "Unknown"
cml_all5$repay_method[cml_all5$repay_method == "P"] <- "Interest only / pension"
cml_all5$repay_method[cml_all5$repay_method == "U"] <- "Interest only / unknown"

# Redefine type borrower
cml_all5$type_borrower[cml_all5$type_borrower == 1] <- "first-time buyer"
cml_all5$type_borrower[cml_all5$type_borrower == 2] <- "home movers"
cml_all5$type_borrower[cml_all5$type_borrower == 3] <- "remortgagers"
cml_all5$type_borrower[cml_all5$type_borrower == 4] <- "right to buy"
cml_all5$type_borrower[cml_all5$type_borrower == 5] <- "other"
cml_all5$type_borrower[cml_all5$type_borrower == 6] <- "unknown"

# Redefine main employment status
cml_all5$main_emp_status[cml_all5$main_emp_status == "E"] <- "employed"
cml_all5$main_emp_status[cml_all5$main_emp_status == "O"] <- "other"
cml_all5$main_emp_status[cml_all5$main_emp_status == "R"] <- "retired"
cml_all5$main_emp_status[cml_all5$main_emp_status == "S"] <- "self-employed"

# Redefine ftb 
cml_all5$ftb[cml_all5$ftb == "1"] <- "first-time buyer"
cml_all5$ftb[cml_all5$ftb == "2"] <- "not first-time buyer"

# Redefine type dwelling
cml_all5$type_dwelling[cml_all5$type_dwelling == -99] <- NA
cml_all5$type_dwelling[cml_all5$type_dwelling == 1]   <- "bungalow"
cml_all5$type_dwelling[cml_all5$type_dwelling == 2]   <- "detached"
cml_all5$type_dwelling[cml_all5$type_dwelling == 3]   <- "semi-detached"
cml_all5$type_dwelling[cml_all5$type_dwelling == 4]   <- "terraced"
cml_all5$type_dwelling[cml_all5$type_dwelling == 5]   <- "flat/maisonette"
cml_all5$type_dwelling[cml_all5$type_dwelling == 6]   <- "flat/maisonette"
cml_all5$type_dwelling[cml_all5$type_dwelling == 7]   <- "other"
  
# Redefine number bedrooms
cml_all5$num_bedrooms[cml_all5$num_bedrooms == "NA"] <- NA

# Redefine new_dwelling
cml_all5$new_dwelling[cml_all5$new_dwelling == -99]     <- NA
cml_all5$new_dwelling[cml_all5$new_dwelling == 1]       <- "new"
cml_all5$new_dwelling[cml_all5$new_dwelling == 2]       <- "old"
cml_all5$new_dwelling[cml_all5$new_dwelling == "NaN"]   <- NA

write.csv(cml_all5, "Mortgage data - tidy.csv")




# Encoding ----------------------------------------------------------------

df <- fread("Mortgage data - tidy.csv")
df <- df[ , -"ftb"]
head(df)

write.csv(df, "Mortgage data - tidy.csv")

df %>% group_by(type_int_rate) %>% tally()
# 1 - Capped rate
# 2 - Discounted variable rate
# 3 - Fixed rate
# 4 - Other
# 5 - Standard variable rate
# 6 - Tracker

df %>% group_by(repay_method) %>% tally()
# 1 - Capital and interest
# 2 - Interest only / endowment
# 3 - Interest only / ISA
# 4 - Interest only / pension
# 5 - Interest only / unknown
# 6 - Mix of capital and interest and interest only
# 7 - Unknown

df %>% group_by(type_borrower) %>% tally()
# 1 - First-time buyer
# 2 - Home movers
# 3 - Right to buy

df %>% group_by(main_emp_status) %>% tally()
# 1 - Employed
# 2 - Other
# 3 - Retired
# 4 - Self-employed

df %>% group_by(type_dwelling) %>% tally()
# 1 - Bungalow
# 2 - Detached
# 3 - Flat/Maisonette
# 4 - Other
# 5 - Semi-detached
# 6 - Terraced

df %>% group_by(new_dwelling) %>% tally()
# 1 - New
# 2 - Old


#One-hot-encoding features:
library(ade4)
library(data.table)


ohe_feats <-  c('type_int_rate', 'repay_method', 'type_borrower', 
              'main_emp_status', 'type_dwelling', 'new_dwelling')
for (f in ohe_feats){
  df_all_dummy <-  acm.disjonctif(df_all[f])
  df_all[f] <-  NULL
  df_all <-  cbind(df_all, df_all_dummy)
}



