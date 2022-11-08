# ------------------------------------------------------------------------------------------------ #
#                           RST 2022 Data Validation Exercise                                      #
#                                                                                                  #
# The following code is used to validate your data and identify issues with the data that must     #
# be addressed. Keep in mind that every script you use to validate data you receive will vary      #
# depending on the specifics of that dataset. Be critical and careful in what issues may arise as  #
# you look at the data and documentation you receive from a provider. This code will produce a log #
# that will highlight the issues this data has in red. It will not break when there is an issue    #
# with the data.                                                                                   #
#                                                                                                  #
# ------------------------------------------------------------------------------------------------ #


# Change your working directory to match the folder path for where your csv data is stored
rm(list=ls())
setwd("INSERT DIRECTORY PATH TO THE FOLDER THAT CONTAINS THIS FILE HERE")

# If the library is not found, run the command install.packages("dplyr") below
# install.packages("dplyr")
library(dplyr)

# If you want to log your outputs to a txt file, use the sink command with your file name
sink("R_data_validation_log.txt")

# Import csv data as dataframes
pull1 <- read.csv("data/pull1.csv")
pull2 <- read.csv("data/pull2.csv")
pull3 <- read.csv("data/pull3.csv")

pullList = list(pull1, pull2, pull3)

validate_data <- function() {
  for (pull in pullList) {
    pull_number <- pull$pull[1]
    # Does ID uniquely identify observations in pull?
    check_dup_id <- duplicated(pull$id)
    if (any(check_dup_id) == TRUE) {
      duplicated_ids <- pull %>% group_by(id) %>% filter(n() > 1)
      print(paste("ERROR: DUPLICATE IDS EXIST IN DATASET", pull_number, "THE FOLLOWING HAVE A NON-UNIQUE ID"))
      duplicated_ids %>% as_tibble() %>% select(id, name_first, name_last, dob_day, dob_month, dob_year) %>% print(n=Inf, quote = TRUE)
      
      check_dup_id_name <- duplicated(pull[c("id", "name_first", "name_last")])
      if (any(check_dup_id_name) == TRUE) {
        print(paste("ERROR: ID, first and last name DO NOT uniquely identify observations in `pull", pull_number))
      } else {
        print(paste("ID, first and last name uniquely identify observations in pull", pull_number))
      }
    } else {
      print(paste("No duplicate IDs exist in pull", pull_number))
    }
  }
  rm(check_dup_id, check_dup_id_name, duplicated_ids, pull, pull_number)
  # Duplicate IDs exist in the data! We need to flag this for the data provider to understand why.
  
  # In pull 2, the data is a string instead of numeric. We'll destring pull2 and verify this didn't create any missing variables.
  pull2$id = as.numeric(pull2$id)
  sum(is.na(pull2$id))
  
  if (sum(is.na(pull2$id)) == 0) {
  	print("No missing variables created, all IDs were numeric.")
  } else {
    print("ERROR: Nonnumeric IDs identified.")
  }
  # Great, it looked like the data just had some leading zeroes for the IDs.")
  ## Be very careful when you are changing your variables from strings to numeric. In some cases, leading zeroes are necessary -- such as when you are dealing with zip codes.
  
  # Step 2 -- Append data.
  allpulls <- rbind(pull1, pull2, pull3)
  allpulls = allpulls[order(allpulls$id, allpulls$name_last, allpulls$name_first, allpulls$pull, decreasing = FALSE),]
  # We can also write this merged data set to a CSV file if we wish to store it for future use.
  # write.csv(allpulls, "data/allpulls.csv")
  
  # STEP 3 -- Verify we have appropriate follow-up information on the people is consistent.
  incorrect_number_of_pulls <- allpulls %>% group_by(id,name_first, name_last) %>% filter(n() != 2 & pull == (1 | 2))
  if (nrow(incorrect_number_of_pulls) > 0) {
    print("Some people did not have a follow up recorded in data. See the following:")
    incorrect_number_of_pulls %>% as_tibble() %>% select(id, name_first, name_last, pull) %>% print(n = Inf)
  } else {
    print("Follow up visits recorded for all expected individuals.")
  }
  rm(incorrect_number_of_pulls)
  
  nonconsecutive_pulls <- allpulls %>% group_by(id,name_first, name_last) %>% filter((n() == 2) & (pull[2] != (pull[1] + 1)))
  if (nrow(nonconsecutive_pulls) > 0) {
    print("Some people did not have a CONSECUTIVE follow up recorded in data. See the following:")
    nonconsecutive_pulls %>% as_tibble() %>% select(id, name_first, name_last, pull) %>% print(n = Inf)
  } else {
    print("Follow up visits recorded consecutively for all expected individuals.")
  }
  rm(nonconsecutive_pulls)
  
  keylist <- c("male", "dob_day", "dob_month", "dob_year", "region_s", "region_mid", "region_ne", "region_w", "educ", "treat")
  for (key in keylist) {
    inconsistent_pulls <- allpulls %>% group_by(id,name_first, name_last) %>% filter((n() == 2) & ((!!as.symbol(key))[1] != (!!as.symbol(key))[2]))
    if (nrow(inconsistent_pulls) > 0) {
      print(paste("The variable", key, "is not consistent across pulls for the following people:"))
      inconsistent_pulls %>% as_tibble() %>% select(id, name_first, name_last, all_of(key), pull) %>% print(n = Inf)
    } else {
      print(paste("The variable", key,  "is consistent across pulls."))
    }
  }
  rm(key, keylist, inconsistent_pulls)
    
  # It seems like there are a few quality issues with our data. 
  ## Some seem to be due to typos and others look like the provider changed how they coded key variables.
  
  
  # We will now check that the reported values fall within the expected range for each variable.
  # Rather than rewriting the code to do so for every variable, we can write one function 
  # and use it for all of the variables we want to check.
  range_check <- function(string_variable_name, minimum, maximum) {
    out_of_range_entries <- allpulls %>% filter(between((!!as.symbol(string_variable_name)), minimum, maximum) == FALSE)
    if (nrow(out_of_range_entries) > 0) {
      print(paste("Unexpected value(s) in the", string_variable_name, "variable. See below:"))
      out_of_range_entries %>% as_tibble() %>% select(id, name_first, name_last, (!!as.symbol(string_variable_name)), pull) %>% print(n = Inf)
    } else {
      print(paste("All values in", string_variable_name, "variable are between", minimum, "and", maximum))
    }
  }
  
  # Step 4 -- Verify education is coded between 0 and 4.
  range_check("educ", 0, 4)
  #In this case, this is most likely not a typo, but rather a change in how education was coded for one pull.
  
  # Step 5 -- Confirm that birth dates are input correctly
  # Check that the values of birth years are reasonable
  range_check("dob_year", 1922, 2022)

  # Check that the birth day values are between 1 and 31
  range_check("dob_day", 1, 31)
  
  # Check that the birth month values are between 1 and 12
  range_check("dob_month", 1, 12)
  
  # STEP 6 -- Confirm that treatment is assigned correctly. 
  region_check <- allpulls %>% filter(rowSums(allpulls[c("region_s", "region_mid", "region_ne", "region_w")]) != 1)
  if (nrow(region_check) > 0) {
    print("Some observations are either assigned to multiple regions or not assigned to a region.")
    region_check %>% as_tibble() %>% select(id, name_first, name_last, region_s, region_mid, region_ne, region_w, pull) %>% print(n = Inf)
  } else {
    print("All observations were assigned to exactly one region.")
  }
  rm(region_check)
  
  # Step 7 -- Confirm that sex is coded as expected
  range_check("male", 0, 1)
  # We expect that male is an indicator variable with 1 for male and 0 for female.
  # It looks like for one of the pulls, rather than code this variable as 0 and 1, 
  # they coded it as 1 and 2. Confirm this is the case with the provider.
  
  #STEP 8 -- Confirm that application date matches application status
  missing_apply_date <- allpulls %>% filter((prog_apply == 1) & (is.na(day_apply | month_apply | year_apply) == TRUE))
  if (nrow(missing_apply_date) > 0) {
    print("Some individuals coded as applying to the program, but no date included.")
    missing_apply_date %>% as_tibble() %>% select(id, name_first, name_last, prog_apply, month_apply, day_apply, year_apply, pull) %>% print(n = Inf)
  } else {
    print("All individuals who are marked as applying have an application date.")
  }
  rm(missing_apply_date)
  
  missing_apply_dummy <- allpulls %>% filter((prog_apply == 0) & (is.na(day_apply | month_apply | year_apply) == FALSE)) 
  if (nrow(missing_apply_dummy) > 0) {
    print("Some individuals coded as not applying to the program, but an application date is included.")
    missing_apply_dummy %>% as_tibble() %>% select(id, name_first, name_last, prog_apply, month_apply, day_apply, year_apply, pull) %>% print(n = Inf)
  } else {
    print("All individuals who are marked as not applying have a missing application date.")
  }
  rm(missing_apply_dummy)
  
  # STEP 9 -- Confirm that application dates are inputted correctly.
  range_check("year_apply", 2021, 2022)
  range_check("month_apply", 1, 12)
  range_check("day_apply", 1, 31)
  
}

validate_data()

## Use the sink() command to close your log file
sink()