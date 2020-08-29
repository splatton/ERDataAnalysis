#This file will contain functions with the purpose of processing a completely cleaned and joined frame for analysis by scaling and reordering columns, as well as creating new variable which require scaled inputs to use.

#This first function will group ages into subgroups based on decades of life.

age_grouping <- function(working_frame) {
  library(dplyr)
  working_frame <- filter(working_frame, !(is.na(Patient.Age)))
  working_frame <- mutate(working_frame, Age.Grouping = NA)
  for (i in 1:nrow(working_frame)) {
    temp_age <- working_frame[i, 'Patient.Age']
    if (temp_age < 19) {
      working_frame[i, 'Age.Grouping'] <- '<19'
    }
    else if ((temp_age < 41) & (temp_age >= 19)) {
      working_frame[i, 'Age.Grouping'] <- '19-40'
    }
    else if ((temp_age < 66) & (temp_age >= 41)) {
      working_frame[i, 'Age.Grouping'] <- '41-65'
    }
    else if (temp_age >= 65) {
      working_frame[i, 'Age.Grouping'] <- '>65'
    }
  }
  working_frame[, 'Age.Grouping'] <- as.factor(working_frame[, 'Age.Grouping'])
  return(working_frame)
}

#This next function will take patients and sort them into grouped hourly arrival times with three-hour intervals.

hour_grouping <- function(working_frame) {
  library(dplyr)
  working_frame <- mutate(working_frame, Hour.Grouping = NA)
  for (i in 1:nrow(working_frame)) {
    temp_hour <- working_frame[i, 'Hour_of_Day']
    if (temp_hour < 3) {
      working_frame[i, 'Hour.Grouping'] <- '0-2'
    }
    else if ((temp_hour < 6) & (temp_hour >= 3)) {
      working_frame[i, 'Hour.Grouping'] <- '3-5'
    }
    else if ((temp_hour < 9) & (temp_hour >= 6)) {
      working_frame[i, 'Hour.Grouping'] <- '6-8'
    }
    else if ((temp_hour < 12) & (temp_hour >= 9)) {
      working_frame[i, 'Hour.Grouping'] <- '9-11'
    }
    else if ((temp_hour < 15) & (temp_hour >= 12)) {
      working_frame[i, 'Hour.Grouping'] <- '12-14'
    }
    else if ((temp_hour < 18) & (temp_hour >= 15)) {
      working_frame[i, 'Hour.Grouping'] <- '15-17'
    }
    else if ((temp_hour < 21) & (temp_hour >= 18)) {
      working_frame[i, 'Hour.Grouping'] <- '18-20'
    }
    else if (temp_hour >= 21) {
      working_frame[i, 'Hour.Grouping'] <- '21-23'
    }
  }
  working_frame[, 'Hour.Grouping'] <- as.factor(working_frame[, 'Hour.Grouping'])
  return(working_frame)
}

#FOR PRESS-GANEY ANALYSIS. This next function will select only discharged patients and will dispose of columns not useful for imputation or PG analysis. It will also turn any remaining character or logical vectors to factors. It will substitue Unknown or None for NA.

change_columns_4_analysis <- function(cleaned_data) {
  library(dplyr)
  working_data <- cleaned_data %>%
    select(-Pat.Acct..) %>%
    select(-INTNO) %>%
    select(-(ADMIT.ORDER.DATE:Disp.A)) %>%
    select(-(DIS:FIRST.ER.PROVIDER)) %>%
    select(-PHY.DIS.A.) %>%
    select(-(BED.Type:Holdtime)) %>%
    select(-(MED.ORD.D.T:CT.ORD.D.T)) %>%
    select(-Patient_Status) %>%
    select(-(LAST.RAD.SIGNED:DISP.T)) %>%
    select(-(ER.PROVIDER.MNEMONIC.NAME...DISP:LAST.MED.ADMINISTERED)) %>%
    select(-Hour_of_Day) %>%
    select(-(Very.Poor.n:Very.Good.n)) %>%
    select(-Doc) %>%
    select(-PGLOS)
  #Next we will filter only for patients that were discharged. This will cause us to lose some PG data.
  working_data <- filter(working_data, !is.na(Disp.D))
  #Now we will filter out for any columns that are missing values in the Likely.Doc column.
  working_data <- filter(working_data, !(is.na(Likely.Doc)))
  working_data <- filter(working_data, !(Likely.Doc == "EDDOC - Generic MD for EDM(TXG-EDDOC-ED)"))
  #Next we will change the time columns to numbers.
  working_data <- mutate(working_data, ARR = as.numeric(ARR))
  working_data[, 'Disp.D'] <- as.numeric(working_data[, 'Disp.D'])
  working_data[, 'GreetToPainmed'] <- as.numeric(working_data[, 'GreetToPainmed'])
  working_data[, 'GreetToLab'] <- as.numeric(working_data[, 'GreetToLab'])
  #Next we will take any NAs in the Race and Sex columns and change them to 'Unknown'.
  for (i in 1:nrow(working_data)) {
    if (is.na(working_data[i, 'Race'])) {
      working_data[i, 'Race'] <- 'Unknown'
    }
  }
  for (i in 1:nrow(working_data)) {
    if (is.na(working_data[i, 'Sex'])) {
      working_data[i, 'Sex'] <- 'Unknown'
    }
  }
  #Next we will change any non-factor variables to factors.
  working_data <- as.data.frame(unclass(working_data))
  working_data[, 'LWBS'] <- as.factor(working_data[, 'LWBS'])
  working_data[, 'TopBox'] <- as.factor(working_data[, 'TopBox'])
  #working_data[, 'Quarter'] <- as.factor(working_data[, 'Quarter'])
  return(working_data)
}

#This next function will impute values for numerical data.

PG_impute <- function(cleaned_data) {
  library(caret)
  library(dplyr)
  library(RANN)
  #First we will separate out the columns that we want imputed since we don't want to impute certain things like the TopBox value.
  working_impute_frame <- cleaned_data %>%
    select(-TopBox) %>%
    select(-Score) %>%
    select(-Even.Day)
  #Now we will impute the remaining values using knn.
  impute_model <- preProcess(working_impute_frame, method = 'knnImpute')
  working_impute_frame <- predict(impute_model, newdata = working_impute_frame)
  #Now we will join the removed data back into the total frame to create out output.
  remainder_frame <- select(cleaned_data, Score, TopBox, Even.Day)
  final_frame <- cbind(working_impute_frame, remainder_frame)
  return(final_frame)
}

#This next function will select for only PG respondents adn then will implement one-hot encoding for categorical variables. This will focus on predicting TopBox respondents only.

PG_one_hot <- function(working_data) {
  library(caret)
  library(dplyr)
  library(RANN)
  #First we will take only the know PG scores. We also want to get rid of the Scores column.
  #working_data <- filter(imputed_PG, !(is.na(TopBox)))
  #working_data <- select(working_data, -Score)
  #Also, we will lose the Self.Pay column since this factor has only one level.
  #working_data <- select(working_data, -Self.Pay)
  #We will also temporarily lose the nursing column due to the number of factors. We may try to add this back in later when we can select for nurses that have an n > 10.
  #working_data <- select(working_data, - NURSE)
  #Next we will one-hot encode the rest of the data.
  dummies_model <- dummyVars(TopBox ~ ., data = working_data)
  final_data <- data.frame(predict(dummies_model, newdata = working_data))
  final_data <- mutate(final_data, TopBox = as.numeric(working_data$TopBox)-1)
  return(final_data)
}
