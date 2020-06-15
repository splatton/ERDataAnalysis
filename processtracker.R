#This next function will append a column at the end of the table of a cleaned tracker file by adding a column that tells how many holds are present in the department at the time of arrival.

holds_by_arrival <- function(cleaned_trackdata) {
  library(dplyr)
  NumHolds <- vector()
  working_trackdata <- filter(cleaned_trackdata, !(is.na(cleaned_trackdata$Hold.StartTime)))
  working_trackdata <- filter(working_trackdata, !(is.na(working_trackdata$DEP)))
  for (i in 1:nrow(cleaned_trackdata)) {
    tf_vector <- ((cleaned_trackdata[i, 'ARR'] >= working_trackdata[, 'Hold.StartTime']) & (cleaned_trackdata[i, 'ARR'] <= working_trackdata[, 'DEP']))
    count <- sum(tf_vector, na.rm = TRUE)
    NumHolds <- c(NumHolds, count)
  }
  changed_trackdata <- cbind(cleaned_trackdata, NumHolds)
  return(changed_trackdata)
}

#This next function will count the number of patients in the department at each individual patient's arrival time.

total_census_arrival <- function(cleaned_trackdata) {
  library(dplyr)
  NumPts <- vector()
  working_trackdata <- filter(cleaned_trackdata, !(is.na(cleaned_trackdata$ARR)))
  working_trackdata <- filter(working_trackdata, !(is.na(working_trackdata$DEP)))
  for (i in 1:nrow(cleaned_trackdata)) {
    tf_vector <- ((cleaned_trackdata[i, 'ARR'] >= working_trackdata[, 'ARR']) & (cleaned_trackdata[i, 'ARR'] <= working_trackdata[, 'DEP']))
    count <- sum(tf_vector, na.rm = TRUE)
    NumPts <- c(NumPts, count)
  }
  changed_trackdata <- cbind(cleaned_trackdata, NumPts)
  return(changed_trackdata)
}

#This next function will create a column that expresses the volume intensity at the time of arrival noted by the number of patients that have arrived in the ED for a thirty minute window on either side.
arrival_itensity <- function(cleaned_trackdata) {
  library(dplyr)
  library(lubridate)
  NumPts <- vector()
  working_trackdata <- filter(cleaned_trackdata, !(is.na(cleaned_trackdata$ARR)))
  for (i in 1:nrow(cleaned_trackdata)) {
    tf_vector <- (((cleaned_trackdata[i, 'ARR'] + minutes(30)) >= working_trackdata[, 'ARR']) & ((cleaned_trackdata[i, 'ARR'] - minutes(30)) <= working_trackdata[, 'ARR']))
    count <- sum(tf_vector, na.rm = TRUE)
    NumPts <- c(NumPts, count)
  }
  Arr_Int <- NumPts
  changed_trackdata <- cbind(cleaned_trackdata, Arr_Int)
  return(changed_trackdata)
}

#This function will return the number of ICU holds present in the ED at any one time.

ICUholds_by_arrival <- function(cleaned_trackdata) {
  library(dplyr)
  NumHolds <- vector()
  working_trackdata <- filter(cleaned_trackdata, !(is.na(cleaned_trackdata$Hold.StartTime)))
  working_trackdata <- filter(working_trackdata, !(is.na(working_trackdata$DEP)))
  working_trackdata <- filter(working_trackdata, BED.Type == 'ICU')
  for (i in 1:nrow(cleaned_trackdata)) {
    tf_vector <- ((cleaned_trackdata[i, 'ARR'] >= working_trackdata[, 'Hold.StartTime']) & (cleaned_trackdata[i, 'ARR'] <= working_trackdata[, 'DEP']))
    count <- sum(tf_vector, na.rm = TRUE)
    NumHolds <- c(NumHolds, count)
  }
  ICUHolds <- NumHolds
  changed_trackdata <- cbind(cleaned_trackdata, ICUHolds)
  return(changed_trackdata)
}

#This next function will calculate the EMS arrival intensity - the count of all ambulance arrivals within 30 minutes on either side.

EMSarrival_itensity <- function(cleaned_trackdata) {
  library(dplyr)
  library(lubridate)
  NumPts <- vector()
  working_trackdata <- filter(cleaned_trackdata, !(is.na(cleaned_trackdata$ARR)))
  working_trackdata <- filter(working_trackdata, Rescue != 'N')
  for (i in 1:nrow(cleaned_trackdata)) {
    tf_vector <- (((cleaned_trackdata[i, 'ARR'] + minutes(30)) >= working_trackdata[, 'ARR']) & ((cleaned_trackdata[i, 'ARR'] - minutes(30)) <= working_trackdata[, 'ARR']))
    count <- sum(tf_vector, na.rm = TRUE)
    NumPts <- c(NumPts, count)
  }
  EMS_Int <- NumPts
  changed_trackdata <- cbind(cleaned_trackdata, EMS_Int)
  return(changed_trackdata)
}

#This following function will attempt to guess the physician taking care of the patient. The function will also add likely active start and stop times. This function will only be run with ED tracker data so that it can then be merged into the PG data. An interesting part of this will be how it allows us to see if the PG assessments capture the correct doctor. To do this, the function will first check to see if the FIRST.ER.PROVIDER is an MD or a DO. The function will then check to see if this patient arrived between 6 and 7 AM and was undispositioned until after 7:30. If this is the case, the assumed physician is the next physician seen. At this point, these values are placed in the column for likely physician. Finally, all further patients are assumed to be cared for by the same physician as the previous patient until another physician is listed.

#This next funtion will process all of the column-adding functions together, as well as add in some new basic functionality. The basic functions this adds in are dependent on the above processes and this includes: %of census that are holds,

tracker_process_complete <- function(cleaned_trackdata) {
  working_data <- holds_by_arrival(cleaned_trackdata)
  working_data <- total_census_arrival(working_data)
  working_data <- mutate(working_data, PercentHolds = NumHolds/NumPts)
  working_data <- arrival_itensity(working_data)
  working_data <- EMSarrival_itensity(working_data)
  working_data <- ICUholds_by_arrival(working_data)
  return(working_data)
}
