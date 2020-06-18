#This file will contain the processing that will add columns to the frame to allow better processing of the correct variables. Many of these functions will require a fully joined frame that contains nursing, rads, and doctor information.

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

AMA_density <- function(cleaned_trackdata) {
  library(dplyr)
  library(lubridate)
  NumPts <- vector()
  working_trackdata <- filter(cleaned_trackdata, !(is.na(cleaned_trackdata$DEP)))
  working_trackdata <- filter(working_trackdata, !(is.na(working_trackdata$ARR)))
  working_trackdata <- filter(working_trackdata, AMA == 'Y')
  for (i in 1:nrow(cleaned_trackdata)) {
    tf_vector <- (((cleaned_trackdata[i, 'ARR'] + minutes(30)) >= working_trackdata[, 'DEP']) & ((cleaned_trackdata[i, 'ARR'] - minutes(30)) <= working_trackdata[, 'DEP']))
    count <- sum(tf_vector, na.rm = TRUE)
    NumPts <- c(NumPts, count)
  }
  AMA_Int <- NumPts
  changed_trackdata <- cbind(cleaned_trackdata, AMA_Int)
  return(changed_trackdata)
}

LWBS_density <- function(cleaned_trackdata) {
  library(dplyr)
  library(lubridate)
  NumPts <- vector()
  working_trackdata <- filter(cleaned_trackdata, !(is.na(cleaned_trackdata$DEP)))
  working_trackdata <- filter(working_trackdata, !(is.na(working_trackdata$ARR)))
  working_trackdata <- filter(working_trackdata, LWBS)
  for (i in 1:nrow(cleaned_trackdata)) {
    tf_vector <- (((cleaned_trackdata[i, 'ARR'] + minutes(30)) >= working_trackdata[, 'DEP']) & ((cleaned_trackdata[i, 'ARR'] - minutes(30)) <= working_trackdata[, 'DEP']))
    count <- sum(tf_vector, na.rm = TRUE)
    NumPts <- c(NumPts, count)
  }
  LWBS_Int <- NumPts
  changed_trackdata <- cbind(cleaned_trackdata, LWBS_Int)
  return(changed_trackdata)
}

#THIS FUNCTION REQUIRES A JOINED NURSING TRACKER.
#This function will calculate the number of active nurses in the department for each patient.
active_nurses <- function(clean_rneddata) {
  clean_rneddata <- mutate(clean_rneddata, ActiveRN = 0)
  for (i in 1:nrow(clean_rneddata)) {
    #This calculates the different formulas for finding how many active nurses are in the department accounting for shift change.
    if(3 <= hour(clean_rneddata[i, 'ARR']) & hour(clean_rneddata[i, 'ARR']) < 6) {
      loop_frame <- filter(clean_rneddata, date(clean_rneddata[i, 'ARR']) == date(ARR))
      loop_frame <- filter(loop_frame, (3 <= hour(ARR) & hour(ARR) < 6))
      clean_rneddata[i, 'ActiveRN'] <- nrow(distinct(loop_frame, NURSE))
    }
    else if(6 <= hour(clean_rneddata[i, 'ARR']) & hour(clean_rneddata[i, 'ARR']) < 9) {
      loop_frame <- filter(clean_rneddata, date(clean_rneddata[i, 'ARR']) == date(ARR))
      loop_frame <- filter(loop_frame, (6 <= hour(ARR) & hour(ARR) < 9))
      clean_rneddata[i, 'ActiveRN'] <- nrow(distinct(loop_frame, NURSE))
    }
    else if(16 <= hour(clean_rneddata[i, 'ARR']) & hour(clean_rneddata[i, 'ARR']) < 18) {
      loop_frame <- filter(clean_rneddata, date(clean_rneddata[i, 'ARR']) == date(ARR))
      loop_frame <- filter(loop_frame, (16 <= hour(ARR) & hour(ARR) < 18))
      clean_rneddata[i, 'ActiveRN'] <- nrow(distinct(loop_frame, NURSE))
    }
    else if(18 <= hour(clean_rneddata[i, 'ARR']) & hour(clean_rneddata[i, 'ARR']) < 20) {
      loop_frame <- filter(clean_rneddata, date(clean_rneddata[i, 'ARR']) == date(ARR))
      loop_frame <- filter(loop_frame, (18 <= hour(ARR) & hour(ARR) < 20))
      clean_rneddata[i, 'ActiveRN'] <- nrow(distinct(loop_frame, NURSE))
    }
    else {
      loop_frame <- filter(clean_rneddata, (clean_rneddata[i, 'ARR'] - hours(2)) <= ARR & ARR <= clean_rneddata[i, 'ARR'])
      clean_rneddata[i, 'ActiveRN'] <- nrow(distinct(loop_frame, NURSE))
    }
  }
  return(clean_rneddata)
}

#BELOW REQUIRES MERGED TRACKER AND DISPO DATA.
#This function will attempt to calculate the physician's undispo'd patients.
phys_pt_load <- function(cleaned_dispoeddata) {
  library(dplyr)
  NumPts <- vector()
  working_trackdata <- filter(cleaned_dispoeddata, !(is.na(cleaned_dispoeddata$ARR)))
  working_trackdata <- filter(working_trackdata, !(is.na(working_trackdata$DEP)))
  for (i in 1:nrow(cleaned_dispoeddata)) {
    tf_vector <- ((cleaned_dispoeddata[i, 'ARR'] >= working_trackdata[, 'ARR']) & (cleaned_dispoeddata[i, 'ARR'] <= working_trackdata[, 'DEP']) & (cleaned_dispoeddata[i, 'Likely.Doc'] == working_trackdata[, 'Likely.Doc']))
    count <- sum(tf_vector, na.rm = TRUE)
    NumPts <- c(NumPts, count)
  }
  PhysLoad <- NumPts
  changed_trackdata <- cbind(cleaned_dispoeddata, PhysLoad)
  return(changed_trackdata)
}

#This next funtion will process all of the column-adding functions together, as well as add in some new basic functionality. The basic functions this adds in are dependent on the above processes and this includes: %of census that are holds,

tracker_process_complete <- function(cleaned_trackdata, cleaned_nursedata, cleaned_dispodata, cleaned_PGdata) {
  working_data <- holds_by_arrival(cleaned_trackdata)
  working_data <- total_census_arrival(working_data)
  working_data <- mutate(working_data, PercentHolds = NumHolds/NumPts)
  working_data <- arrival_itensity(working_data)
  working_data <- EMSarrival_itensity(working_data)
  working_data <- ICUholds_by_arrival(working_data)
  #Now we add in columns for nursing data after processing all of the tracker data.
  working_data <- full_join(working_data, cleaned_nursedata, by = c('ARR', 'Pat.Acct..'))
  working_data <- mutate(working_data, GreetToPainmed = PAINMED.GVN.D.T - PHY)
  working_data <- mutate(working_data, GreetToLab = LAB.COLL.D.T - PHY)
  working_data <- mutate(working_data, HoldsPerNurse = NumHolds/ActiveRN)
  working_data <- mutate(working_data, PtsPerNurse = NumPts/ActiveRN)
  #Next we add in columns from the dispo tracker.
  working_data <- full_join(working_data, cleaned_dispodata, by = c('ARR', 'Pat.Acct..'))
  working_data <- phys_pt_load(working_data)
  #Now we add in columns for PG data after processing all above data.
  working_data <- full_join(working_data, cleaned_PGdata, by = c('ARR', 'Patient.Age'))
  return(working_data)
}

#This next function will scale the appropriate columns for better analysis. This requires a complete union of ED tracker, nursing tracker, and dispo tracker.

scale_tracker <- function(complete_tracker) {
  working_tracker <- mutate(working_tracker, Good.Day = LWBS_Int+AMA_Int+PtsPerNurse+NumHolds+ICUHolds+NumPts+EMS_Int+PhysLoad+Arr_Int)
  
}
