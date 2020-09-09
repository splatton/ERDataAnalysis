#This file will contain the processing that will add columns to the frame to allow better processing of the correct variables. Many of these functions will require a fully joined frame that contains nursing, rads, and doctor information.

#This function will calculate the total number of dispositions in the next hour.

next_hour_dispos <- function(cleaned_trackdata) {
  library(dplyr)
  library(lubridate)
  onehour <- hours(1)
  NextHrDisp <- vector()
  for (i in 1:nrow(cleaned_trackdata)) {
    tf_vector <- ((cleaned_trackdata[i, 'ARR'] <= cleaned_trackdata[, 'DIS']) & ((cleaned_trackdata[i, 'ARR'] + onehour) >= cleaned_trackdata[, 'DIS']))
    count <- sum(tf_vector, na.rm = TRUE)
    NextHrDisp <- c(NextHrDisp, count)
  }
  cleaned_trackdata <- cbind(cleaned_trackdata, NextHrDisp)
  return(cleaned_trackdata)
}

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

#This next function calculates the AMA intensity within 30 minutes on either side of the patient.

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

#This function calculates the LWBS intensity on either side of the patient by 30 minutes.

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

#This next function gives us the time of day (hour) that the patient arrived.

time_of_day <- function(cleaned_trackdata) {
  library(dplyr)
  library(lubridate)
  cleaned_trackdata <- mutate(cleaned_trackdata, Hour_of_Day = hour(ARR))
  return(cleaned_trackdata)
}

#This next function will create add two columns to the cleaned data. The first will be a value that is the total sum of time all OTHER patients spent in the ED while this patient was in the ED. The second value will be that amount divided by the total time this patient is in the ED.

ED_patient_density <- function(cleaned_trackdata) {
  library(dplyr)
  library(lubridate)
  PtDensity <- vector()
  PtDensityPerTime <- vector()
  working_trackdata <- filter(cleaned_trackdata, !(is.na(DEP)))
  working_trackdata <- filter(working_trackdata, !(is.na(ARR)))
  for(i in 1:nrow(cleaned_trackdata)) {
    if(is.na(cleaned_trackdata$ARR[i]) | is.na(cleaned_trackdata$DEP[i])) {
      PtDensity <- c(PtDensity, NA)
      PtDensityPerTime <- c(PtDensityPerTime, NA)
      next
    }
    temp_dur_vec <- c(0)
    temp_time_frame <- filter(working_trackdata, ARR < cleaned_trackdata$DEP[i], DEP > cleaned_trackdata$ARR[i])
    if(nrow(temp_time_frame) >= 1) {
    for(j in 1:nrow(temp_time_frame)) {
      temp_ARR <- temp_time_frame$ARR[j]
      temp_DEP <- temp_time_frame$DEP[j]
      if(cleaned_trackdata$ARR[i] > temp_ARR) {
        temp_ARR <- cleaned_trackdata$ARR[i]
      }
      if(cleaned_trackdata$DEP[i] < temp_DEP) {
        temp_DEP <- cleaned_trackdata$DEP[i]
      }
      temp_dur_j <- difftime(temp_DEP, temp_ARR, units = "mins")
      temp_dur_vec <- c(temp_dur_vec, temp_dur_j)
    }
    visit_time <- difftime(cleaned_trackdata$DEP[i], cleaned_trackdata$ARR[i], units = "mins")
    PtDensity <- c(PtDensity, sum(temp_dur_vec))
    PtDensityPerTime <- c(PtDensityPerTime, (sum(temp_dur_vec)/as.numeric(visit_time)))
    }
    else {
      PtDensity <- c(PtDensity, 0)
      PtDensityPerTime <- c(PtDensityPerTime, 0)
    }
  }
  cleaned_trackdata <- cbind(cleaned_trackdata, PtDensity, PtDensityPerTime)
  return(cleaned_trackdata)
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
#This function will attempt to calculate the physician's total undeparted patients, including holds.

phys_pt_load <- function(cleaned_dispoeddata) {
  library(dplyr)
  NumPts <- vector()
  TotalUndisp <- vector()
  UndispNumPts <- vector()
  PhysMentWork <- vector()
  working_trackdata <- filter(cleaned_dispoeddata, !(is.na(cleaned_dispoeddata$ARR)))
  working_trackdata <- filter(working_trackdata, !(is.na(working_trackdata$DEP)))
  for (i in 1:nrow(cleaned_dispoeddata)) {
    #The first TF vector is for total number of patients owned by the physician.
    tf_vector1 <- ((cleaned_dispoeddata[i, 'ARR'] >= working_trackdata[, 'ARR']) & (cleaned_dispoeddata[i, 'ARR'] <= working_trackdata[, 'DEP']) & (cleaned_dispoeddata[i, 'Likely.Doc'] == working_trackdata[, 'Likely.Doc']))
    count1 <- sum(tf_vector1, na.rm = TRUE)
    NumPts <- c(NumPts, count1)
    #The second TF vector is for total undispo'd patients.
    tf_vector2 <- ((cleaned_dispoeddata[i, 'ARR'] >= working_trackdata[, 'ARR']) & (cleaned_dispoeddata[i, 'ARR'] <= working_trackdata[, 'DIS']))
    count2 <- sum(tf_vector2, na.rm = TRUE)
    TotalUndisp <- c(TotalUndisp, count2)
    #The third TF vector determines the number of undispo'd patients specific to the physician.
    tf_vector3 <- tf_vector2 & (cleaned_dispoeddata[i, 'Likely.Doc'] == working_trackdata[, 'Likely.Doc'])
    count3 <- sum(tf_vector3, na.rm = TRUE)
    UndispNumPts <- c(UndispNumPts, count3)
    #This final part uses the third TF fr undispo'd patients owned by a specific physician and then the cleaned frame by patients still in the ED. It then plugs it into the mental work function.
    ment_work_frame <- cbind(working_trackdata, tf_vector3)
    ment_work_frame <- filter(ment_work_frame, tf_vector1)
    PhysMentWork <- c(PhysMentWork, phys_mental_work(ment_work_frame))
  }
  PhysLoad <- NumPts
  changed_trackdata <- cbind(cleaned_dispoeddata, PhysLoad, TotalUndisp, UndispNumPts, PhysMentWork)
  return(changed_trackdata)
}

#This function is made to work on the above phys_pt_load function and requires the frame to be filtered for patients that were actually in the ED and under the physician's name at the time of patient arrival. It will calculate a weighted total work level for the dataframe inputted. The inputted dataframe will also have a TF column that indicates T if the patient is undispo'd. This function is called within the function above and does not need to be called separately.

phys_mental_work <- function(phys_frame) {
  if (nrow(phys_frame) < 1) {
    return(0)
  }
  tr_frame <- data.frame(Level = c('Level 1', 'Level 2', 'Level 3', 'Level 4', 'Level 5', 'Unknown'), No = c(3, 2, 2, 1, 1, 1))
  ment_work_sum <- 0
  for (k in 1:nrow(phys_frame)) {
    if (phys_frame[k, 'tf_vector3'] | is.na(phys_frame[k, 'tf_vector3'])) {
      multiplier <- 1
    }
    else {
      multiplier <- 0.2
    }
    ment_work_sum <- ment_work_sum + (multiplier*subset(tr_frame, Level == phys_frame[k, 'Triage.Level'])[,'No'])
  }
  return(ment_work_sum)
}



#This next funtion will process all of the column-adding functions together, as well as add in some new basic functionality. The basic functions this adds in are dependent on the above processes and this includes: %of census that are holds,

tracker_process_complete <- function(cleaned_trackdata, cleaned_nursedata, cleaned_dispodata, cleaned_PGdata) {
  working_data <- holds_by_arrival(cleaned_trackdata)
  working_data <- total_census_arrival(working_data)
  working_data <- mutate(working_data, PercentHolds = ifelse(NumPts > 0, NumHolds/NumPts, NA))
  working_data <- arrival_itensity(working_data)
  working_data <- EMSarrival_itensity(working_data)
  working_data <- ICUholds_by_arrival(working_data)
  working_data <- LWBS_density(working_data)
  working_data <- AMA_density(working_data)
  working_data <- next_hour_dispos(working_data)
  working_data <- time_of_day(working_data)
  working_data <- ED_patient_density(working_data)
  #Now we add in columns for nursing data after processing all of the tracker data.
  working_data <- full_join(working_data, cleaned_nursedata, by = c('ARR', 'Pat.Acct..'))
  working_data <- active_nurses(working_data)
  working_data <- mutate(working_data, GreetToPainmed = PAINMED.GVN.D.T - PHY)
  working_data <- mutate(working_data, GreetToLab = LAB.COLL.D.T - PHY)
  working_data <- mutate(working_data, HoldsPerNurse = ifelse(ActiveRN > 0, NumHolds/ActiveRN, NA))
  working_data <- mutate(working_data, PtsPerNurse = ifelse(ActiveRN > 0, NumPts/ActiveRN, NA))
  #Next we add in columns from the dispo tracker.
  working_data <- full_join(working_data, cleaned_dispodata, by = c('ARR', 'Pat.Acct..'))
  working_data <- phys_pt_load(working_data)
  working_data <- mutate(working_data, UndispToDispRate = ifelse(NextHrDisp > 0, TotalUndisp/NextHrDisp, NA))
  working_data <- MLprocess(working_data)
  #Now we add in columns for PG data after processing all above data.
  working_data <- full_join(working_data, cleaned_PGdata, by = c('ARR', 'Patient.Age'))
  #Now we add in a column for the day of the week.
  working_data <- mutate(working_data, Day.Of.Week = wday(ARR))
  return(working_data)
}

#This next function will focus on processing Excel files that contain multiple sheets of data from across the division. Any sheet with the sheet name 'DESCRIPTIONS' should be discarded. The sheets will be kept in a list format for now and this will be what is returned.

process_division_list <- function(division_directory) {
  
}
