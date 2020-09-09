#This file will focus on cleaning the Radiology TAT data to interface with existing data.

radscleaner <- function(radspath, skip_num = 0) {
  library(dplyr)
  library(lubridate)
  library(caret)
  library(RANN)
  working_data <- read.csv(radspath, skip = skip_num, stringsAsFactors = FALSE)
  working_data <- working_data %>%
    select(-(GROUP.NAME:FACILITY.NAME)) %>%
    select(-(BH.PATIENT:PEDS)) %>%
    select(-NUMBER) %>%
    select(-(ADMIT.D.T:GRT.D.T)) %>%
    select(-PO.CONT.D.T) %>%
    select(-DRAFT.D.T) %>%
    select(-(DISP.D.T:Patient.Type)) %>%
    select(-GRT.ORD) %>%
    select(-ST.CMP) %>%
    select(-ST.SGN) %>%
    select(-GRT.SGN)
  #This next part Will add a key category to the radiology exam table - the Exam.Type - a reflection of the practical category of radiology exam.
  radskey <- read.csv("radskey.csv", stringsAsFactors = FALSE)
  working_data <- mutate(working_data, Exam.Type = TEST.NAME)
  for (i in 1:nrow(working_data)) {
    if(working_data[i,'TEST.NAME'] %in% radskey$TEST.NAME) {
      working_data[i,'Exam.Type'] <- radskey$Exam.Type[which(radskey$TEST.NAME == working_data[i,'TEST.NAME'])]
    }
  }
  #This next part will remove the listed order and convert the times to appropriate posixCT classes. As well, we will rename some of the columns for joining.
  working_data <- select(working_data, -TEST.NAME)
  working_data <- rename(working_data, ARR = ARRIVAL.D.T)
  working_data <- rename(working_data, Pat.Acct.. = Pat.Acc.)
  working_data[,'ARR'] <- as.POSIXct(mdy_hm(working_data[,'ARR']))
  working_data[,'ORD.D.T'] <- as.POSIXct(mdy_hm(working_data[,'ORD.D.T']))
  working_data[,'START.TAKEN.D.T'] <- as.POSIXct(mdy_hm(working_data[,'START.TAKEN.D.T']))
  working_data[,'CMP.D.T'] <- as.POSIXct(mdy_hm(working_data[,'CMP.D.T']))
  working_data[,'SGN.D.T'] <- as.POSIXct(mdy_hm(working_data[,'SGN.D.T']))
  #This next function will one-hot encode the Exam.Type and then group the results by the patient account number and arrival time.
  rads_dummy <- dummyVars(~ Exam.Type, data = working_data)
  working_data <- cbind(working_data, data.frame(predict(rads_dummy, newdata = working_data)))
  return(working_data)
}

rads_to_PG_cleaner <- function(cleaned_rads) {
  library(dplyr)
  cleaned_rads <- cleaned_rads %>%
    select(-TYPE) %>%
    select(-(ORD.D.T:Exam.Type))
  cleaned_rads <- summarize_at(group_by(cleaned_rads, Pat.Acct.., ARR), vars(Exam.TypeABD.XRAY:Exam.TypeVASCULAR.US), sum)
  return(cleaned_rads)
}