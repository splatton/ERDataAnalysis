#This function will clean any nursing tracker reports for merging with the general ED tracking reports.

nursing_cleaner <- function(trackpath, skip_num = 0) {
  #Loads libraries.
  library(lubridate)
  library(dplyr)
  library(stringr)
  #Gets the csv and skips the extra data at the top.
  uncleaned_frame <- read.csv(trackpath, skip = skip_num, stringsAsFactors = FALSE)
  uncleaned_frame <- select(uncleaned_frame, -(GROUP.NAME:FACILITY.NAME))
  uncleaned_frame <- select(uncleaned_frame, -(BH.PATIENT:FIRST.ER.LOCATION))
  uncleaned_frame <- select(uncleaned_frame, -(FOUND.BY), -(DISP.D.T:DEPART.D.T))
  uncleaned_frame <- select(uncleaned_frame, -X.1, -X.2, -LAB.ORD.D.T)
  uncleaned_frame <- select(uncleaned_frame, -(PO.CONT.D.T:ASSIGN.LEAVE))
  uncleaned_frame <- select(uncleaned_frame, -(FIRST.LAB.ORD.COL:COLLECTED.before.ORD))
  uncleaned_frame <- select(uncleaned_frame, -(CT.PO.CONT:Chief.Complaint))
  uncleaned_frame <- select(uncleaned_frame, -ANTIBIOTIC.NAME, -LAB.RECV.D.T)
  uncleaned_frame[, 'ARRIVAL.D.T'] <- as.POSIXct(mdy_hm(uncleaned_frame[, 'ARRIVAL.D.T']))
  uncleaned_frame[, 'MED.ORD.D.T'] <- as.POSIXct(mdy_hm(uncleaned_frame[, 'MED.ORD.D.T']))
  uncleaned_frame[, 'MED.GVN.D.T'] <- as.POSIXct(mdy_hm(uncleaned_frame[, 'MED.GVN.D.T']))
  uncleaned_frame[, 'PAINMED.ORD.D.T'] <- as.POSIXct(mdy_hm(uncleaned_frame[, 'PAINMED.ORD.D.T']))
  uncleaned_frame[, 'PAINMED.GVN.D.T'] <- as.POSIXct(mdy_hm(uncleaned_frame[, 'PAINMED.GVN.D.T']))
  uncleaned_frame[, 'ANITBIOTIC.MED.ORD.D.T'] <- as.POSIXct(mdy_hm(uncleaned_frame[, 'ANITBIOTIC.MED.ORD.D.T']))
  uncleaned_frame[, 'ANTIBIOTIC.ADMIN.D.T'] <- as.POSIXct(mdy_hm(uncleaned_frame[, 'ANTIBIOTIC.ADMIN.D.T']))
  uncleaned_frame[, 'LAB.COLL.D.T'] <- as.POSIXct(mdy_hm(uncleaned_frame[, 'LAB.COLL.D.T']))
  uncleaned_frame[, 'UA.ORD.D.T'] <- as.POSIXct(mdy_hm(uncleaned_frame[, 'UA.ORD.D.T']))
  uncleaned_frame[, 'UA.COLL.D.T'] <- as.POSIXct(mdy_hm(uncleaned_frame[, 'UA.COLL.D.T']))
  uncleaned_frame[, 'CT.ORD.D.T'] <- as.POSIXct(mdy_hm(uncleaned_frame[, 'CT.ORD.D.T']))
  uncleaned_frame %>%
    mutate(MED.ORD.ADM = MED.GVN.D.T - MED.ORD.D.T) %>%
    mutate(PAINMED.ORD.ADM = PAINMED.GVN.D.T - PAINMED.ORD.D.T) %>%
    mutate(UA.ORD.COL = UA.COLL.D.T - UA.ORD.D.T)
  uncleaned_frame <- rename(uncleaned_frame, ARR = ARRIVAL.D.T, Pat.Acct.. = Pat.Acc.)
  cleaned_frame <- arrange(uncleaned_frame, ARR)
  #Now we will call an external function that removes the ecoding from the nurses' names to allow us to sort them appropriately.
  cleaned_frame <- nurse_name_cleaner(cleaned_frame)
  return(cleaned_frame)
}

#This function will appropriately clean the nurses' names to allow us to sort them appropriately.

nurse_name_cleaner <- function(nurse_data) {
  library(stringr)
  for (i in 1:nrow(nurse_data)) {
    nurse_data[i, 'NURSE'] <- str_split(nurse_data[i, 'NURSE'], "[:space:]", n = 2)[[1]][1]
    nurse_data[i, 'NURSE'] <- str_split(nurse_data[i, 'NURSE'], "\\(", n = 2)[[1]][1]
  }
  return(nurse_data)
}

#This function will take a data frame and change the column labeled 'NURSE' so that it only contains nurses with greater than X total patient encounters recorded. It replaces these nurses with small numbers of encounters with the string 'Unkown'.

nurse_name_freq_cleaner <- function(nurse_data, min_num) {
  library(dplyr)
  nurse_summary <- summarize(group_by(nurse_data, NURSE), n = n())
  nurse_summary <- mutate(nurse_summary, MaxNURSE = ifelse(n >= min_num, NURSE, 'Nope'))
  for (i in 1:nrow(nurse_data)) {
    if(!(nurse_data[i, 'NURSE'] %in% nurse_summary$MaxNURSE)) {
      nurse_data[i, 'NURSE'] <- 'UNKNOWN'
    }
  }
  return(nurse_data)
}
