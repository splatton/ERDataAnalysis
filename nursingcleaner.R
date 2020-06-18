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
  return(uncleaned_frame)
}
