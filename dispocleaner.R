#This function will clean any dispo tracker reports for merging with the general ED tracking reports.

dispo_cleaner <- function(trackpath, skip_num = 0) {
  #Loads libraries.
  library(lubridate)
  library(dplyr)
  library(stringr)
  #Gets the csv and skips the extra data at the top.
  uncleaned_frame <- read.csv(trackpath, skip = skip_num, stringsAsFactors = FALSE)
  uncleaned_frame <- select(uncleaned_frame, -(GROUP:FACILITY))
  uncleaned_frame <- rename(uncleaned_frame, ARR = ARRIVAL, Pat.Acct.. = PT.ACCT..)
  uncleaned_frame <- rename(uncleaned_frame, Likely.Doc = FIRST.ER.PROVIDER)
  uncleaned_frame <- select(uncleaned_frame, -(INTNO:SELF.PAY))
  uncleaned_frame <- select(uncleaned_frame, -(TRIAGE:LAST.MED.ADMIN))
  uncleaned_frame <- select(uncleaned_frame, -(LAST.RESULT..LAB.MED.RAD.:DISP.D))
  uncleaned_frame <- select(uncleaned_frame, -(DISP.A:LAST.READY..LAB.MED.RAD.))
  uncleaned_frame <- select(uncleaned_frame, -(DISPO.NURSE.MNEMONIC.NAME...DISP:LAST.LAB.TEST.VERIFIED))
  uncleaned_frame <- select(uncleaned_frame, -(LAST.RESULT.TYPE:X.1))
  uncleaned_frame[, 'ARR'] <- parse_date_time(uncleaned_frame[, 'ARR'], '%m/%d/%Y %I:%M:%S %p')
  uncleaned_frame[, 'LAST.RAD.SIGNED'] <- parse_date_time(uncleaned_frame[, 'LAST.RAD.SIGNED'], '%m/%d/%Y %I:%M:%S %p')
  cleaned_frame <- arrange(uncleaned_frame, ARR)
  return(cleaned_frame)
}
